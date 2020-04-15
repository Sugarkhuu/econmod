rm(list = ls())
library(data.table)
library(tidyverse)
library(forecast)
library(lubridate)
library(zoo)
library(leaflet)
library(dygraphs)
library(dygraphs,warn.conflicts = F)
library(xts, warn.conflicts = F)

fix_mixed_sessions = function(df,seq_mean){
  print(sprintf('Starting fix_mixed_sessions ..................................'))
  df = df %>% ungroup() %>% arrange(group_old, RECORD_DATE) %>% group_by(group_old) %>% mutate(hat_diff = GETON_CNT_hat-lag(GETON_CNT_hat))
  bad_estimates = df %>% ungroup() %>% filter(hat_diff<0, empty==T) %>% select(index, hat_diff, group_old)
  print(sprintf('Number of bad_groups: %i',nrow(bad_estimates)))
  if(nrow(bad_estimates)>0){
    bad_groups = unique(bad_estimates$group_old)
    for(i in 1:length(bad_groups)){
      bad_group = bad_groups[i]
      bad_route_id  = df$BUSROUTE_ID[df$group_old==bad_group][1]
      bad_seq_first = df$BUSSTOP_SEQ[df$group_old==bad_group][1]
      bad_stop_means = df %>% ungroup() %>% filter(BUSROUTE_ID==bad_route_id & BUSSTOP_SEQ==bad_seq_first) %>% select(stop_mean)
      skip_seqs = seq_mean %>% ungroup() %>% filter(BUSROUTE_ID==bad_route_id & BUSSTOP_SEQ<bad_seq_first)
      skip_add = sum(skip_seqs$w)
      bad_max = df$max[df$group_old==bad_group]
      df$min[df$group_old==bad_group] = bad_max*(1-skip_add)
    }
    print(sprintf('Finished fix_mixed_sessions ..................................'))
  } else {
    print('No mixed sessions. Returning. ')
  }
  return(df)
}

fix_skipped_sessions = function(df,seq_mean){
  print(sprintf('Starting fix_skipped_sessions ..................................'))
  df = df %>% ungroup() %>% arrange(group, RECORD_DATE) %>% group_by(group) %>% 
    mutate(seq_diff = BUSSTOP_SEQ-lag(BUSSTOP_SEQ)) %>% ungroup()          
  skipped = df %>% filter(seq_diff != 1, empty==T) %>% filter(BUSSTOP_SEQ!=1)
  print(sprintf('Number of stops skipped: %i',nrow(skipped)))
  if(nrow(skipped)>0){
    for(i in 1:nrow(skipped)){
      skip_group    = skipped$group[i]
      skip_route    = skipped$BUSROUTE_ID[i]
      skip_jump_seq = skipped$BUSSTOP_SEQ[i]
      seq_prev = skip_jump_seq-skipped$seq_diff[i]
      jump_ind = which(df$group==skip_group & df$BUSSTOP_SEQ==skip_jump_seq & df$seq_diff!=1)
      skip_seqs = seq_mean %>% ungroup() %>% filter(BUSROUTE_ID==skip_route & BUSSTOP_SEQ<skip_jump_seq & BUSSTOP_SEQ>seq_prev)
      skip_add = sum(skip_seqs$w)
      df[jump_ind,'w'] = df[jump_ind,'w'] +  skip_add
    }
    print(sprintf('Finished fix_skipped_sessions ..................................'))
  } else{
    print('No mixed sessions. Returning. ')
  }
  return(df)
}

gen_features <- function(df){
  df <- df %>% arrange(BUS_ID) %>%
    mutate(RECORD_DATE = ymd_hms(RECORD_DATE),
           day         = day(RECORD_DATE),
           hour        = hour(RECORD_DATE),
           weekday     = weekdays(RECORD_DATE),
           ymd         = format(as.Date(RECORD_DATE), "%Y-%m-%d"))
  return(df) 
}

# tic()         
loadData <- function(train_path, test_path, fixed_path){
  input_path  <- "/home/sugarkhuu/Documents/Documents/my/machineLearning/phase2/data/"
  train       <- fread(paste(input_path, train_path,sep=""))
  test        <- fread(paste(input_path, test_path,sep=""))
  fixed       <- fread(paste(input_path, fixed_path,sep=""))
  # Append the main dataframes with the routes
  test        <- test   %>% left_join(fixed, 
                                      by = c('BUSSTOP_ID','BUSROUTE_ID','BUSSTOP_SEQ'))
  train       <- train  %>% left_join(fixed, 
                                      by = c('BUSSTOP_ID','BUSROUTE_ID','BUSSTOP_SEQ'))
  return(list(train,test,fixed))
}

give_me = function(df, type) {
  if(type == 'mean' | type == 1){
    result <- df %>% group_by(BUSROUTE_ID, BUSSTOP_SEQ) %>% 
      summarise(stop_mean = mean(GETON_CNT_ADD, na.rm = T)) %>% ungroup() %>% 
      group_by(BUSROUTE_ID) %>% mutate(w_raw = stop_mean/sum(stop_mean)) %>% 
      mutate(w = ifelse(w_raw>0.2,0.2,w_raw)) %>%     
      ungroup()
    # near max stop_seq, there might be mistaken weights
    # result_seq <- df %>% group_by(BUSROUTE_ID) %>%
    #   summarise(max_seq = round(max(BUSSTOP_SEQ, na.rm = T)/2, digits = 0)) %>% ungroup() %>% select(BUSROUTE_ID,max_seq)
    # result = merge(x =result, y = result_seq, by = "BUSROUTE_ID", all = TRUE)
    # result$w[result$BUSSTOP_SEQ-result$max_seq==1] = result$w[result$BUSSTOP_SEQ-result$max_seq==1] - 0.05
    # result$w[result$BUSSTOP_SEQ-result$max_seq==0] = result$w[result$BUSSTOP_SEQ-result$max_seq==0] - 0.05
    # result$w[result$BUSSTOP_SEQ-result$max_seq==-1] = result$w[result$BUSSTOP_SEQ-result$max_seq==-1] + 0.1
    }
  # downing from 0.2 to 0.15. not diff. >0.1 0.1 improves marginally. for >0.2 to 0.2 9.
  else if(type == 'median'| type == 2){
    result <- df %>% group_by(BUSROUTE_ID, BUSSTOP_SEQ) %>% 
      summarise(stop_mean = median(GETON_CNT_ADD, na.rm = T)) %>% ungroup() %>% 
      group_by(BUSROUTE_ID) %>% mutate(w_raw = stop_mean/sum(stop_mean)) %>% 
      mutate(w = ifelse(w_raw>0.2,0.1,w_raw)) %>%     
      ungroup()}
  else if(type == 'mode'| type == 3){
    result <- df %>% group_by(BUSROUTE_ID, BUSSTOP_SEQ) %>% 
      summarise(stop_mean = Mode(GETON_CNT_ADD, na.rm = T)) %>% ungroup() %>% 
      group_by(BUSROUTE_ID) %>% mutate(w_raw = stop_mean/sum(stop_mean)) %>% 
      mutate(w = ifelse(w_raw>0.2,0.1,w_raw)) %>%     
      ungroup()
  }else{
    result <- print('Use only three of them: mean or median or mode. Also 1 or 2 or 3')
  }
  return(result)
}

weight_interp <- function(df,seq_mean){
  print(sprintf('Starting estimation ..................................'))
  df = df %>% group_by(BUSROUTE_ID, group) %>%
    mutate(w_g = w / sum(w)) %>% ungroup()
  df$w_g[is.nan(df$w_g)] = 0  # Note: Can be 0.05 or sth like that? # 0.15 was worse than 0
  df   = df %>% mutate(incr = (max-min)*w_g)
  df   = df %>% group_by(group) %>% 
    mutate(incr_cum = cumsum(incr)+min) %>% ungroup()
  df   = df %>% mutate(GETON_CNT_hat = 
                         ifelse(!is.na(GETON_CNT),GETON_CNT,
                                incr_cum))
  # df   = fix_mixed_sessions(df, seq_mean)
  # df   = fix_skipped_sessions(df, seq_mean)
  print(sprintf('Finished estimation ..................................'))
  return(df)
}

# Load data and generate features -----------------------------------------------------------
train_path <- 'training_phase2.csv'
test_path  <- 'test_phase2.csv'
fixed_path <- 'fixed_routes.csv'

res        <- loadData(train_path, test_path, fixed_path)
train      <- res[[1]]
test       <- res[[2]]
fixed      <- res[[3]]

smp_size <- floor(0.5 * nrow(train))

## set the seed to make your partition reproducible
set.seed(542)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

# train_t <- train[train_ind, ]
test_t <- train[-train_ind, ]
test = test_t
smp_size_t <- floor(0.75 * nrow(test))
test_ind <- sample(seq_len(nrow(test)), size = smp_size_t)
test$real = test$GETON_CNT

# test= test %>% filter(BUSSTOP_SEQ>lag(BUSSTOP_SEQ) & GETON_CNT>lag(GETON_CNT))

test$GETON_CNT[test_ind] = NA
test$GETON_CNT[1] = test_t$GETON_CNT[1]
test$GETON_CNT[nrow(test_t)] = test_t$GETON_CNT[nrow(test_t)]
#test$GETON_CNT[test$index ==2899085] = test_t$GETON_CNT[test$index ==2899085]

train      <- gen_features(train)
test       <- gen_features(test)

train <- train %>% arrange(BUS_ID, RECORD_DATE) %>% group_by(BUS_ID,ymd) %>% 
  mutate(GETON_CNT_ADD = GETON_CNT-lag(GETON_CNT)) %>% ungroup() %>% 
  mutate(GETON_CNT_ADD = ifelse(GETON_CNT_ADD <0, 2, GETON_CNT_ADD))%>% 
  mutate(GETON_CNT_ADD = ifelse(is.na(GETON_CNT_ADD), ifelse(BUSSTOP_SEQ==1,GETON_CNT,5), GETON_CNT_ADD)) %>%
  ungroup()

# if negative making worse. if nan also making worse
test <- test %>% group_by(BUS_ID,ymd) %>%  arrange(BUS_ID, RECORD_DATE) %>%  ungroup()

seq_mean = give_me(train ,type = 'mean')
# seq_mean_adj = give_me(train ,type = 'mean')
# seq_mean$stop_mean[seq_mean$stop_mean<0.1] = seq_mean_adj$stop_mean[seq_mean$stop_mean<0.1]

test = test %>% left_join(seq_mean, 
                          by = c('BUSROUTE_ID','BUSSTOP_SEQ'))

test$GETON_CNT[1] = test_t$GETON_CNT[1]
test$GETON_CNT[nrow(test)] = test_t$GETON_CNT[nrow(test_t)]

test$interp = na.approx(test$GETON_CNT)
test$min = na.locf(test$GETON_CNT)
test$max = na.locf(test$GETON_CNT, fromLast = T)
test$GETON_CNT = ifelse(is.na(test$GETON_CNT) & test$max == test$min,
                        test$max, 
                        test$GETON_CNT)
test$empty     = is.na(test$GETON_CNT)
test$empty_lag = lag(test$empty)
test$empty_ind = ifelse(test$empty == F & test$empty_lag == F,-1,test$max)
# test$group     = rleid(test$empty_ind)
test$group     = rleid(test$empty_ind)
test$group_old = rleid(test$empty)  ### don't delete - is used!!!
# 3. Estimation part ------------------------------------------------
# run it two times: fixes should be done only by running again. See messages. 
# res_df     = weight_interp(test,seq_mean)
# res_df     = weight_interp(res_df,seq_mean) ### now use res_df
# 5. Whether travel time affects. If a bus takes too long to arrive, then more people vice versa
# time delay versus geton_add regression
train <- train %>% arrange(BUS_ID, RECORD_DATE) %>% group_by(BUS_ID,ymd) %>% 
  mutate(travel_time = as.numeric(RECORD_DATE-lag(RECORD_DATE),units="secs")) %>% ungroup() %>% 
  mutate(travel_time = ifelse(travel_time <0, 180, travel_time))%>% 
  mutate(travel_time = ifelse(is.na(travel_time), 180, travel_time)) %>%
  ungroup()
test <- test %>% arrange(BUS_ID, RECORD_DATE) %>% group_by(BUS_ID,ymd) %>% 
  mutate(travel_time = as.numeric(RECORD_DATE-lag(RECORD_DATE),units="secs")) %>% ungroup() %>% 
  mutate(travel_time = ifelse(travel_time <0, 180, travel_time))%>% 
  mutate(travel_time = ifelse(is.na(travel_time), 180, travel_time)) %>%
  ungroup()

## improves a bit by 150 rather than 0. very little, -1e2

seq_time_mean = train %>% group_by(BUSROUTE_ID, BUSSTOP_SEQ) %>% 
  summarise(mean_time = mean(travel_time))
test %>% group_by(BUSROUTE_ID, BUSSTOP_SEQ) %>% 
  summarise(mean_time = mean(travel_time))
train = train %>% left_join(seq_mean, 
                            by = c('BUSROUTE_ID','BUSSTOP_SEQ'))
train = train %>% left_join(seq_time_mean, 
                            by = c('BUSROUTE_ID','BUSSTOP_SEQ'))
test = test %>% left_join(seq_time_mean, 
                          by = c('BUSROUTE_ID','BUSSTOP_SEQ'))
train$demeaned_geton = train$GETON_CNT_ADD/train$stop_mean
train$demeaned_travel_time = train$travel_time/train$mean_time
short_train = train %>% filter(demeaned_travel_time<2)
# short_train = train %>% filter(demeaned_travel_time>0.5) - worsening by 0.2
# trying down from 10 to 5-demaned travel time. 9.76 -> 9.73 -> 9.71 with <2
# short_train = short_train[1:5000,]
# short_train %>% ggplot(aes(demeaned_travel_time,demeaned_geton))+geom_point() + geom_smooth(method = "lm")
model = lm(demeaned_geton~demeaned_travel_time,data=short_train)
beta = model$coefficients[[2]]
test$demeaned_travel_time = test$travel_time/test$mean_time
test$demeaned_travel_time[is.na(test$demeaned_travel_time)] <- 1
test$demeaned_travel_time[test$demeaned_travel_time>2] <- 2
test$demeaned_travel_time[test$demeaned_travel_time<0.5] <- 1
#changing from 1.5 to 1 improves by 0.03. 10.03 -> 9.76 when turning off >1.5 part
test$w[is.na(test$w)] <- 0
test$w2 = test$w + test$w*(test$demeaned_travel_time-1)*beta
test$w = test$w2

res_df = weight_interp(test,seq_mean)

### real submission part

# res_df = weight_interp(res_df,seq_mean)
# 
# # res_df = res_df %>% mutate(GETON_CNT_hat = ifelse(GETON_CNT_hat>max, max, GETON_CNT_hat))
# # res_df = res_df %>% mutate(GETON_CNT_hat = ifelse(GETON_CNT_hat<min, min, GETON_CNT_hat))
# 
# 
# res_df = res_df %>% mutate(GETON_CNT_hat = ifelse(GETON_CNT_hat>max, max, GETON_CNT_hat))
# res_df = res_df %>% mutate(GETON_CNT_hat = ifelse(GETON_CNT_hat<min, min, GETON_CNT_hat))
# submission <- res_df %>% ungroup %>% select(index, GETON_CNT_hat)
# submission$GETON_CNT_hat = na.approx(submission$GETON_CNT_hat,method='linear')
# names(submission)[names(submission) == "GETON_CNT_hat"] <- "GETON_CNT"
# submission = submission %>% arrange(index)
# fwrite(submission,'submission.csv')
# 
# 
# 
# res_138 = fread('/home/sugarkhuu/Documents/Documents/my/machineLearning/phase2/data/139.csv')
# # # # colnames(res_df)
# # # 
# res_df_new = res_df
# res_df_new = res_df %>% arrange(index)
# # res_df_new$geton_mean = mean$GETON_CNT
# # res_df_new$geton_median = median$GETON_CNT
# res_df_new$geton_138 = res_138$GETON_CNT
# # # 
# plot_result = function(df, first, length){
#   # see = df %>% select(RECORD_DATE, GETON_CNT,GETON_CNT_hat)
#   # see = df %>% select(RECORD_DATE, GETON_CNT,mean_geton)
#   see = df %>% select(RECORD_DATE, GETON_CNT,interp, GETON_CNT_hat, geton_138, min, max)
#   i = first
#   see_n = see[i:(i+length),]
#   see_n = see_n %>% select(-RECORD_DATE)
#   see_n =xts(see_n, order.by = seq(as.Date('2000-01-01'),
#                                    as.Date('2000-01-01')+nrow(see_n)-1,
#                                    by = 1))
#   dygraph(see_n)
# }
# # # 
# # # res_df_plot = res_df_new %>% filter(BUSROUTE_ID == 11100010)
# # # train_plot = train %>% group_by(BUSROUTE_ID, BUSSTOP_SEQ) %>% mutate(mean_geton =median(GETON_CNT)) %>% ungroup()%>%filter(BUSROUTE_ID == 11100010)
# plot_result(res_df_new,1,200)
# # # 
# # # 
# # # 
# res_df_new$diff = res_df_new$GETON_CNT_hat - res_df_new$geton_138
# neg = res_df_new %>% filter(diff < 0)
# pos = res_df_new %>% filter(diff > 0)
# mean(neg$diff)
# mean(pos$diff)
# min(neg$diff)
# max(pos$diff)
# # 
# mean(abs(res_df_new$GETON_CNT_hat - res_df_new$geton_138),na.rm=T)
# # 
# # # orig_mod = res_df
# # # mean(abs(res_df$GETON_CNT_hat - res_df$real))

## training part

plot_result = function(df, first, length){
  # see = df %>% select(RECORD_DATE, GETON_CNT,GETON_CNT_hat)
  see = df %>% select(RECORD_DATE, GETON_CNT,real, GETON_CNT_hat, min, max)
  # see = df %>% select(RECORD_DATE, GETON_CNT,interp, GETON_CNT_hat, geton_mean, geton_median, geton_140, min, max)
  i = first
  see_n = see[i:(i+length),]
  see_n = see_n %>% select(-RECORD_DATE)
  see_n =xts(see_n, order.by = seq(as.Date('2000-01-01'),
                                   as.Date('2000-01-01')+nrow(see_n)-1,
                                   by = 1))
  dygraph(see_n)
}

# res_df_plot = res_df_new %>% filter(BUSROUTE_ID == 11100010)
# train_plot = train %>% group_by(BUSROUTE_ID, BUSSTOP_SEQ) %>% mutate(mean_geton =median(GETON_CNT)) %>% ungroup()%>%filter(BUSROUTE_ID == 11100010)
ressss = res_df
res_df_plot = res_df %>% filter(BUSROUTE_ID == 11100510)
plot_result(res_df,10000,1000)

mean(abs(res_df$GETON_CNT_hat - res_df$real),na.rm=T)


