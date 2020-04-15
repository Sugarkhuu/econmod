function [mNew,paramNew,stdNew,a,b,mult]=find_shock_mult(a,b,eps,N,param,model,dd,range,std_list,std)
    % calcultes common multiplier that scales the standard deviations of the shocks
    % inputs:
    % a        minimum value of the starting interval
    % b        maximum value of the starting interval
    % eps      tolerance for terminating the algorithm
    % N        maximum number of iterations
    % param    parameters
    % model    model
    % dd       database (model data)
    % range    range for estimation (historical)
    % std_list list of shocks
    % std      database of standard deviations
    % outputs:
    % nNew     model with rescaled stds
    % paramNew paramters with rescaled stds
    % stdNew   rescaled standard deviations
    % a,b      final intervals of multipliers
    % mult     final common multiplier
    
      % Store the initial search intervall
      int_min = a;
      int_max = b;
      c = (-1+sqrt(5))/2;
      x1 = c*a + (1-c)*b;
      x2 = (1-c)*a + c*b;
    
      pp=dbbatch(std,'$0','x1*std.$0','namelist',std_list);
      p=dbbatch(param,'$0','x1*param.$0','namelist',std_list);
      m = assign(model, p);
      m = sstate(m);
      m = solve(m);
      [fx1] = loglik(m,dd,range,'deviation',false,'std',pp,'relative',false);
    
      pp=dbbatch(std,'$0','x2*std.$0','namelist',std_list);
      p=dbbatch(param,'$0','x2*param.$0','namelist',std_list);
      m = assign(model, p);
      m = sstate(m);
      m = solve(m);
      [fx2] = loglik(m,dd,range,'deviation',false,'std',pp,'relative',false);
    
      fprintf('------------------------------------------------------\n');
      fprintf(' x1 x2 f(x1) f(x2) b - a\n');
      fprintf('------------------------------------------------------\n');
      fprintf('%.4e %.4e %.4e %.4e %.4e\n', x1, x2, fx1, fx2, b-a);
    
      for i = 1:N-2
          if fx1 < fx2
              b = x2;
              x2 = x1;
              fx2 = fx1;
              x1 = c*a + (1-c)*b;
              pp=dbbatch(std,'$0','x1*std.$0','namelist',std_list);
              p=dbbatch(param,'$0','x1*param.$0','namelist',std_list);
              m = assign(model, p);
              m = sstate(m);
              m = solve(m);
              [fx1] = loglik(m,dd,range,'deviation',false,'std',pp,'relative',false);
          else
              a = x1;
              x1 = x2;
              fx1 = fx2;
              x2 = (1-c)*a + c*b;
              pp=dbbatch(std,'$0','x2*std.$0','namelist',std_list);
              p=dbbatch(param,'$0','x2*param.$0','namelist',std_list);
              m = assign(model, p);
              m = sstate(m);
              m = solve(m);
              [fx2] = loglik(m,dd,range,'deviation',false,'std',pp,'relative',false);
          end;
          fprintf('%.4e %.4e %.4e %.4e %.4e\n', x1, x2, fx1, fx2, b-a);
          if (abs(b-a) < eps)
              fprintf('succeeded after %d steps\n', i);
              if     abs(a-int_min)<eps
                 error('The optimum search has stopped on the lower boundary.Please adjust the search intervall by attributes multmin and multmax.');
              elseif abs(a-int_max)<eps
                  error('The optimum search has stopped on the upper boundary.Please adjust the search intervall by attributes multmin and multmax');
              end
              mult=(a+b)/2;
              stdNew=dbbatch(std,'$0','mult*std.$0','namelist',std_list);
              paramNew=dbbatch(param,'$0','mult*param.$0','namelist',std_list);
              mNew = assign(m,paramNew);
              mNew = sstate(mNew);
              mNew = solve(mNew);
              stdvec = get(mNew, 'stdvec');
              enames = get(mNew, 'elist');
              for ii=1:numel(enames)
                  stdvec(ii) = paramNew.(['std_' enames{ii}]);
              end
              set(mNew, 'stdvec', stdvec);
              return;
          end;
      end;
      fprintf('failed requirements after %d steps\n', N);
    end
    
    