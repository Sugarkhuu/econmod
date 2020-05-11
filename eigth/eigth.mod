!variables:transition
'Output (log)'                     l_y
'Potential output (log)'           l_y_tnd
l_y_gap
e_l_y_gap
dl_y_tnd
y

cpi
l_cpi
dl_cpi

cpi_f
l_cpi_f
dl_cpi_f
e_dl_cpi_f

cpi_xf
l_cpi_xf
dl_cpi_xf
e_dl_cpi_xf
dl_disc

rmci
d4l_cpi
rmc_xf
rmc_f

l_z_gap
l_z
l_z_tnd
dl_z_tnd

l_rp_gap
l_rp
l_rp_tnd
dl_r_p_tnd
dev_cpi
l_mnt_usd
e_l_mnt_usd
prem
e_dl_cpi_tar
dl_cpi_tar

r
i
i_tnd
r_tnd
r_gap
r_us_tnd

i_us
i_us_tnd
l_cpi_foreign
dl_cpi_foreign
dl_cpi_foreign_tar



!shocks:transition
shock_l_y_gap, shock_dl_y_tnd
shock_dl_cpi_f, shock_dl_cpi_xf, shock_dl_disc

shock_i
shock_dl_r_p_tnd
shock_dl_cpi_foreign_tar
shock_dl_cpi_foreign

shock_l_mnt_usd
shock_dl_z_tnd
shock_prem
shock_l_z_tnd
shock_r_tnd
shock_dl_cpi_tar

!variables:log
y
obs_y
cpi
obs_cpi
cpi_f
obs_cpi_f
cpi_xf
obs_cpi_xf


!parameters
c1_l_y_gap
c2_l_y_gap
c3_l_y_gap
c1_dl_y_tnd
ss_dl_y_tnd

c_rmci


c_lw_f
c1_dl_cpi_f
c1_dl_cpi_xf
c4_dl_cpi_f
c4_dl_cpi_xf

c5_dl_cpi_f
c5_dl_cpi_xf
ss_dl_cpi_tar

c1_rmc_f
c1_rmc_xf

c1_dl_r_p_tnd

c_i
c2_i
c3_i

c1_dl_z_tnd
c1_prem
ss_prem
ss_dl_z_tnd
ss_dl_r_p_tnd

c1_dl_cpi_foreign

c1_r_us_tnd
ss_r_us_tnd

c1_dl_cpi_foreign_tar
ss_dl_cpi_foreign_tar

c1_dl_cpi_tar


!equations:transition

'I. IS curve'

l_y_gap =  c1_l_y_gap*l_y_gap{-1}
         + c2_l_y_gap*e_l_y_gap
         - c3_l_y_gap*rmci{-1}
%        + l_y_foreign_gap
         + shock_l_y_gap;

l_y = l_y_tnd + l_y_gap;
l_y = 100*log(y);
e_l_y_gap = l_y_gap{+1};

dl_y_tnd = 4*(l_y_tnd - l_y_tnd{-1}); % QoQ @ar
dl_y_tnd = c1_dl_y_tnd*dl_y_tnd{-1} + (1-c1_dl_y_tnd)*ss_dl_y_tnd + shock_dl_y_tnd;

rmci = c_rmci*(-l_z_gap) + (1-c_rmci)*r_gap;

'II. Phillips curve'


'Overall inflation'
dl_cpi = c_lw_f*dl_cpi_f + (1-c_lw_f)*dl_cpi_xf + dl_disc;
dl_disc = shock_dl_disc;

'Food PC'
dl_cpi_f = c1_dl_cpi_f*dl_cpi_f{-1}
           + (1-c1_dl_cpi_f)*e_dl_cpi_f
%          + c2_dl_cpi_f*dl_food_im_f{-1}
%          + c3_dl_cpi_f*dl_cpi_foreign_im_f
           + c4_dl_cpi_f*rmc_f
           + shock_dl_cpi_f;

'Non-Food PC'
dl_cpi_xf = c1_dl_cpi_xf*dl_cpi_xf{-1}
           + (1-c1_dl_cpi_xf)*e_dl_cpi_xf
%          + c2_dl_cpi_xf*dl_oil_im_f{-1}
%          + c3_dl_cpi_xf*dl_cpi_foreign_im_xf
           + c4_dl_cpi_xf*rmc_xf
           + shock_dl_cpi_xf;


e_dl_cpi_f  = dl_cpi_f{+1};
e_dl_cpi_xf = dl_cpi_xf{+1};


rmc_f = (1-c1_rmc_f)*l_y_gap + c1_rmc_f*(l_z_gap - (1-c_lw_f)*l_rp_gap);
rmc_xf = (1-c1_rmc_xf)*l_y_gap + c1_rmc_xf*(l_z_gap + c_lw_f*l_rp_gap);

% F/XF Relative prices
l_rp = l_cpi_f - l_cpi_xf;
l_rp = l_rp_gap + l_rp_tnd;
dl_r_p_tnd = (l_rp_tnd - l_rp_tnd{-1})*4;
dl_r_p_tnd = c1_dl_r_p_tnd*dl_r_p_tnd{-1} + (1-c1_dl_r_p_tnd)*ss_dl_r_p_tnd + shock_dl_r_p_tnd;

'CPI identities'

dl_cpi = 4*(l_cpi - l_cpi{-1});
dl_cpi_f = 4*(l_cpi_f - l_cpi_f{-1});
dl_cpi_xf = 4*(l_cpi_xf - l_cpi_xf{-1});

l_cpi = 100*log(cpi);
l_cpi_f = 100*log(cpi_f);
l_cpi_xf = 100*log(cpi_xf);
d4l_cpi = l_cpi - l_cpi{-4};

'III. Taylor rule or Monetary policy'

i = c_i*i{-1}  + (1-c_i)*(i_tnd + c2_i*dev_cpi + c3_i*l_y_gap) + shock_i;

i_tnd = r_tnd + dl_cpi_tar;
dev_cpi = dl_cpi{+1} - e_dl_cpi_tar;
e_dl_cpi_tar = dl_cpi_tar{+1};
dl_cpi_tar = c1_dl_cpi_tar*dl_cpi_tar{-1} + (1-c1_dl_cpi_tar)*ss_dl_cpi_tar + shock_dl_cpi_tar;


'IV. UIP condition'

l_mnt_usd = e_l_mnt_usd - (i - i_us - prem)/4 + shock_l_mnt_usd;
e_l_mnt_usd = l_mnt_usd{+1};

r = i - d4l_cpi{+4};
r = r_tnd + r_gap;

r_tnd = r_us_tnd + dl_z_tnd + prem + shock_r_tnd; % real UIP

l_z = l_mnt_usd - l_cpi + l_cpi_foreign;
l_z = l_z_tnd + l_z_gap;

l_z_tnd = l_z_tnd{-1} + dl_z_tnd/4+shock_l_z_tnd; 
dl_z_tnd = c1_dl_z_tnd*dl_z_tnd{-1} + (1-c1_dl_z_tnd)*ss_dl_z_tnd + shock_dl_z_tnd;

prem = c1_prem*prem{-1} + (1-c1_prem)*ss_prem + shock_prem;


% foreign variables

dl_cpi_foreign = 4*(l_cpi_foreign - l_cpi_foreign{-1}); 
dl_cpi_foreign = c1_dl_cpi_foreign*dl_cpi_foreign{-1} + (1-c1_dl_cpi_foreign)*dl_cpi_foreign_tar + shock_dl_cpi_foreign;

r_us_tnd = c1_r_us_tnd*r_us_tnd{-1} + (1-c1_r_us_tnd)*ss_r_us_tnd;

i_us = i_us_tnd;
i_us_tnd = r_us_tnd + dl_cpi_foreign_tar{+1};
dl_cpi_foreign_tar = c1_dl_cpi_foreign_tar*dl_cpi_foreign_tar{-1} + (1-c1_dl_cpi_foreign_tar)*ss_dl_cpi_foreign_tar + shock_dl_cpi_foreign_tar;

!variables:measurement
obs_y
obs_cpi
obs_cpi_f
obs_cpi_xf
obs_dl_cpi_foreign

!equations:measurement
obs_y = y;
obs_cpi    = cpi;
obs_cpi_f  = cpi_f;
obs_cpi_xf = cpi_xf;
obs_dl_cpi_foreign = dl_cpi_foreign;
