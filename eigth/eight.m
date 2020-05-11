%% Preliminary

% add IRIS to the path
cd /home/sugarkhuu/Documents/Documents/my/modelling/econmod/sixth
addpath /home/sugarkhuu/Documents/IRIS-Toolbox-Release-20180319

% configure IRIS
irisstartup

disp('Forecast starts ...');
p   = struct();
sixth_param;


m = model('sixth.mod','linear',true,'assign',p);
m = solve(m);
ms = sstate(m);


d=struct();
d.obs_y = tseries();

d.obs_y(qq(2018,1):qq(2019,4)) = ...
[100.546316773439
105.781594142913
92.1793414415762
101.232565810543
105.596889704456
106.254394062807
109.114767757587
111.560505992009];

cpi_data = [100	100	100
101.366986500287	100.864580613362	101.582303308969
101.847342558696	97.9158561771647	103.532265293638
102.408156908953	97.9635639640592	104.312982456765
103.007698426008	100.81471876577	103.947546851825
103.211177534037	102.44253091977	103.54059751158
nan	102.742431439906	102.908362345795
108.458370839468	106.184341138713	nan];

d.obs_cpi    = tseries();
d.obs_cpi_f  = tseries();
d.obs_cpi_xf = tseries();
d.obs_dl_cpi_foreign = tseries();

d.obs_cpi(qq(2018,1):qq(2019,4))    = cpi_data(:,1);
d.obs_cpi_f(qq(2018,1):qq(2019,4))  = cpi_data(:,2);
d.obs_cpi_xf(qq(2018,1):qq(2019,4)) = cpi_data(:,3);
d.obs_dl_cpi_foreign(qq(2018,1):qq(2019,4)) = NaN;

[~, f, v, ~, pe, co] = filter(m, d, qq(2018,1):qq(2019,4)+40);
a=f.mean;
keyboard
