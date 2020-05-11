function myPlot(varargin)

myColorMap = [ ...
0    0.4470    0.7410;
    0.8500    0.3250    0.0980;
    0.9290    0.6940    0.1250;
    0.4940    0.1840    0.5560;
    0.4660    0.6740    0.1880;
    0.3010    0.7450    0.9330;
    0.6350    0.0780    0.1840];

tmp = struct(varargin{:});
nvars = size(tmp.bar,2);

X = tmp.bar(tmp.range);
Xneg = X;
Xneg(Xneg>0) = 0;
Xpos = X;
Xpos(Xpos<0) = 0;
line = tmp.line(tmp.range);

set(gca, 'ColorOrder', [myColorMap(1:nvars,:);myColorMap(1:nvars,:)],'NextPlot', 'replacechildren');

hp = bar(Xneg,'stack');
hold on
hn = bar(Xpos,'stack');
hl = plot(line,'linewidth',2.5);
hold off

end