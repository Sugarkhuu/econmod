function stddev = std_gen(m, mult, rng)
  stddev = struct();
  enames = get(m,'enames');
  for i = 1:numel(enames)
    stddev.(['std_' enames{i}]) = tseries(rng, 1);
  end
  stddev = dboverlay(stddev, mult);
  p = get(m,'parameters');

  iver = iris_version_num();
  if iver >= 20170000
    stddev = dbbatch(stddev,'$0','p.$0*stddev.$0','fresh=',true);
  else
    stddev = dbbatch(stddev,'std_$0','p.std_$0*stddev.std_$0','namelist', ...
                     enames,'merge',false);
  end

end