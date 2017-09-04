# When generating an integer random walk with getIntegerRandomWalk(), we need
# some kind of thresholds for activating different regimes of pitch effects.
# Here we set these thresholds.
slope_q1 = -.1
midpoint_q1 = 33
slope_q2 = -.1
midpoint_q2 = 66

noiseThresholdsDict = list(pitchEffects_amount = 0:100,
                           q1 = NA,
                           q2 = NA)
noiseThresholdsDict$q1 = 100 / (1 + exp(
  -slope_q1 * (noiseThresholdsDict$pitchEffects_amount - midpoint_q1)
))
noiseThresholdsDict$q2 = 100 / (1 + exp(
  -slope_q2 * (noiseThresholdsDict$pitchEffects_amount - midpoint_q2)
))
# plot (noiseThresholdsDict$pitchEffects_amount, noiseThresholdsDict$q1, type='l', col='red')
# points (noiseThresholdsDict$pitchEffects_amount, noiseThresholdsDict$q2, type='l', col='blue')
