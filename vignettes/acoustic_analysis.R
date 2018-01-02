## ------------------------------------------------------------------------
library(soundgen)
s1 = soundgen(sylLen = 900, temperature = 0,
              pitchAnchors = list(time = c(0, .3, .8, 1), 
                                  value = c(300, 900, 400, 2300)),
              noiseAnchors = c(-40, 0), subDep = 100, 
              jitterDep = 0.5, nonlinBalance = 100)
# playme(s1)  # replay as many times as needed w/o re-synthesizing the sound

## ------------------------------------------------------------------------
true_pitch = getSmoothContour(anchors = list(time = c(0, .3, .8, 1),
                                             value = c(300, 900, 400, 2300)),
                              len = 1000)  # any length will do
median(true_pitch)  # 611 Hz

