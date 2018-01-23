## ------------------------------------------------------------------------
playback = c(TRUE, FALSE)[2]

## ------------------------------------------------------------------------
library(soundgen)
s = soundgen(play = playback)  # default sound: a short [a] by a male speaker
# 's' is a numeric vector - the waveform. You can save it, play it, plot it, ...

# names(presets)  # speakers in the preset library
# names(presets$Chimpanzee)  # presets per speaker
s = eval(parse(text = presets$Chimpanzee$Scream_conflict))  # a screaming chimp
# playme(s)

## ------------------------------------------------------------------------
s = soundgen(formants = 'uai', repeatBout = 1, nSyl = 3, play = playback)
# to replay without re-generating the sound, type "playme(s)"

## ------------------------------------------------------------------------
s = soundgen(formants = 'uai', repeatBout = 3, nSyl = 1, play = playback)
# playme(s)

## ------------------------------------------------------------------------
sound = soundgen(pitchAnchors = 440, play = playback)  # steady pitch at 440 Hz
sound = soundgen(pitchAnchors = 3000:2000, play = playback)  # downward chirp
sound = soundgen(pitchAnchors = c(150, 250, 100), sylLen = 700, play = playback)  # up and down

## ----fig.width = 4, fig.height = 4---------------------------------------
anchors = (sin(1:70 / 3) * .25 + 1) * 350
plot(anchors, type = 'l', xlab = 'Time (points)', ylab = 'Pitch (Hz)')
sound = soundgen(pitchAnchors = anchors, sylLen = 1000, play = playback)

## ----fig.width = 4, fig.height = 4---------------------------------------
a = getSmoothContour(anchors = data.frame(time = c(0, .1, 1), 
                                          value = c(350, 700, 350)),
  len = 7000, thisIsPitch = TRUE, plot = TRUE, samplingRate = 3500)

## ------------------------------------------------------------------------
sound = soundgen(sylLen = 900, play = playback,
                 pitchAnchors = data.frame(time = c(0, .1, 1), 
                                           value = c(350, 700, 350)))

## ----fig.width = 5, fig.height = 5---------------------------------------
s = soundgen(sylLen = 400, plot = TRUE, play = playback,
             pitchAnchors = data.frame(time = c(0, 0.2, .201, .4, 1), 
                                       value = c(900, 1200, 1800, 2000, 1500)))

## ----fig.width = 5, fig.height = 5---------------------------------------
s = soundgen(nSyl = 5, sylLen = 200, pauseLen = 140, plot = TRUE, play = playback,
             pitchAnchors = data.frame(time = c(0, 0.65, 1), 
                                       value = c(977, 1540, 826)),
             pitchAnchorsGlobal = data.frame(time = c(0, .5, 1), 
                                             value = c(-6, 7, 0)))
# pitchAnchorsGlobal = c(-6, 7, 0) is equivalent, since time steps are equal

## ------------------------------------------------------------------------
# 5-Hz vibrato 1 semitone in depth
s1 = soundgen(vibratoDep = 1, vibratoFreq = 5, sylLen = 1000, play = playback,
              pitchAnchors = c(300, 280))

# slower (3 Hz) and deeper (3 semitones) vibrato
s2 = soundgen(vibratoDep = 3, vibratoFreq = 3, sylLen = 1000, play = playback,
              pitchAnchors = c(300, 280))

## ------------------------------------------------------------------------
# the sound is a bit different each time, because temperature is above zero
s = soundgen(repeatBout = 3, temperature = 0.3, play = playback)
# Setting repeatBout = 3 is equivalent to:
# for (i in 1:3) soundgen(temperature = 0.3, play = playback)

## ------------------------------------------------------------------------
# despite the high temperature, temporal structure does not vary at all, 
# while formants are more variable than the default
s = soundgen(repeatBout = 3, nSyl = 2, temperature = .3, play = playback,
             tempEffects = list(sylLenDep = 0, formDrift = 3))

## ------------------------------------------------------------------------
mf = c(-1,  # male: 100% lower f0, 25% lower formants, 25% longer vocal tract
       0,   # neutral (default)
       1)   # female: 100% higher f0, 25% higher formants, 25% shorter vocal tract
# See e.g. http://www.santiagobarreda.com/vignettes/v1/v1.html

for (i in mf) {
  s = soundgen(maleFemale = i, formants = NA, vocalTract = 17, play = playback)
  # Since `formants` are not specified, but temperature is above zero, a 
  # schwa-like sound with approximately equidistant formants is generated using
  # `vocalTract` (cm) to calculate the expected formant dispersion.
}

## ------------------------------------------------------------------------
cb = c(-1,  # max creaky
       -.5, # moderately creaky
       0,   # neutral (default)
       .5,  # moderately breathy
       1)   # max breathy (no tonal component)
for (i in cb) {
  soundgen(creakyBreathy = i, play = playback)
}

## ----fig.width = 5, fig.height = 5---------------------------------------
# each syllable has a 20-dB dip in the middle (note the dumb-bell shapes 
# in the oscillogram under the spectrogram), and there is an overall fade-out
# over the entire bout
s = soundgen(nSyl = 4, plot = TRUE, osc = TRUE, play = playback,
             amplAnchors = data.frame(time = c(0, .3, 1),  # unequal time steps
                                      value = c(80, 60, 80)), # same as 0, -20, 0
             amplAnchorsGlobal = c(80, 40))  # equal time steps, so shorthand is ok

## ----fig.width = 5, fig.height = 5---------------------------------------
s = soundgen(sylLen = 1000, formants = NA,
             # set the depth of AM (0% = none, 100% = max)
             amDep = c(0, 100),   
             # set AM frequency in Hz (vectorized)
             amFreq = c(50, 25),  
             # set the shape: 0 = close to sine, -1 = notches, +1 = clicks
             amShape = 0,  
             plot = TRUE, osc = TRUE, play = playback)

## ----fig.width = 7, fig.height = 3---------------------------------------
s = rnorm(500) * seq(1, 0, length.out = 500)
s1 = flatEnv(s, plot = TRUE, killDC = TRUE, windowLength_points = 50)

## ----fig.show = "hold", fig.width = 7, fig.height = 3--------------------
# Here is a sound with sharp attack
s = soundgen(sylLen = 1000, addSilence = 0, 
             attackLen = 10)  
# playme(s)
s1 = fade(s, fadeIn = 200, fadeOut = 300, samplingRate = 16000,
          shape = 'cos', plot = TRUE)
# playme(s1)
# different fades are available: linear, logarithmic, etc

## ------------------------------------------------------------------------
s = soundgen(formants = 'ai', play = playback)
s = soundgen(formants = 'aaai', play = playback)

## ------------------------------------------------------------------------
# shorthand specification with three stationary formants
formants = c(300, 2500, 3200)

# shorthand specification with two moving formants
formants = list(f1 = c(300, 900), f2 = c(2500, 1500))

# full specification with two moving formants and non-default amplitude and bandwidth
formants = list(
  f1 = data.frame(time = c(0, 1), 
                  freq = c(300, 900), 
                  amp = c(30, 10), 
                  width = 120),
  f2 = data.frame(time = c(0, 1), 
                  freq = c(2500, 1500), 
                  amp = 30, 
                  width = c(0, 240)))

## ----results = "hide"----------------------------------------------------
estimateVTL(formants = c(500, 1500, 2550))  # 17.5 cm
estimateVTL(formants = c(300, 1100, 2000))  # 22.8 cm

## ----fig.show = "hold", fig.width = 7, fig.height = 4--------------------
s1 = soundgen(formants = c(800, 1200), play = playback, plot = TRUE)
s1 = soundgen(formants = c(800, 1200), formantDepStoch = 0,
              play = playback, plot = TRUE)

## ----fig.show = "hold", fig.width = 7, fig.height = 4--------------------
s1 = soundgen(formants = NULL, vocalTract = 24, play = playback, plot = TRUE)
s1 = soundgen(formants = NULL, vocalTract = 12, play = playback, plot = TRUE)

## ----fig.width = 4, fig.height = 4---------------------------------------
# plotting directly from getSpectralEnvelope() in spectrogram form
s = getSpectralEnvelope(nr = 1024,  # freq bins in FFT frame (window_length / 2)
                        nc = 50,    # time bins
                        samplingRate = 16000, 
                        formants = formants,
                        plot = TRUE, 
                        dur = 1500,   # just an example
                        colorTheme = 'seewave',
                        lipRad = 6) 
# Note that lip radiation is also specified here (dB). 
# This has the effect of amplifying higher frequencies to mimic lip radiation. 

## ----fig.show = "hold", fig.width = 5, fig.height = 4--------------------
formants = list(
               f1   = data.frame(time = c(0, 1), freq = c(880, 900), 
                                 amp = c(30, 20), width = c(80, 120)), 
               f1.5 = data.frame(time = c(0, 1), freq = 600, 
                                 amp = c(0, 30), width = 80),   # additional pole
               f1.7 = data.frame(time=c(0, 1), freq = 750, 
                                 amp = c(0, -30), width = 80),  # zero
               f2   = data.frame(time = c(0, 1), freq = c(1480, 1250), 
                                 amp = c(30, 20), width = c(120, 200)), 
               f3   = data.frame(time=c(0, 1), freq = c(2900, 3100), 
                                 amp = 25, width = 200))
# se = getSpectralEnvelope(nr = 512, nc = 100, formants = formants, plot = TRUE)
s = soundgen(sylLen = 1500, play = playback, 
             pitchAnchors = 140, formants = formants)
spectrogram(s, samplingRate = 16000, ylim = c(0, 4), contrast = .5, 
     windowLength = 10, step = 5, colorTheme = 'seewave')
# long-term average spectrum (less helpful for moving formants but very good for stationary):
# seewave::meanspectrogram(s, f = 16000, wl = 256)  

## ----fig.width = 7, fig.height = 5---------------------------------------
s = soundgen(sylLen = 1200, play = playback, pitchAnchors = 140, 
             mouthAnchors = list(time = c(0, .3, .75, 1), 
                                 value = c(0, 0, .7, 0)))
spectrogram(s, samplingRate = 16000, 
            ylim = c(0, 4), contrast = .5, 
            windowLength = 10, step = 5, 
            colorTheme = 'seewave', osc = TRUE)

## ----fig.show = "hold", fig.width = 4, fig.height = 4--------------------
# strong F0, rolloff with a "shoulder"
r = getRolloff(rolloff = c(-5, -20),  # rolloff parameters are vectorized
               rolloffOct = -3,
               rolloffParab = -10, rolloffParabHarm = 13, 
               pitch_per_gc = c(170, 340), plot = TRUE)

# to generate the corresponding sound:
s = soundgen(sylLen = 1000, rolloff = c(-5, -20), rolloffOct = -3,
             rolloffParab = -10, rolloffParabHarm = 13,
             pitchAnchors = c(170, 340),  play = playback)

## ----fig.width = 7, fig.height = 3---------------------------------------
# Not a good idea: samplingRate is too low
s1 = soundgen(pitchAnchors = c(1500, 800), glottisAnchors = 75, 
              samplingRate = 16000, play = playback)

# This sounds better but takes a long time to synthesize:
s2 = suppressWarnings(soundgen(pitchAnchors = c(1500, 800), glottisAnchors = 75, 
              samplingRate = 80000, play = playback, 
              invalidArgAction = 'ignore'))
# NB: invalidArgAction = 'ignore' forces a "weird" samplingRate value
# to be accepted without question

# Now this is what this feature is meant for: vocal fry
s3 = soundgen(sylLen = 1500, pitchAnchors = c(75, 40), 
              glottisAnchors = c(0, 700), 
              samplingRate = 16000, play = playback)
plot(s3, type = 'l', xlab = '', ylab = '')

## ----fig.show = "hold", fig.width = 3, fig.height = 3--------------------
s1 = soundgen(subFreq = 400, subDep = 150, nonlinBalance = 100,
              jitterDep = 0, shimmerDep = 0, temperature = 0, 
              sylLen = 500, pitchAnchors = c(800, 900),
              play = playback, plot = TRUE, ylim = c(0, 3))
s2 = soundgen(subFreq = 400, subDep = 400, nonlinBalance = 100,
              jitterDep = 0, shimmerDep = 0, temperature = 0, 
              sylLen = 500, pitchAnchors = c(800, 900),
              play = playback, plot = TRUE, ylim = c(0, 3))

## ----fig.width = 5, fig.height = 5---------------------------------------
s = soundgen(subFreq = 75, subDep = 130, nonlinBalance = 100,
             jitterDep = 0, shimmerDep = 0, temperature = 0, 
             sylLen = 800, formants = NULL, play = playback,
             pitchAnchors = data.frame(time=c(0, .3, .9, 1), 
                                       value = c(1200, 1547, 1487, 1154)))
spectrogram(s, 16000, windowLength = 50, ylim = c(0, 5), contrast = .7)

## ------------------------------------------------------------------------
# To get jitter without subharmonics, set `temperature = 0, subDep = 0` 
# and specify the required jitter depth and period
s1 = soundgen(jitterLen = 50, jitterDep = 1,  # shaky voice
              sylLen = 1000, subDep = 0, nonlinBalance = 100,
              pitchAnchors =  c(150, 170), play = playback)
s2 = soundgen(jitterLen = 1, jitterDep = 1,   # harsh voice
              sylLen = 1000, subDep = 0, nonlinBalance = 100,
              pitchAnchors =  c(150, 170), play = playback)

## ------------------------------------------------------------------------
s = soundgen(repeatBout = 2, sylLen = 140, pauseLen = 100, 
             vocalTract = 8, formants = NULL, rolloff = 0,
             pitchAnchors = c(559, 785, 557), mouthAnchors =  c(0, 0.5, 0),
             nonlinBalance = 100, jitterDep = 1, subDep = 60, play = playback) 

## ----fig.width = 7, fig.height = 7---------------------------------------
s = soundgen(sylLen = 1200, 
             pitchAnchors = list(
  time = c(0, 80, 81, 230, 231, 385, 
           # 500 time anchors here - an episode of "chaos"
           seq(385, 850, length.out = 500), 
           851, 1020, 1021, 1085),
  value = c(700, 1130, 1000, 1200, 1860, 1840, 
            # random f0 jumps b/w 1.2 & 1.8 KHz 
            sample(c(1200, 1800), size = 500, replace = T), 
            1620, 1540, 1220, 900)),
  temperature = 0.05, 
  tempEffects = list(pitchAnchorsDep = 0),
  nonlinBalance = 100, subDep = 0, jitterDep = .3,
  rolloffKHz = 0, rolloff = 0, formants = c(900, 1300, 3300, 4300),
  samplingRate = 22000, play = playback, plot = TRUE, osc = TRUE)

## ------------------------------------------------------------------------
s = soundgen(sylLen = 800, 
             mouthAnchors = rnorm(n = 5, mean = .5, sd = .3),
             play = playback)

## ------------------------------------------------------------------------
s = soundgen(noiseAnchors = data.frame(time = c(0, 500), value = c(-40, 20)),
         formantsNoise = NA,  # breathing - same formants as for voiced
         sylLen = 500, play = playback)

## ----fig.width = 5, fig.height = 5---------------------------------------
s = soundgen(noiseAnchors = data.frame(time = c(0, 500), value = c(-40, 20)),
         # specify noise filter ≠ voiced filter to get ~[s]
         formantsNoise = list(
           f1 = data.frame(freq = 6000,
                           amp = 50, 
                           width = 1000)
           ), 
         rolloffNoise = 0, pitchAnchors = NA,
         sylLen = 500, play = playback, plot = TRUE)

## ----fig.width = 7, fig.height = 5---------------------------------------
s = soundgen(nSyl = 2, sylLen = 120, pauseLen = 120, 
             temperature = 0, rolloffNoise = -5, 
             noiseAnchors = data.frame(time = c(39, 56, 209), 
                                       value = c(-80, -10, -50)),
             formants = list(f1 = c(860, 530),  f2 = c(1280, 2400)),
             formantsNoise = c(420, 1200),
             plot = TRUE, osc = TRUE, play = playback)

## ----fig.width = 5, fig.height = 4---------------------------------------
a = getSmoothContour(anchors = data.frame(time = c(-50, 200, 300), 
                                          value = c(-80, 20, -80)),
                     voiced = 200, plot = TRUE, ylim = c(-80, 40), main = '')

## ------------------------------------------------------------------------
s1 = soundgen(vocalTract = 17.5,  # ~human throat (17.5 cm)
              formants = NULL, attackLen = 200, play = playback,
              noiseAnchors = list(time = c(0, 800), value = c(40, 40)))
# NB: since there is no voiced component, the only way to control syllable length
# is by specifying the appropriate noiseAnchors$time, in this case 0 to 800 ms

s2 = soundgen(vocalTract = 30,    # a large animal
              formants = NULL, attackLen = 200, play = playback,
              noiseAnchors = list(time = c(0, 800), value = c(40, 40)))
# NB: voiced component not generated, since noiseAnchors$value >= 40 dB
# Another way to remove the voiced component is to write pitchAnchors = NULL

## ----fig.width = 4, fig.height = 3---------------------------------------
s3 = soundgen(vocalTract = 17.5, formantsNoise = c(1000, 2000),  
              noiseAnchors = list(time = c(0, 800), value = 40),
              play = playback, plot = TRUE)

## ------------------------------------------------------------------------
s1 = soundgen(vocalTract = 17.5, rolloffNoise = 0,
              formants = NULL, attackLen = 200, play = playback,
              noiseAnchors = list(time = c(0, 800), value = c(40, 40)))

s2 = soundgen(vocalTract = 17.5, rolloffNoise = -10,
              formants = NULL, attackLen = 200, play = playback,
              noiseAnchors = list(time = c(0, 800), value = c(40, 40)))
# NB: voiced component not generated, since noiseAnchors$value >= 40 dB

## ----fig.show = "hold", fig.width = 7, fig.height = 3--------------------
par(mfrow = c(1, 2))
sound1 = sin(2 * pi * 1:5000 * 100 / 16000) # pure tone, 100 Hz
sound2 = sin(2 * pi * 1:5000 * 200 / 16000) # pure tone, 200 Hz

# simple concatenation
comb1 = c(sound1, sound2)
# playme(comb1)  # note the click
plot(comb1[4000:5500], type = 'l', xlab = '', ylab = '')  
# note the abrupt transition, which creates the click
# spectrogram(comb1, 16000)  

# cross-fade
comb2 = crossFade(sound1, sound2, samplingRate = 16000, crossLen = 50)
# playme(comb2)  # no click
plot(comb2[4000:5500], type = 'l', xlab = '', ylab = '')  
# gradual transition
# spectrogram(comb2, 16000)
par(mfrow = c(1, 1))

## ----fig.width = 5, fig.height = 5---------------------------------------
cow1 = soundgen(sylLen = 1400, 
                pitchAnchors = list(time = c(0, 11/14, 1), 
                                    value = c(75, 130, 200)), 
                temperature = 0.1, 
                rolloff = -6, rolloffOct = -3, rolloffParab = 12,
                mouthOpenThres = 0.6, 
                formants = NULL, vocalTract = 36.5, 
                mouthAnchors = list(time = c(0, 0.82, 1), 
                                    value = c(0.6, 0, 1)), 
                noiseAnchors = list(time = c(0, 1400), 
                                    value = c(-25, -25)), 
                rolloffNoise = -4, addSilence = 0)
cow2 = soundgen(sylLen = 310, pitchAnchors = c(359, 359), 
                temperature = 0.05, nonlinBalance = 100, 
                subFreq = 150, subDep = 70, jitterDep = 1.3, 
                rolloff = -6, rolloffOct = -3, rolloffKHz = -0, 
                formants = NULL, vocalTract = 36.5, 
                noiseAnchors = list(time = c(0, 26, 317, 562), 
                                    value = c(-80, -23, -22, -80)), 
                rolloffNoise = -6, 
                attackLen = 0, addSilence = 0)
s = crossFade(cow1 * 3, cow2,  # adjust the relative volume by scaling
              samplingRate = 16000, crossLen = 150)
# playme(s, 16000)
spectrogram(s, 16000, osc=T, ylim = c(0, 4))

## ----fig.show = "hold", fig.width = 5, fig.height = 5--------------------
sound1 = soundgen(sylLen = 700, pitchAnchors = 250:180, 
                  formants = 'aaao', 
                  addSilence = 100, play = playback)
sound2 = soundgen(nSyl = 2, sylLen = 150, 
                  pitchAnchors = 4300:2200, attackLen = 10,
                  formants = NA, temperature = 0, 
                  addSilence = 0, play = playback)

insertionTime = .1 + .15  # silence + 150 ms
samplingRate = 16000
insertionPoint = insertionTime * samplingRate
comb = addVectors(sound1, 
                  sound2 * .05,  # to make sound2 quieter relative to sound1
                  insertionPoint = insertionPoint)
# soundgen softens attack by default, so no clicks
# playme(comb)
spectrogram(comb, 16000, windowLength = 10, ylim = c(0, 5), 
            contrast = .5, colorTheme = 'seewave')

## ----fig.show = "hold", fig.width = 7, fig.height = 3--------------------
a = data.frame(time=c(0, .2, .9, 1), value=c(100, 110, 180, 110))
b = data.frame(time=c(0, .3, .5, .8, 1), value=c(300, 220, 190, 400, 350))
par(mfrow = c(1, 3))
plot (a, type = 'b', ylim = c(100, 400), main = 'Original curves')
points (b, type = 'b', col = 'blue')
m = soundgen:::morphDF(a, b, nMorphs = 15, method = 'smooth', 
                       plot = TRUE, main = 'Morphing curves')
m = soundgen:::morphDF(a, b, nMorphs = 15, method = 'perAnchor', 
                       plot = TRUE, main = 'Morphing anchors')
par(mfrow = c(1, 1))

## ------------------------------------------------------------------------
m = morph(formula1 = list(repeatBout = 2),
          # equivalently: formula1 = 'soundgen(repeatBout = 2)',
          formula2 = presets$Misc$Dog_bark,
          nMorphs = 5, playMorphs = playback)
# use $formulas to access formulas for each morph, $sounds for waveforms
# m$formulas[[4]]
# playme(m$sounds[[3]])

## ------------------------------------------------------------------------
target = soundgen(repeatBout = 3, sylLen = 120, pauseLen = 70,
                  pitchAnchors = c(300, 200),
                  rolloff = -5, play = playback)  # we hope to reproduce this sound
# playme(target)

m1 = matchPars(target = target,
               samplingRate = 16000,
               maxIter = 0)  # no optimization, only acoustic analysis
# ignore the warning about failing to improve the fit: we don't want to optimize yet

# m1$pars contains a list of soundgen settings
cand1 = do.call(soundgen, c(m1$pars, list(play = playback, temperature = 0)))
# playme(cand1)

## ---- eval = FALSE-------------------------------------------------------
#  call('soundgen', m1$pars)
#  # copy-paste from the console and remove "list(...)" to get your call to soundgen():
#  # soundgen(samplingRate = 16000, nSyl = 3, sylLen = 79, pauseLen = 114,
#  #     pitchAnchors = list(time = c(0, 0.5, 1), value = c(274, 253, 216)),
#  #     formants = list(f1 = list(freq = 821, width = 122),
#  #                     f2 = list(freq = 1266, width = 36),
#  #                     f3 = list(freq = 2888, width = 117)))

## ---- eval = FALSE-------------------------------------------------------
#  m2 = matchPars(target = target,
#                 samplingRate = 16000,
#                 pars = 'rolloff',
#                 maxIter = 100)
#  
#  # rolloff should be moving from default (-12) to target (-5):
#  sapply(m2$history, function(x) {
#    paste('Rolloff:', round(x$pars$rolloff, 1),
#          '; fit to target:', round(x$sim, 2))
#  })
#  
#  cand2 = do.call(soundgen, c(m2$pars, list(play = playback, temperature = 0)))

