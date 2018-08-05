## ----fig.width = 5, fig.height = 5---------------------------------------
library(soundgen)
s1 = soundgen(sylLen = 900, temperature = 0,
              pitchAnchors = list(time = c(0, .3, .8, 1), 
                                  value = c(300, 900, 400, 2300)),
              noiseAnchors = c(-40, 0), subDep = 100, 
              jitterDep = 0.5, nonlinBalance = 100,
              plot = TRUE, ylim = c(0, 4))
# playme(s1)  # replay as many times as needed w/o re-synthesizing the sound

## ------------------------------------------------------------------------
true_pitch = getSmoothContour(anchors = list(time = c(0, .3, .8, 1),
                                             value = c(300, 900, 400, 2300)),
                              len = 1000)  # any length will do
median(true_pitch)  # 611 Hz

## ----fig.show = "hold", fig.height = 5, fig.width = 7--------------------
a1 = analyze(s1, samplingRate = 16000, plot = TRUE, ylim = c(0, 4))
# summary(a1)  # many acoustic predictors measured for each FFT frame
median(true_pitch)  # true value, as synthesized above
median(a1$pitch, na.rm = TRUE)  # our estimate
# Pitch postprocessing is stochastic (see below), so the contour may vary.
# Many candidates are off target, mainly b/c of misleading subharmonics.

## ------------------------------------------------------------------------
spec = seewave::spec(s1, f = 16000, plot = FALSE)  # FFT of the entire sound
avSpec = seewave::meanspec(s1, f = 16000, plot = FALSE)  # STFT followed by averaging
# either way, you get a dataframe with two columns: frequencies and their power
head(avSpec)

## ------------------------------------------------------------------------
spgm = spectrogram(s1, samplingRate = 16000, output = 'original', plot = FALSE)
# rownames give you frequencies, colnames are time stamps
str(spgm)

## ------------------------------------------------------------------------
# Transform power to pdf (all columns should sum to 1):
spgm_norm = apply(spgm, 2, function(x) x / sum(x))
# Set up a dataframe to store the output
out = data.frame(skew = rep(NA, ncol(spgm)),
                 quantile66 = NA,
                 ratio500 = NA)
# Process each STFT frame
for (i in 1:ncol(spgm_norm)) {
  # Power spectrum for this frame
  df = data.frame(
    freq = as.numeric(rownames(spgm_norm)),  # frequency (kHz)
    d = spgm_norm[, i]                       # density
  )
  # plot(df, type = 'l')
  
  # Skewness (see https://en.wikipedia.org/wiki/Central_moment)
  m = sum(df$freq * df$d)  # spectral centroid, kHz
  out$skew[i] = sum((df$freq - m)^3 * df$d)
  
  # 66.6th percentile (2/3 of density below this frequency)
  out$quantile66[i] = df$freq[min(which(cumsum(df$d) >= 2/3))]  # in kHz
  
  # Energy above/below 500 Hz
  out$ratio500[i] = sum(df$d[df$freq >= .5]) / sum(df$d[df$freq < .5])
}
summary(out)

## ----fig.show = "hold", fig.height = 5, fig.width = 7--------------------
a = analyze(s1, samplingRate = 16000, plot = TRUE, ylim = c(0, 4),
            pitchMethods = c('autocor', 'cep', 'dom', 'spec'))

## ----fig.show = "hold", fig.height = 5, fig.width = 7--------------------
par(mfrow = c(1, 2))
# default prior in soundgen
a1 = analyze(s1, samplingRate = 16000, plot = FALSE, priorPlot = TRUE,
             priorMean = HzToSemitones(300), priorSD = 6)  
# narrow peak at 2 kHz
a2 = analyze(s1, samplingRate = 16000, plot = FALSE, priorPlot = TRUE,
             priorMean = HzToSemitones(2000), priorSD = 1)
par(mfrow = c(1, 1))

## ----fig.show = "hold", fig.height = 5, fig.width = 7--------------------
a = analyze(s1, samplingRate = 16000, 
            plot = TRUE, ylim = c(0, 4), priorMean = NA,
            pitchMethods = 'autocor',
            autocorThres = .45,
            nCands = 3)

## ----fig.show = "hold", fig.height = 5, fig.width = 7--------------------
a = analyze(s1, 
            samplingRate = 16000, plot = TRUE, ylim = c(0, 4), priorMean = NA,
            pitchMethods = 'dom',
            domThres = .1,
            domSmooth = 500)

## ----fig.show = "hold", fig.height = 5, fig.width = 7--------------------
a = analyze(s1, 
            samplingRate = 16000, plot = TRUE, ylim = c(0, 4), priorMean = NA,
            pitchMethods = 'cep',
            cepThres = .3,
            cepSmooth = 3,
            nCands = 2)

## ----fig.show = "hold", fig.height = 5, fig.width = 7--------------------
a = analyze(s1, 
            samplingRate = 16000, plot = TRUE, ylim = c(0, 4), priorMean = NA,
            pitchMethods = 'spec',
            specPeak = .4,
            nCands = 2)

## ----fig.height = 5, fig.width = 7---------------------------------------
a = analyze(
  s1, 
  samplingRate = 16000, plot = TRUE, ylim = c(0, 4), priorMean = NA,
  shortestSyl = 0, shortestPause = 0,  # any length of voiced fragments
  interpolWin = NULL,     # don't interpolate missing F0 values
  pathfinding = 'none',   # don't look for optimal path through candidates
  snakeStep = NULL,       # don't run the snake
  smooth = NULL           # don't run median smoothing
)       

## ----fig.show = "hold", fig.height = 3, fig.width = 7--------------------
par(mfrow = c(1, 2))
a1 = analyze(s1, samplingRate = 16000, plotSpec = FALSE, priorMean = NA,
             pitchMethods = 'cep', cepThres = .35, step = 25,
             snakeStep = NULL, smooth = 0)  
a2 = analyze(s1, samplingRate = 16000, plotSpec = FALSE, priorMean = NA,
             pitchMethods = 'cep', cepThres = .35, step = 25,
             snakeStep = NULL, smooth = 0,
             interpolWin = NULL, pathfinding = 'none')  # disable interpolation
par(mfrow = c(1, 1))

## ----fig.show = "hold", fig.height = 3, fig.width = 7--------------------
par(mfrow = c(1, 2))
a1 = analyze(s1, samplingRate = 16000, plotSpec = FALSE, priorMean = NA,
             pitchMethods = 'cep', cepThres = .15, nCands = 3,
             snakeStep = NULL, smooth = 0, interpolTol = Inf,
             certWeight = 0)  # minimize pitch jumps
a2 = analyze(s1, samplingRate = 16000, plotSpec = FALSE, priorMean = NA,
             pitchMethods = 'cep', cepThres = .15, nCands = 3,
             snakeStep = NULL, smooth = 0, interpolTol = Inf,
             certWeight = 1)  # minimize deviation from candidates
par(mfrow = c(1, 1))

## ----fig.height = 5, fig.width = 7---------------------------------------
a1 = analyze(s1, samplingRate = 16000, plot = FALSE, priorMean = NA,
             pitchMethods = 'cep', cepThres = .2, nCands = 2,
             pathfinding = 'none', smooth = 0, interpolTol = Inf,
             certWeight = 0.1,  # like pathfinding, the snake is affected by certWeight
             snakeStep = 0.05, snakePlot = TRUE)

## ----fig.show = "hold", fig.height = 3, fig.width = 7--------------------
par(mfrow = c(1, 2))
a1 = analyze(s1, samplingRate = 16000, plotSpec = FALSE, priorMean = NA,
             pitchMethods = 'cep', cepThres = .2, nCands = 2,
             pathfinding = 'none', snakeStep = NULL, interpolTol = Inf,
             smooth = 0)
a2 = analyze(s1, samplingRate = 16000, plotSpec = FALSE, priorMean = NA,
             pitchMethods = 'cep', cepThres = .2, nCands = 2,
             pathfinding = 'none', snakeStep = NULL, interpolTol = Inf,
             smooth = 1)
par(mfrow = c(1, 1))

## ----fig.height = 5, fig.width = 7---------------------------------------
a = analyze(s1, samplingRate = 16000, plot = TRUE, priorMean = NA,
            # options for spectrogram(): see ?spectrogram
            contrast = .75,
            brightness = -0.5,
            colorTheme = 'seewave',
            ylim = c(0, 4),
            # + other pars passed to seewave::filled.contour.modif2()
            
            # options for plotting the final pitch contour (line)
            pitchPlot = list(       
              col = 'black',
              lwd = 5,
              lty = 3
              # + other pars passed to base::lines()
            ),
            
            # options for plotting pitch candidates (points)
            candPlot = list(  
              levels = c('autocor', 'cep', 'spec', 'dom'),
              col = c('green', 'violet', 'red', 'orange'),
              pch = c(16, 7, 2, 3),
              cex = 3
            ))

## ----eval = FALSE--------------------------------------------------------
#  a = analyze(s1, samplingRate = 16000, plot = TRUE, savePath = '~/Downloads',
#              width = 900, height = 500, units = 'px')

## ----fig.height = 3, fig.width = 7---------------------------------------
# for info on using soundgen() function, see the vignette on sound synthesis 
s2 = soundgen(nSyl = 8, sylLen = 50, pauseLen = 70, temperature = 0,
              pitchAnchors = c(368, 284),
              noiseAnchors = list(time = c(0, 67, 86, 186), 
                                  value = c(-45, -47, -89, -120)),
              rolloffNoise = -8, amplAnchorsGlobal = c(0, -20))
# spectrogram(s2, samplingRate = 16000, osc = TRUE)
# playme(s2, samplingRate = 16000)
a = segment(s2, samplingRate = 16000, plot = TRUE)

## ----fig.show = "hold", fig.height = 7, fig.width = 5--------------------
par(mfrow = c(3, 1))
a1 = segment(s2, samplingRate = 16000, plot = TRUE, 
             windowLength = 40, overlap = 0)   # overlap too low
a2 = suppressWarnings(segment(s2, samplingRate = 16000, plot = TRUE, 
             windowLength = 5, overlap = 80))  # window too short
a3 = segment(s2, samplingRate = 16000, plot = TRUE, 
             windowLength = 150, overlap = 80) # window too long
par(mfrow = c(1, 1))

## ----fig.show = "hold", fig.height = 5, fig.width = 5--------------------
par(mfrow = c(2, 1))
a1 = segment(s2, samplingRate = 16000, plot = TRUE, 
             shortestSyl = 80)    # too long, but at least bursts are detected
a2 = segment(s2, samplingRate = 16000, plot = TRUE, 
             shortestPause = 80)  # merges syllables
par(mfrow = c(1, 1))

## ----fig.show = "hold", fig.height = 5, fig.width = 5--------------------
par(mfrow = c(2, 1))
# absolute threshold burstThres set too high
a1 = segment(s2, samplingRate = 16000, plot = TRUE, 
             burstThres = 0.5)
# improper syllable merging due to shortestPause, but overriden by manually 
# specified interburst
a2 = segment(s2, samplingRate = 16000, plot = TRUE, 
             shortestPause = 80, interburst = 100) 
par(mfrow = c(1, 1))

## ----fig.show = "hold", fig.height = 6, fig.width = 7--------------------
s3 = c(soundgen(), soundgen(nSyl = 4, sylLen = 50, pauseLen = 70, 
       formants = NA, pitchAnchors = c(500, 330)))
# playme(s3, 16000)
m = ssm(s3, samplingRate = 16000)

## ----fig.show = "hold", fig.height = 6, fig.width = 7--------------------
par(mfrow = c(2, 1))
m1 = ssm(s3, samplingRate = 16000,
         input = 'audiogram', simil = 'cor', norm = FALSE, 
         ssmWin = 10, kernelLen = 150)  # detailed, local features
m2 = ssm(s3, samplingRate = 16000,
         input = 'mfcc', simil = 'cosine', norm = TRUE, 
         ssmWin = 50, kernelLen = 600)  # more global
par(mfrow = c(1, 1))

## ----eval = FALSE--------------------------------------------------------
#  # checking combinations of pitch tracking methods
#  myfolder = 'path.to.260.wav.files'
#  key = log(pitchManual)
#  p = c('autocor', 'cep', 'spec', 'dom')
#  pp = c(list(p),
#         combn(p, 3, simplify = FALSE),
#         combn(p, 2, simplify = FALSE),
#         combn(p, 1, simplify = FALSE))
#  out = list()
#  res = data.frame('pars' = sapply(pp, function(x) paste(x, collapse = ',')),
#                   cor1 = rep(NA, length(pp)),
#                   cor2 = rep(NA, length(pp)))
#  # repeating the analysis for each combination of methods in pp
#  for (i in 1:length(pp)) {
#    out[[i]] = analyzeFolder(myfolder, plot = FALSE, verbose = FALSE, step = 50,
#                             pitchMethods = pp[[i]])$pitch_median
#    res$cor1[i] = cor(log(out[[i]]), log(pitchManual), use = 'pairwise.complete.obs')
#    res$cor2[i] = cor(log(out[[i]]), log(pitchManual), use = 'pairwise.complete.obs') *
#      (1 - mean(is.na(out[[i]]) & !is.na(key)))
#    print(res[i, ])
#  }
#  res[order(res$cor1, decreasing = TRUE), ]  # max correlation regardless of NA
#  res[order(res$cor2, decreasing = TRUE), ]  # max correlation penalized for NA

## ----eval = FALSE--------------------------------------------------------
#  myfolder = 'path.to.260.wav.files'
#  key = log(pitchManual)
#  out = list()
#  pars = expand.grid(windowLength = c(17, 35, 50),
#                     smooth = c(0, 1, 2))
#  for (i in 1:nrow(pars)) {
#    out[[i]] = suppressWarnings(analyzeFolder(myfolder, plot = FALSE, verbose = FALSE, step = 25,
#                 pitchMethods = c('autocor','dom','spec'),
#                 windowLength = pars$windowLength[i],
#                 smooth = pars$smooth[i]))$pitch_median
#    print(cor(log(out[[i]]), key, use = 'pairwise.complete.obs'))
#    print(cor(log(out[[i]]), key, use = 'pairwise.complete.obs') *
#            (1 - mean(is.na(out[[i]]) & !is.na(key))))
#  }
#  pars$r1 = sapply(out, function(x) {
#    cor(log(x), key, use = 'pairwise.complete.obs')
#  })
#  pars$r2 = sapply(out, function(x) {
#    cor(log(x), key, use = 'pairwise.complete.obs') *
#      (1 - mean(is.na(x) & !is.na(key)))
#  })
#  pars
#  
#  v = 6  # pick some combination of par values to explore
#  trial = log(out[[v]])
#  cor (key, trial, use = 'pairwise.complete.obs')
#  cor (key, trial, use = 'pairwise.complete.obs') * (1 - mean(is.na(trial) & !is.na(key)))
#  plot (key, trial)
#  abline(a=0, b=1, col='red')

