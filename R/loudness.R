#' Get loudness
#'
#' Estimates subjective loudness per frame, in sone. Based on EMBSD speech
#' quality measure, particularly the matlab code in Yang (1999) and Timoney et
#' al. (2004). Note that there are many ways to estimate loudness and many other
#' factors, ignored by this model, that could influence subjectively experienced
#' loudness. Please treat the output with a healthy dose of skepticism! Also
#' note that the absolute value of calculated loudness critically depends on the
#' chosen "measured" sound pressure level (SPL). \code{getLoudness} estimates
#' how loud a sound will be experienced if it is played back at an SPL of
#' \code{SPL_measured} dB. The most meaningful way to use the output is to
#' compare the loudness of several sounds analyzed with identical settings or of
#' different segments within the same recording.
#'
#' Algorithm: calibrates the sound to the desired SPL (Timoney et al., 2004),
#' extracts a \code{\link{spectrogram}}, converts to bark scale
#' (\code{\link[tuneR]{audspec}}), spreads the spectrum to account for frequency
#' masking across the critical bands (Yang, 1999), converts dB to phon by using
#' standard equal loudness curves (ISO 226), converts phon to sone (Timoney et
#' al., 2004), sums across all critical bands, and applies a correction
#' coefficient to standardize output. Calibrated so as to return a loudness of 1
#' sone for a 1 kHz pure tone with SPL of 40 dB.
#' @inheritParams spectrogram
#' @param samplingRate sampling rate of \code{x} (only needed if
#'   \code{x} is a numeric vector, rather than an audio file), must be > 2000 Hz
#' @param scale the maximum possible value of \code{x} (only needed if \code{x}
#'   is a numeric vector, rather than an audio file); defaults to observed
#'   \code{max(abs(x))} if it is greater than 1 and to 1 otherwise
#' @param SPL_measured sound pressure level at which the sound is presented, dB
#' @param Pref reference pressure, Pa
#' @param spreadSpectrum if TRUE, applies a spreading function to account for
#'   frequency masking
#' @param mar margins of the spectrogram
#' @param ... other plotting parameters passed to \code{\link{spectrogram}}
#' @return Returns a list of length two: \describe{
#'   \item{specSone}{spectrum in sone: a matrix with frequency on the bark
#'   scale in rows and time (STFT frames) in columns}
#'   \item{loudness}{a vector of loudness per STFT frame (sone)}
#'   }
#' @references \itemize{
#'   \item ISO 226 as implemented by Jeff Tackett (2005) on
#'   https://www.mathworks.com/matlabcentral/fileexchange/
#'   7028-iso-226-equal-loudness-level-contour-signal \item Timoney, J.,
#'   Lysaght, T., Schoenwiesner, M., & MacManus, L. (2004). Implementing
#'   loudness models in matlab. \item Yang, W. (1999). Enhanced Modified Bark
#'   Spectral Distortion (EMBSD): An Objective Speech Quality Measure Based on
#'   Audible Distortion and Cognitive Model. Temple University. }
#' @export
#' @examples
#' sounds = list(
#'   white_noise = runif(8000, -1, 1),
#'   white_noise2 = runif(8000, -1, 1) / 2,  # ~6 dB quieter
#'   pure_tone_1KHz = sin(2*pi*1000/16000*(1:8000))  # pure tone at 1 kHz
#' )
#' loud = rep(0, length(sounds)); names(loud) = names(sounds)
#' for (i in 1:length(sounds)) {
#'   # playme(sounds[[i]], 16000)
#'   l = getLoudness(
#'     x = sounds[[i]], samplingRate = 16000, scale = 1,
#'     windowLength = 20, step = NULL,
#'     overlap = 50, SPL_measured = 40,
#'     Pref = 2e-5, plot = FALSE)
#'   loud[i] = mean(l$loudness)
#' }
#' loud
#' # white noise (sound 1) is twice as loud as pure tone at 1 KHz (sound 3),
#' # and note that the same white noise with lower amplitude has lower loudness
#' # (provided that "scale" is specified)
#' # compare: lapply(sounds, range)
#'
#' \dontrun{
#'   s = soundgen()
#'   l = getLoudness(s, SPL_measured = 70,
#'                   samplingRate = 16000, plot = TRUE, osc = TRUE)
#'   # The estimated loudness in sone depends on target SPL
#'   l = getLoudness(s, SPL_measured = 40,
#'                   samplingRate = 16000, plot = TRUE)
#'
#'   # ...but not (much) on windowLength and samplingRate
#'   l = getLoudness(soundgen(), SPL_measured = 40, windowLength = 50,
#'                   samplingRate = 16000, plot = TRUE)
#'
#'   # input can be an audio file
#'   getLoudness('~/Downloads/temp/032_ut_anger_30-m-roar-curse.wav')
#' }
getLoudness = function(x,
                       samplingRate = NULL,
                       scale = NULL,
                       windowLength = 50,
                       step = NULL,
                       overlap = 50,
                       SPL_measured = 70,
                       Pref = 2e-5,
                       spreadSpectrum = TRUE,
                       plot = TRUE,
                       mar = c(5.1, 4.1, 4.1, 4.1),
                       ...) {
  # import sound
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  if (class(x) == 'character') {
    extension = substr(x, nchar(x) - 2, nchar(x))
    if (extension == 'wav' | extension == 'WAV') {
      sound_wav = tuneR::readWave(x)
    } else if (extension == 'mp3' | extension == 'MP3') {
      sound_wav = tuneR::readMP3(x)
    } else {
      stop('Input not recognized: must be a numeric vector or wav/mp3 file')
    }
    samplingRate = sound_wav@samp.rate
    sound = sound_wav@left
    scale = 2 ^ (sound_wav@bit - 1) # range(sound)
  } else if (class(x) == 'numeric' & length(x) > 1) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    } else {
      sound = x
    }
    m = max(abs(sound))
    if (is.null(scale)) {
      scale = max(m, 1)
      warning(paste('Scale not specified. Assuming that max amplitude is', scale))
    } else if (is.numeric(scale)) {
      if (scale < m) {
        scale = m
        warning(paste('Scale exceeds the observed range; resetting to', m))
      }
    }
  }
  if (samplingRate < 2000) return(NA)  # need at least 8 barks (1 kHz) Niquist

  # scale to dB SPL
  sound_scaled = scaleSPL(sound,
                          scale = scale,
                          SPL_measured = SPL_measured,
                          Pref = Pref)
  # range(sound); range(sound_scaled)
  # log10(sqrt(mean(sound_scaled ^ 2))) * 20
  # (should be the same as SPL_measured w/o scale adjustment)

  # get power spectrum
  powerSpec = spectrogram(
    sound_scaled, samplingRate = samplingRate,
    windowLength = windowLength, step = step,
    output = 'original', normalize = FALSE,
    plot = plot, mar = mar, ...) ^ 2
  # range(log10(powerSpec) * 10)

  # normalize power spectrum by the size of STFT frame
  # windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
  powerSpec_scaled = powerSpec / nrow(powerSpec)  # same as * 2 / windowLength_points
  # range(log10(powerSpec_scaled) * 10)
  # image(t(powerSpec_scaled))

  # get auditory spectrum
  audSpec = tuneR::audspec(
    powerSpec_scaled,
    sr = samplingRate,
    fbtype = 'bark')$aspectrum
  # image(t(audSpec))
  # range(log10(audSpec) * 10)
  # plot(audSpec[, 1], type = 'l')
  # plot(log10(audSpec[, 1]) * 10, type = 'l')

  # throw away very high frequencies
  if (samplingRate > 44100) {
    message(paste('Sampling rate above 44100, but discarding frequencies above 27 barks',
                  '(27 KHz) as inaudible to humans'))
    audSpec = audSpec[1:27, ]  # max 27 barks
  }

  # apply spreading function (NB: linear, not dB scale!)
  if (spreadSpectrum) {
    nonZeroCols = which(colSums(audSpec) > 0)
    for (c in nonZeroCols) {
      audSpec[, c] = spreadSpec(audSpec[, c])
    }
    # image(t(audSpec))
    # range(log10(audSpec) * 10)
    # plot(audSpec[, 1], type = 'l')
    # plot(log10(audSpec[, 1]) * 10, type = 'l')
  }

  # convert spectrum to sone
  specSone = matrix(0, nrow = nrow(audSpec), ncol = ncol(audSpec))
  for (i in 1:ncol(specSone)) {
    # spectrum in dB SPL
    y = 10 * log10(audSpec[, i])
    # plot(y, type = 'b')

    # dB SPL to phons (8 barks ~= 1 kHz, reference value for equal loudness curves)
    n_phonCurve = which.min(abs(y[8] - as.numeric(names(phonCurves))))
    # correction curve for frame i
    curve = phonCurves[[n_phonCurve]][1:length(y), ]
    y_phon = y + curve$spl[8] - curve$spl
    # plot(y_phon, type = 'b')

    # ignore frequency bins below hearing threshold
    y_phon[y_phon < curve$hearingThres_dB | y_phon < 0] = 0

    # phons to sone
    specSone[, i] = phon2sone(y_phon)
    # plot(specSone[, i], type = 'b')
  }
  # image(t(specSone))
  loudness = apply(specSone, 2, sum)

  # empirical normalization (see commented-out code below the function)
  loudness = loudness / (5.73 +  6.56 * windowLength ^ .35) /
    (.0357 + .0345 * samplingRate ^ .3113)

  # plotting
  if (plot) {
    # spectrogram(sound, samplingRate = 16000, osc = TRUE)
    op = par(c('mar', 'new')) # save user's original pars
    par(new = TRUE, mar = mar)
    # adjust the timing of loudness to match the actual time stamps
    # in getFrameBank (~the middle of each fft frame)
    windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
    duration = length(sound) / samplingRate
    X = seq(1, max(1, (length(sound) - windowLength_points)),
            step / 1000 * samplingRate) / samplingRate * 1000 + windowLength / 2
    plot(x = X,
         y = loudness,
         type = "b",
         xlim = c(0, duration * 1000),
         xaxs = "i", yaxs = "i",
         axes = FALSE, bty = "n",
         xlab = "", ylab = "")
    axis(side = 4, at = pretty(range(loudness)))
    mtext("Loudness, sone", side = 4, line = 3)
    par('mar' = op$mar, 'new' = op$new)  # restore original pars
  }
  return(list(specSone = specSone, loudness = loudness))
}

# ## EMPIRICAL CALIBRATION OF LOUDNESS (SONE) RETURNED BY getLoudness()
# # Simple linear scaling can correct for a given windowLength, but
# # how does scaling coef depend on windowLength and samplingRate?
# wl = expand.grid(windowLength = seq(10, 150, by = 10),
#                  samplingRate = c(16000, 24000, 32000, 44000))
# wl$windowLength_points = 2 ^ (ceiling(log2(wl$windowLength * samplingRate / 1000)))
# for (i in 1:nrow(wl)) {
#   sound = sin(2*pi*1000/wl$samplingRate[i]*(1:20000))
#   wl$loudness[i] = getLoudness(x = sound, samplingRate = wl$samplingRate[i], windowLength = wl$windowLength[i], step = NULL, overlap = 0, SPL_measured = 40, Pref = 2e-5, plot = FALSE)$loudness[1]
# }
# # plot(wl$windowLength, wl$loudness)
# library(ggplot2)
# ggplot(wl, aes(x = windowLength, y = loudness, color = as.factor(samplingRate))) +
#   geom_point() +
#   geom_line()
#
# # account for windowLength
# samplingRate = 44100
# sound = sin(2*pi*1000/samplingRate*(1:20000))
# wl = data.frame(windowLength = seq(5, 200, by = 5))
# wl$windowLength_points = 2 ^ (ceiling(log2(wl$windowLength * samplingRate / 1000)))
# for (i in 1:nrow(wl)) {
#   wl$loudness[i] = getLoudness(x = sound, samplingRate = samplingRate, windowLength = wl$windowLength[i], step = NULL, overlap = 0, SPL_measured = 40, Pref = 2e-5, plot = FALSE)$loudness[1]
# }
# plot(wl$windowLength, wl$loudness)
# # wl
# # plot(wl$windowLength_points, wl$loudness)
#
# mod = nls(loudness ~ a + b * windowLength ^ c, wl, start = list(a = 0, b = 1, c = .5))
# plot(wl$windowLength, wl$loudness)
# lines(wl$windowLength, predict(mod, list(windowLength = wl$windowLength)))
# summary(mod)
# # use these regression coefficients to calculate scaling factor
# # as a function of windowLength
#
#
# # account for samplingRate
# windowLength = 30
# wl = data.frame(samplingRate = seq(16000, 44000, by = 1000))
# for (i in 1:nrow(wl)) {
#   sound = sin(2*pi*1000/wl$samplingRate[i]*(1:20000))
#   wl$loudness[i] = getLoudness(x = sound, samplingRate = wl$samplingRate[i], windowLength = windowLength, step = NULL, overlap = 0, SPL_measured = 40, Pref = 2e-5, plot = FALSE)$loudness[1]
# }
# plot(wl$samplingRate, wl$loudness)
# mod = nls(loudness ~ a + b * samplingRate ^ c, wl, start = list(a = 0, b = 1, c = .5))
# plot(wl$samplingRate, wl$loudness)
# lines(wl$samplingRate, predict(mod, list(samplingRate = wl$samplingRate)))
# summary(mod)
#
# # CHECKING THE CALIBRATION
# samplingRate = 24000
# sound = sin(2*pi*1000/samplingRate*(1:20000))
# cal = data.frame(SPL_measured = seq(40, 80, by = 10))
# for (i in 1:nrow(cal)) {
#   cal$loudness[i] = mean(getLoudness(x = sound, samplingRate = samplingRate, windowLength = 20, step = NULL, overlap = 50, SPL_measured = cal$SPL_measured[i], Pref = 2e-5, plot = FALSE)$loudness)
# }
# cal  # loudness should be 1, 2, 4, 8, 16 sone
# cal$loudness / cal$loudness[1]
# plot(cal$SPL_measured, cal$loudness / cal$loudness[1] - c(1, 2, 4, 8, 16))


#' Loudness per folder
#'
#' A wrapper around \code{\link{getLoudness}} that goes through all wav/mp3 files in
#' a folder and returns either a list with loudness values per STFT frame from each file
#' or, if \code{summary = TRUE}, a dataframe with a single summary value of loudness
#' per file. This summary value can be mean, max and so on, as per
#' \code{summaryFun}.
#' @param myfolder path to folder containing wav/mp3 files
#' @inheritParams getLoudness
#' @param summary if TRUE, returns only a single value of loudness per file
#' @param summaryFun the function used to summarize loudness values across all STFT frames (if
#'   \code{summary = TRUE})
#' @param verbose if TRUE, reports estimated time left
#' @export
#' @examples
#' \dontrun{
#' getLoudnessFolder('~/Downloads/temp')
#' # Compare:
#' analyzeFolder('~/Downloads/temp', pitchMethods = NULL,
#'               plot = FALSE)$loudness_mean
#' # (per STFT frame; should be very similar, but not identical, because
#' # analyze() discards frames considered silent or too noisy)
#'
#' # custom summaryFun
#' difRan = function(x) diff(range(x))
#' getLoudnessFolder('~/Downloads/temp', summaryFun = c('mean', 'difRan'))
#'
#' # save loudness values per frame without summarizing
#' l = getLoudnessFolder('~/Downloads/temp', summary = FALSE)
#' }
getLoudnessFolder = function(myfolder,
                             windowLength = 50,
                             step = NULL,
                             overlap = 50,
                             SPL_measured = 70,
                             Pref = 2e-5,
                             spreadSpectrum = TRUE,
                             summary = TRUE,
                             summaryFun = 'mean',
                             verbose = TRUE) {
  time_start = proc.time()  # timing
  filenames = list.files(myfolder, pattern = "*.wav|.mp3", full.names = TRUE)
  # in order to provide more accurate estimates of time to completion,
  # check the size of all files in the target folder
  filesizes = file.info(filenames)$size

  # match par-s
  myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'myfolder', 'verbose', 'summary', 'summaryFun')]

  result = list()
  for (i in 1:length(filenames)) {
    result[[i]] = do.call(getLoudness, c(filenames[i], myPars, plot = FALSE))$loudness
    if (verbose) {
      reportTime(i = i, nIter = length(filenames),
                 time_start = time_start, jobs = filesizes)
    }
  }

  # prepare output
  if (summary == TRUE) {
    output = data.frame(sound = basename(filenames))
    for (s in 1:length(summaryFun)) {
      # for each summary function...
      f = eval(parse(text = summaryFun[s]))
      for (i in 1:length(result)) {
        # for each sound file...
        mySummary = do.call(f, list(na.omit(result[[i]])))
        # for smth like range, collapse and convert to character
        if (length(mySummary) > 1) {
          mySummary = paste0(mySummary, collapse = ', ')
        }
        output[i, summaryFun[s]] = mySummary
      }
    }
  } else {
    output = result
    names(output) = basename(filenames)
  }
  return(output)
}

