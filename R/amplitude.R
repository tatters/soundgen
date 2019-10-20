#' Get amplitude envelope
#'
#' Internal soundgen function
#'
#' Returns the smoothed amplitude envelope of a waveform on the original scale.
#' @inheritParams flatEnv
#' @param method 'peak' for peak amplitude per window, 'rms' for root mean
#'   square amplitude, 'mean' for mean (for DC offset removal), 'hil' for
#'   Hilbert
#' @keywords internal
#' @examples
#' a = rnorm(500) * seq(1, 0, length.out = 500)
#' windowLength_points = 50
#' plot(a, type = 'l')
#' points(soundgen:::getEnv(a, windowLength_points, 'rms'),
#'        type = 'l', col = 'red')
#' points(soundgen:::getEnv(a, windowLength_points, 'peak'),
#'        type = 'l', col = 'green')
#' points(soundgen:::getEnv(a, windowLength_points, 'hil'),
#'        type = 'l', col = 'blue')
#' points(soundgen:::getEnv(a, windowLength_points, 'mean'),
#'        type = 'l', lty = 3, lwd = 3)
getEnv = function(sound,
                  windowLength_points,
                  method = c('rms', 'peak', 'mean', 'hil')[1]) {
  len = length(sound)
  if (method == 'peak') sound_abs = abs(sound)  # avoid repeated calculations

  if (windowLength_points >= len / 2) {
    # short sound relative to window - just take beginning and end (2 points)
    s = c(1, len)
  } else {
    s = c(1,
          seq(from = floor(windowLength_points / 2),
              to = length(sound) - floor(windowLength_points / 2),
              by = windowLength_points),
          length(sound))
  }
  # s is a sequence of starting indices for windows over which we average
  envShort = rep(NA, length(s) - 1)
  for (i in 1:(length(s) - 1)) {
    seg = s[i] : s[i+1]
    if (method == 'peak') {
      # get moving peak amplitude
      envShort[i] = max(sound_abs[seg])
    } else if (method == 'rms') {
      # get moving RMS amplitude
      envShort[i] = sqrt(mean(sound[seg] ^ 2))
    } else if (method == 'mean') {
      envShort[i] = mean(sound[seg])
    } else if (method == 'hil') {
      envShort[i] = mean(Mod(seewave::hilbert(sound[seg],
                                              f = 1,  # not actually needed
                                              fftw = FALSE)))
    }
  }

  # upsample and smooth
  env = getSmoothContour(
    anchors = data.frame(time = seq(0, 1, length.out = length(envShort)),
                         value = envShort),
    len = length(sound)
  )
  return(env)
}


#' Kill DC
#'
#' Removes DC offset or similar disbalance in a waveform dynamically, by
#' subtracting a smoothed ~moving average. Simplified compared to a true moving
#' average, but very fast (a few ms per second of 44100 audio).
#' @inheritParams flatEnv
#' @param plot if TRUE, plots the original sound, smoothed moving average, and
#'   modified sound
#' @keywords internal
#' @examples
#' # remove static DC offset
#' a = rnorm(500) + .3
#' b = soundgen:::killDC(a, windowLength_points = 500, plot = TRUE)
#'
#' # remove trend
#' a = rnorm(500) + seq(0, 1, length.out = 500)
#' b = soundgen:::killDC(a, windowLength_points = 100, plot = TRUE)
#'
#' # can also be used as a high-pass filter
#' a = rnorm(500) + sin(1:500 / 50)
#' b = soundgen:::killDC(a, windowLength_points = 25, plot = TRUE)
killDC = function(sound,
                  windowLength = 200,
                  samplingRate = 16000,
                  windowLength_points = NULL,
                  plot = FALSE) {
  if (!is.numeric(windowLength_points)) {
    if (is.numeric(windowLength)) {
      if (is.numeric(samplingRate)) {
        windowLength_points = windowLength / 1000 * samplingRate
      } else {
        stop(paste('Please, specify either windowLength (ms) plus samplingRate (Hz)',
                   'or the length of smoothing window in points (windowLength_points)'))
      }
    }
  }

  env = getEnv(sound = sound,
               windowLength_points = windowLength_points,
               method = 'mean')
  soundNorm = sound - env

  if (plot) {
    op = par('mfrow')
    par(mfrow = c(1, 2))
    plot(sound, type = 'l', main = 'Original')
    points(env, type = 'l', lty = 1, col = 'blue')
    points(rep(0, length(sound)), type = 'l', lty = 2)
    plot(soundNorm, type = 'l', main = 'Env removed')
    points(rep(0, length(sound)), type = 'l', col = 'blue')
    par(mfrow = op)
  }
  return(soundNorm)
}


#' RMS amplitude per frame
#'
#' Calculates root mean square (RMS) amplitude in overlapping frames, providing
#' an envelope of RMS amplitude as a measure of sound intensity. Longer windows
#' provide smoother, more robust estimates; shorter windows and more overlap
#' improve temporal resolution, but they also increase processing time and make
#' the contour less smooth.
#'
#' Note that you can also get similar estimates per frame from
#' \code{\link{analyze}} on a normalized scale of 0 to 1, but \code{getRMS} is
#' much faster, operates on the original scale, and plots the amplitude contour.
#' If you need RMS for the entire sound instead of per frame, you can simply
#' calculate it as \code{sqrt(mean(x^2))}, where \code{x} is your waveform.
#' Having RMS estimates per frame gives more flexibility: RMS per sound can be
#' calculated as the mean / median / max of RMS values per frame.
#'
#' @seealso \code{\link{getRMSFolder}}
#'   \code{\link{analyze}}\code{\link{getLoudness}}
#'
#' @inheritParams spectrogram
#' @param scale maximum possible amplitude of input used for normalization (not
#'   needed for audio files)
#' @param normalize if TRUE, RMS amplitude is normalized to [0, 1]
#' @param killDC if TRUE, removed DC offset (see also \code{\link{flatEnv}})
#' @param windowDC the window for calculating DC offset, ms
#' @param xlab,ylab general graphical parameters
#' @param type,col,lwd graphical parameters pertaining to the RMS envelope
#' @param ... other graphical parameters
#' @return Returns a numeric vector of RMS amplitudes per frame on the scale of
#'   input. Names give time stamps for the center of each frame, in ms.
#' @export
#' @examples
#' s = soundgen() + .1  # with added DC offset
#' plot(s, type = 'l')
#' r = getRMS(s, samplingRate = 16000,
#'   windowLength = 40, overlap = 50, killDC = TRUE,
#'   col = 'green', lty = 2, main = 'RMS envelope')
#' # short window = jagged envelope
#' r = getRMS(s, samplingRate = 16000,
#'   windowLength = 5, overlap = 0, killDC = TRUE,
#'   col = 'green', lty = 2, main = 'RMS envelope')
#' \dontrun{
#' r = getRMS('~/Downloads/temp/032_ut_anger_30-m-roar-curse.wav')
#' }
getRMS = function(x,
                  samplingRate = NULL,
                  windowLength = 50,
                  step = NULL,
                  overlap = 75,
                  killDC = FALSE,
                  scale = NULL,
                  normalize = TRUE,
                  windowDC = 200,
                  plot = TRUE,
                  xlab = 'Time, ms',
                  ylab = '',
                  type = 'b',
                  col = 'blue',
                  lwd = 2,
                  ...) {
  sound = NULL
  if (overlap < 0 | overlap > 100) {
    warning('overlap must be >0 and <= 100%; resetting to 70')
    overlap = 70
  }
  if (is.null(step)) step = windowLength * (1 - overlap / 100)

  # import audio
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
    windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
    sound = sound_wav@left
    scale = 2^(sound_wav@bit - 1)
    if (windowLength_points > (length(sound) / 2)) {
      windowLength_points = floor(length(sound) / 4) * 2
      step = round(windowLength * (1 - overlap / 100))
    }
    if (windowLength_points == 0) {
      stop('The sound and/or the windowLength is too short')
    }
    duration = length(sound) / samplingRate
  } else if (class(x) == 'numeric' & length(x) > 1) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    } else {
      sound = x
      m = max(abs(sound))
      if (is.null(scale)) {
        scale = max(m, 1)
        message(paste('Scale not specified. Assuming that max amplitude is', scale))
      } else if (is.numeric(scale)) {
        if (scale < m) {
          scale = m
          warning(paste('Scale cannot be smaller than observed max; resetting to', m))
        }
      }
      duration = length(sound) / samplingRate
      windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
      if (windowLength_points > (length(sound) / 2)) {
        windowLength_points = floor(length(sound) / 4) * 2
        step = round(windowLength * (1 - overlap / 100))
      }
      if (windowLength_points == 0) {
        stop('The sound and/or the windowLength is too short')
      }
    }
  }

  # DC offset
  if (killDC) {
    sound = killDC(sound, windowLength = windowDC, samplingRate = samplingRate)
  }

  # calculate RMS per frame
  step_points = round(step / 1000 * samplingRate)
  myseq = seq(1, max(1, (length(sound) - windowLength_points)), step_points)
  r = apply(as.matrix(myseq), 1, function(x) {
    sqrt(mean(sound[x:(windowLength_points + x - 1)] ^ 2))
  })
  names(r) = round(myseq / samplingRate * 1000 + windowLength / 2)

  # plotting
  if (plot) {
    time = 1:length(sound) / samplingRate * 1000
    plot(time, sound, type = 'n', xlab = xlab, ylab = ylab, ...)
    points(time, sound, type = 'l')
    points(as.numeric(names(r)), r, type = type, col = col, lwd = lwd, ...)
  }

  if (normalize) r = r / scale
  return(r)
}


#' RMS amplitude per folder
#'
#' A wrapper around \code{\link{getRMS}} that goes through all wav/mp3 files in
#' a folder and returns either a list with RMS values per frame from each file
#' or, if \code{summary = TRUE}, a dataframe with a single summary value of RMS
#' per file. This summary value can be mean, max and so on, as per
#' \code{summaryFun}.
#'
#' @seealso \code{\link{getRMS}} \code{\link{analyze}}\code{\link{getLoudness}}
#'
#' @param myfolder path to folder containing wav/mp3 files
#' @inheritParams getRMS
#' @param summary if TRUE, returns only a single value of RMS per file
#' @param summaryFun the function used to summarize RMS values across all frames
#'   (if \code{summary = TRUE})
#' @param verbose if TRUE, reports estimated time left
#' @export
#' @examples
#' \dontrun{
#' getRMSFolder('~/Downloads/temp')
#' # Compare:
#' analyzeFolder('~/Downloads/temp', pitchMethods = NULL,
#'               plot = FALSE)$ampl_mean
#' # (per STFT frame, but should be very similar)
#'
#' User-defined summary functions:
#' difRan = function(x) diff(range(x))
#' getRMSFolder('~/Downloads/temp', summaryFun = c('mean', 'difRan'))
#'
#' meanSD = function(x) {
#'   paste0('mean = ', round(mean(x), 2), '; sd = ', round(sd(x), 2))
#' }
#' getRMSFolder('~/Downloads/temp', summaryFun = 'meanSD')
#' }
getRMSFolder = function(myfolder,
                        windowLength = 50,
                        step = NULL,
                        overlap = 70,
                        normalize = TRUE,
                        killDC = FALSE,
                        windowDC = 200,
                        summary = TRUE,
                        summaryFun = 'mean',
                        verbose = TRUE) {
  time_start = proc.time()  # timing
  filenames = list.files(myfolder, pattern = "*.wav|.mp3", full.names = TRUE)
  if (length(filenames) < 1) stop('No audio files to analyze')
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
    result[[i]] = do.call(getRMS, c(filenames[i], myPars, plot = FALSE, verbose = FALSE))
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

#' Normalize folder
#'
#' Normalizes the amplitude of all wav/mp3 files in a folder based on their peak
#' or RMS amplitude or subjective loudness. This is good for playback
#' experiments, which require that all sounds should have similar intensity or
#' loudness.
#'
#' Algorithm: first all files are rescaled to have the same peak amplitude of
#' \code{maxAmp} dB. If \code{type = 'peak'}, the process ends here. If
#' \code{type = 'rms'}, there are two additional steps. First the original RMS
#' amplitude of all files is calculated per frame by \code{\link{getRMS}}. The
#' "quietest" sound with the lowest summary RMS value is not modified, so its
#' peak amplitude remains \code{maxAmp} dB. All the remaining sounds are
#' rescaled linearly, so that their summary RMS values becomes the same as that
#' of the "quietest" sound, and their peak amplitudes become smaller,
#' \code{<maxAmp}. Finally, if \code{type = 'loudness'}, the subjective
#' loudness of each sound is estimated by \code{\link{getLoudness}}, which
#' assumes frequency sensitivity typical of human hearing. The following
#' normalization procedure is similar to that for \code{type = 'rms'}.
#'
#' @seealso \code{\link{getRMS}} \code{\link{analyze}}\code{\link{getLoudness}}
#'
#' @inheritParams getRMSFolder
#' @param type normalize so the output files has the same peak amplitude
#'   ('peak'), root mean square amplitude ('rms'), or subjective loudness in
#'   sone ('loudness')
#' @param maxAmp maximum amplitude in dB (0 = max possible, -10 = 10 dB below
#'   max possible, etc.)
#' @param summaryFun should the output files have the same mean / median / max
#'   etc rms amplitude or loudness? (summaryFun has no effect if type = 'peak')
#' @param savepath full path to where the normalized files should be saved
#'   (defaults to '/normalized')
#' @export
#' @examples
#' \dontrun{
#' # put a few short audio files in a folder, eg '~/Downloads/temp'
#' getRMSFolder('~/Downloads/temp', summaryFun = 'mean')  # different
#' normalizeFolder('~/Downloads/temp', type = 'rms', summaryFun = 'mean',
#'   savepath = '~/Downloads/temp/normalized')
#' getRMSFolder('~/Downloads/temp/normalized', summaryFun = 'mean')  # same
#' # If the saved audio files are treated as stereo with one channel missing,
#' # try reconverting with ffmpeg (saving is handled by tuneR::writeWave)
#' }
normalizeFolder = function(myfolder,
                           type = c('peak', 'rms', 'loudness')[1],
                           maxAmp = 0,
                           summaryFun = 'mean',
                           windowLength = 50,
                           step = NULL,
                           overlap = 70,
                           killDC = FALSE,
                           windowDC = 200,
                           savepath = NULL,
                           verbose = TRUE) {
  time_start = proc.time()  # timing
  filenames = list.files(myfolder, pattern = "*.wav|.mp3", full.names = TRUE)
  # in order to provide more accurate estimates of time to completion,
  # check the size of all files in the target folder
  n = length(filenames)

  ## load all the files
  if (verbose) print('Loading...')
  files = vector('list', n)
  for (i in 1:n) {
    ext = substr(filenames[i], (nchar(filenames[i]) - 2), nchar(filenames[i]))
    if (ext %in% c('wav', 'WAV')) {
      files[[i]] = tuneR::readWave(filenames[i])
    } else if (ext %in% c('mp3', 'MP3')) {
      files[[i]] = tuneR::readMP3(filenames[i])
    } else {
      stop('Input not recognized')
    }
  }

  ## process all the files
  if (verbose) print('Processing...')
  # for either peak or RMS normalization, start by peak normalization to maxAmp dB
  level = 10 ^ (maxAmp / 20)
  for (i in 1:n) {
    files[[i]] = tuneR::normalize(files[[i]],
                                  unit = as.character(files[[i]]@bit),
                                  rescale = TRUE, level = level)
  }

  # for RMS- or loudness-normalization, perform additional steps
  if (type %in% c('rms', 'loudness')) {
    perSound = vector('list', n)
    if (type == 'rms') {
      for (i in 1:n) {
        # calculate the RMS amplitude of each file
        perSound[[i]] = getRMS(files[[i]]@left,
                               samplingRate = files[[i]]@samp.rate,
                               windowLength = windowLength,
                               step = step,
                               overlap = overlap,
                               scale = 2^(files[[i]]@bit - 1),
                               killDC = killDC,
                               windowDC = windowDC,
                               plot = FALSE)
      }
    } else if (type == 'loudness') {
      for (i in 1:n) {
        # estimate subjective loudness of each file
        perSound[[i]] = getLoudness(as.numeric(files[[i]]@left),
                                    samplingRate = files[[i]]@samp.rate,
                                    scale = 2^(files[[i]]@bit - 1),
                                    windowLength = windowLength,
                                    step = step,
                                    plot = FALSE)$loudness
      }
    }

    # summary measure per file
    summaryPerSound = unlist(lapply(perSound, summaryFun))
    names(summaryPerSound) = basename(filenames)

    # find the quietest file
    ref = which.min(summaryPerSound)

    # the quietest file is untouched, but all others are rescaled to have the
    # same RMS/loudness as the quietest one
    for (i in 1:n) {
      if (i != ref) {
        if (type == 'rms') {
          rescale = summaryPerSound[ref] / summaryPerSound[i]
        } else if (type == 'loudness') {
          rescale = (summaryPerSound[ref] / summaryPerSound[i]) ^ (5 / 3)
        }
        files[[i]]@left = as.integer(round(files[[i]]@left * rescale))
      }
    }
  }

  # save the rescaled files
  if (is.null(savepath)) savepath = paste0(myfolder, '/normalized')
  if (!is.na(savepath)) {
    print('Saving...')
    if (!dir.exists(savepath)) dir.create(savepath)
    for (i in 1:n) {
      file_wo_ext = sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(filenames[i]))
      tuneR::writeWave(
        files[[i]],
        filename = paste0(savepath, '/', file_wo_ext, '.wav')
      )
    }
  }

  # report time
  if (verbose) {
    reportTime(i = n, nIter = n, time_start = time_start)
  }
}
# Where is the ^5/3 in loudness adjustment coming from?
# s = '~/Downloads/temp/145_ut_effort_24.wav'
# s1 = tuneR::readWave(s)
# s2 = as.numeric(s1@left)
# range(s2)
#
# mean(getLoudness(s2, samplingRate = s1@samp.rate, scale = 2^(s1@bit-1), plot = FALSE)$loudness)
# mean(getLoudness(s2 / 10, samplingRate = s1@samp.rate, scale = 2^(s1@bit-1), plot = FALSE)$loudness)
#
# out = data.frame(coef = seq(0, 1, length.out = 100), loud = NA)
# for (i in 1:nrow(out)) {
#   out$loud[i] = mean(getLoudness(s2 * out$coef[i],
#                                  samplingRate = s1@samp.rate,
#                                  scale = 2^(s1@bit-1),
#                                  plot = FALSE)$loudness)
# }
# plot(out, type = 'l')
#
# mod = nls(loud ~ a + b * coef ^ c, out, start = list(a = 0, b = 1, c = .5))
# plot(out, type = 'l')
# points(out$coef, predict(mod, list(coef = out$coef)), type = 'b', col = 'green')
# summary(mod)  # a = 0, b = 12, c = 0.6
# # so loud1/loud2 = coef1^c / coef2^c = (coef1/coef2)^c, where c = 0.6,
# # so coef1/coef2 = (loud1/loud2)^(1/0.6) = (loud1/loud2)^(5/3)


#' Flat envelope
#'
#' Flattens the amplitude envelope of a waveform. This is achieved by dividing
#' the waveform by some function of its smoothed amplitude envelope (Hilbert,
#' peak or root mean square).
#' @param sound input vector oscillating about zero
#' @param windowLength the length of smoothing window, ms
#' @param samplingRate the sampling rate, Hz. Only needed if the length of
#'   smoothing window is specified in ms rather than points
#' @param method 'hil' for Hilbert envelope, 'rms' for root mean square
#'   amplitude, 'peak' for peak amplitude per window
#' @param windowLength_points the length of smoothing window, points. If
#'   specified, overrides both \code{windowLength} and \code{samplingRate}
#' @param killDC if TRUE, dynamically removes DC offset or similar deviations of
#'   average waveform from zero
#' @param dynamicRange parts of sound quieter than \code{-dynamicRange} dB will
#'   not be amplified
#' @param plot if TRUE, plots the original sound, smoothed envelope, and
#'   flattened sound
#' @export
#' @examples
#' a = rnorm(500) * seq(1, 0, length.out = 500)
#' b = flatEnv(a, plot = TRUE, windowLength_points = 5)    # too short
#' c = flatEnv(a, plot = TRUE, windowLength_points = 250)  # too long
#' d = flatEnv(a, plot = TRUE, windowLength_points = 50)   # about right
#'
#' \dontrun{
#' s = soundgen(sylLen = 1000, ampl = c(0, -40, 0), plot = TRUE, osc = TRUE)
#' # playme(s)
#' s_flat1 = flatEnv(s, plot = TRUE, windowLength = 50, method = 'hil')
#' s_flat2 = flatEnv(s, plot = TRUE, windowLength = 10, method = 'rms')
#' s_flat3 = flatEnv(s, plot = TRUE, windowLength = 10, method = 'peak')
#' # playme(s_flat2)
#'
#' # Remove DC offset
#' s1 = c(rep(0, 50), runif(1000, -1, 1), rep(0, 50)) +
#'      seq(.3, 1, length.out = 1100)
#' s2 = flatEnv(s1, plot = TRUE, windowLength_points = 50, killDC = FALSE)
#' s3 = flatEnv(s1, plot = TRUE, windowLength_points = 50, killDC = TRUE)
#' }
flatEnv = function(sound,
                   windowLength = 200,
                   samplingRate = 16000,
                   method = c('hil', 'rms', 'peak')[1],
                   windowLength_points = NULL,
                   killDC = FALSE,
                   dynamicRange = 80,
                   plot = FALSE) {
  if (!is.numeric(windowLength_points)) {
    if (is.numeric(windowLength)) {
      if (is.numeric(samplingRate)) {
        windowLength_points = windowLength / 1000 * samplingRate
      } else {
        stop(paste('Please specify either windowLength (ms) plus samplingRate (Hz)',
                   'or the length of smoothing window in points (windowLength_points)'))
      }
    }
  }

  m = max(abs(sound))       # original scale (eg -1 to +1 gives m = 1)
  soundNorm = sound / m    # normalize
  throwaway_lin = 10 ^ (-dynamicRange / 20)  # from dB to linear
  # get smoothed amplitude envelope
  env = getEnv(sound = soundNorm,
               windowLength_points = windowLength_points,
               method = method)
  env = env / max(abs(env))

  # don't amplify very quiet sections
  env_cut = env
  env_cut[env_cut < throwaway_lin] = 1
  # flatten amplitude envelope
  soundFlat = soundNorm / env_cut
  # re-normalize to original scale
  soundFlat = soundFlat / max(abs(soundFlat)) * m
  # remove DC offset
  if (killDC) {
    soundFlat = killDC(sound = soundFlat,
                       windowLength_points = windowLength_points,
                       plot = FALSE)
  }

  if (plot) {
    op = par('mfrow')
    par(mfrow = c(1, 2))
    plot(sound, type = 'l', main = 'Original')
    points(env * m, type = 'l', lty = 1, col = 'blue')
    plot(soundFlat, type = 'l', main = 'Flattened')
    par(mfrow = op)
  }
  return(soundFlat)
}


#' Transplant envelope
#'
#' Extracts a smoothed amplitude envelope of the \code{donor} sound and applies
#' it to the \code{recipient} sound. Both sounds are provided as numeric
#' vectors; they can differ in length and sampling rate. Note that the result
#' depends on the amount of smoothing (controlled by \code{windowLength}) and
#' the chosen method of calculating the envelope. Very similar to
#' \code{\link[seewave]{setenv}}, but with a different smoothing algorithm and
#' with a choice of several types of envelope: hil, rms, or peak.
#'
#' @seealso \code{\link{flatEnv}}, \code{\link[seewave]{setenv}}
#'
#' @param donor the sound that "donates" the amplitude envelope
#' @param recipient the sound that needs to have its amplitude envelope adjusted
#' @param samplingRateD,samplingRateR sampling rate of the donor and recipient,
#'   respectively
#' @inheritParams flatEnv
#'
#' @examples
#' donor = rnorm(500) * seq(1, 0, length.out = 500)
#' recipient = soundgen(sylLen = 600, addSilence = 50)
#' transplantEnv(donor, samplingRateD = 200,
#'                recipient, samplingRateR = 16000,
#'                windowLength = 50, method = 'hil', plot = TRUE)
#' transplantEnv(donor, samplingRateD = 200,
#'                recipient, samplingRateR = 16000,
#'                windowLength = 10, method = 'peak', plot = TRUE)
transplantEnv = function(
  donor,
  samplingRateD,
  recipient,
  samplingRateR = samplingRateD,
  windowLength = 200,
  method = c('hil', 'rms', 'peak')[1],
  killDC = FALSE,
  dynamicRange = 80,
  plot = FALSE
) {
  windowLength_points_donor = windowLength / 1000 * samplingRateD
  windowLength_points_recip = windowLength / 1000 * samplingRateR
  throwaway_lin = 10 ^ (-dynamicRange / 20)  # from dB to linear

  # get the amplitude envelope of the recipient
  env_recipient = getEnv(sound = recipient,
                         windowLength_points = windowLength_points_recip,
                         method = method)
  len_recipient = length(env_recipient)
  # don't amplify very quiet sections
  env_recip_cut = env_recipient
  env_recip_cut[env_recip_cut < throwaway_lin] = 1

  # get the amplitude envelope of the donor
  env_donor = getEnv(sound = donor,
                     windowLength_points = windowLength_points_donor,
                     method = method)
  env_donor1 = env_donor / max(env_donor)  # normalize
  env_donor1 = approx(env_donor1, n = len_recipient)$y

  # flatten the envelope of the recipient and apply the donor's envelope
  out = recipient / env_recip_cut * env_donor1
  if (plot) {
    len_donor = length(env_donor)
    time_donor = seq(0, len_donor / samplingRateD, length.out = len_donor)
    max_donor = max(abs(donor))

    time_recipient = seq(0, len_recipient / samplingRateR, length.out = len_recipient)
    max_recipient = max(abs(recipient))
    max_out = max(abs(out))

    op = par('mfrow')
    par(mfrow = c(1, 3))

    plot(time_donor, donor, type = 'l', main = 'Donor', ylim = c(-max_donor, max_donor), xlab = '', ylab = 'Amplitude')
    points(time_donor, env_donor, type = 'l', lty = 1, col = 'blue')

    plot(time_recipient, recipient, type = 'l', main = 'Recipient', ylim = c(-max_recipient, max_recipient), xlab = 'Time, s', ylab = '')
    points(time_recipient, env_recipient, type = 'l', lty = 1, col = 'blue')

    plot(time_recipient, out, type = 'l', main = 'Output', ylim = c(-max_out, max_out), xlab = '', ylab = '')
    points(time_recipient, getEnv(out, windowLength_points_recip, method), type = 'l', lty = 1, col = 'blue')

    par(mfrow = op)
  }
  invisible(out)
}
