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
#' @inheritParams spectrogram
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
                  overlap = 70,
                  killDC = FALSE,
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
    maxAmpl = 2^(sound_wav@bit - 1)
    if (windowLength_points > (length(sound) / 2)) {
      windowLength_points = floor(length(sound) / 4) * 2
      step = round(windowLength * (1 - overlap / 100))
    }
    if (windowLength_points == 0) {
      stop('The sound and/or the windowLength is too short for plotting a spectrogram')
    }
    duration = length(sound) / samplingRate
  } else if (class(x) == 'numeric' & length(x) > 1) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    } else {
      sound = x
      maxAmpl = max(abs(sound))
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
  return(r)
}


#' RMS amplitude per folder
#'
#' A wrapper around \code{\link{getRMS}} that goes through all wav/mp3 files in
#' a folder and returns either a list with RMS values per frame from each file
#' or, if \code{summary = TRUE}, a dataframe with a single summary value of RMS
#' per file. This summary value can be mean, max and so on, as per
#' \code{summaryFun}.
#' @param myfolder path to folder containing wav/mp3 files
#' @inheritParams getRMS
#' @param summary if TRUE, returns only a single value of RMS per file
#' @param summaryFun the function used to summarize RMS values per frame (if
#'   \code{summary = TRUE})
#' @param verbose if TRUE, reports estimated time left
#' @export
#' @examples
#' \dontrun{
#' getRMSFolder('~/Downloads/temp')
#' getRMSFolder('~/Downloads/temp', summaryFun = function(x) diff(range(x)))
#' }
getRMSFolder = function(myfolder,
                        samplingRate = NULL,
                        windowLength = 50,
                        step = NULL,
                        overlap = 70,
                        killDC = FALSE,
                        windowDC = 200,
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
    result[[i]] = do.call(getRMS, c(filenames[i], myPars, plot = FALSE, verbose = FALSE))
    if (verbose) {
      reportTime(i = i, nIter = length(filenames),
                 time_start = time_start, jobs = filesizes)
    }
  }

  # prepare output
  if (summary == TRUE) {
    output = data.frame(sound = basename(filenames))
    output$rms = unlist(lapply(result, summaryFun))
  } else {
    output = result
    names(output) = basename(filenames)
  }
  return(output)
}

#' Normalize folder
#'
#' Normalizes the amplitude of all wav/mp3 files in a folder based on their peak or RMS amplitude or on their subjective loudness. This is good for playback experiments, which require that all sounds should have similar intensity or loudness.
#'
#' Algorithm: first all files are rescaled to have the same peak amplitude of \code{maxAmp} dB. If \code{type = 'peak'}, the process ends here. If \code{type = 'rms'}, there are two additional steps. First the original RMS amplitude of all files is calculated per frame by \code{\link{getRMS}}. The "quietest" sound with the lowest summary RMS value is not modified, so its peak amplitude remains \code{maxAmp} dB. All the remaining sounds are rescaled linearly, so that their summary RMS values becomes the same as that of the "quietest" sound, and their peak amplitudes become smaller, \code{<maxAmp}. Finally, if \code{\link{type = 'loudness'}}, the subjective loudness of each sound is estimated by \code{\link{getLoudness}}, which assumes frequency sensitivity typical of human hearing. The following normalization procedure is similar to that for \code{type = 'rms'}.
#' @inheritParams getRMSFolder
#' @examples
#' \dontrun{
#' getRMSFolder('~/Downloads/temp', summaryFun = 'mean')
#' normalizeFolder('~/Downloads/temp', type = 'loudness', summaryFun = 'mean',
#'   savepath = '~/Downloads/temp/normalized')
#' getRMSFolder('~/Downloads/temp/normalized', summaryFun = 'mean')
#' mean(getLoudness('~/Downloads/temp/normalized/032_ut_anger_30-m-roar-curse.wav')$loudness)
#' mean(getLoudness('~/Downloads/temp/normalized/110_ut_disgust_42-c_weird-tone-lemon.wav')$loudness)
#' mean(getLoudness('~/Downloads/temp/normalized/118_ut_disgust_52-m-toilet.wav')$loudness)
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
      tuneR::writeWave(
        files[[i]],
        filename = paste0(savepath, '/', basename(filenames[i]))
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

