#' Get RMS folder
#'
#' Deprecated; use \code{\link{getRMS}} instead
#' @param ... any input parameters
getRMSFolder = function(...) {
  message('getRMSFolder() is deprecated; please use getRMS() instead')
}


#' RMS amplitude
#'
#' Calculates root mean square (RMS) amplitude in overlapping windows, providing
#' an envelope of RMS amplitude - a measure of sound intensity. Longer windows
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
#' @seealso \code{\link{analyze}} \code{\link{getLoudness}}
#'
#' @inheritParams spectrogram
#' @inheritParams segment
#' @param killDC if TRUE, removed DC offset (see also \code{\link{flatEnv}})
#' @param normalize if TRUE, the RMS amplitude is returned as proportion of
#'   the maximum possible amplitude as given by \code{scale}
#' @param windowDC the window for calculating DC offset, ms
#' @param plot if TRUE, plot a contour of RMS amplitude
#' @param xlab,ylab general graphical parameters
#' @param type,col,lwd graphical parameters pertaining to the RMS envelope
#' @param ... other graphical parameters
#'
#' @return Returns a list containing: \itemize{\item{detailed}{a list of RMS
#'   amplitudes per frame for each sound, on the scale of input; names give time
#'   stamps for the center of each frame, in ms.} \item{summary}{a dataframe
#'   with summary measures, one row per sound}}
#'
#' @export
#' @examples
#' s = soundgen() + .25  # with added DC offset
#' osc(s)
#' r = getRMS(s, samplingRate = 16000,
#'   windowLength = 40, overlap = 50, killDC = TRUE,
#'   plot = TRUE, type = 'l', lty = 2, main = 'RMS envelope')
#' # short window = jagged envelope
#' r = getRMS(s, samplingRate = 16000,
#'   windowLength = 5, overlap = 0, killDC = TRUE,
#'   plot = TRUE, col = 'blue', pch = 13, main = 'RMS envelope')
#' \dontrun{
#' r = getRMS('~/Downloads/temp', savePlots = '~/Downloads/temp/plots')
#' r$summary
#'
#' # Compare:
#' analyze('~/Downloads/temp', pitchMethods = NULL,
#'         plot = FALSE)$ampl_mean
#' # (per STFT frame, but should be very similar)
#'
#' User-defined summary functions:
#' ran = function(x) diff(range(x))
#' meanSD = function(x) {
#'   paste0('mean = ', round(mean(x), 2), '; sd = ', round(sd(x), 2))
#' }
#' getRMS('~/Downloads/temp', summaryFun = c('mean', 'ran', 'meanSD'))$summary
#' }
getRMS = function(x,
                  samplingRate = NULL,
                  scale = NULL,
                  from = NULL,
                  to = NULL,
                  windowLength = 50,
                  step = NULL,
                  overlap = 70,
                  killDC = FALSE,
                  normalize = TRUE,
                  windowDC = 200,
                  summaryFun = 'mean',
                  reportEvery = NULL,
                  plot = FALSE,
                  savePlots = NULL,
                  xlab = '',
                  ylab = '',
                  type = 'b',
                  col = 'green',
                  lwd = 2,
                  width = 900,
                  height = 500,
                  units = 'px',
                  res = NA,
                  ...) {
  # match args
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to',
    'savePlots', 'reportEvery', 'summaryFun')]
  pa = processAudio(x,
                    samplingRate = samplingRate,
                    scale = scale,
                    from = from,
                    to = to,
                    funToCall = 'getRMSSound',
                    savePlots = savePlots,
                    myPars = myPars,
                    reportEvery = reportEvery
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots)) {
    htmlPlots(
      htmlFile = paste0(pa$input$savePlots, '00_clickablePlots_rms.html'),
      plotFiles = paste0(pa$input$savePlots, pa$input$filenames_base, "_rms.png"),
      audioFiles = pa$input$filenames,
      width = paste0(width, units))
  }

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in 1:pa$input$n) {
      temp[[i]] = summarizeAnalyze(
        data.frame(ampl = pa$result[[i]]),
        summaryFun = summaryFun,
        var_noSummary = NULL)
    }
    mysum_all = cbind(data.frame(file = pa$input$filenames_base),
                      do.call('rbind', temp))
  } else {
    mysum_all = NULL
  }
  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(list(
    detailed = pa$result,
    summary = mysum_all
  ))
}


#' RMS amplitude per sound
#'
#' Internal soundgen function called by \code{\link{getRMS}}.
#' @param audio a list returned by \code{readAudio}
#' @inheritParams getRMS
#' @keywords internal
getRMSSound = function(audio,
                       windowLength = 50,
                       step = NULL,
                       overlap = 75,
                       killDC = FALSE,
                       normalize = TRUE,
                       windowDC = 200,
                       plot = TRUE,
                       xlab = '',
                       ylab = '',
                       type = 'b',
                       col = 'green',
                       lwd = 2,
                       width = 900,
                       height = 500,
                       units = 'px',
                       res = NA,
                       ...) {
  if (overlap < 0 | overlap > 100) {
    warning('overlap must be >0 and <= 100%; resetting to 70')
    overlap = 70
  }
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  if (!is.numeric(windowLength) | windowLength <= 0 |
      windowLength > (audio$duration * 1000)) {
    windowLength = min(50, audio$duration / 2 * 1000)
    warning(paste0('"windowLength" must be between 0 and sound_duration ms;
            defaulting to ', windowLength, ' ms'))
  }
  windowLength_points = floor(windowLength / 1000 * audio$samplingRate / 2) * 2
  step_points = round(step / 1000 * audio$samplingRate)
  step = step_points / audio$samplingRate * 1000
  windowLength = windowLength_points / audio$samplingRate * 1000
  # step_points can only be an integer, introducing small timing errors in long sounds

  # DC offset
  if (killDC) {
    audio$sound = killDC(audio$sound, windowLength = windowDC,
                         samplingRate = audio$samplingRate)
  }

  # calculate RMS per frame
  myseq = seq(1, max(1, (audio$ls - windowLength_points)), step_points)
  r = apply(as.matrix(myseq), 1, function(x) {
    sqrt(mean(audio$sound[x:(windowLength_points + x - 1)] ^ 2))
  })
  names(r) = myseq / audio$samplingRate * 1000 + windowLength / 2

  # plotting
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_base, "_rms.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    time = 1:audio$ls / audio$samplingRate * 1000
    plot(time, audio$sound, type = 'n', xlab = xlab, ylab = ylab, xaxt = 'n',
         ylim = c(-audio$scale, audio$scale), ...)
    time_location = axTicks(1)
    time_labels = convert_sec_to_hms(time_location / 1000, 3)
    axis(side = 1, at = time_location, labels = time_labels)
    points(time, audio$sound, type = 'l')
    points(as.numeric(names(r)), r, type = type, col = col, lwd = lwd, ...)
    if (is.character(audio$savePlots)) dev.off()
  }

  if (normalize) r = r / audio$scale
  return(r)
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
#' @seealso \code{\link{getRMS}} \code{\link{analyze}} \code{\link{getLoudness}}
#'
#' @inheritParams getRMS
#' @param myfolder full path to folder containing input audio files
#' @param type normalize so the output files has the same peak amplitude
#'   ('peak'), root mean square amplitude ('rms'), or subjective loudness in
#'   sone ('loudness')
#' @param maxAmp maximum amplitude in dB (0 = max possible, -10 = 10 dB below
#'   max possible, etc.)
#' @param summaryFun should the output files have the same mean / median / max
#'   etc rms amplitude or loudness? (summaryFun has no effect if type = 'peak')
#' @param saveAudio full path to where the normalized files should be saved
#'   (defaults to 'myfolder/normalized')
#' @export
#' @examples
#' \dontrun{
#' # put a few short audio files in a folder, eg '~/Downloads/temp'
#' getRMS('~/Downloads/temp2', summaryFun = 'mean')$summary  # different
#' normalizeFolder('~/Downloads/temp2', type = 'rms', summaryFun = 'mean',
#'   saveAudio = '~/Downloads/temp2/normalized')
#' getRMS('~/Downloads/temp2/normalized', summaryFun = 'mean')$summary  # same
#' # If the saved audio files are treated as stereo with one channel missing,
#' # try reconverting with ffmpeg (saving is handled by tuneR::writeWave)
#' }
normalizeFolder = function(
  myfolder,
  type = c('peak', 'rms', 'loudness')[1],
  maxAmp = 0,
  summaryFun = 'mean',
  windowLength = 50,
  step = NULL,
  overlap = 70,
  killDC = FALSE,
  windowDC = 200,
  saveAudio = NULL,
  reportEvery = NULL
) {
  time_start = proc.time()  # timing
  filenames = list.files(myfolder, pattern = "*.wav|.mp3|.WAV|.MP3",
                         full.names = TRUE)
  filesizes = file.info(filenames)$size
  if (length(filenames) < 1) {
    stop(paste('No wav/mp3 files found in', myfolder))
  }
  # in order to provide more accurate estimates of time to completion,
  # check the size of all files in the target folder
  n = length(filenames)

  ## load all the files
  print('Loading...')
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
  print('Processing...')
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
        perSound[[i]] = getRMS(files[[i]],
                               windowLength = windowLength,
                               step = step,
                               overlap = overlap,
                               scale = 2^(files[[i]]@bit - 1),
                               killDC = killDC,
                               windowDC = windowDC,
                               plot = FALSE)$detailed
      }
    } else if (type == 'loudness') {
      for (i in 1:n) {
        # estimate subjective loudness of each file
        perSound[[i]] = getLoudness(files[[i]],
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
  if (is.null(saveAudio)) saveAudio = paste0(myfolder, '/normalized')
  if (!is.na(saveAudio)) {
    print('Saving...')
    if (!dir.exists(saveAudio)) dir.create(saveAudio)
    for (i in 1:n) {
      file_wo_ext = sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(filenames[i]))
      tuneR::writeWave(
        files[[i]],
        filename = paste0(saveAudio, '/', file_wo_ext, '.wav')
      )
    }
  }

  # report time
  if (is.null(reportEvery) || is.finite(reportEvery)) {
    reportTime(i = n, nIter = n, reportEvery = reportEvery,
               time_start = time_start, jobs = filesizes)
  }
}


#' Flat envelope / compressor
#'
#' Applies a compressor - that is, flattens the amplitude envelope of a
#' waveform, reducing the difference in amplitude between loud and quiet
#' sections. This is achieved by dividing the waveform by some function of its
#' smoothed amplitude envelope (Hilbert, peak or root mean square).
#'
#' @inheritParams spectrogram
#' @inheritParams segment
#' @param compression the amount of compression to apply: 0 = none, 1 = maximum
#' @param method hil = Hilbert envelope, rms = root mean square amplitude, peak
#'   = peak amplitude per window
#' @param windowLength the length of smoothing window, ms
#' @param windowLength_points the length of smoothing window, points. If
#'   specified, overrides \code{windowLength}
#' @param killDC if TRUE, dynamically removes DC offset or similar deviations of
#'   average waveform from zero (see examples)
#' @param dynamicRange parts of sound quieter than \code{-dynamicRange} dB will
#'   not be amplified
#' @param plot if TRUE, plots the original sound, the smoothed envelope, and
#'   the compressed sound
#' @param saveAudio full path to the folder in which to save the compressed
#'   sound(s)
#' @param col the color of amplitude contours
#' @param ... other graphical parameters passed to \code{points()} that control
#'   the appearance of amplitude contours, eg \code{lwd, lty}, etc.
#'
#' @return If the input is a single audio (file, Wave, or numeric vector),
#'   returns the compressed waveform as a numeric vector with the original
#'   sampling rate and scale. If the input is a folder with several audio files,
#'   returns a list of compressed waveforms, one for each file.
#' @export
#' @examples
#' a = rnorm(500) * seq(1, 0, length.out = 500)
#' b = flatEnv(a, 1000, plot = TRUE, windowLength_points = 5)    # too short
#' c = flatEnv(a, 1000, plot = TRUE, windowLength_points = 250)  # too long
#' d = flatEnv(a, 1000, plot = TRUE, windowLength_points = 50)   # about right
#'
#' \dontrun{
#' s = soundgen(sylLen = 1000, ampl = c(0, -40, 0), plot = TRUE)
#' # playme(s)
#' s_flat1 = flatEnv(s, 16000, dynamicRange = 60, plot = TRUE,
#'                   windowLength = 50, method = 'hil')
#' s_flat2 = flatEnv(s, 16000, dynamicRange = 60, plot = TRUE,
#'                   windowLength = 10, method = 'rms')
#' s_flat3 = flatEnv(s, 16000, dynamicRange = 60, plot = TRUE,
#'                   windowLength = 10, method = 'peak')
#' # playme(s_flat2)
#'
#' # Remove DC offset
#' s1 = c(rep(0, 50), runif(1000, -1, 1), rep(0, 50)) +
#'      seq(.3, 1, length.out = 1100)
#' s2 = flatEnv(s1, 16000, plot = TRUE, windowLength_points = 50, killDC = FALSE)
#' s3 = flatEnv(s1, 16000, plot = TRUE, windowLength_points = 50, killDC = TRUE)
#'
#' # Compress and save all audio files in a folder
#' s4 = flatEnv('~/Downloads/temp2',
#'              method = 'peak', compression = .5,
#'              saveAudio = '~/Downloads/temp2/compressed',
#'              savePlots = '~/Downloads/temp2/compressed',
#'              col = 'green', lwd = 5)
#' osc(s4[[1]])
#' }
flatEnv = function(x,
                   samplingRate = NULL,
                   scale = NULL,
                   compression = 1,
                   method = c('hil', 'rms', 'peak')[1],
                   windowLength = 50,
                   windowLength_points = NULL,
                   killDC = FALSE,
                   dynamicRange = 40,
                   reportEvery = NULL,
                   saveAudio = NULL,
                   plot = FALSE,
                   savePlots = NULL,
                   col = 'blue',
                   width = 900,
                   height = 500,
                   units = 'px',
                   res = NA,
                   ...) {
  # match args
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'reportEvery', 'savePlots', 'saveAudio')]

  pa = processAudio(x = x,
                    samplingRate = samplingRate,
                    scale = scale,
                    saveAudio = saveAudio,
                    savePlots = savePlots,
                    funToCall = 'flatEnvSound',
                    myPars = myPars,
                    reportEvery = reportEvery
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    if (is.null(pa$input$saveAudio)) {
      audioFiles = pa$input$filenames
    } else {
      audioFiles = paste0(pa$input$saveAudio, pa$input$filenames_base, '.wav')
    }
    htmlPlots(
      htmlFile = paste0(pa$input$savePlots, '00_clickablePlots_compressor.html'),
      plotFiles = paste0(pa$input$savePlots, pa$input$filenames_base, "_compressor.png"),
      audioFiles = audioFiles,
      width = paste0(width, units))
  }

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' @rdname flatEnv
#' @export
compressor = flatEnv


#' Flat envelope per sound
#'
#' Internal soundgen function
#'
#' @param audio a list returned by \code{readAudio}
#' @inheritParams flatEnv
#' @inheritParams segment
#' @keywords internal
flatEnvSound = function(audio,
                        compression = 1,
                        method = c('hil', 'rms', 'peak')[1],
                        windowLength = 50,
                        windowLength_points = NULL,
                        killDC = FALSE,
                        dynamicRange = 40,
                        saveAudio = NULL,
                        plot = FALSE,
                        savePlots = NULL,
                        col = 'blue',
                        width = 900,
                        height = 500,
                        units = 'px',
                        res = NA,
                        ...) {
  if (!is.numeric(windowLength_points)) {
    if (is.numeric(windowLength)) {
      if (is.numeric(audio$samplingRate)) {
        windowLength_points = windowLength / 1000 * audio$samplingRate
      } else {
        stop(paste('Please specify either windowLength (ms) plus samplingRate (Hz)',
                   'or the length of smoothing window in points (windowLength_points)'))
      }
    }
  }

  # audio$scale = original scale (eg -1 to +1 gives m = 1)
  throwaway_lin = 10 ^ (-dynamicRange / 20) * audio$scale  # from dB to linear + normalize

  # remove DC offset
  if (killDC) {
    soundFlat = killDC(sound = audio$sound,
                       windowLength_points = windowLength_points,
                       plot = FALSE)
  } else {
    soundFlat = audio$sound
  }

  # get smoothed amplitude envelope
  env = getEnv(sound = soundFlat,
               windowLength_points = windowLength_points,
               method = method)
  env[env < 0] = 0

  # don't amplify very quiet sections
  idx = which(env > throwaway_lin)
  # flatten amplitude envelope
  soundFlat[idx] = soundFlat[idx] * (1 - compression) +
    soundFlat[idx] / env[idx] * audio$scale * compression
  # re-normalize to original scale
  soundFlat = soundFlat / max(abs(soundFlat)) * audio$scale

  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_base, "_compressor.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    ylim = range(c(audio$sound, soundFlat))
    op = par('mfrow')
    par(mfrow = c(1, 2))
    plot(audio$sound, type = 'l', main = 'Original', ylim = ylim)
    points(env, type = 'l', col = col, ...)

    env_new = getEnv(sound = soundFlat,
                     windowLength_points = windowLength_points,
                     method = method)
    plot(soundFlat, type = 'l', main = 'Compressed', ylim = ylim)
    points(env_new, type = 'l', col = col, ...)
    par(mfrow = op)
    if (is.character(audio$savePlots)) dev.off()
  }

  if (!is.null(audio$saveAudio)) {
    if (!dir.exists(audio$saveAudio)) dir.create(audio$saveAudio)
    seewave::savewav(
      soundFlat, f = audio$samplingRate,
      filename = paste0(audio$saveAudio, '/', audio$filename_base, '.wav'))
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
#' @seealso \code{\link{flatEnv}} \code{\link[seewave]{setenv}}
#'
#' @param donor the sound that "donates" the amplitude envelope (numeric vector
#'   - NOT an audio file)
#' @param recipient the sound that needs to have its amplitude envelope adjusted
#'   (numeric vector - NOT an audio file)
#' @param samplingRateD,samplingRateR sampling rate of the donor and recipient,
#'   respectively (if only samplingRateD is provided, samplingRateR is assumed
#'   to be the same)
#' @inheritParams flatEnv
#' @return Returns the recipient sound with the donor's amplitude envelope - a
#'   numeric vector with the same sampling rate as the recipient
#' @export
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
  windowLength = 50,
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

    time_recipient = seq(0, len_recipient / samplingRateR,
                         length.out = len_recipient)
    max_recipient = max(abs(recipient))
    max_out = max(abs(out))

    op = par('mfrow')
    par(mfrow = c(1, 3))

    plot(time_donor, donor, type = 'l', main = 'Donor',
         ylim = c(-max_donor, max_donor),
         xlab = '', ylab = 'Amplitude')
    points(time_donor, env_donor, type = 'l',
           lty = 1, col = 'blue')

    plot(time_recipient, recipient, type = 'l',
         main = 'Recipient', ylim = c(-max_recipient, max_recipient),
         xlab = 'Time, s', ylab = '')
    points(time_recipient, env_recipient, type = 'l',
           lty = 1, col = 'blue')

    plot(time_recipient, out, type = 'l',
         main = 'Output', ylim = c(-max_out, max_out),
         xlab = '', ylab = '')
    points(time_recipient, getEnv(out, windowLength_points_recip, method),
           type = 'l', lty = 1, col = 'blue')

    par(mfrow = op)
  }
  invisible(out)
}


#' Add amplitude modulation
#'
#' Adds sinusidal or logistic amplitude modulation to a sound. This produces
#' additional harmonics in the spectrum at ±am_freq around each original
#' harmonic and makes the sound rough. The optimal frequency for creating a
#' perception of roughness is ~70 Hz (Fastl & Zwicker "Psychoacoustics").
#' Sinusoidal AM creates a single pair of new harmonics, while non-sinusoidal AM
#' creates more extra harmonics (see examples).
#' @param x path to a .wav file or a vector of amplitudes with specified
#'   samplingRate
#' @inheritParams soundgen
#' @inheritParams segment
#' @param plot if TRUE, plots the amplitude modulation
#' @export
#' @examples
#' sound1 = soundgen(pitch = c(200, 300), addSilence = 0)
#' s1 = addAM(sound1, 16000, amDep = c(0, 50, 0), amFreq = 75, plot = TRUE)
#' # playme(s1)
#' \dontrun{
#' # Parameters can be specified as in the soundgen() function, eg:
#' s2 = addAM(sound1, 16000,
#'          amDep = list(time = c(0, 50, 52, 200, 201, 300),
#'                       value = c(0, 0, 35, 25, 0, 0)),
#'          plot = TRUE, play = TRUE)
#'
#' # Sinusoidal AM produces exactly 2 extra harmonics at ±am_freq around each f0
#' harmonic:
#' s3 = addAM(sound1, 16000, amDep = 30, amFreq = c(50, 80), amType = 'sine',
#'            plot = TRUE, play = TRUE)
#' spectrogram(s3, 16000, windowLength = 150, ylim = c(0, 2))
#'
#' # Non-sinusoidal AM produces multiple new harmonics, which can resemble
#' subharmonics...
#' s4 = addAM(sound1, 16000, amDep = 70, amFreq = 50, amShape = -1,
#'            plot = TRUE, play = TRUE)
#' spectrogram(s4, 16000, windowLength = 150, ylim = c(0, 2))
#'
#' # ...but more often look like sidebands
#' sound3 = soundgen(sylLen = 600, pitch = c(800, 1300, 1100), addSilence = 0)
#' s5 = addAM(sound3, 16000, amDep = c(0, 30, 100, 40, 0),
#'            amFreq = 105, amShape = -.3,
#'            plot = TRUE, play = TRUE)
#' spectrogram(s5, 16000, ylim = c(0, 5))
#'
#' # Feel free to add AM stochastically:
#' s6 = addAM(sound1, 16000,
#'            amDep = rnorm(10, 40, 20), amFreq = rnorm(20, 70, 20),
#'            plot = TRUE, play = TRUE)
#' spectrogram(s6, 16000, windowLength = 150, ylim = c(0, 2))
#'
#' # If am_freq is locked to an integer ratio of f0, we can get subharmonics
#' # For ex., here is with pitch 400-600-400 Hz (soundgen interpolates pitch
#' # on a log scale and am_freq on a linear scale, so we align them by extracting
#' # a long contour on a log scale for both)
#' con = getSmoothContour(anchors = c(400, 600, 400),
#'                        len = 20, thisIsPitch = TRUE)
#' s = soundgen(sylLen = 1500, pitch = con, amFreq = con/3, amDep = 30,
#'              plot = TRUE, play = TRUE, ylim = c(0, 3))
#' }
addAM = function(x,
                 samplingRate = NULL,
                 amDep = 25,
                 amFreq = 30,
                 amType = c('logistic', 'sine')[1],
                 amShape = 0,
                 invalidArgAction = c('adjust', 'abort', 'ignore')[1],
                 plot = FALSE,
                 play = FALSE,
                 saveAudio = NULL,
                 reportEvery = NULL) {
  # match args
  myPars = c(as.list(environment()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'from', 'to', 'reportEvery')]
  pa = processAudio(x,
                    samplingRate = samplingRate,
                    funToCall = 'addAMSound',
                    myPars = myPars,
                    reportEvery = reportEvery
  )
  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Add AM to a sound
#'
#' Internal soundgen function, see \code{\link{addAM}}.
#'
#' @inheritParams addAM
#' @param sound numeric vector
#' @param ls the length of \code{sound}, points
#' @param checkFormat only FALSE when called internally by soundgen()
#' @keywords internal
addAMSound = function(
  audio,
  amDep = 25,
  amFreq = 30,
  amType = c('logistic', 'sine')[1],
  amShape = 0,
  invalidArgAction = c('adjust', 'abort', 'ignore')[1],
  plot = FALSE,
  play = FALSE,
  saveAudio = NULL,
  filename_base = NULL,
  checkFormat = TRUE) {
  # check the format of AM pars
  if (checkFormat) {
    anchors = c('amDep', 'amFreq', 'amShape')
    for (anchor in anchors) {
      assign(anchor, reformatAnchors(get(anchor)))
    }
  }

  # vectorize
  amPar_vect = c('amDep', 'amFreq', 'amShape')
  # just to get rid of of NOTE on CRAN:
  amDep_vector = amFreq_vector = amShape_vector = vector()
  for (p in amPar_vect) {
    p_unique_value = unique(get(p)$value)
    if (length(p_unique_value) > 1) {
      if (invalidArgAction == 'ignore') {
        valueFloor_p = valueCeiling_p = NULL
      } else {
        valueFloor_p = permittedValues[p, 'low']
        valueCeiling_p = permittedValues[p, 'high']
      }
      p_vectorized = getSmoothContour(
        anchors = get(p),
        len = audio$ls,
        interpol = 'approx',
        valueFloor = valueFloor_p,
        valueCeiling = valueCeiling_p
      )
      # plot(p_vectorized, type = 'l')
      assign(paste0(p, '_vector'), p_vectorized)
    } else {
      assign(paste0(p, '_vector'), p_unique_value)
    }
  }

  # prepare am vector
  if (amType == 'sine') {
    if (length(amFreq_vector) == 1) {
      int = amFreq_vector * (1:audio$ls)
    } else {
      int = cumsum(amFreq_vector)
    }
    sig = .5 + .5 * cos(2 * pi * int / audio$samplingRate)
  } else {
    sig = getSigmoid(len = audio$ls,
                     samplingRate = audio$samplingRate,
                     freq = amFreq_vector,
                     shape = amShape_vector)
  }
  # plot(sig, type = 'l')
  # sig is on a scale [0, 1]
  am = 1 - sig * amDep_vector / 100
  sound_am = audio$sound * am

  if (plot) {
    osc(x = am,
        samplingRate = audio$samplingRate,
        main = 'Amplitude modulation',
        xlab = 'Time, ms',
        ylab = '',
        ylim = c(0, 1),
        midline = FALSE)
  }
  if (play) playme(sound_am, audio$samplingRate)
  if (!is.null(saveAudio)) {
    if (!dir.exists(saveAudio)) dir.create(saveAudio)
    seewave::savewav(sound_am, f = audio$samplingRate, filename = audio$filename_base)
  }
  invisible(sound_am)
}



#' Get amplitude envelope
#'
#' Internal soundgen function
#'
#' Returns the smoothed amplitude envelope of a waveform on the original scale.
#' NB: unlike seewave::env, this function returns an envelope of the same length
#' as the original sound, regardless of the amount of smoothing.
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
        stop(paste('Please specify either windowLength (ms) plus samplingRate (Hz)',
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

