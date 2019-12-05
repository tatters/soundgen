## FINDING SYLLABLES AND VOCAL BURSTS ##

#' Segment a sound
#'
#' Finds syllables and bursts. Syllables are defined as continuous segments with
#' amplitude above threshold. Bursts are defined as local maxima in amplitude
#' envelope that are high enough both in absolute terms (relative to the global
#' maximum) and with respect to the surrounding region (relative to local
#' mimima). See vignette('acoustic_analysis', package = 'soundgen') for details.
#'
#' The algorithm is very flexible, but the parameters may be hard to optimize by
#' hand. If you have an annotated sample of the sort of audio you are planning
#' to analyze, with syllables and/or bursts counted manually, you can use it for
#' automatic optimization of control parameters (see
#' \code{\link{optimizePars}}. The defaults are the results of just such
#' optimization against 260 human vocalizations in Anikin, A. & Persson, T.
#' (2017). Non-linguistic vocalizations from online amateur videos for emotion
#' research: a validated corpus. Behavior Research Methods, 49(2): 758-771.
#'
#' @seealso \code{\link{segmentFolder}} \code{\link{analyze}}  \code{\link{ssm}}
#'
#' @param x path to a .wav or .mp3 file or a vector of amplitudes with specified
#'   samplingRate
#' @param samplingRate sampling rate of \code{x} (only needed if \code{x} is a
#'   numeric vector, rather than an audio file)
#' @param windowLength,overlap length (ms) and overlap (%) of the smoothing
#'   window used to produce the amplitude envelope, see
#'   \code{\link[seewave]{env}}
#' @param shortestSyl minimum acceptable length of syllables, ms
#' @param shortestPause minimum acceptable break between syllables, ms.
#'   Syllables separated by less time are merged. To avoid merging, specify
#'   \code{shortestPause = NA}
#' @param sylThres amplitude threshold for syllable detection (as a
#'   proportion of global mean amplitude of smoothed envelope)
#' @param interburst minimum time between two consecutive bursts (ms). If
#'   specified, it overrides \code{interburstMult}
#' @param interburstMult multiplier of the default minimum interburst
#'   interval (median syllable length or, if no syllables are detected, the same
#'   number as \code{shortestSyl}). Only used if \code{interburst} is
#'   not specified. Larger values improve detection of unusually broad shallow
#'   peaks, while smaller values improve the detection of sharp narrow peaks
#' @param burstThres to qualify as a burst, a local maximum has to be at least
#'   \code{burstThres} times the height of the global maximum of amplitude
#'   envelope
#' @param peakToTrough to qualify as a burst, a local maximum has to be at
#'   least \code{peakToTrough} times the local minimum on the LEFT over
#'   analysis window (which is controlled by \code{interburst} or
#'   \code{interburstMult})
#' @param troughLeft,troughRight should local maxima be compared to the trough
#'   on the left and/or right of it? Default to TRUE and FALSE, respectively
#' @param summary if TRUE, returns only a summary of the number and spacing of
#'   syllables and vocal bursts. If FALSE, returns a list containing full stats
#'   on each syllable and bursts (location, duration, amplitude, ...)
#' @param plot if TRUE, produces a segmentation plot
#' @param savePath full path to the folder in which to save the plots. Defaults
#'   to NA
#' @param sylPlot a list of graphical parameters for displaying the syllables
#' @param burstPlot a list of graphical parameters for displaying the bursts
#' @param col,xlab,ylab,main main plotting parameters
#' @param width,height,units,res parameters passed to
#'   \code{\link[grDevices]{png}} if the plot is saved
#' @param ... other graphical parameters passed to \code{\link[graphics]{plot}}
#' @return If \code{summary = TRUE}, returns only a summary of the number and
#'   spacing of syllables and vocal bursts. If \code{summary = FALSE}, returns a
#'   list containing full stats on each syllable and bursts (location, duration,
#'   amplitude, ...).
#' @export
#' @examples
#' sound = soundgen(nSyl = 8, sylLen = 50, pauseLen = 70,
#'   pitch = c(368, 284), temperature = 0.1,
#'   noise = list(time = c(0, 67, 86, 186), value = c(-45, -47, -89, -120)),
#'   rolloff_noise = -8, amplGlobal = c(0, -20),
#'   dynamicRange = 120)
#' spectrogram(sound, samplingRate = 16000, osc = TRUE)
#'  # playme(sound, samplingRate = 16000)
#'
#' s = segment(sound, samplingRate = 16000, plot = TRUE)
#' # accept quicker and quieter syllables
#' s = segment(sound, samplingRate = 16000, plot = TRUE,
#'   shortestSyl = 25, shortestPause = 25, sylThres = .2, burstThres = .05)
#'
#' # just a summary
#' segment(sound, samplingRate = 16000, summary = TRUE)
#' # Note that syllables are slightly longer and pauses shorter than they should
#' # be (b/c of the smoothing of amplitude envelope), while interburst intervals
#' # are right on target
#'
#' # customizing the plot
#' s = segment(sound, samplingRate = 16000, plot = TRUE,
#'             shortestSyl = 25, shortestPause = 25,
#'             sylThres = .2, burstThres = .05,
#'             col = 'black', lwd = .5,
#'             sylPlot = list(lty = 2, col = 'gray20'),
#'             burstPlot = list(pch = 16, col = 'gray80'),
#'             xlab = 'ms', cex.lab = 1.2, main = 'My awesome plot')
#'
#' \dontrun{
#' # customize the resolution of saved plot
#' s = segment(sound, samplingRate = 16000, savePath = '~/Downloads/',
#'             width = 1920, height = 1080, units = 'px')
#' }
segment = function(x,
                   samplingRate = NULL,
                   windowLength = 40,
                   overlap = 80,
                   shortestSyl = 40,
                   shortestPause = 40,
                   sylThres = 0.9,
                   interburst = NULL,
                   interburstMult = 1,
                   burstThres = 0.075,
                   peakToTrough = 3,
                   troughLeft = TRUE,
                   troughRight = FALSE,
                   summary = FALSE,
                   plot = FALSE,
                   savePath = NA,
                   col = 'green',
                   xlab = 'Time, ms',
                   ylab = 'Amplitude',
                   main = NULL,
                   width = 900,
                   height = 500,
                   units = 'px',
                   res = NA,
                   sylPlot = list(
                     lty = 1,
                     lwd = 2,
                     col = 'blue'
                   ),
                   burstPlot = list(
                     pch = 8,
                     cex = 3,
                     col = 'red'
                   ),
                   ...) {
  mergeSyl = ifelse(is.numeric(shortestPause), TRUE, FALSE)
  if (windowLength < 10) {
    warning('windowLength < 10 ms is slow and usually not very useful')
  }
  if (overlap < 0) overlap = 0
  if (overlap > 99) overlap = 99

  ## import a sound
  if (class(x)[1] == 'character') {
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
    plotname = tail(unlist(strsplit(x, '/')), n = 1)
    plotname = substring(plotname, 1, nchar(plotname) - 4)
    if (is.null(main)) main = plotname
  }  else if (class(x)[1] == 'numeric' & length(x) > 1) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    } else {
      sound = x
      plotname = 'newPlot'
      if (is.null(main)) main = ''
    }
  }

  ## normalize
  sound = sound - mean(sound)  # center around 0
  sound = sound / max(abs(sound))  # range approx. -1 to 1
  len = length(sound)
  # plot(sound, type='l')

  ## extract amplitude envelope
  windowLength_points = ceiling(windowLength * samplingRate / 1000)
  if (windowLength_points > len / 2) {
    windowLength_points = len / 2
  }

  sound_downsampled = seewave::env(
    sound,
    f = samplingRate,
    envt = 'hil',
    msmooth = c(windowLength_points, overlap),
    fftw = FALSE,
    plot = FALSE
  )
  timestep_points = windowLength_points - (windowLength_points * overlap / 100)
  timestep = timestep_points / samplingRate * 1000
  time_stamps = (seq(1, len - windowLength_points, by = timestep_points) +
                   windowLength_points / 2) / samplingRate * 1000
  # timing starts from the middle of the first window
  envelope = data.frame(time = time_stamps,
                        value = sound_downsampled)
  # plot(envelope, type='l')

  ## find syllables and get descriptives
  threshold = mean(envelope$value) * sylThres
  syllables = findSyllables(envelope = envelope,
                            timestep = timestep,
                            threshold = threshold,
                            shortestSyl = shortestSyl,
                            shortestPause = shortestPause,
                            mergeSyl = mergeSyl)

  ## find bursts and get descriptives
  # calculate the window for analyzing bursts based on the median duration of
  # syllables (if no syllables are detected, just use the specified shortest
  # acceptable syllable length)
  if (is.null(interburst)) {
    median_scaled = suppressWarnings(median(syllables$sylLen) * interburstMult)
    interburst = ifelse(is.numeric(median_scaled) & length(median_scaled) > 0,
                        median_scaled,
                        shortestSyl)
  }
  bursts = findBursts(envelope = envelope,
                      timestep = timestep,
                      interburst = interburst,
                      burstThres = burstThres,
                      peakToTrough = peakToTrough,
                      troughLeft = troughLeft,
                      troughRight = troughRight
  )

  ## plotting (optional)
  if (is.character(savePath)) plot = TRUE
  if (plot) {
    # defaults
    if (is.null(sylPlot$lty)) sylPlot$lty = 1
    if (is.null(sylPlot$lwd)) sylPlot$lwd = 2
    if (is.null(sylPlot$col)) sylPlot$col = 'blue'
    if (is.null(burstPlot$pch)) burstPlot$pch = 8
    if (is.null(burstPlot$cex)) burstPlot$cex = 3
    if (is.null(burstPlot$col)) burstPlot$col = 'red'

    if (is.character(savePath)) {
      # make sure the last character of savePath is "/"
      last_char = substr(savePath, nchar(savePath), nchar(savePath))
      if(last_char != '/') savePath = paste0(savePath, '/')
      png(filename = paste0(savePath, plotname, ".png"),
          width = width, height = height, units = units, res = res)
    }

    op = par(c('mar', 'xaxt', 'yaxt', 'mfrow')) # save user's original pars
    layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = c(2, 1))
    par(mar = c(op$mar[1:2], 0, op$mar[4]), xaxt = 's', yaxt = 's')
    xlim = c(0, len / samplingRate * 1000)
    plot(x = 1:len / samplingRate * 1000,
         y = sound, type = 'l', xlim = xlim,
         axes = FALSE, xaxs = "i", yaxs = "i", bty = 'o',
         xlab = xlab, ylab = '', main = '', ...)
    box()
    axis(side = 1, ...)
    abline(h = 0, lty = 2)
    par(mar = c(0, op$mar[2:4]), xaxt = 'n', yaxt = 's')
    plot(x = envelope$time, y = envelope$value, type = 'l',
         xlim =xlim, col = col,
         xaxs = "i", xlab = '', ylab = ylab, main = main, ...)
    points(bursts, col = burstPlot$col, cex = burstPlot$cex, pch = burstPlot$pch)
    for (s in 1:nrow(syllables)) {
      segments(x0 = syllables$start[s], y0 = threshold,
               x1 = syllables$end[s], y1 = threshold,
               lty = sylPlot$lty, lwd = sylPlot$lwd, col = sylPlot$col)
    }
    # restore original pars
    par('mar' = op$mar, 'xaxt' = op$xaxt, 'yaxt' = op$yaxt, 'mfrow' = op$mfrow)
    if (is.character(savePath)){
      dev.off()
    }
  }

  if (summary) {
    ## prepare a dataframe containing descriptives for syllables and bursts
    result = data.frame(
      nSyl = nrow(syllables),
      sylLen_mean = suppressWarnings(mean(syllables$sylLen)),
      sylLen_median = ifelse(nrow(syllables) > 0,
                             median(syllables$sylLen),
                             NA),  # otherwise returns NULL
      sylLen_sd = sd(syllables$sylLen),
      pauseLen_mean = suppressWarnings(mean(syllables$pauseLen, na.rm = TRUE)),
      pauseLen_median = ifelse(nrow(syllables) > 1,
                               median(syllables$pauseLen, na.rm = TRUE),
                               NA),  # otherwise returns NULL
      pauseLen_sd = sd(syllables$pauseLen, na.rm = TRUE),
      nBursts = nrow(bursts),
      interburst_mean = suppressWarnings(mean(bursts$interburstInt, na.rm = TRUE)),
      interburst_median = ifelse(nrow(bursts) > 0,
                                 median(bursts$interburstInt, na.rm = TRUE),
                                 NA),  # otherwise returns NULL
      interburst_sd = sd(bursts$interburstInt, na.rm = TRUE)
    )
    result[apply(result, c(1, 2), is.nan)] = NA
  } else {
    result = list(syllables = syllables, bursts = bursts)
  }

  return(result)
}


#' Segment all files in a folder
#'
#' Finds syllables and bursts in all .wav files in a folder.
#'
#' This is just a convenient wrapper for \code{\link{segment}} intended for
#' analyzing the syllables and bursts in a large number of audio files at a
#' time. In verbose mode, it also reports ETA every ten iterations. With default
#' settings, running time should be about a second per minute of audio.
#'
#' @seealso \code{\link{segment}}
#'
#' @param myfolder full path to target folder
#' @inheritParams segment
#' @param savePlots if TRUE, saves plots as .png files
#' @param htmlPlots if TRUE, saves an html file with clickable plots
#' @param verbose,reportEvery if TRUE, reports progress every \code{reportEvery}
#'   files and estimated time left
#' @return If \code{summary} is TRUE, returns a dataframe with one row per audio
#'   file. If \code{summary} is FALSE, returns a list of detailed descriptives.
#' @export
#' @examples
#' \dontrun{
#' # Download 260 sounds from the supplements to Anikin & Persson (2017) at
#' # http://cogsci.se/publications.html
#' # unzip them into a folder, say '~/Downloads/temp'
#' myfolder = '~/Downloads/temp'  # 260 .wav files live here
#' s = segmentFolder(myfolder, verbose = TRUE, savePlot = TRUE)
#'
#' # Check accuracy: import a manual count of syllables (our "key")
#' key = segmentManual  # a vector of 260 integers
#' trial = as.numeric(s$nBursts)
#' cor(key, trial, use = 'pairwise.complete.obs')
#' boxplot(trial ~ as.integer(key), xlab='key')
#' abline(a=0, b=1, col='red')
#' }
segmentFolder = function(myfolder,
                         htmlPlots = TRUE,
                         shortestSyl = 40,
                         shortestPause = 40,
                         sylThres = 0.9,
                         interburst = NULL,
                         interburstMult = 1,
                         burstThres = 0.075,
                         peakToTrough = 3,
                         troughLeft = TRUE,
                         troughRight = FALSE,
                         windowLength = 40,
                         overlap = 80,
                         summary = TRUE,
                         plot = FALSE,
                         savePlots = FALSE,
                         savePath = NA,
                         verbose = TRUE,
                         reportEvery = 10,
                         col = 'green',
                         xlab = 'Time, ms',
                         ylab = 'Amplitude',
                         main = NULL,
                         width = 900,
                         height = 500,
                         units = 'px',
                         res = NA,
                         sylPlot = list(
                           lty = 1,
                           lwd = 2,
                           col = 'blue'
                         ),
                         burstPlot = list(
                           pch = 8,
                           cex = 3,
                           col = 'red'
                         ),
                         ...) {
  # deprecated pars
  if (!missing(savePath)) {
    message('savePath is deprecated; use savePlots = TRUE instead')
  }

  time_start = proc.time()  # timing
  # open all .wav files in folder
  filenames = list.files(myfolder, pattern = "*.wav|.mp3", full.names = TRUE)
  filesizes = file.info(filenames)$size
  myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'myfolder', 'htmlPlots', 'verbose', 'savePlots',
    'reportEvery', 'sylPlot', 'burstPlot')]  # otherwise flattens lists
  # exclude ...
  myPars = myPars[1:(length(myPars)-1)]
  # add back sylPlot and burstPlot
  myPars$sylPlot = sylPlot
  myPars$burstPlot = burstPlot
  if (savePlots) myPars$savePath = myfolder

  result = list()
  for (i in 1:length(filenames)) {
    result[[i]] = do.call(segment, c(filenames[i], myPars, ...))
    if (verbose) {
      if (i %% reportEvery == 0) {
        reportTime(i = i, nIter = length(filenames),
                   time_start = time_start, jobs = filesizes)
      }
    }
  }

  # prepare output
  if (summary == TRUE) {
    output = as.data.frame(t(sapply(result, rbind)))
    output$sound = apply(matrix(1:length(filenames)), 1, function(x) {
      tail(unlist(strsplit(filenames[x], '/')), 1)
    })
    output = output[, c('sound', colnames(output)[1:(ncol(output) - 1)])]
    output = as.data.frame(apply(output, 2, unlist))
  } else {
    output = result
    names(output) = filenames
  }

  if (htmlPlots & savePlots) {
    htmlPlots(myfolder, myfiles = filenames)
  }

  return (output)
}
