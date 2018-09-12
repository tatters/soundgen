### FUNCTIONS FOR PREPARING AND PLOTTING A SPECTROGRAM ###

#' Spectrogram
#'
#' Produces the spectrogram of a sound using short-term Fourier transform. This
#' is a simplified version of \code{\link[seewave]{spectro}} with fewer plotting
#' options, but with added routines for noise reduction, smoothing in time and
#' frequency domains, and controlling contrast and brightness.
#' @param x path to a .wav file or a vector of amplitudes with specified
#'   samplingRate
#' @param samplingRate sampling rate of \code{x} (only needed if
#'   \code{x} is a numeric vector, rather than a .wav file)
#' @param dynamicRange dynamic range, dB. All values with power more than one
#'   dynamicRange under maximum are treated as zero
#' @param windowLength length of FFT window, ms
#' @param overlap overlap between successive FFT frames, \%
#' @param step you can override \code{overlap} by specifying FFT step, ms
#' @param wn window type: gaussian, hanning, hamming, bartlett, rectangular,
#'   blackman, flattop
#' @param zp window length after zero padding, points
#' @param smoothFreq,smoothTime length of the window, in data points (0 to
#'   +inf), for calculating a rolling median. Applies median smoothing to
#'   spectrogram in frequency and time domains, respectively
#' @param qTime the quantile to be subtracted for each frequency
#'   bin. For ex., if qTime = 0.5, the median of each frequency
#'   bin (over the entire sound duration) will be calculated and subtracted from
#'   each frame (see examples)
#' @param percentNoise percentage of frames (0 to 100\%) used for calculating noise
#'   spectrum
#' @param noiseReduction how much noise to remove (0 to +inf, recommended 0 to
#'   2). 0 = no noise reduction, 2 = strong noise reduction: \eqn{spectrum -
#'   (noiseReduction * noiseSpectrum)}, where noiseSpectrum is the average
#'   spectrum of frames with entropy exceeding the quantile set by
#'   \code{percentNoise}
#' @param contrast spectrum is exponentiated by contrast (-inf to +inf,
#'   recommended -1 to +1). Contrast >0 increases sharpness, <0 decreases
#'   sharpness
#' @param brightness how much to "lighten" the image (>0 = lighter, <0 = darker)
#' @param method plot spectrum ('spectrum') or spectral derivative
#'   ('spectralDerivative')
#' @param output specifies what to return: nothing ('none'), unmodified
#'   spectrogram ('original'), or denoised and/or smoothed spectrogram
#'   ('processed')
#' @param ylim frequency range to plot, kHz (defaults to 0 to Nyquist frequency)
#' @param plot should a spectrogram be plotted? TRUE / FALSE
#' @param osc,osc_dB should an oscillogram be shown under the spectrogram? TRUE/
#'   FALSE. If `osc_dB`, the oscillogram is displayed on a dB scale. See
#'   \code{\link{osc_dB}} for details
#' @param heights a vector of length two specifying the relative height of the
#'   spectrogram and the oscillogram
#' @param colorTheme black and white ('bw'), as in seewave package ('seewave'),
#'   or any palette from \code{\link[grDevices]{palette}} such as
#'   'heat.colors', 'cm.colors', etc
#' @param xlab,ylab,main,mar graphical parameters
#' @param ... other graphical parameters passed to
#'   \code{seewave:::filled.contour.modif2}
#' @param frameBank ignore (only needed for pitch tracking)
#' @param duration ignore (only needed for pitch tracking)
#' @export
#' @return Returns nothing (if output = 'none'), absolute - not power! -
#'   spectrum (if output = 'original'), denoised and/or smoothed spectrum (if
#'   output = 'processed'), or spectral derivatives (if method =
#'   'spectralDerivative') as a matrix of real numbers.
#' @examples
#' # synthesize a sound 1 s long, with gradually increasing hissing noise
#' sound = soundgen(sylLen = 1000, temperature = 0.001, noiseAnchors = list(
#'   time = c(0, 1300), value = c(-120, 0)), formantsNoise = list(
#'   f1 = list(freq = 5000, width = 10000)))
#' # playme(sound, samplingRate = 16000)
#'
#' # basic spectrogram
#' spectrogram(sound, samplingRate = 16000)
#'
#' \dontrun{
#' # change dynamic range
#' spectrogram(sound, samplingRate = 16000, dynamicRange = 40)
#' spectrogram(sound, samplingRate = 16000, dynamicRange = 120)
#'
#' # add an oscillogram
#' spectrogram(sound, samplingRate = 16000, osc = TRUE)
#'
#' # oscillogram on a dB scale, same height as spectrogram
#' spectrogram(sound, samplingRate = 16000,
#'             osc_dB = TRUE, heights = c(1, 1))
#'
#' # broad-band instead of narrow-band
#' spectrogram(sound, samplingRate = 16000, windowLength = 5)
#'
#' # focus only on values in the upper 5% of power for each frequency bin
#' spectrogram(sound, samplingRate = 16000, qTime = 0.95)
#'
#' # detect 10% of the noisiest frames based on entropy and remove the pattern
#' # found in those frames (in this cases, breathing)
#' spectrogram(sound, samplingRate = 16000,  noiseReduction = 1.1,
#'   brightness = -2)  # white noise gone
#'
#' # apply median smoothing in both time and frequency domains
#' spectrogram(sound, samplingRate = 16000, smoothFreq = 5,
#'   smoothTime = 5)
#'
#' # increase contrast, reduce brightness
#' spectrogram(sound, samplingRate = 16000, contrast = 1, brightness = -1)
#'
#' # add bells and whistles
#' spectrogram(sound, samplingRate = 16000, osc = TRUE, noiseReduction = 1.1,
#'   brightness = -1, colorTheme = 'heat.colors',
#'   ylim = c(0,5), cex.lab = .75, main = 'My spectrogram')
#' }
spectrogram = function(x,
                       samplingRate = NULL,
                       dynamicRange = 80,
                       windowLength = 50,
                       step = NULL,
                       overlap = 70,
                       wn = 'gaussian',
                       zp = 0,
                       smoothFreq = 0,
                       smoothTime = 0,
                       qTime = 0,
                       percentNoise = 10,
                       noiseReduction = 0,
                       contrast = .2,
                       brightness = 0,
                       method = c('spectrum', 'spectralDerivative')[1],
                       output = c('none', 'original', 'processed')[1],
                       ylim = NULL,
                       plot = TRUE,
                       osc = FALSE,
                       osc_dB = FALSE,
                       heights = c(3, 1),
                       colorTheme = c('bw', 'seewave', '...')[1],
                       xlab = 'Time, ms',
                       ylab = 'Frequency, KHz',
                       mar = c(5.1, 4.1, 4.1, 2),
                       main = '',
                       frameBank = NULL,
                       duration = NULL,
                       ...) {
  if (overlap < 0 | overlap > 100) {
    warning('overlap must be >0 and <= 100%; resetting to 70')
    overlap = 70
  }
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  # import audio
  if (class(x) == 'character') {
    sound_wav = tuneR::readWave(x)
    samplingRate = sound_wav@samp.rate
    windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
    sound = sound_wav@left
    maxAmpl = 2^(sound_wav@bit - 1)
    if (windowLength_points > (length(sound) / 2)) {
      windowLength_points = floor(length(sound) / 4) * 2
      step = windowLength_points / samplingRate * 1000 * (1 - overlap / 100)
    }
    if (windowLength_points == 0) {
      stop('The sound and/or the windowLength is too short for plotting a spectrogram')
    }
    duration = length(sound) / samplingRate
    frameBank = getFrameBank(
      sound = sound,
      samplingRate = samplingRate,
      windowLength_points = windowLength_points,
      step = step,
      zp = zp,
      wn = wn,
      filter = NULL
    )
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
        step = windowLength_points / samplingRate * 1000 * (1 - overlap / 100)
      }
      if (windowLength_points == 0) {
        stop('The sound and/or the windowLength is too short for plotting a spectrogram')
      }
      frameBank = getFrameBank(
        sound = x,
        samplingRate = samplingRate,
        windowLength_points = windowLength_points,
        step = step,
        zp = zp,
        wn = wn,
        filter = NULL
      )
    }
  }
  if (class(frameBank) != 'matrix') {
    stop(
      'Input format not recognized. Please provide path to .wav file,
      a vector of amplitudes plus samplingRate, or a preprocessed frameBank'
    )
  }

  # fix default settings
  if (is.null(ylim)) {
    ylim = c(0, floor(samplingRate / 2 / 1000))
  }
  contrast_exp = exp(3 * contrast)
  brightness_exp = exp(3 * brightness)
  # visualization: plot(exp(3 * seq(-1, 1, by = .01)), type = 'l')

  # FFT
  windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
  if (exists('sound') && windowLength_points > (length(sound) / 2)) {
    windowLength_points = floor(length(sound) / 4) * 2
    step = windowLength_points / samplingRate * 1000 * (1 - overlap / 100)
  }
  if (windowLength_points == 0) {
    stop('The sound and/or the windowLength is too short for plotting a spectrogram')
  }

  # fft of each frame
  z = apply(frameBank, 2, function(x) stats::fft(x)[1:(floor(nrow(frameBank) / 2))])
  if (!is.matrix(z)) z = matrix(z, ncol = 1)
  X = seq(0, duration * 1000, length.out = ncol(z))  # time stamp
  if (length(X) < 2) {
    stop('The sound is too short for plotting a spectrogram')
  }
  Y = seq(0,
          (samplingRate / 2) - (samplingRate / windowLength_points),
          length.out = nrow(z)) / 1000  # frequency stamp
  if (length(Y) < 2) {
    stop('The sound and/or the windowLength is too short for plotting a spectrogram')
  }
  rownames(z) = Y
  colnames(z) = X
  Z = t(abs(z))

  if (method == 'spectralDerivative') {
    # first derivative of spectrum by time
    dZ_dt = cbind(rep(0, nrow(Z)), t(apply(Z, 1, diff)))
    # first derivative of spectrum by frequency
    dZ_df = rbind(rep(0, ncol(Z)), apply(Z, 2, diff))
    Z1 = sqrt(dZ_dt ^ 2 + dZ_df ^ 2)  # length of gradient vector
  } else {
    Z1 = Z # this is our raw spectrogram
  }

  # set to zero under dynamic range
  threshold = max(Z1) / 10^(dynamicRange/20)
  Z1[Z1 < threshold] = 0

  # removing noise. NB: the order of these operations is crucial,
  # don't change it!
  if (smoothTime > 1) {
    Z1 = t(apply(Z1, 1, function(x) {
      zoo::rollmedian(x, k = smoothTime, fill = 0)
    }))  # time domain
  }
  if (smoothFreq > 1) {
    Z1 = apply(Z1, 2, function(x) {
      zoo::rollmedian(x, k = smoothFreq, fill = 0)
    }) # freq domain
  }
  if (qTime > 0) {
    Z1 = t(apply(Z1, 1, function(x) {
      x - quantile(x, probs = qTime)
    }))  # for each freq bin, subtract median or another quantile
  }

  # re-normalize
  positives = which(Z1 > 0)
  nonpositives = which(Z1 <= 0)
  Z1[positives] = log(Z1[positives])
  if (length(positives) > 0 & length(nonpositives) > 0) {
    Z1[nonpositives] = min(Z1[positives])
  }
  Z1 = Z1 - min(Z1)

  if (noiseReduction > 0) {
    # silence frames with entropy above threshold
    entr = apply(Z1, 1, function(x) getEntropy(x)) # Z1 >= 0
    q = quantile(entr, probs = 1 - percentNoise/100, na.rm = TRUE) # the entropy of
    # silent frames is NA
    # plot(entr, type='l'); lines(x=1:length(entr),y=rep(q,length(entr)), col='blue', lty=2)
    idx = as.numeric(which(entr >= q))
    if (length(idx) > 0) {
      noise_spectrum = as.numeric(apply (Z1[idx, , drop = FALSE], 2, mean))
      # plot(noise_spectrum, type = 'l')
      Z1 = t(apply(Z1, 1, function(x) x - noiseReduction * noise_spectrum))
    }
    # re-normalize
    Z1[Z1 <= 0] = 0
  }

  if (contrast_exp != 1) {
    Z1 = Z1 ^ contrast_exp
  }

  if (any(Z1 != 0)) Z1 = Z1 / max(Z1)
  if (brightness_exp != 1) {
    Z1 = Z1 / brightness_exp
  }
  if (brightness_exp < 1) {
    Z1[Z1 > 1] = 1 # otherwise values >1 are shown as white instead of black
  }

  # spectrogram of the modified fft
  if (colorTheme == 'bw') {
    color.palette = function(x) gray(seq(from = 1, to = 0, length = x))
  } else if (colorTheme == 'seewave') {
    color.palette = seewave::spectro.colors
  } else {
    colFun = match.fun(colorTheme)
    color.palette = function(x) rev(colFun(x))
  }

  if (plot) {
    op = par(c('mar', 'xaxt', 'yaxt', 'mfrow')) # save user's original pars
    if (osc | osc_dB) {
      if (osc_dB) {
        sound = osc_dB(sound,
                       dynamicRange = dynamicRange,
                       maxAmpl = maxAmpl,
                       plot = FALSE,
                       returnWave = TRUE)
        ylim_osc = c(-dynamicRange, dynamicRange)
      } else {
        ylim_osc = c(-maxAmpl, maxAmpl)
      }
      layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = heights)
      par(mar = c(mar[1:2], 0, mar[4]), xaxt = 's', yaxt = 's')
      plot(
        seq(1, duration * 1000, length.out = length(sound)),
        sound,
        type = "l",
        ylim = ylim_osc,
        axes = FALSE, xaxs = "i", yaxs = "i", bty = 'o',
        xlab = xlab, ylab = '', main = '', ...)
      box()
      axis(side = 1)
      if (osc_dB) {
        axis(side = 4, at = seq(0, dynamicRange, by = 10))
        mtext("dB", side = 2, line = 3)
      }
      abline(h = 0, lty = 2)
      par(mar = c(0, mar[2:4]), xaxt = 'n', yaxt = 's')
      xlab = ''
    } else {
      par(mar = mar)
    }

    seewave::filled.contour.modif2(
      x = X, y = Y, z = Z1,
      levels = seq(0, 1, length = 30),
      color.palette = color.palette,
      ylim = ylim, main = main,
      xlab = xlab, ylab = ylab,
      ...
    )
    # restore original pars
    par('mar' = op$mar, 'xaxt' = op$xaxt, 'yaxt' = op$yaxt, 'mfrow' = op$mfrow)
  }

  if (output == 'original') {
    return(t(Z))  # before denoising
  } else if (output == 'processed') {
    return(t(Z1))  # denoised spectrum / spectralDerivative
  }
}


#' Save spectrograms per folder
#'
#' Creates spectrograms of all .wav files in a folder and save them as .jpeg
#' files in the same folder. This is a lot faster than running
#' \code{\link{analyzeFolder}} if you don't need pitch tracking. By default it
#' also creates an html file with a list of audio files and their spectrograms
#' in the same folder. If you open it in a browser that supports playing .wav
#' files (e.g. Firefox or Chrome), you can view the spetrograms and click on
#' them to play each sound. Unlike \code{\link{analyzeFolder}},
#' spectrogramFolder supports plotting both a spectrogram and an oscillogram if
#' \code{osc = TRUE}.
#' @inheritParams spectrogram
#' @inheritParams analyzeFolder
#' @param myfolder full path to the folder containing .wav files
#' @param htmlPlots if TRUE, saves an html file with clickable plots
#' @param ... other parameters passed to \code{\link{spectrogram}}
#' @export
#' @examples
#' \dontrun{
#' spectrogramFolder(
#'   '~/Downloads/temp',
#'   windowLength = 40, overlap = 75,  # spectrogram pars
#'   width = 1500, height = 900,        # passed to jpeg()
#'   osc = TRUE, osc_dB = TRUE, heights = c(1, 1)
#' )
#' # note that the folder now also contains an html file with clickable plots
#' }
spectrogramFolder = function(myfolder,
                             htmlPlots = TRUE,
                             verbose = TRUE,
                             windowLength = 50,
                             step = NULL,
                             overlap = 50,
                             wn = 'gaussian',
                             zp = 0,
                             ylim = NULL,
                             osc = TRUE,
                             xlab = 'Time, ms',
                             ylab = 'kHz',
                             width = 900,
                             height = 500,
                             units = 'px',
                             res = NA,
                             ...) {
  time_start = proc.time()  # timing
  filenames = list.files(myfolder, pattern = "*.wav", full.names = TRUE)
  # in order to provide more accurate estimates of time to completion,
  # check the size of all files in the target folder
  filesizes = apply(as.matrix(filenames), 1, function(x) file.info(x)$size)

  for (i in 1:length(filenames)) {
    # strip .wav extension
    f = substr(as.character(filenames[i]), 1, nchar(as.character(filenames[i])) - 4)
    jpeg(filename = paste0(f, ".jpg"),
         width = width, height = height,
         units = units, res = res)
    do.call(spectrogram, list(
      x = filenames[i],
      windowLength = windowLength,
      step = step,
      overlap = overlap,
      wn = wn,
      zp = zp,
      ylim = ylim,
      osc = osc,
      xlab = xlab,
      ylab = ylab,
      main = basename(f),
      ...))
    dev.off()
    if (verbose) {
      reportTime(i = i, nIter = length(filenames),
                 time_start = time_start, jobs = filesizes)
    }
  }
  if (htmlPlots) {
    htmlPlots(myfolder, myfiles = filenames)
  }
}


#' Fourier transform windows (seewave)
#'
#' Internal soundgen function
#'
#' Generates different Fourier Transform windows. Just like
#' \code{\link[seewave]{ftwindow}}, but with the addition of a gaussian window.
#' @param wl window length, in points
#' @param wn window type (defaults to gaussian)
#' @keywords internal
ftwindow_modif = function (wl, wn = "gaussian") {
  if (wn == "bartlett")
    w = seewave::bartlett.w(wl)
  if (wn == "blackman")
    w = seewave::blackman.w(wl)
  if (wn == "flattop")
    w = seewave::flattop.w(wl)
  if (wn == "hamming")
    w = seewave::hamming.w(wl)
  if (wn == "hanning")
    w = seewave::hanning.w(wl)
  if (wn == "rectangle")
    w = seewave::rectangle.w(wl)
  if (wn == "gaussian")
    w = gaussian.w(wl)
  return(w)
}

#' Gaussian window
#'
#' Internal soundgen function.
#'
#' Generates a gaussian window of length n. Based on the formula by P. Boersma (PRAAT)
#' @param n window length, in points
#' @keywords internal
gaussian.w = function(n) {
  if (n == 0)
    stop("'n' must be a positive integer")
  w = (exp(-12 * (((1:n) / n) - 0.5) ^ 2) - exp(-12)) / (1 - exp(-12))
  # Boersma (PRAAT)
  return(w)
}

#' Frame bank
#'
#' Internal soundgen function.
#'
#' A subroutine of \code{\link{spec}} that saves windowed (and optionally
#' zero-padded) frames, i.e. chunks of the sound file of the right size and
#' spacing. Handy for further processing.
#' @param sound numeric vector
#' @inheritParams spectrogram
#' @param windowLength_points length of fft window (points)
#' @param filter fft window filter (defaults to NULL)
#' @return A matrix with \code{nrow = windowLength_points/2} and \code{ncol}
#'   depending on \code{length(sound)} and \code{step}
#' @keywords internal
#' @examples
#' a = soundgen:::getFrameBank(sin(1:1000), 16000, 512, 'gaussian', 15, 0)
getFrameBank = function(sound,
                        samplingRate,
                        windowLength_points,
                        wn,
                        step,
                        zp,
                        filter = NULL) {
  # # normalize to range from no less than -1 to no more than +1
  if (!is.numeric(sound)) {
    return(NA)
  }
  if (any(is.na(sound))) {
    sound[is.na(sound)] = 0
  }
  if (any(sound != 0)) {
    sound = sound - mean(sound)
    sound = sound / max(abs(max(sound)), abs(min(sound)))
  }
  duration = length(sound) / samplingRate
  myseq = seq(1, max(1, (length(sound) - windowLength_points)),
              step / 1000 * samplingRate)
  if (is.null(filter)) {
    filter = ftwindow_modif(wl = windowLength_points, wn = wn)
  }
  zpExtra = floor((zp - windowLength_points) / 2) * 2 # how many extra zeroes
  # we pad with. Made even
  if (zpExtra > 0) {
    frameBank = apply(as.matrix(myseq), 1, function(x) {
      c(rep(0, zpExtra / 2),
        sound[x:(windowLength_points + x - 1)] * filter,
        rep(0, zpExtra / 2))
    })
  } else {
    frameBank = apply(as.matrix(myseq), 1, function(x) {
      sound[x:(windowLength_points + x - 1)] * filter
    })
  }
  return(frameBank)
}
