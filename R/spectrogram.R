### FUNCTIONS FOR PREPARING AND PLOTTING A SPECTROGRAM ###

#' Spectrogram
#'
#' Produces the spectrogram of a sound using short-term Fourier transform.
#' Inspired by \code{\link[seewave]{spectro}}, this function offers added
#' routines for noise reduction, smoothing in time and frequency domains, manual
#' control of contrast and brightness, plotting the oscillogram on a dB scale,
#' grid, etc.
#'
#' Many soundgen functions call \code{spectrogram}, and you can pass along most
#' of its graphical parameters from functions like \code{\link{soundgen}},
#' \code{\link{analyze}}, etc. However, in some cases this will not work (eg for
#' "units") or may produce unexpected results. If in doubt, omit extra graphical
#' parameters.
#'
#' @seealso \code{\link{modulationSpectrum}} \code{\link{ssm}}
#'   \code{\link{osc_dB}}
#'
#' @param x path to a .wav or .mp3 file or a vector of amplitudes with specified
#'   samplingRate
#' @param samplingRate sampling rate of \code{x} (only needed if \code{x} is a
#'   numeric vector, rather than an audio file)
#' @param dynamicRange dynamic range, dB. All values more than one dynamicRange
#'   under maximum are treated as zero
#' @param windowLength length of FFT window, ms
#' @param overlap overlap between successive FFT frames, \%
#' @param step you can override \code{overlap} by specifying FFT step, ms
#' @param wn window type: gaussian, hanning, hamming, bartlett, rectangular,
#'   blackman, flattop
#' @param normalize if TRUE, scales input prior to FFT
#' @param scale maximum possible amplitude of input used for normalization of
#'   input vector (not needed if input is an audio file)
#' @param zp window length after zero padding, points
#' @param smoothFreq,smoothTime length of the window, in data points (0 to
#'   +inf), for calculating a rolling median. Applies median smoothing to
#'   spectrogram in frequency and time domains, respectively
#' @param qTime the quantile to be subtracted for each frequency bin. For ex.,
#'   if qTime = 0.5, the median of each frequency bin (over the entire sound
#'   duration) will be calculated and subtracted from each frame (see examples)
#' @param percentNoise percentage of frames (0 to 100\%) used for calculating
#'   noise spectrum
#' @param noiseReduction how much noise to remove (0 to +inf, recommended 0 to
#'   2). 0 = no noise reduction, 2 = strong noise reduction: \eqn{spectrum -
#'   (noiseReduction * noiseSpectrum)}, where noiseSpectrum is the average
#'   spectrum of frames with entropy exceeding the quantile set by
#'   \code{percentNoise}
#' @param contrast spectrum is exponentiated by contrast (-inf to +inf,
#'   recommended -1 to +1). Contrast >0 increases sharpness, <0 decreases
#'   sharpness
#' @param brightness how much to "lighten" the image (>0 = lighter, <0 = darker)
#' @param maxPoints the maximum number of "pixels" in the oscillogram (if any)
#'   and spectrogram; good for plotting long audio files; defaults to c(1e5,
#'   5e5)
#' @param method plot spectrum ('spectrum') or spectral derivative
#'   ('spectralDerivative')
#' @param output specifies what to return: nothing ('none'), unmodified
#'   spectrogram ('original'), denoised and/or smoothed spectrogram
#'   ('processed'), or unmodified spectrogram with the imaginary part giving
#'   phase ('complex')
#' @param ylim frequency range to plot, kHz (defaults to 0 to Nyquist frequency)
#' @param yScale scale of the frequency axis: 'linear' = linear, 'log' =
#'   logarithmic
#' @param plot should a spectrogram be plotted? TRUE / FALSE
#' @param osc,osc_dB should an oscillogram be shown under the spectrogram? TRUE/
#'   FALSE. If `osc_dB`, the oscillogram is displayed on a dB scale. See
#'   \code{\link{osc_dB}} for details
#' @param heights a vector of length two specifying the relative height of the
#'   spectrogram and the oscillogram (including time axes labels)
#' @param padWithSilence if TRUE, pads the sound with just enough silence to
#'   resolve the edges properly (only the original region is plotted, so
#'   apparent duration doesn't change)
#' @param colorTheme black and white ('bw'), as in seewave package ('seewave'),
#'   or any palette from \code{\link[grDevices]{palette}} such as 'heat.colors',
#'   'cm.colors', etc
#' @param units deprecated
#' @param xlab,ylab,main,mar,xaxp graphical parameters
#' @param grid if numeric, adds n = \code{grid} dotted lines per kHz
#' @param ... other graphical parameters
#' @param internal ignore (only used internally)
#' @export
#' @return Returns nothing (if output = 'none'), absolute - not power! -
#'   spectrum (if output = 'original'), denoised and/or smoothed spectrum (if
#'   output = 'processed'), or spectral derivatives (if method =
#'   'spectralDerivative') as a matrix of real numbers.
#' @seealso \code{\link{modulationSpectrum}} \code{\link{ssm}}
#' @examples
#' # synthesize a sound 1 s long, with gradually increasing hissing noise
#' sound = soundgen(sylLen = 500, temperature = 0.001, noise = list(
#'   time = c(0, 650), value = c(-40, 0)), formantsNoise = list(
#'   f1 = list(freq = 5000, width = 10000)))
#' # playme(sound, samplingRate = 16000)
#'
#' # basic spectrogram
#' spectrogram(sound, samplingRate = 16000)
#'
#' \dontrun{
#' # add bells and whistles
#' spectrogram(sound, samplingRate = 16000,
#'   osc = TRUE,  # plot oscillogram under the spectrogram
#'   noiseReduction = 1.1,  # subtract the spectrum of noisy parts
#'   brightness = -1,  # reduce brightness
#'   colorTheme = 'heat.colors',  # pick color theme
#'   cex.lab = .75, cex.axis = .75,  # text size and other base graphics pars
#'   grid = 5,  # lines per kHz; to customize, add manually with graphics::grid()
#'   ylim = c(0, 5),  # always in kHz
#'   main = 'My spectrogram' # title
#'   # + axis labels, etc
#' )
#'
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
#' # frequencies on a logarithmic scale
#' spectrogram(sound, samplingRate = 16000,
#'             yScale = 'log', ylim = c(.05, 8))
#'
#' # broad-band instead of narrow-band
#' spectrogram(sound, samplingRate = 16000, windowLength = 5)
#'
#' # focus only on values in the upper 5% for each frequency bin
#' spectrogram(sound, samplingRate = 16000, qTime = 0.95)
#'
#' # detect 10% of the noisiest frames based on entropy and remove the pattern
#' # found in those frames (in this cases, breathing)
#' spectrogram(sound, samplingRate = 16000,  noiseReduction = 1.1,
#'   brightness = -2)  # white noise attenuated
#'
#' # apply median smoothing in both time and frequency domains
#' spectrogram(sound, samplingRate = 16000, smoothFreq = 5,
#'   smoothTime = 5)
#'
#' # increase contrast, reduce brightness
#' spectrogram(sound, samplingRate = 16000, contrast = 1, brightness = -1)
#'
#' # specify location of tick marks etc - see ?par() for base graphics
#' spectrogram(sound, samplingRate = 16000,
#'             ylim = c(0, 3), yaxp = c(0, 3, 5), xaxp = c(0, .8, 10))
#'
#' # Plot long audio files with reduced resolution
#' # (# ~4 s to process + 10 s to plot a 3-min song)
#' sp = spectrogram('~/Downloads/temp.wav', overlap = 0, osc = TRUE,
#'   maxPoints = c(1e5, 5e5),  # limit the number of pixels in osc/spec
#'   output = 'original', ylim = c(0, 6))
#' nrow(sp) * ncol(sp) / 5e5  # spec downsampled by a factor of ~9
#' }
spectrogram = function(
  x,
  samplingRate = NULL,
  dynamicRange = 80,
  windowLength = 50,
  step = NULL,
  overlap = 70,
  wn = 'gaussian',
  zp = 0,
  normalize = TRUE,
  scale = NULL,
  smoothFreq = 0,
  smoothTime = 0,
  qTime = 0,
  percentNoise = 10,
  noiseReduction = 0,
  method = c('spectrum', 'spectralDerivative')[1],
  output = c('original', 'processed', 'complex')[1],
  plot = TRUE,
  osc = FALSE,
  osc_dB = FALSE,
  heights = c(3, 1),
  ylim = NULL,
  yScale = c('linear', 'log')[1],
  contrast = .2,
  brightness = 0,
  maxPoints = c(1e5, 5e5),
  padWithSilence = TRUE,
  colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
  units = 'deprecated',
  xlab = NULL,
  ylab = NULL,
  xaxp = NULL,
  mar = c(5.1, 4.1, 4.1, 2),
  main = '',
  grid = NULL,
  internal = NULL,
  ...
) {
  sound = NULL
  duration = internal$duration
  if (overlap < 0 | overlap > 100) {
    warning('overlap must be >0 and <= 100%; resetting to 70')
    overlap = 70
  }
  if (is.null(step)) step = windowLength * (1 - overlap / 100)

  # import audio
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
    windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
    sound = sound_wav@left
    maxAmpl = 2^(sound_wav@bit - 1)
    ls = length(sound)
    if (windowLength_points > (ls / 2)) {
      windowLength_points = floor(ls / 4) * 2
      step = windowLength_points / samplingRate * 1000 * (1 - overlap / 100)
    }
    if (windowLength_points == 0) {
      stop('The sound and/or the windowLength is too short for plotting a spectrogram')
    }
    duration = ls / samplingRate
  } else if (class(x)[1] == 'numeric' & length(x) > 1) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    } else {
      sound = x
      if (is.null(scale)) {
        maxAmpl = max(abs(sound))
      } else {
        maxAmpl = scale
      }
      ls = length(sound)
      duration = ls / samplingRate
      windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
      if (windowLength_points > (ls / 2)) {
        windowLength_points = floor(ls / 4) * 2
        step = windowLength_points / samplingRate * 1000 * (1 - overlap / 100)
      }
      if (windowLength_points == 0) {
        stop('The sound and/or the windowLength is too short for plotting a spectrogram')
      }
    }
  }

  # Get a bank of windowed frames
  if (is.null(internal$timeShift)) internal$timeShift = 0
  if (!is.null(sound)) {
    frameBank = getFrameBank(
      sound = sound,
      samplingRate = samplingRate,
      windowLength_points = windowLength_points,
      step = step,
      zp = zp,
      normalize = normalize,
      wn = wn,
      filter = NULL,
      padWithSilence = padWithSilence,
      timeShift = internal$timeShift
    )
  } else {
    frameBank = internal$frameBank
  }
  if (class(frameBank)[1] != 'matrix') {
    stop(
      'Input format not recognized. Please provide path to .wav or .mp3 file,
      a vector of amplitudes plus samplingRate, or a preprocessed frameBank'
    )
  }

  # fix default settings
  contrast_exp = exp(3 * contrast)
  brightness_exp = exp(3 * brightness)
  # visualization: plot(exp(3 * seq(-1, 1, by = .01)), type = 'l')

  # Prepare for FFT
  windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
  if (!is.null(sound)) {
    if (windowLength_points > (ls / 2)) {
      windowLength_points = floor(ls / 4) * 2
      step = windowLength_points / samplingRate * 1000 * (1 - overlap / 100)
    }
  }
  if (windowLength_points == 0) {
    stop('The sound and/or the windowLength is too short for plotting a spectrogram')
  }

  # time stamps
  X = as.numeric(colnames(frameBank))
  # adjust the timing of spectrogram to match the actual time stamps
  # in getFrameBank (~the middle of each fft frame)
  if (length(X) < 2) {
    message('The sound is too short for plotting a spectrogram')
    return(NA)
  }

  # frequency stamps
  n1 = floor(windowLength_points / 2)
  bin_width = samplingRate / windowLength_points
  Y = (0:(n1 - 1)) * bin_width / 1000
  if (length(Y) < 2) {
    message('The sound and/or the windowLength is too short for plotting a spectrogram')
    return(NA)
  }

  # fft of each frame
  z = apply(frameBank, 2, function(x) stats::fft(x)[1:n1])
  if (!is.matrix(z)) z = matrix(z, ncol = 1)
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

  if (plot) {
    # produce a spectrogram of the modified fft
    color.palette = switchColorTheme(colorTheme)
    op = par(c('mar', 'xaxt', 'yaxt', 'mfrow')) # save user's original pars
    if (is.null(xlab)) xlab = ''
    if (!is.null(maxPoints)) {
      if (length(maxPoints) == 1) maxPoints = c(maxPoints, maxPoints)
    }
    if (is.null(ylim)) ylim = c(0, samplingRate / 2 / 1000)

    lx = length(X)
    ly = length(Y)
    x_ms = X[lx] < 1    # need to convert x-scale
    y_Hz = ylim[2] < 1  # need to convert y-scale

    if (osc | osc_dB) {
      # For long files, downsample before plotting
      if (!is.null(maxPoints) && maxPoints[1] < ls) {
        myseq = round(seq(1, ls, by = ls / maxPoints[1]))
        sound = sound[myseq]
        ls = length(myseq)
      }

      if (osc_dB) {
        sound = osc(sound,
                    dynamicRange = dynamicRange,
                    dB = TRUE,
                    maxAmpl = maxAmpl,
                    plot = FALSE,
                    returnWave = TRUE)
        ylim_osc = c(-2 * dynamicRange, 0)
      } else {
        ylim_osc = c(-maxAmpl, maxAmpl)
      }

      layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = heights)
      par(mar = c(mar[1:2], 0, mar[4]), xaxt = 's', yaxt = 's')
      time_stamps = seq(0, duration, length.out = ls) + internal$timeShift
      plot(
        time_stamps,
        sound,
        type = "l",
        ylim = ylim_osc,
        axes = FALSE, xaxs = "i", yaxs = "i", bty = 'o',
        xlab = xlab, ylab = '', main = '', ...)
      box()
      time_location = axTicks(1, axp = xaxp)
      time_labels = convert_sec_to_hms(time_location)
      axis(side = 1, at = time_location, labels = time_labels, ...)

      if (osc_dB) {
        axis(side = 4, at = seq(-dynamicRange, 0, by = 10), ...)
        abline(h = -dynamicRange, lty = 2, col = 'gray70')
        mtext("dB", side = 2, line = 3, ...)
      } else {
        abline(h = 0, lty = 2, col = 'gray70')
      }
      par(mar = c(0, mar[2:4]), xaxt = 'n', yaxt = 's')
      xlab = ''
    } else {
      par(mar = mar)
    }

    if (x_ms) {
      xlim = c(0, duration * 1000) + internal$timeShift * 1000
    } else {
      X = X / 1000
      xlim = c(0, duration) + internal$timeShift
    }
    if (y_Hz) {
      Y = Y * 1000
      ylim = ylim * 1000
      min_log_freq = 10
      if (is.null(ylab)) ylab = 'Frequency, Hz'
    }  else {
      min_log_freq = .01
      if (is.null(ylab)) ylab = 'Frequency, kHz'
    }
    if (yScale == 'log' & ylim[1] < min_log_freq)  ylim[1] = min_log_freq
    idx_y = which(Y >= (ylim[1] / 1.05) & Y <= (ylim[2] * 1.05))
    # 1.05 to avoid having a bit of white space
    Y = Y[idx_y]
    ly = length(Y)
    Z1_plot = Z1[, idx_y]

    # For long files, downsample before plotting
    lxy = lx *ly
    if (!is.null(maxPoints) && maxPoints[2] < lxy) {
      message(paste('Plotting with reduced resolution;',
                    'increase maxPoints or set to NULL to override'))
      downs = sqrt(lxy / maxPoints[2])
      seqx = round(seq(1, lx, length.out = lx / downs))
      seqy = round(seq(1, ly, length.out = ly / downs))
      X = X[seqx]
      Y = Y[seqy]
      Z1_plot = Z1[seqx, seqy]
    }

    filled.contour.mod(
      x = X, y = Y, z = Z1_plot,
      levels = seq(0, 1, length = 30),
      color.palette = color.palette,
      ylim = ylim, main = main,
      xlab = xlab, ylab = ylab,
      xlim = xlim, xaxt = 'n',
      log = ifelse(yScale == 'log', 'y', ''),
      ...
    )
    if (!(osc | osc_dB)) {
      time_location = axTicks(1, axp = xaxp)
      time_labels = convert_sec_to_hms(time_location)
      axis(side = 1, at = time_location, labels = time_labels, ...)
    }
    if (is.numeric(grid)) {
      n_grid_per_kHz = diff(range(ylim)) * grid
      if (Y[length(Y)] < 1) n_grid_per_kHz = n_grid_per_kHz / 1000
      grid(nx = n_grid_per_kHz, ny = n_grid_per_kHz,
           col = rgb(0, 0, 0, .25, maxColorValue = 1), lty = 3)
      # grid(nx = NULL, ny = NULL,
      #      col = rgb(0, 0, 0, .25, maxColorValue = 1), lty = 3,
      #      equilogs = TRUE)
    }
    if (!is.null(internal$pitch)) {
      do.call(addPitchCands, internal$pitch)
    }
    # restore original pars
    par('mar' = op$mar, 'xaxt' = op$xaxt, 'yaxt' = op$yaxt, 'mfrow' = op$mfrow)
  }

  if (output == 'original') {
    out = t(Z)  # before denoising
  } else if (output == 'processed') {
    out = t(Z1)  # denoised spectrum / spectralDerivative
  } else if (output == 'complex') {
    out = z  # with the imaginary part
  } else {
    out = NULL
  }
  invisible(out)
}


#' Save spectrograms per folder
#'
#' Creates spectrograms of all wav/mp3 files in a folder and saves them as .png
#' files in the same folder. This is a lot faster than running
#' \code{\link{analyzeFolder}} if you don't need pitch tracking. By default it
#' also creates an html file with a list of audio files and their spectrograms
#' in the same folder. If you open it in a browser that supports playing .wav
#' and/or .mp3 files (e.g. Firefox or Chrome), you can view the spetrograms and
#' click on them to play each sound. Unlike \code{\link{analyzeFolder}},
#' spectrogramFolder supports plotting both a spectrogram and an oscillogram if
#' \code{osc = TRUE}. The default approximate width of images in html (flexbox)
#' is determined by the \code{width} parameter (ie it is the same as the width
#' of png images, in pixels).
#' @inheritParams spectrogram
#' @inheritParams analyzeFolder
#' @param myfolder full path to the folder containing wav/mp3 files
#' @param htmlPlots if TRUE, saves an html file with clickable plots
#' @param ... other parameters passed to \code{\link{spectrogram}}
#' @export
#' @examples
#' \dontrun{
#' spectrogramFolder(
#'   '~/Downloads/temp',
#'   windowLength = 40, overlap = 75,  # spectrogram pars
#'   width = 1500, height = 900,        # passed to png()
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
  filenames = list.files(myfolder, pattern = "*.wav|.mp3|.WAV|.MP3", full.names = TRUE)
  if (length(filenames) < 1) {
    stop(paste('No wav/mp3 files found in', myfolder))
  }
  # in order to provide more accurate estimates of time to completion,
  # check the size of all files in the target folder
  filesizes = file.info(filenames)$size

  for (i in 1:length(filenames)) {
    # remove file extension
    f = substr(as.character(filenames[i]), 1, nchar(as.character(filenames[i])) - 4)
    png(filename = paste0(f, ".png"),
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
    htmlPlots(myfolder, myfiles = filenames, width = paste0(width, units))
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
#' @examples
#' wns = c('bartlett', 'blackman', 'flattop', 'hamming', 'hanning', 'rectangle', 'gaussian')
#' l = 200
#' par(mfrow = c(4, 2))
#' for (w in wns) {
#'   plot(1:l, soundgen:::ftwindow_modif(wl = l, wn = w), type = 'b', main = w)
#' }
#' par(mfrow = c(1, 1))
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
#' @param timeShift time (s) added to timestamps
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
                        normalize = TRUE,
                        filter = NULL,
                        padWithSilence = FALSE,
                        timeShift = NULL) {
  # # normalize to range from no less than -1 to no more than +1
  if (!is.numeric(sound)) return(NA)
  sound[is.na(sound)] = 0
  if (normalize & any(sound != 0)) {
    sound = sound - mean(sound)
    sound = sound / max(abs(max(sound)), abs(min(sound)))
  }
  step_points = round(step / 1000 * samplingRate)

  if (padWithSilence) {
    # pad with silence to make sure edges are properly analyzed
    sound = c(rep(0, windowLength_points / 2),
              sound,
              rep(0, (windowLength_points + step_points)))
  }
  myseq = seq(1, max(1, (length(sound) - windowLength_points)),
              by = step_points)
  if (padWithSilence) {
    time_stamps = (myseq - 1) *
      1000 / samplingRate
  } else {
    time_stamps = (myseq - 1 + windowLength_points / 2) *
      1000 / samplingRate
  }
  if (!is.null(timeShift)) time_stamps = time_stamps + round(timeShift * 1000)

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
  colnames(frameBank) = time_stamps
  return(frameBank)
}


#' Modified filled.contour
#'
#' Internal soundgen function
#'
#' A bare-bones version of \code{\link[graphics]{filled.contour}} that does not
#' plot a legend and accepts some additional graphical parameters like tick
#' marks.
#' @param x,y locations of grid lines
#' @param z numeric matrix of values to plot
#' @param xlim,ylim,zlim limits for the plot
#' @param levels levels for partitioning z
#' @param nlevels numbers of levels for partitioning z
#' @param color.palette color palette function
#' @param col list of colors instead of color.palette
#' @param asp,xaxs,yaxs,... graphical parameters passed to plot.window() and
#'   axis()
#' @param axisX,axisY plot the axis or not (logical)
#' @param log log = 'y' log-transforms the y axis
#' @keywords internal
filled.contour.mod = function(
  x = seq(0, 1, len = nrow(z)),
  y = seq(0, 1, len = ncol(z)),
  z,
  xlim = range(x, finite = TRUE),
  ylim = range(y, finite = TRUE),
  zlim = range(z, finite = TRUE),
  levels = pretty(zlim, nlevels),
  nlevels = 30,
  color.palette = function(n) grDevices::hcl.colors(n, "YlOrRd", rev = TRUE),
  col = color.palette(length(levels) - 1),
  asp = NA,
  xaxs = "i",
  yaxs = "i",
  log = '',
  axisX = TRUE,
  axisY = TRUE,
  ...
) {
  suppressWarnings({
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs,
                asp = asp, log = log, ...)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
      stop("no proper 'z' matrix specified")
    if (!is.double(z))  storage.mode(z) = "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), col = col)
    title(...)
    if (axisX) axis(1, ...)
    if (axisY) axis(2, ...)
  })
  invisible()
}


#' Oscillogram
#'
#' Plots the oscillogram (waveform) of a sound on a linear or logarithmic scale
#' (in dB). To get a dB scale, centers and normalizes the sound, then takes a
#' logarithm of the positive part and a flipped negative part, which is
#' analogous to "Waveform (dB)" view in Audacity. For more plotting options,
#' check \code{\link[seewave]{oscillo}}.
#' @return If \code{returnWave = TRUE}, returns the input waveform on the
#'   original or dB scale: a vector with range from `-dynamicRange` to
#'   `dynamicRange`.
#' @param x path to a .wav file or a vector of amplitudes with specified
#'   samplingRate
#' @param dynamicRange dynamic range of the oscillogram, dB
#' @param dB if TRUE, plots on a dB instead of linear scale
#' @param maxAmpl the maximum theoretically possible value indicating on which
#'   scale the sound is coded: 1 if the range is -1 to +1, 2^15 for 16-bit wav
#'   files, etc
#' @param samplingRate sampling rate of \code{x} (only needed if \code{x} is a
#'   numeric vector, rather than a .wav file)
#' @param returnWave if TRUE, returns a log-transformed waveform as a numeric vector
#' @param plot if TRUE, plots the oscillogram
#' @param xlab,ylab axis labels
#' @param bty box type (see `?par`)
#' @param midline if TRUE, draws a line at 0 dB
#' @param maxPoints the maximum number of points to plot (speeds up the plotting
#'   of long audio files, but beware of antialiasing)
#' @param ... Other graphical parameters passed on to `plot()`
#' @export
#' @examples
#' sound = sin(1:2000/10) *
#'         getSmoothContour(anchors = c(1, .01, .5), len = 2000)
#'
#' # Oscillogram on a linear scale without bells and whistles, just base R
#' plot(sound, type = 'l')
#'
#' # Oscillogram options with soundgen
#' osc(sound)             # linear
#' osc(sound, dB = TRUE)  # dB
#'
#' # For numeric vectors, indicate max amplitude
#' osc(sound, maxAmpl = 100, dB = TRUE)
#'
#' # Embellish and customize the plot
#' o = osc(sound, dB = TRUE, samplingRate = 1000, midline = FALSE,
#'         main = 'My waveform', col = 'blue', returnWave = TRUE)
#' abline(h = -80, col = 'orange', lty = 3)
#' o[1:10]  # the waveform in dB
#'
#' \dontrun{
#' # audio file
#' data(sheep, package = 'seewave')
#' osc(sheep@left, samplingRate = sheep@samp.rate, dB = TRUE)
#'
#' # for long files, reduce the resolution to plot quickly (careful: if the
#' resolution is too low, antialiasing may cause artifacts)
#' osc(sheep@left, samplingRate = sheep@samp.rate, dB = TRUE, maxPoints = 2500)
#' osc(sound, samplingRate = 5000, maxPoints = 100)
#'
#' # files several minutes long can be plotted in under a second
#' osc('~/Downloads/temp.wav', maxPoints = 20000)
#' }
osc = function(
  x,
  dynamicRange = 80,
  dB = FALSE,
  maxAmpl = NULL,
  samplingRate = NULL,
  returnWave = FALSE,
  plot = TRUE,
  xlab = NULL,
  ylab = NULL,
  bty = 'n',
  midline = TRUE,
  maxPoints = 10000,
  ...
) {
  # import a sound
  if (class(x)[1] == 'character') {
    sound_wav = tuneR::readWave(x)
    samplingRate = sound_wav@samp.rate
    sound = sound_wav@left
    if (is.null(maxAmpl)) maxAmpl = 2^(sound_wav@bit - 1)
  } else if (is.numeric(x)) {
    sound = x
  }

  # get original range
  rs = range(sound)
  d = diff(rs)
  if (!is.null(maxAmpl)) {
    mult = d / 2 / maxAmpl
    m = maxAmpl
  } else {
    mult = 1  # assume max loudness
    m = max(abs(rs))
  }

  if (dB) {
    # center and normalize to range from -1 to +1, unless it is quieter than maxAmpl
    ms = mean(sound)
    s1 = sound - ms
    rs = rs - ms
    s1 = s1 / max(abs(rs)) * mult

    # treat smaller values as 0 (beyond dynamic range)
    floor = 10^(-dynamicRange / 20)
    zero = which(abs(s1) < floor)

    # get indices of values above/below midline
    pos = which(s1 > floor)
    neg = which(s1 < -floor)

    # log-transform
    sound[pos] = 20 * log10(s1[pos])
    sound[neg] = -20 * log10(-s1[neg]) - 2 * dynamicRange
    sound[zero] = -dynamicRange
    midline_pos = -dynamicRange
  } else {
    midline_pos = mean(rs)
  }

  # plot
  if (plot) {
    # For long files, downsample before plotting
    l = length(sound)
    if (!is.null(maxPoints) && maxPoints < l) {
      myseq = round(seq(1, l, by = l / maxPoints))
      maxPoints = length(myseq)
      sound_plot = sound[myseq]
    } else {
      maxPoints = l
      sound_plot = sound
    }

    # Get time stamps
    if (!is.null(samplingRate)) {
      time = seq(1, l, length.out = maxPoints) / samplingRate * 1000
      if (is.null(xlab)) xlab = 'Time, ms'
    } else {
      time = seq(1, l, length.out = maxPoints)
      if (is.null(xlab)) xlab = 'Time, points'
    }
    if (is.null(ylab)) if (dB) ylab = 'dB' else ylab = ''
    if (dB) ylim = c(-2 * dynamicRange, 0) else ylim = c(-m, m)

    # plot
    plot(time, sound_plot, type = 'l', xlab = xlab, ylab = ylab,
         bty = bty, xaxt = 'n', yaxt = 'n', ylim = ylim, ...)
    time_location = axTicks(1)
    time_labels = convert_sec_to_hms(time_location / 1000)
    axis(side = 1, at = time_location, labels = time_labels)
    if (dB) {
      axis(side = 2, at = seq(-dynamicRange, 0, by = 10))
    } else {
      axis(side = 2)
    }
    if (midline) abline(h = midline_pos, lty = 2, col = 'gray70')
  }

  if (returnWave) invisible(sound)
}

#' Oscillogram on a decibell scale
#'
#' Deprecated; use \code{link{osc}} instead.
#' @inheritParams osc
#' @export
#' @examples
#' sound = sin(1:2000/10) *
#'         getSmoothContour(anchors = c(1, .01, .5), len = 2000)
#' osc_dB(sound)
osc_dB = function(
  x,
  dynamicRange = 80,
  dB = FALSE,
  maxAmpl = NULL,
  samplingRate = NULL,
  returnWave = FALSE,
  plot = TRUE,
  xlab = NULL,
  ylab = NULL,
  bty = 'n',
  midline = TRUE,
  maxPoints = NULL,
  ...
) {
  message("osc_dB is deprecated; please use osc(dB = TRUE) isntead")
  myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude ...
  myPars = myPars[1:(length(myPars)-1)]
  do.call(osc, myPars)
}

#' Get smooth spectrum
#'
#' Internal soundgen function.
#' @param sound the audio (numeric, any scale)
#' @param samplingRate the sampling rate, Hz
#' @param len the desired resolution of the output
#' @param loessSpan passed to loess to control the amount of smoothing (.01 =
#'   minimal smoothing, 1 = strong smoothing)
#' @keywords internal
#' @examples
#' s = soundgen(sylLen = 100, pitch = 500, addSilence = FALSE)
#' soundgen:::getSmoothSpectrum(s, 16000, len = 500, loessSpan = .01, plot = TRUE)
#' soundgen:::getSmoothSpectrum(s, 16000, len = 500, loessSpan = .1, plot = TRUE)
#' soundgen:::getSmoothSpectrum(s, 16000, len = 500, loessSpan = .5, plot = TRUE)
#' soundgen:::getSmoothSpectrum(s, 16000, len = 500, loessSpan = 1, plot = TRUE)
getSmoothSpectrum = function(sound,
                             samplingRate,
                             len,
                             loessSpan,
                             plot = FALSE,
                             xlab = 'Frequency, kHz',
                             ylab = 'dB',
                             type = 'l',
                             ...) {
  # Get high-res mean spectrum with seewave
  # (faster than smoothing the raw, super-long spectrum)
  wl = round(min(2048, length(sound) - 1) / 2) * 2  # must be even, otherwise seewave complains
  spec = as.data.frame(seewave::meanspec(
    sound, f = 16000, wl = wl, ovlp = 0,
    dB = 'max0', plot = FALSE))
  # plot(spec, type = 'l')

  # Smooth this mean spectrum with loess and upsample to /len/
  l = suppressWarnings(loess(spec$y ~ spec$x, span = loessSpan))
  # plot(spec$x, predict(l), type = 'l')
  freq_loess = seq(spec$x[1], spec$x[nrow(spec)], length.out = len)
  ampl_loess = try(predict(l, freq_loess, silent = TRUE))
  out = data.frame(freq = freq_loess, ampl = ampl_loess)

  if (plot) plot(out, type = type, xlab = xlab, ylab = ylab, ...)

  invisible(out)
}

