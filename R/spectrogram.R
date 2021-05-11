### FUNCTIONS FOR PREPARING AND PLOTTING A SPECTROGRAM ###

#' Spectrogram folder
#'
#' Deprecated; use \code{\link{spectrogram}} instead
#' @param ... any input parameters
spectrogramFolder = function(...) {
  message('spectrogramFolder() is deprecated; please use spectrogram() instead')
}


#' Spectrogram
#'
#' Produces the spectrogram of a sound using short-time Fourier transform.
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
#' @seealso \code{\link{osc}} \code{\link{modulationSpectrum}} \code{\link{ssm}}
#'
#' @param x path to a folder, one or more wav or mp3 files c('file1.wav',
#'   'file2.mp3'), Wave object, numeric vector, or a list of Wave objects or
#'   numeric vectors
#' @param samplingRate sampling rate of \code{x} (only needed if \code{x} is a
#'   numeric vector)
#' @param scale maximum possible amplitude of input used for normalization of
#'   input vector (only needed if \code{x} is a numeric vector)
#' @param from,to if NULL (default), analyzes the whole sound, otherwise
#'   from...to (s)
#' @param dynamicRange dynamic range, dB. All values more than one dynamicRange
#'   under maximum are treated as zero
#' @param windowLength length of FFT window, ms
#' @param overlap overlap between successive FFT frames, \%
#' @param step you can override \code{overlap} by specifying FFT step, ms (NB:
#'   because digital audio is sampled at discrete time intervals of
#'   1/samplingRate, the actual step and thus the time stamps of STFT frames
#'   may be slightly different, eg 24.98866 instead of 25.0 ms)
#' @param wn window type accepted by \code{\link[seewave]{ftwindow}}, currently
#'   gaussian, hanning, hamming, bartlett, rectangular, blackman, flattop
#' @param normalize if TRUE, scales input prior to FFT
#' @param zp window length after zero padding, points
#' @param smoothFreq,smoothTime length of the window for median smoothing in
#'   frequency and time domains, respectively, points
#' @param qTime the quantile to be subtracted for each frequency bin. For ex.,
#'   if qTime = 0.5, the median of each frequency bin (over the entire sound
#'   duration) will be calculated and subtracted from each frame (see examples)
#' @param percentNoise percentage of frames (0 to 100\%) used for calculating
#'   noise spectrum
#' @param noiseReduction how much noise to remove (non-negative number,
#'   recommended 0 to 2). 0 = no noise reduction, 2 = strong noise reduction:
#'   \eqn{spectrum - (noiseReduction * noiseSpectrum)}, where noiseSpectrum is
#'   the average spectrum of frames with entropy exceeding the quantile set by
#'   \code{percentNoise}
#' @param contrast spectrum is exponentiated by contrast (any real number,
#'   recommended -1 to +1). Contrast >0 increases sharpness, <0 decreases
#'   sharpness
#' @param brightness how much to "lighten" the image (>0 = lighter, <0 = darker)
#' @param maxPoints the maximum number of "pixels" in the oscillogram (if any)
#'   and spectrogram; good for quickly plotting long audio files; defaults to
#'   c(1e5, 5e5)
#' @param method plot spectrum ('spectrum') or spectral derivative
#'   ('spectralDerivative')
#' @param output specifies what to return: nothing ('none'), unmodified
#'   spectrogram ('original'), denoised and/or smoothed spectrogram
#'   ('processed'), or unmodified spectrogram with the imaginary part giving
#'   phase ('complex')
#' @param reportEvery when processing multiple inputs, report estimated time
#'   left every ... iterations (NULL = default, NA = don't report)
#' @param plot should a spectrogram be plotted? TRUE / FALSE
#' @param savePlots full path to the folder in which to save the plots (NULL =
#'   don't save, '' = same folder as audio)
#' @param osc "none" = no oscillogram; "linear" = on the original scale; "dB" =
#'   in decibels
#' @param ylim frequency range to plot, kHz (defaults to 0 to Nyquist frequency)
#' @param yScale scale of the frequency axis: 'linear' = linear, 'log' =
#'   logarithmic
#' @param heights a vector of length two specifying the relative height of the
#'   spectrogram and the oscillogram (including time axes labels)
#' @param padWithSilence if TRUE, pads the sound with just enough silence to
#'   resolve the edges properly (only the original region is plotted, so the
#'   apparent duration doesn't change)
#' @param colorTheme black and white ('bw'), as in seewave package ('seewave'),
#'   or any palette from \code{\link[grDevices]{palette}} such as 'heat.colors',
#'   'cm.colors', etc
#' @param extraContour a vector of arbitrary length scaled in Hz that will be
#'   plotted over the spectrogram (eg pitch contour); can also be a list with
#'   extra graphical parameters such as lwd, col, etc. (see examples)
#' @param xlab,ylab,main,mar,xaxp graphical parameters for plotting
#' @param grid if numeric, adds n = \code{grid} dotted lines per kHz
#' @param width,height,units,res graphical parameters for saving plots passed to
#'   \code{\link[grDevices]{png}}
#' @param ... other graphical parameters
#' @export
#' @return Returns nothing (if output = 'none'), absolute - not power! -
#'   spectrum (if output = 'original'), denoised and/or smoothed spectrum (if
#'   output = 'processed'), or spectral derivatives (if method =
#'   'spectralDerivative') as a matrix of real numbers.
#' @examples
#' # synthesize a sound 500 ms long, with gradually increasing hissing noise
#' sound = soundgen(sylLen = 500, temperature = 0.001, noise = list(
#'   time = c(0, 650), value = c(-40, 0)), formantsNoise = list(
#'   f1 = list(freq = 5000, width = 10000)))
#' # playme(sound, samplingRate = 16000)
#'
#' # basic spectrogram
#' spectrogram(sound, samplingRate = 16000, yScale = 'log')
#'
#' \dontrun{
#' # add bells and whistles
#' spectrogram(sound, samplingRate = 16000,
#'   osc = 'dB',  # plot oscillogram in dB
#'   heights = c(2, 1),  # spectro/osc height ratio
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
#' # remove the oscillogram
#' spectrogram(sound, samplingRate = 16000, osc = 'none')  # or NULL etc
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
#' data(sheep, package = 'seewave')
#' sp = spectrogram(sheep, overlap = 0,
#'   maxPoints = c(1e4, 5e3),  # limit the number of pixels in osc/spec
#'   output = 'original')
#' nrow(sp) * ncol(sp) / 5e3  # spec downsampled by a factor of ~2
#'
#' # Plot some arbitrary contour over the spectrogram (simply calling lines()
#' # will not work if osc = TRUE b/c the plot layout is modified)
#' s = soundgen()
#' an = analyze(s, 16000, plot = FALSE)
#' spectrogram(s, 16000, extraContour = an$detailed$dom, ylim = c(0, 2))
#' # For values that are not in Hz, normalize any way you like
#' spectrogram(s, 16000, ylim = c(0, 2), extraContour = list(
#'   x = an$detailed$loudness / max(an$detailed$loudness, na.rm = TRUE) * 2000,
#'   # ylim[2] = 2000 Hz
#'   type = 'b', pch = 5, lwd = 2, lty = 2, col = 'blue'))
#' }
spectrogram = function(
  x,
  samplingRate = NULL,
  scale = NULL,
  from = NULL,
  to = NULL,
  dynamicRange = 80,
  windowLength = 50,
  step = NULL,
  overlap = 70,
  wn = 'gaussian',
  zp = 0,
  normalize = TRUE,
  smoothFreq = 0,
  smoothTime = 0,
  qTime = 0,
  percentNoise = 10,
  noiseReduction = 0,
  method = c('spectrum', 'spectralDerivative')[1],
  output = c('original', 'processed', 'complex')[1],
  reportEvery = NULL,
  plot = TRUE,
  savePlots = NULL,
  osc = c('none', 'linear', 'dB')[2],
  heights = c(3, 1),
  ylim = NULL,
  yScale = c('linear', 'log')[1],
  contrast = .2,
  brightness = 0,
  maxPoints = c(1e5, 5e5),
  padWithSilence = TRUE,
  colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
  extraContour = NULL,
  xlab = NULL,
  ylab = NULL,
  xaxp = NULL,
  mar = c(5.1, 4.1, 4.1, 2),
  main = NULL,
  grid = NULL,
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  ...
) {
  # match args
  myPars = c(as.list(environment()), list(...))
  # myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to', 'reportEvery', 'savePlots')]

  # call .spectrogram
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.spectrogram',
    myPars = myPars,
    reportEvery = reportEvery,
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots)) {
    htmlPlots(
      htmlFile = paste0(pa$input$savePlots, '00_clickablePlots_spectrogram.html'),
      plotFiles = paste0(pa$input$filenames_noExt, "_spectrogram.png"),
      audioFiles = if (savePlots == '') pa$input$filenames_base else pa$input$filenames,
      width = paste0(width, units))
  }
  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}


#' Spectrogram per sound
#'
#' Internal soundgen function called by \code{\link{spectrogram}} and
#' \code{\link{analyze}}.
#' @inheritParams spectrogram
#' @param internal a long list of stuff for plotting pitch contours passed by
#'   analyze()
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.spectrogram = function(
  audio,
  dynamicRange = 80,
  windowLength = 50,
  step = NULL,
  overlap = 70,
  wn = 'gaussian',
  zp = 0,
  normalize = TRUE,
  smoothFreq = 0,
  smoothTime = 0,
  qTime = 0,
  percentNoise = 10,
  noiseReduction = 0,
  method = c('spectrum', 'spectralDerivative')[1],
  output = c('original', 'processed', 'complex')[1],
  plot = TRUE,
  osc = c('none', 'linear', 'dB')[2],
  heights = c(3, 1),
  ylim = NULL,
  yScale = c('linear', 'log')[1],
  contrast = .2,
  brightness = 0,
  maxPoints = c(1e5, 5e5),
  padWithSilence = TRUE,
  colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
  extraContour = NULL,
  xlab = NULL,
  ylab = NULL,
  xaxp = NULL,
  mar = c(5.1, 4.1, 4.1, 2),
  main = NULL,
  grid = NULL,
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  internal = NULL,
  ...
) {
  if (!is.null(step)) overlap = 100 * (1 - step / windowLength)
  if (overlap < 0 | overlap > 100) {
    warning('overlap must be >0 and <= 100%; resetting to 70')
    overlap = 70
  }
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  windowLength_points = floor(windowLength / 1000 * audio$samplingRate / 2) * 2
  if (windowLength_points > (audio$ls / 2)) {
    windowLength_points = floor(audio$ls / 4) * 2
    step = windowLength_points / audio$samplingRate * 1000 * (1 - overlap / 100)
  }
  if (windowLength_points == 0) {
    stop('The sound and/or windowLength are too short for plotting a spectrogram')
  }

  # Get a bank of windowed frames
  if (is.null(internal$frameBank)) {
    internal$frameBank = getFrameBank(
      sound = audio$sound,
      samplingRate = audio$samplingRate,
      windowLength_points = windowLength_points,
      step = step,
      zp = zp,
      normalize = normalize,
      wn = wn,
      filter = NULL,
      padWithSilence = padWithSilence,
      timeShift = audio$timeShift
    )
  }

  # fix default settings
  contrast_exp = exp(3 * contrast)
  brightness_exp = exp(3 * brightness)
  # visualization: plot(exp(3 * seq(-1, 1, by = .01)), type = 'l')

  # time stamps
  X = as.numeric(colnames(internal$frameBank))
  # adjust the timing of spectrogram to match the actual time stamps
  # in getFrameBank (~the middle of each fft frame)
  if (length(X) < 2) {
    message('The sound is too short for plotting a spectrogram')
    return(NA)
  }

  # frequency stamps
  zpExtra = max(0, floor((zp - windowLength_points) / 2) * 2)
  windowLength_points = windowLength_points + zpExtra
  n1 = floor(windowLength_points / 2)  # zpExtra
  bin_width = audio$samplingRate / windowLength_points
  Y = (0:(n1 - 1)) * bin_width / 1000
  if (length(Y) < 2) {
    message('The sound and/or the windowLength is too short for plotting a spectrogram')
    return(NA)
  }

  # fft of each frame
  z = apply(internal$frameBank, 2, function(x) stats::fft(x)[1:n1])
  if (!is.matrix(z)) z = matrix(z, ncol = 1)
  rownames(z) = Y
  colnames(z) = X
  Z = t(abs(z))
  # image(Z)

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

  # contrast & brightness
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

  # plot
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_spectrogram.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    # produce a spectrogram of the modified fft
    plotSpec(
      X = X, Y = Y, Z = Z1,
      audio = audio, internal = internal, dynamicRange = dynamicRange,
      osc = osc, heights = heights, ylim = ylim, yScale = yScale,
      maxPoints = maxPoints, colorTheme = colorTheme,
      extraContour = extraContour,
      xlab = xlab, ylab = ylab, xaxp = xaxp,
      mar = mar, main = main, grid = grid,
      width = width, height = height,
      units = units, res = res,
      ...
    )
  }
  if (is.character(audio$savePlots)) {
    dev.off()
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
  # normalize to range from no less than -1 to no more than +1
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
    filter = seewave::ftwindow(wl = windowLength_points, wn = wn)
  }

  # zero padding
  zpExtra = max(0, floor((zp - windowLength_points) / 2) * 2)
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
  maxPoints = 5e5,
  ...
) {
  suppressWarnings({
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs,
                asp = asp, log = log, ...)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
      stop("no proper 'z' matrix specified")
    if (!is.double(z))  storage.mode(z) = "double"

    # for very large matrices, downsample before plotting to avoid delays
    if (!is.null(maxPoints)) {
      lx = length(x)
      ly = length(y)
      lxy = lx *ly
      if (maxPoints < lxy) {
        message(paste('Plotting with reduced resolution;',
                      'increase maxPoints or set to NULL to override'))
        downs = sqrt(lxy / maxPoints)
        seqx = seq(1, lx, length.out = ceiling(lx / downs))
        seqy = seq(1, ly, length.out = ceiling(ly / downs))
        x = x[seqx]
        y = y[seqy]
        z = z[seqx, seqy]
      }
    }

    .filled.contour(as.double(x), as.double(y), z, as.double(levels), col = col)
    title(...)
    if (axisX) axis(1, ...)
    if (axisY) axis(2, ...)
  })
  invisible()
}


#' Get smooth spectrum
#'
#' Internal soundgen function.
#' @param sound the audio (numeric, any scale)
#' @inheritParams spectrogram
#' @param a pre-extracted spectrum in dB with columns "freq" and "ampl"
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
#'
#' sp = seewave::meanspec(s, f = 16000, dB = 'max0')
#' colnames(sp) = c('freq', 'ampl')
#' soundgen:::getSmoothSpectrum(spectrum = sp, len = 500, loessSpan = .1, plot = TRUE)
getSmoothSpectrum = function(sound,
                             samplingRate = NULL,
                             spectrum = NULL,
                             len,
                             loessSpan,
                             windowLength = 100,
                             overlap = 0,
                             plot = FALSE,
                             xlab = 'Frequency, kHz',
                             ylab = 'dB',
                             type = 'l',
                             ...) {
  if (is.null(spectrum)) {
    # assume that input is a sound
    # Get high-res mean spectrum with seewave
    # (faster than smoothing the raw, super-long spectrum)
    if (is.null(samplingRate)) stop('Please provide samplingRate')
    wl = round(min(windowLength / 1000 * samplingRate, length(sound) - 1) / 2) * 2
    # must be even, otherwise seewave complains
    spectrum = as.data.frame(seewave::meanspec(
      sound, f = samplingRate, wl = wl, ovlp = overlap,
      dB = 'max0', plot = FALSE))
    colnames(spectrum) = c('freq', 'ampl')
    # plot(spectrum, type = 'l')
  } else {
    spectrum = as.data.frame(spectrum)
  }

  # Smooth this mean spectrum with loess and upsample to /len/
  l = suppressWarnings(loess(spectrum$ampl ~ spectrum$freq, span = loessSpan))
  # plot(spectrum$freq, predict(l), type = 'l')
  freq_loess = seq(spectrum$freq[1], spectrum$freq[nrow(spectrum)], length.out = len)
  ampl_loess = try(predict(l, freq_loess, silent = TRUE))
  out = data.frame(freq = freq_loess, ampl = ampl_loess)

  if (plot) plot(out, type = type, xlab = xlab, ylab = ylab, ...)

  invisible(out)
}


#' Plot spectrogram
#'
#' Internal soundgen function
#'
#' Helper function called by spectrogram() etc to plot a spectrogram.
#' @param X time stamps, ms
#' @param Y frequency stamps, kHz
#' @param Z time in rows, frequency in columns (NB: this is the transpose of the
#'   exported spectrogram!)
#' @param audio a list returned by \code{readAudio}
#' @inheritParams spectrogram
#' @keywords internal
plotSpec = function(
  X, Y, Z,
  audio = NULL,
  internal = NULL,
  dynamicRange = 80,
  osc = c('none', 'linear', 'dB')[2],
  heights = c(3, 1),
  ylim = NULL,
  yScale = c('linear', 'log')[1],
  contrast = .2,
  brightness = 0,
  maxPoints = c(1e5, 5e5),
  padWithSilence = TRUE,
  colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
  extraContour = NULL,
  xlab = NULL,
  ylab = NULL,
  xaxp = NULL,
  mar = c(5.1, 4.1, 4.1, 2),
  main = NULL,
  grid = NULL,
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  ...
) {
  # produce a spectrogram of the modified fft
  color.palette = switchColorTheme(colorTheme)
  if (osc == TRUE) osc = 'linear' else if (!is.character(osc)) osc = 'none'
  op = par(c('mar', 'xaxt', 'yaxt', 'mfrow')) # save user's original pars
  if (is.null(xlab)) xlab = ''
  if (!is.null(maxPoints)) {
    if (length(maxPoints) == 1) maxPoints = c(maxPoints, maxPoints)
  }
  if (is.null(ylim)) ylim = c(0, audio$samplingRate / 2 / 1000)
  if (is.null(main)) {
    if (audio$filename_noExt == 'sound') {
      main = ''
    } else {
      main = audio$filename_noExt
    }
  }

  lx = length(X)
  ly = length(Y)
  x_ms = X[lx] < 1    # need to convert x-scale
  y_Hz = ylim[2] < 1  # need to convert y-scale

  if (osc %in% c('linear', 'dB')) {
    # For long files, downsample before plotting
    if (!is.null(maxPoints) && maxPoints[1] < audio$ls) {
      myseq = seq(1, audio$ls, by = ceiling(audio$ls / maxPoints[1]))
      audio$sound = audio$sound[myseq]
      audio$ls = length(myseq)
    }

    if (osc == 'dB') {
      audio$sound = .osc(
        audio,
        dynamicRange = dynamicRange,
        dB = TRUE,
        plot = FALSE,
        returnWave = TRUE)
      ylim_osc = c(-2 * dynamicRange, 0)
    } else {
      ylim_osc = c(-audio$scale, audio$scale)
    }

    layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = heights)
    par(mar = c(mar[1:2], 0, mar[4]), xaxt = 's', yaxt = 's')
    time_stamps = seq(0, audio$duration, length.out = audio$ls) + audio$timeShift
    plot(
      time_stamps,
      audio$sound,
      type = "l",
      ylim = ylim_osc,
      axes = FALSE, xaxs = "i", yaxs = "i", bty = 'o',
      xlab = xlab, ylab = '', main = '', ...)
    box()
    time_location = axTicks(1, axp = xaxp)
    time_labels = convert_sec_to_hms(time_location, 3)
    axis(side = 1, at = time_location, labels = time_labels, ...)

    if (osc == 'dB') {
      axis(side = 4, at = seq(-dynamicRange, 0, by = 10), ...)
      abline(h = -dynamicRange, lty = 2, col = 'gray70')
      # mtext("dB", side = 2, line = 3, ...)
    } else {
      abline(h = 0, lty = 2, col = 'gray70')
    }
    par(mar = c(0, mar[2:4]), xaxt = 'n', yaxt = 's')
    xlab = ''
  } else {
    par(mar = mar)
  }

  if (x_ms) {
    xlim = c(0, audio$duration * 1000) + audio$timeShift * 1000
  } else {
    X = X / 1000
    xlim = c(0, audio$duration) + audio$timeShift
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
  Z = Z[, idx_y]

  filled.contour.mod(
    x = X, y = Y, z = Z,
    levels = seq(0, 1, length = 30),
    color.palette = color.palette,
    ylim = ylim, main = main,
    xlab = xlab, ylab = ylab,
    xlim = xlim, xaxt = 'n',
    log = ifelse(yScale == 'log', 'y', ''),
    maxPoints = maxPoints[2],
    ...
  )
  if (!(osc %in% c('linear', 'dB'))) {
    time_location = axTicks(1, axp = xaxp)
    time_labels = convert_sec_to_hms(time_location, 3)
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
    do.call(addPitchCands, c(internal$pitch, list(y_Hz = y_Hz)))
  }

  # add an extra contour, if any
  if (!is.null(extraContour)) {
    extraContour_pars = list()
    if (is.list(extraContour)) {
      if (length(extraContour) > 1)
        extraContour_pars = extraContour[2:length(extraContour)]
      cnt = extraContour[[1]]
    } else {
      cnt = extraContour
    }
    # make sure the contour's length = ncol(spectrogram)
    lc = length(cnt)
    cnt = approx(x = 1:lc, y = cnt,
                 xout = seq(1, lc, length.out = length(X)),
                 na.rm = FALSE)$y  # see ex. in ?approx on handling NAs
    do.call(addPitchCands, list(
      extraContour = cnt, extraContour_pars = extraContour_pars,
      y_Hz = y_Hz, timestamps = X,
      pitchCands = NA, pitchCert = NA, pitchSource = NA, pitch = NA))
  }
  # restore original pars
  par('mar' = op$mar, 'xaxt' = op$xaxt, 'yaxt' = op$yaxt, 'mfrow' = op$mfrow)
}
