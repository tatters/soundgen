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
#' @inheritParams spectrogram
#' @param dB if TRUE, plots on a dB instead of linear scale
#' @param returnWave if TRUE, returns a log-transformed waveform as a numeric vector
#' @param plot if TRUE, plots the oscillogram
#' @param xlab,ylab axis labels
#' @param ylim override default amplitude scale for non-centered sounds
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
#' # For numeric vectors, indicate samplingRate and scale (max amplitude)
#' osc(sound, samplingRate = 1000, scale = 100, dB = TRUE)
#'
#' # Embellish and customize the plot
#' o = osc(sound, samplingRate = 1000, dB = TRUE, midline = FALSE,
#'         main = 'My waveform', col = 'blue', returnWave = TRUE)
#' abline(h = -80, col = 'orange', lty = 3)
#' o[1:10]  # the waveform in dB
#'
#' \dontrun{
#' # Wave object
#' data(sheep, package = 'seewave')
#' osc(sheep, dB = TRUE)
#'
#' # for long files, reduce the resolution to plot quickly (careful: if the
#' # resolution is too low, antialiasing may cause artifacts)
#' osc(sheep, dB = TRUE, maxPoints = 2500)
#' osc(sound, samplingRate = 5000, maxPoints = 100)
#'
#' # files several minutes long can be plotted in under a second
#' osc('~/Downloads/speechEx.wav', maxPoints = 20000)
#'
#' # saves oscillograms of all audio files in a folder
#' osc('~/Downloads/temp2', savePlots = '')
#' }
osc = function(
  x,
  samplingRate = NULL,
  scale = NULL,
  from = NULL,
  to = NULL,
  dynamicRange = 80,
  dB = FALSE,
  returnWave = FALSE,
  reportEvery = NULL,
  plot = TRUE,
  savePlots = NULL,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  ylim = NULL,
  bty = 'n',
  midline = TRUE,
  maxPoints = 10000,
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

  # call .osc
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.osc',
    myPars = myPars,
    reportEvery = reportEvery,
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots)) {
    htmlPlots(
      htmlFile = paste0(pa$input$savePlots, '00_clickablePlots_osc.html'),
      plotFiles = paste0(pa$input$filenames_noExt, "_osc.png"),
      audioFiles = if (savePlots == '') pa$input$filenames_base else pa$input$filenames,
      width = paste0(width, units))
  }
  if (returnWave) {
    if (pa$input$n == 1) pa$result = pa$result[[1]]
    invisible(pa$result)
  }
}


#' Oscillogram per sound
#'
#' Internal soundgen function called by \code{\link{osc}}.
#' @inheritParams osc
#' @keywords internal
.osc = function(
  audio,
  dynamicRange = 80,
  dB = FALSE,
  returnWave = FALSE,
  plot = TRUE,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  ylim = NULL,
  bty = 'n',
  midline = TRUE,
  maxPoints = 10000,
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  ...
) {
  # get original range
  rs = range(audio$sound)
  d = diff(rs)
  if (!is.null(audio$scale)) {
    mult = d / 2 / audio$scale
    m = audio$scale
  } else {
    mult = 1  # assume max loudness
    m = max(abs(rs))
  }

  if (dB) {
    # center and normalize to range from -1 to +1, unless it is quieter than maxAmpl
    ms = median(audio$sound)
    s1 = audio$sound - ms
    rs = rs - ms
    s1 = s1 / max(abs(rs)) * mult

    # treat smaller values as 0 (beyond dynamic range)
    floor = 10^(-dynamicRange / 20)
    zero = which(abs(s1) < floor)

    # get indices of values above/below midline
    pos = which(s1 > floor)
    neg = which(s1 < -floor)

    # log-transform
    audio$sound[pos] = 20 * log10(s1[pos])
    audio$sound[neg] = -20 * log10(-s1[neg]) - 2 * dynamicRange
    audio$sound[zero] = -dynamicRange
    midline_pos = -dynamicRange
  } else {
    if (prod(rs) < 0) {
      midline_pos = 0
    } else {
      midline_pos = median(rs)
    }
  }

  # plot
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_osc.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    if (is.null(main)) {
      if (audio$filename_noExt == 'sound') {
        main = ''
      } else {
        main = audio$filename_noExt
      }
    }
    # For long files, downsample before plotting
    if (!is.null(maxPoints) && maxPoints < audio$ls) {
      myseq = seq(1, audio$ls, by = ceiling(audio$ls / maxPoints))
      maxPoints = length(myseq)
      sound_plot = audio$sound[myseq]
    } else {
      maxPoints = audio$ls
      sound_plot = audio$sound
    }

    # Get time stamps
    if (!is.null(audio$samplingRate)) {
      time = seq(1, audio$ls, length.out = maxPoints) / audio$samplingRate * 1000
      if (is.null(xlab)) xlab = 'Time, ms'
    } else {
      time = seq(1, audio$ls, length.out = maxPoints)
      if (is.null(xlab)) xlab = 'Time, points'
    }
    if (is.null(ylab)) if (dB) ylab = 'dB' else ylab = ''
    if (is.null(ylim)) if (dB) ylim = c(-2 * dynamicRange, 0) else ylim = c(-m, m)

    # plot
    plot(time, sound_plot, type = 'l', main = main, xlab = xlab, ylab = ylab,
         bty = bty, xaxt = 'n', yaxt = 'n', ylim = ylim, ...)
    time_location = axTicks(1)
    if (!is.null(audio$samplingRate)) {
      time_labels = convert_sec_to_hms(time_location / 1000, 3)
    } else {
      time_labels = time_location
    }
    axis(side = 1, at = time_location, labels = time_labels)
    if (dB) {
      axis(side = 2, at = seq(-dynamicRange, 0, by = 10))
    } else {
      axis(side = 2)
    }
    if (midline) abline(h = midline_pos, lty = 2, col = 'gray70')
  }
  if (is.character(audio$savePlots)) {
    dev.off()
  }

  if (returnWave) return(audio$sound)
}
