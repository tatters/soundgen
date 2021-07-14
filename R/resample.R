#' Resample a vector
#'
#' Changes the sampling rate without introducing artefacts like aliasing.
#' Algorithm: to downsample, applies a low-pass filter, then decimates with
#' \code{approx}; to upsample, performs linear interpolation with \code{approx},
#' then applies a low-pass filter. NAs can be interpolated or preserved in the
#' output.
#' @inheritParams spectrogram
#' @inheritParams segment
#' @param mult multiplier of sampling rate: new sampling rate = old sampling
#'   rate x mult, so 1 = no effect, >1 = upsample, <1 = downsample
#' @param samplingRate_new an alternative to \code{mult} provided that the old
#'   \code{samplingRate is know} (NB: \code{mult} takes precedence)
#' @param lowPass if TRUE, applies a low-pass filter prior to decimation to
#'   avoid aliasing
#' @param na.rm if TRUE, NAs are interpolated, otherwise they are preserved in
#'   the output
#' @export
#' @examples
#' ## Example 1: a short vector with NAs
#' x = c(NA, 1, 2, 3, NA, NA, 6, 7, 8, NA)
#'
#' # upsample
#' soundgen::resample(x, mult = 3.5, lowPass = FALSE, plot = TRUE)  # just approx
#' soundgen::resample(x, mult = 3.5, lowPass = TRUE, plot = TRUE) # low-pass + approx
#' soundgen::resample(x, mult = 3.5, lowPass = FALSE, na.rm = TRUE, plot = TRUE)
#'
#' # downsample
#' soundgen::resample(x, mult = 0.5, lowPass = TRUE, plot = TRUE)
#' soundgen::resample(x, mult = 0.5, na.rm = TRUE, plot = TRUE)
#'
#'
#' ## Example 2: a sound
#' silence = rep(0, 10)
#' samplingRate = 1000
#' fr = seq(100, 300, length.out = 400)
#' x = c(silence, sin(cumsum(fr) * 2 * pi / samplingRate), silence)
#' spectrogram(x, samplingRate)
#'
#' # downsample
#' x1 = soundgen:::resample(x, mult = 1 / 2.5)
#' spectrogram(x1, samplingRate / 2.5)  # no aliasing
#' # cf:
#' x1bad = soundgen:::resample(x, mult = 1 / 2.5, lowPass = FALSE)
#' spectrogram(x1bad, samplingRate / 2.5)  # aliasing
#'
#' # upsample
#' x2 = soundgen:::resample(x, mult = 3)
#' spectrogram(x2, samplingRate * 3)  # nothing above the old Nyquist
#' # cf:
#' x2bad = soundgen:::resample(x, mult = 3, lowPass = FALSE)
#' spectrogram(x2bad, samplingRate * 3)  # high-frequency artefacts
#'
#' \dontrun{
#' # Example 3: resample all audio files in a folder to 8000 Hz
#' resample('~/Downloads/temp', saveAudio = '~/Downloads/temp/sr8000/',
#'          samplingRate_new = 8000, savePlots = '~/Downloads/temp/sr8000/')
#' }
resample = function(x,
                    samplingRate = NULL,
                    samplingRate_new = NULL,
                    mult = NULL,
                    lowPass = TRUE,
                    na.rm = FALSE,
                    reportEvery = NULL,
                    saveAudio = NULL,
                    plot = FALSE,
                    savePlots = NULL,
                    width = 900,
                    height = 500,
                    units = 'px',
                    res = NA,
                    ...) {
  # check mult / samplingRate_new
  if (is.null(mult)) {
    if (is.null(samplingRate_new))
      stop('Please specify either mult or samplingRate_new')
  } else {
    if (!is.null(samplingRate_new))
      warning('samplingRate_new will be ignored because mult is specified')
  }
  if (is.null(samplingRate)) samplingRate = 1  # just to avoid warnings

  # match args
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'savePlots', 'saveAudio', 'width', 'height', 'units')]

  pa = processAudio(x = x,
                    samplingRate = samplingRate,
                    saveAudio = saveAudio,
                    savePlots = savePlots,
                    funToCall = '.resample',
                    myPars = myPars,
                    reportEvery = reportEvery
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    if (is.null(pa$input$saveAudio)) {
      audioFiles = pa$input$filenames
    } else {
      audioFiles = paste0(pa$input$saveAudio, pa$input$filenames_noExt, '.wav')
    }
    htmlPlots(
      htmlFile = paste0(pa$input$savePlots, '00_clickablePlots_resample.html'),
      plotFiles = paste0(pa$input$savePlots, pa$input$filenames_noExt, "_resample.png"),
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


#' Resample per sound
#'
#' Internal soundgen function
#'
#' @param audio a list returned by \code{readAudio}
#' @inheritParams resample
#' @inheritParams segment
#' @keywords internal
.resample = function(audio,
                     mult = NULL,
                     samplingRate_new = NULL,
                     lowPass = TRUE,
                     na.rm = FALSE,
                     plot = FALSE,
                     width = 900,
                     height = 500,
                     units = 'px',
                     res = NA,
                     ...) {
  if (is.null(mult)) mult = samplingRate_new / audio$samplingRate
  x = audio$sound
  n1 = length(x)
  n2 = round(n1 * mult)
  idx_na = which(is.na(x))
  n_na = length(idx_na)
  any_na = n_na > 0
  if (any_na) {
    if (n_na == n1) return(rep(NA, n2))
    x_noNA = intplNA(x, idx_na = idx_na)
  } else {
    x_noNA = x
  }
  if (!any(diff(x_noNA) != 0)) return(rep(x[1], n2))

  if (!is.finite(mult) | mult < 0) {
    stop('mult must be a real positive number')
  } else if (mult == 1 | length(idx_na) == n1) {
    # nothing to do
    out = x
  } else if (mult < 1) {
    # downsample
    if (lowPass) x_noNA = bandpass(x_noNA, samplingRate = 1000, upr = 1000 / 2 * mult)
    out = approx(x_noNA, n = n2)$y
  } else if (mult > 1) {
    # upsample
    out = approx(x_noNA, n = n2)$y
    if (lowPass) out = bandpass(out, samplingRate = 1000, upr = 1000 / 2 / mult)
  }

  # put NAs back in
  if (!na.rm & any_na) {
    # find NA positions in the new sound
    d = diff(is.na(x))  # 1 = beginning of NA episode, -1 = end of NA episode
    beg = which(d == 1) + 1
    end = which(d == -1) + 1
    if (is.na(x[1])) beg = c(1, beg)
    if (is.na(x[n1])) end = c(end, n1)
    time_stamps = seq(0, 1, length.out = n1)
    na_pos_01 = data.frame(beg = time_stamps[beg], end = time_stamps[end])
    na_pos2 = round(na_pos_01 * n2)  # from % to position indices
    na_pos2_vector = unlist(apply(na_pos2, 1, function(x) x[1]:x[2]))
    na_pos2_vector = na_pos2_vector[na_pos2_vector > 0 &
                                      na_pos2_vector < n2]
    out[na_pos2_vector] = NA  # fill in NAs in the new vector
  }

  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_resample.png"),
        width = width, height = height, units = units, res = res)
  }
  if (!exists('time_stamps')) time_stamps = seq(0, 1, length.out = n1)
  if (plot) {
    plot(time_stamps, x, type = 'p',
         xlab = 'Relative position', ylab = '')
    points(x = seq(0, 1, length.out = n2), y = out,
           type = 'b', col = 'red', pch = 3)
    if (is.character(audio$savePlots)) dev.off()
  }

  if (!is.null(audio$saveAudio)) {
    if (!dir.exists(audio$saveAudio)) dir.create(audio$saveAudio)
    seewave::savewav(
      out, f = audio$samplingRate * mult,
      filename = paste0(audio$saveAudio, '/', audio$filename_noExt, '.wav'))
  }

  return(out)
}
