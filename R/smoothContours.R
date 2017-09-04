# Functions for generating and modifying smooth contours from a few anchors

#' Smooth contour from anchors
#'
#' Returns a smooth contour based on an arbitrary number of anchors. Used by
#' \code{\link{soundgen}} for generating intonation contour, mouth opening, etc.
#' Note that pitch contours are treated as a special case: values are
#' log-transformed prior to smoothing, so that with 2 anchors we get a linear
#' transition on a log scale (as if we were operating with musical notes rather
#' than frequencies in Hz). Pitch plots have two Y axes: one showing Hz and the
#' other showing musical notation.
#' @param anchors a dataframe of anchors with specified time and amplitude.
#'   \code{achors$time} can be in ms (with len=NULL) or in arbitrary units, eg 0
#'   to 1 (with duration determined by len, which must then be provided in ms).
#'   So anchors$time is assumed to be in ms if len=NULL and relative if len is
#'   specified. \code{anchors$value} can be on any scale.
#' @param len the required length of the output contour. If NULL, it will be
#'   calculated based on the maximum time value (in ms) and \code{samplingRate}
#' @param thisIsPitch (boolean) is this a pitch contour? If true, log-transforms
#'   before smoothing and plots in both Hz and musical notation
#' @param method smoothing method: 'spline' or 'loess' (default). Consider
#'   'spline' if you have a lot of anchors and not much patience, since it is
#'   much faster than 'loess', but it may produce weird results when there are
#'   only a few anchors
#' @param valueFloor,valueCeiling lower/upper bounds for the contour
#' @param plot (boolean) produce a plot?
#' @param samplingRate sampling rate used to convert time values to points (Hz)
#' @param voiced,contourLabel graphical pars for plotting breathing contours
#'   (see examples below)
#' @param main,xlim,ylim plotting options
#' @param ... other plotting options passed to \code{plot()}
#' @export
#' @return Returns a numeric vector.
#' @examples
#' a = getSmoothContour(anchors = data.frame(
#'   time = c(50, 137, 300), value = c(0.03, 0.78, 0.5)),
#'   voiced = 200, valueFloor = 0, plot = TRUE, main = '',
#'   samplingRate = 16000) # breathing
#'
#' a = getSmoothContour(anchors = data.frame(
#'   time = c(0, .1), value = c(350, 800)),
#'   len = 5500, thisIsPitch = TRUE, plot = TRUE,
#'   samplingRate = 3500) # pitch
#'
#' # a single anchor gives constant pitch
#' a = getSmoothContour(anchors = data.frame(time = 0, value = 800),
#'   len = 500, thisIsPitch = TRUE, plot = TRUE, samplingRate = 500)
#'
#' # two anchors give loglinear pitch change
#' a = getSmoothContour(anchors = data.frame(
#'   time = c(0, 1), value = c(220, 440)),
#'   len = 500, thisIsPitch = TRUE, plot = TRUE, samplingRate = 500)
getSmoothContour = function(anchors = data.frame(time = c(0, 1), value = c(0, 1)),
                            len = NULL,
                            thisIsPitch = FALSE,
                            method = c('spline', 'loess')[2],
                            valueFloor = NULL,
                            valueCeiling = NULL,
                            plot = FALSE,
                            main = '',
                            xlim = NULL,
                            ylim = NULL,
                            samplingRate = 16000,
                            voiced = NULL,
                            contourLabel = NULL,
                            ...) {
  if (class(anchors) == 'list') {
    anchors = as.data.frame(anchors)
  }
  if (!is.list(anchors)) {  # no anchors specified
    return(NA)
  }
  if (nrow(anchors) > 10 && method == 'loess') {
    method = 'spline'
    # warning('More than 10 anchors; changing interpolation method from loess to spline')
  }

  if (!is.null(valueFloor)) {
    anchors$value[anchors$value < valueFloor] = valueFloor
  }
  if (!is.null(valueCeiling)) {
    anchors$value[anchors$value > valueCeiling] = valueCeiling
  }
  if (thisIsPitch) {
    anchors$value = HzToSemitones(anchors$value)
    if (!is.null(valueFloor))
      valueFloor = HzToSemitones(valueFloor)
    if (!is.null(valueCeiling))
      valueCeiling = HzToSemitones(valueCeiling)
  }

  if (is.null(len)) {
    # if len is null, we expect that anchors$time encoded
    # the desired duration, in ms
    duration_ms = max(anchors$time) - min(anchors$time)
    len = floor(duration_ms * samplingRate / 1000)
  } else {
    anchors$time = anchors$time - min(anchors$time)
    anchors$time = anchors$time / max(anchors$time) # strictly 0 to 1
    duration_ms = len / samplingRate * 1000
  }

  if (!is.numeric(duration_ms) || duration_ms == 0 || !is.numeric(len) || len == 0) {
    return(NA)
  }

  time = 1:len
  if (nrow(anchors) == 1) {
    # flat
    smoothContour = rep(anchors$value[1], len)
  } else if (nrow(anchors) == 2) {
    # linear
    smoothContour = seq(anchors$value[1], anchors$value[2], length.out = len)
  } else {
    # smooth contour
    if (method == 'spline') {
      smoothContour = spline(anchors$value, n = len, x = anchors$time)$y
      # plot(smoothContour, type='l')
    } else if (method == 'loess') {
      anchor_time_points = anchors$time - min(anchors$time)
      anchor_time_points = anchor_time_points / max(anchor_time_points) * len
      anchor_time_points[anchor_time_points == 0] = 1
      anchors_long = as.vector(rep(NA, len))
      anchors_long[anchor_time_points] = anchors$value # plot (anchors_long)

      # let's draw a smooth curve through the given anchors
      span = (1 / (1 + exp(duration_ms / 500)) + 0.5) / 1.1 ^ (nrow(anchors) - 3)
      # NB: need to compensate for variable number of points, namely decrease
      # smoothing as the number of points increases, hence the "1.1^..." term
      # duration_ms = 50:9000
      # span = 1 / (1 + exp(duration_ms / 500)) + 0.5
      # plot(duration_ms, span, type = 'l')
      l = suppressWarnings(loess(anchors_long ~ time, span = span))
      # plot (time, anchors_long)
      smoothContour = try (predict(l, time), silent = TRUE)
      # plot(time, smoothContour)

      # for long duration etc, larger span may be needed to avoid error in loess
      while (class(smoothContour) == 'try-error') {
        span = span + 0.1
        l = suppressWarnings(loess(anchors_long ~ time, span = span))
        smoothContour = try (predict(l, time), silent = TRUE)
      }
      # plot (smoothContour, type = 'l')

      while (sum(smoothContour < valueFloor - 1e-6, na.rm = TRUE) > 0) {
        # in case we get values below valueFloor, less smoothing should be used
        # NB: -1e-6 avoids floating point problem, otherwise we get
        # weird cases of -120 (float) < -120 (integer)
        span = span / 1.1
        l = suppressWarnings(loess(anchors_long ~ time, span = span))
        smoothContour = try (predict(l, time), silent = TRUE)
      }
    }
    smoothContour[smoothContour < valueFloor] = valueFloor
    smoothContour[smoothContour > valueCeiling] = valueCeiling
  }
  # plot (smoothContour, type='l')

  if (plot) {
    op = par("mar") # save user's original margin settings
    idx = seq(1, len, length.out = min(len, 100))
    # for plotting, shorten smoothContour to max 100 points
    # to reduce processing load
    smoothContour_downsampled = smoothContour[idx]

    if (thisIsPitch) {
      # pitch - log-transformed
      if (is.null(ylim) || (
        min(smoothContour_downsampled) < HzToSemitones(ylim[1]) |
        max(smoothContour_downsampled) > HzToSemitones(ylim[2])
      )) {
        ylim = round(c(
          min(smoothContour_downsampled) / 1.1,
          max(smoothContour_downsampled) * 1.1
        ))
      } else {
        ylim = HzToSemitones(ylim)
      }
      lbls_semitones = unique(seq(ylim[1], ylim[2], length.out = 5))
      # unique to remove duplicates, max 5 labels
      lbls_notes = soundgen::notesDict$note[lbls_semitones + 1]
      lbls_Hz = round(semitonesToHz(lbls_semitones))

      par(mar = c(5, 4, 4, 3)) # c(bottom, left, top, right)
      plot(time[idx] / samplingRate * 1000, smoothContour_downsampled,
           type = 'l', yaxt = "n", ylab = 'Frequency, Hz', xlab = 'Time, ms',
           main = 'Pitch contour', ylim = ylim, ...)
      axis(2, at = lbls_semitones, labels = lbls_Hz, las = 1)
      axis(4, at = lbls_semitones, labels = lbls_notes, las = 1)
      points(anchors$time * duration_ms, anchors$value, col = 'blue', cex = 3)
    } else {
      # not pitch - not log-transformed
      if (!max(anchors$time) > 1) {
        anchors$time = anchors$time * duration_ms
      } # presuming that len was specified and anchors$time are on a
      # relative scale, we transform to ms for plotting
      par(mar = c(5, 4, 4, 3)) # c(bottom, left, top, right)
      if (is.null(xlim)) {
        xlim = c(min(0, anchors$time[1]), anchors$time[length(anchors$time)])
      }
      if (is.null(ylim)) {
        ylim = c(min(0, min(anchors$value[1])), max(0, max(anchors$value)))
      }
      x = seq(anchors$time[1],
              anchors$time[length(anchors$time)],
              length.out = length(smoothContour_downsampled))
      plot(x = x, y = smoothContour_downsampled, type = 'l', ylab = 'Amplitude',
           xlab = 'Time, ms', xlim = xlim, ylim = ylim, ...)
      points(anchors$time, anchors$value, col = 'blue', cex = 3)
      if (is.numeric(voiced)) {
        lines(x = c(0, voiced), y = c(0, 0), col = 'blue', lwd = 10)
        text(x = voiced / 2, y = abs(ylim[2] - ylim[1]) / 25,
             adj = 0.5, labels = 'voiced part', col = 'blue')
        text(x = anchors$time[nrow(anchors)],
             y = anchors$value[nrow(anchors)] - (ylim[2] - ylim[1]) / 25,
             adj = 1, labels = contourLabel, col = 'red')
      }
    }
    par("mar" = op)  # restore original margin settings
  }
  # NA's may arise if the first anchor time > 0
  if (nrow(anchors) > 0) smoothContour[is.na(smoothContour)] = 0
  if (thisIsPitch)
    smoothContour = semitonesToHz(smoothContour)
  return(smoothContour)
}


#' Discrete smooth contour from anchors
#'
#' Internal soundgen function.
#'
#' A discrete version of \code{\link{getSmoothContour}} with modified plotting.
#' Intended for plotting variation in parameters across syllables.
#' @param len the number of syllables (equivalently, the length of generated
#'   contour)
#' @inheritParams getSmoothContour
#' @param ylim ylim for plotting
#' @return Numeric vector.
#' @keywords internal
#' @examples
#' # for a bout consisting of 10 syllables
#' soundgen:::getDiscreteContour(len = 10, method = 'spline', plot = TRUE,
#'   ylab = 'Semitones', anchors = data.frame(time = c(0, .2, .6, 1),
#'   value = c(0, -3, 1, 0)))
getDiscreteContour = function(len,
                              anchors = data.frame(time = c(0, 1), value = c(1, 1)),
                              method = c('spline', 'loess')[2],
                              valueFloor = NULL,
                              valueCeiling = NULL,
                              ylim = NULL,
                              plot = FALSE,
                              ...) {
  contour = getSmoothContour(
    len = len,
    anchors = anchors,
    method = method,
    valueFloor = valueFloor,
    valueCeiling = valueCeiling
  )
  if (plot) {
    if (is.null(ylim)) {
      ylim = c(min(min(contour), min(anchors$value)),
               max(max(contour), max(anchors$value)))
    }
    plot (contour, type = 'b', xlab = 'Syllable', col = 'red', bg = 'red',
          cex = 1, pch = 23, ylim = ylim, ...)
    points (x = anchors$time * (len - 1) + 1, y = anchors$value, col = 'blue',
            cex = 3)
  }
  return(contour)
}
