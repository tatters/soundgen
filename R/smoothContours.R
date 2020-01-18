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
#' @param anchors a numeric vector of values or a list/dataframe with one column
#'   (value) or two columns (time and value). \code{achors$time} can be in ms
#'   (with len=NULL) or in arbitrary units, eg 0 to 1 (with duration determined
#'   by len, which must then be provided in ms). So anchors$time is assumed to
#'   be in ms if len=NULL and relative if len is specified. \code{anchors$value}
#'   can be on any scale.
#' @param len the required length of the output contour. If NULL, it will be
#'   calculated based on the maximum time value (in ms) and \code{samplingRate}
#' @param thisIsPitch (boolean) is this a pitch contour? If true, log-transforms
#'   before smoothing and plots in both Hz and musical notation
#' @param normalizeTime if TRUE, normalizes anchors$time values to range from 0 to 1
#' @inheritParams soundgen
#' @param loessSpan parameter that controlled the amount of smoothing when
#'   interpolating pitch etc between anchors; passed on to
#'   \code{\link[stats]{loess}}, so only has an effect if interpol = 'loess'
#' @param valueFloor,valueCeiling lowser/upper bounds for the contour
#' @param plot (boolean) produce a plot?
#' @param samplingRate sampling rate used to convert time values to points (Hz)
#' @param voiced,contourLabel graphical pars for plotting breathing contours
#'   (see examples below)
#' @param NA_to_zero if TRUE, all NAs are replaced with zero
#' @param xlim,ylim plotting options
#' @param ... other plotting options passed to \code{plot()}
#' @export
#' @return Returns a numeric vector.
#' @examples
#' # long format: anchors are a dataframe
#' a = getSmoothContour(anchors = data.frame(
#'   time = c(50, 137, 300), value = c(0.03, 0.78, 0.5)),
#'   normalizeTime = FALSE,
#'   voiced = 200, valueFloor = 0, plot = TRUE, main = '',
#'   samplingRate = 16000) # breathing
#'
#' # short format: anchors are a vector (equal time steps assumed)
#' a = getSmoothContour(anchors = c(350, 800, 600),
#'   len = 5500, thisIsPitch = TRUE, plot = TRUE,
#'   samplingRate = 3500) # pitch
#'
#' # a single anchor gives constant value
#' a = getSmoothContour(anchors = 800,
#'   len = 500, thisIsPitch = TRUE, plot = TRUE, samplingRate = 500)
#'
#' # two pitch anchors give loglinear F0 change
#' a = getSmoothContour(anchors = c(220, 440),
#'   len = 500, thisIsPitch = TRUE, plot = TRUE, samplingRate = 500)
#'
#' ## Two closely spaced anchors produce a pitch jump
#' # one loess for the entire contour
#' a1 = getSmoothContour(anchors = list(time = c(0, .15, .2, .7, 1),
#'     value = c(360, 116, 550, 700, 610)), len = 500, thisIsPitch = TRUE,
#'     plot = TRUE, samplingRate = 500)
#' # two segments with a linear transition
#' a2 = getSmoothContour(anchors = list(time = c(0, .15, .17, .7, 1),
#'     value = c(360, 116, 550, 700, 610)), len = 500, thisIsPitch = TRUE,
#'     plot = TRUE, samplingRate = 500)
#' # two segments with an abrupt jump
#' a3 = getSmoothContour(anchors = list(time = c(0, .15, .155, .7, 1),
#'     value = c(360, 116, 550, 700, 610)), len = 500, thisIsPitch = TRUE,
#'     plot = TRUE, samplingRate = 500)
#' # compare:
#' plot(a2)
#' plot(a3)  # NB: the segment before the jump is upsampled to compensate
getSmoothContour = function(anchors = data.frame(time = c(0, 1), value = c(0, 1)),
                            len = NULL,
                            thisIsPitch = FALSE,
                            normalizeTime = TRUE,
                            interpol = c('approx', 'spline', 'loess')[3],
                            discontThres = .05,
                            jumpThres = .01,
                            loessSpan = NULL,
                            valueFloor = NULL,
                            valueCeiling = NULL,
                            plot = FALSE,
                            xlim = NULL,
                            ylim = NULL,
                            samplingRate = 16000,
                            voiced = NULL,
                            contourLabel = NULL,
                            NA_to_zero = TRUE,
                            ...) {
  my_args = match.call()
  if (is.null(my_args$main)) {
    if (thisIsPitch)
      main = 'Pitch contour'
    else
      main = ''
  }
  # if (is.null(my_args$main)) {

  anchors = reformatAnchors(anchors, normalizeTime = normalizeTime)
  if (is.list(anchors)) {
    if (nrow(anchors) > 10 & nrow(anchors) < 50 & interpol == 'loess') {
      interpol = 'spline'
      # warning('More than 10 anchors; changing interpolation method from loess to spline')
    } else if (nrow(anchors) > 50) {
      interpol = 'approx'
    }
  } else {
    stop('Invalid format of the "anchors" argument')
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

  # if there are more anchors than len, pick just some anchors at random
  if (nrow(anchors) > len) {
    anchors = as.data.frame(anchors)
    idx = seq(1, nrow(anchors), length.out = len)
    anchors = anchors[idx, ]
  }

  if (!is.numeric(duration_ms) | !is.numeric(len)) {
    return(NA)
  } else if (duration_ms == 0 | len == 0) {
    return(NA)
  }

  # get smooth contours
  if (nrow(anchors) == len) {
    smoothContour = anchors$value
  } else if (discontThres <= 0 | nrow(anchors) < 3) {
    smoothContour = drawContour(len = len,
                                anchors = anchors,
                                interpol = interpol,
                                loessSpan = loessSpan,
                                valueFloor = valueFloor,
                                valueCeiling = valueCeiling,
                                duration_ms = duration_ms)
  } else {
    # some anchors might be too close, so we split the contour into segments
    # to avoid weird behavior of loess etc
    sections = splitContour(anchors = anchors,
                            discontThres = discontThres,
                            jumpThres = jumpThres)
    smoothContour = vector()
    for (i in 1:nrow(sections)) {
      segm_len = round((anchors$time[sections$end[i]] -
                          anchors$time[sections$start[i]]) /
                         diff(range(anchors$time)) * len)
      cont = drawContour(len = segm_len,
                         anchors = anchors[sections$start[i]:sections$end[i], ],
                         interpol = interpol,
                         loessSpan = loessSpan,
                         valueFloor = valueFloor,
                         valueCeiling = valueCeiling,
                         duration_ms = duration_ms)
      transition = vector()
      if (i < nrow(sections)) {
        trans_len = round((anchors$time[sections$start[i + 1]] -
                             anchors$time[sections$end[i]]) /
                            diff(range(anchors$time)) * len)
        if (sections$jump[i] & length(cont) > 0) {
          # upsample the segment before the jump to make up for skipped transition
          cont = approx(cont, n = length(cont) + trans_len)$y
        } else {
          # make a linear transition preserving spacing between anchors
          transition = seq(anchors$value[sections$end[i]],
                           anchors$value[sections$start[i + 1]],
                           length.out = trans_len)
        }
      }
      smoothContour = c(smoothContour, cont, transition)
    }
    if (length(smoothContour) != len) {
      smoothContour = approx(smoothContour, n = len)$y
    }
  }
  # plot(smoothContour, type='p')

  if (plot) {
    idx = seq(1, len, length.out = min(len, 100))
    op = par("mar") # save user's original margin settings
    if (len > 100) {
      # for plotting, shorten smoothContour to max 100 points
      # to reduce processing load
      smoothContour_downsampled = smoothContour[idx]
    } else {
      smoothContour_downsampled = smoothContour
    }
    # plot(smoothContour_downsampled)

    # presuming that len was specified and anchors$time are on a
    # relative scale, we transform to ms for plotting
    if (!max(anchors$time) > 1) {
      anchors$time = anchors$time * duration_ms
      time = seq(0, len, length.out = len)
      x = time[idx] / samplingRate * 1000
    } else {  # time is already in ms
      time = seq(anchors$time[1], anchors$time[nrow(anchors)], length.out = len)
      x = time[idx]
    }

    if (thisIsPitch) {
      # pitch - log-transformed
      if (!is.numeric(ylim)) {
        ylim = c(
          min(smoothContour_downsampled) / 1.1,  # can't be negative
          max(smoothContour_downsampled) * 1.1
        )
      } else if (min(smoothContour_downsampled) < HzToSemitones(ylim[1])) {
        ylim[1] = min(smoothContour_downsampled) / 1.1
      } else if (max(smoothContour_downsampled) > HzToSemitones(ylim[2])) {
        ylim[2] = max(smoothContour_downsampled) * 1.1
      } else {
        ylim = HzToSemitones(ylim)
      }
      lbls_semitones = unique(seq(ylim[1], ylim[2], length.out = 5))
      # unique to remove duplicates, max 5 labels
      lbls_notes = soundgen::notesDict$note[round(lbls_semitones) + 1]
      lbls_Hz = round(semitonesToHz(lbls_semitones))

      par(mar = c(5, 4, 4, 3)) # c(bottom, left, top, right)
      plot(x, smoothContour_downsampled,
           type = 'l', yaxt = "n", ylab = 'Frequency, Hz', xlab = 'Time, ms',
           ylim = ylim, ...)
      axis(2, at = lbls_semitones, labels = lbls_Hz, las = 1)
      axis(4, at = lbls_semitones, labels = lbls_notes, las = 1)
      points(anchors$time, anchors$value, col = 'blue', cex = 3)
    } else {
      # not pitch - not log-transformed
      # if (!max(anchors$time) > 1) {
      #   anchors$time = anchors$time * duration_ms
      # } # presuming that len was specified and anchors$time are on a
      # # relative scale, we transform to ms for plotting
      par(mar = c(5, 4, 4, 3)) # c(bottom, left, top, right)
      if (is.null(xlim)) {
        xlim = c(min(0, anchors$time[1]), anchors$time[length(anchors$time)])
      }
      if (is.null(ylim)) {
        m1 = min(c(smoothContour, anchors$value))
        m1 = ifelse(m1 > 0, m1 / 1.1, m1 * 1.1)
        m2 = max(c(smoothContour, anchors$value))
        m2 = ifelse(m2 > 0, m2 * 1.1, m1 / 1.1)
        ylim = c(m1, m2)
        # ylim = c(min(0, min(anchors$value)), max(0, max(anchors$value)))
      }
      # x = seq(anchors$time[1],
      #         anchors$time[length(anchors$time)],
      #         length.out = length(smoothContour_downsampled))
      plot(x, y = smoothContour_downsampled, type = 'l', ylab = 'Amplitude',
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
  if (nrow(anchors) > 0 & NA_to_zero)
    smoothContour[is.na(smoothContour)] = 0
  if (thisIsPitch)
    smoothContour = semitonesToHz(smoothContour)
  invisible(smoothContour)
}


#' Draw contour
#'
#' Internal soundgen function
#'
#' The core part of getSmoothContour() that actually performs the interpolation
#' between anchors.
#' @inheritParams getSmoothContour
#' @param duration_ms contour duration, ms
#' @keywords internal
drawContour = function(len,
                       anchors,
                       interpol,
                       valueFloor,
                       valueCeiling,
                       duration_ms = 500,
                       loessSpan = NULL) {
  time = 1:len
  if (nrow(anchors) == 1) {
    # flat
    smoothContour = rep(anchors$value[1], len)
  } else if (nrow(anchors) == 2) {
    # linear
    smoothContour = seq(anchors$value[1], anchors$value[2], length.out = len)
  } else {
    # smooth contour
    if (interpol == 'approx') {
      if (len > nrow(anchors)) {
        smoothContour = approx(anchors$value, n = len, x = anchors$time)$y
      } else {
        smoothContour = anchors$value
      }
      # plot(smoothContour, type='l')
    } else if (interpol == 'spline') {
      smoothContour = spline(anchors$value, n = len, x = anchors$time)$y
      # plot(smoothContour, type='l')
    } else if (interpol == 'loess') {
      anchor_time_points = anchors$time - min(anchors$time)
      anchor_time_points = anchor_time_points / max(anchor_time_points) * len
      anchor_time_points[anchor_time_points == 0] = 1
      anchors_long = as.vector(rep(NA, len))
      anchors_long[anchor_time_points] = anchors$value # plot (anchors_long)

      # let's draw a smooth curve through the given anchors
      if (is.null(loessSpan)) {
        span = (1 / (1 + exp(duration_ms / 500)) + 0.5) /
          1.1 ^ (nrow(anchors) - 3)
      } else {
        span = loessSpan
      }
      # NB: need to compensate for variable number of points, namely decrease
      # smoothing as the number of points increases, hence the "1.1^..." term
      # duration_ms = 50:9000
      # span = 1 / (1 + exp(duration_ms / 500)) + 0.5
      # plot(duration_ms, span, type = 'l')
      l = suppressWarnings(loess(anchors_long ~ time, span = span))
      # plot (time, anchors_long)
      smoothContour = try(predict(l, time), silent = TRUE)
      # plot(time, smoothContour)

      # for long duration etc, larger span may be needed to avoid error in loess
      if (is.null(loessSpan)) {
        while(class(smoothContour)[1] == 'try-error') {
          span = span + 0.1
          l = suppressWarnings(loess(anchors_long ~ time, span = span))
          smoothContour = try(predict(l, time), silent = TRUE)
        }
        # plot(smoothContour, type = 'l')

        while(sum(smoothContour < valueFloor - 1e-6, na.rm = TRUE) > 0) {
          # in case we get values below valueFloor, less smoothing should be used
          # NB: -1e-6 avoids floating point problem, otherwise we get
          # weird cases of -120 (float) < -120 (integer)
          span = span / 1.1
          l = suppressWarnings(loess(anchors_long ~ time, span = span))
          smoothContour = try(predict(l, time), silent = TRUE)
        }
      }
    }
    smoothContour[smoothContour < valueFloor] = valueFloor
    smoothContour[smoothContour > valueCeiling] = valueCeiling
    return(smoothContour)
  }
}

#' Split contour
#'
#' Internal soundgen function
#'
#' Splits a smooth contour into several segments. A new segments is started if
#' the time step between two anchors is smaller than discontThres.
#' @param anchors a dataframe with two columns: time and value (time on any scale)
#' @inheritParams getSmoothContour
#' @return Returns a dataframe containing the index of anchor rows for start and
#'   end of each segment and whether we want a transition or a jump between
#'   segments.
#' @keywords internal
#' @examples
#' soundgen:::splitContour(data.frame(time = c(0, 370, 650, 655, 1050, 1400),
#'   value = c(360, 316, 345, 550, 610, 590)))
#' soundgen:::splitContour(data.frame(time = c(0, .2, .205, .8, .81, 1),
#'   value = c(360, 316, 345, 550, 610, 590)))
#' soundgen:::splitContour(data.frame(time = c(0, .4, .45, .6, .8, 1),
#'   value = c(360, 316, 345, 550, 610, 590)))
#' soundgen:::splitContour(data.frame(time = c(0, .4, .45, .6, .8, 1),
#'   value = c(360, 316, 345, 550, 610, 590)),
#'   discontThres = .1)
#' soundgen:::splitContour(data.frame(time = c(0, 1),
#'   value = c(360, 590)))
splitContour = function(anchors,
                        discontThres = .05,
                        jumpThres = .01) {
  discont = which(diff(anchors$time) / diff(range(anchors$time)) < discontThres)
  jumps = which(diff(anchors$time) / diff(range(anchors$time)) < jumpThres)
  if (length(discont) > 0) {
    sections = data.frame(start = 1,
                          end = rep(nrow(anchors), length(discont) + 1),
                          jump = FALSE)
    for (j in 1:length(discont)) {
      sections$end[j] = discont[j]
      sections$start[j + 1] = discont[j] + 1
      sections$jump[j] = discont[j] %in% jumps
    }
  } else {
    sections = data.frame(start = 1, end = nrow(anchors), jump = FALSE)
  }
  return(sections)
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
#' soundgen:::getDiscreteContour(len = 10, interpol = 'spline', plot = TRUE,
#'   ylab = 'Semitones', anchors = data.frame(time = c(0, .2, .6, 1),
#'   value = c(0, -3, 1, 0)))
getDiscreteContour = function(len,
                              anchors = data.frame(time = c(0, 1), value = c(1, 1)),
                              interpol = c('spline', 'loess')[2],
                              valueFloor = NULL,
                              valueCeiling = NULL,
                              ylim = NULL,
                              plot = FALSE,
                              ...) {
  contour = getSmoothContour(
    len = len,
    anchors = anchors,
    interpol = interpol,
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

#' Reformat anchors
#'
#' Internal soundgen function.
#'
#' Checks that the anchors are formatted in a valid way and expands them to a
#' standard dataframe with two columns: time and value. NB: works for all
#' anchors except "noise", whose anchors have to be scaled by sylLen and are
#' therefore processed directly in soundgen()
#' @param anchors a numeric vector of values or a list/dataframe with one column
#'   (value) or two columns (time and value)
#' @param normalizeTime if TRUE, normalizes anchors$time values to range from 0 to 1
#' @keywords internal
#' @examples
#' soundgen:::reformatAnchors(150)
#' soundgen:::reformatAnchors(c(150, 200, 220))
#' soundgen:::reformatAnchors(anchors = list(value=c(150, 200, 220)))
#' soundgen:::reformatAnchors(anchors = list(time = c(0, 10, 100),
#'                                           value = c(150, 200, 220)))
#' # returns NA
#' soundgen:::reformatAnchors('aha')
#' # returns NA with a warning
#' soundgen:::reformatAnchors(anchors = list(time = c(0, .1, 1),
#'                                           freq = c(150, 200, 220)))
#' \dontrun{
#' # throws a warning and rearranges in order of time stamps
#' soundgen:::reformatAnchors(anchors = list(time = c(0, .8, .7, 1),
#'                                           value = c(150, 200, 150, 220)))
#' }
reformatAnchors = function(anchors, normalizeTime = TRUE) {
  if (is.numeric(anchors)) {
    # for numeric vectors, assume these are equally spaced anchor values
    anchors_df = data.frame(
      time = seq(0, 1, length.out = max(2, length(anchors))),
      value = anchors
    )
  } else if (is.list(anchors)) {
    # for dataframes or lists, reformat if necessary
    if (class(anchors)[1] != 'dataframe') {
      anchors_df = as.data.frame(anchors)
    }
    if (ncol(anchors_df) == 1) {
      # if there is only one vector, again assume these are values
      anchors_df = data.frame(
        time = seq(0, 1, length.out = max(2, nrow(anchors_df))),
        value = anchors_df[, 1]
      )
    } else if (!identical(colnames(anchors_df), c('time', 'value'))) {
      warning(paste('An anchor should be either numeric or a dataframe',
                    'with two columns: time and value.'))
      return(NA)
    }
  } else {
    return(NA)
  }

  # make sure time values are in the right order
  if (any(diff(anchors_df$time) < 0)) {
    anchors_df = anchors_df[order(anchors_df$time), ]
    warning('Time stamps of anchors should increase monotonically; re-ordering...')
  }

  # make sure time ranges from 0 to 1
  if (normalizeTime) anchors_df$time = zeroOne(anchors_df$time)

  return(anchors_df)
}
