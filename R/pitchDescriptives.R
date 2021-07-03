#' Pitch descriptives
#'
#' Provides common descriptives of time series such as pitch contours, including
#' measures of average / range / variability / slope / inflections etc. Several
#' degrees of smoothing can be applied consecutively. The summaries are produced
#' on the original and log-transformed scales, so this is meant to be used on
#' frequency-related variables in Hz.
#' @param x input: numeric vector, a list of time stamps and values in rows, a
#'   dataframe with one row per file and time/pitch values stored as characters
#'   (as exported by \code{\link{pitch_app}}), or path to csv file containing
#'   the output of \code{\link{pitch_app}} or \code{\link{analyze}}
#' @param step distance between values in s (only needed if input is a vector)
#' @param smoothBW a vector of bandwidths (Hz) for consecutive smoothing of
#'   input using \code{\link{pitchSmoothPraat}}; NA = no smoothing
#' @param inflThres minimum difference (in semitones) between consecutive
#'   extrema to consider them inflections; to apply a different threshold at
#'   each smoothing level, provide \code{inflThres} as a vector of the same
#'   length as \code{smoothBW}; NA = no threshold
#' @param timeUnit specify whether the time stamps (if any) are in ms or s
#' @param ref reference value for transforming Hz to semitones, defaults to
#'   C0 (16.35 Hz)
#' @param extraSummaryFun additional summary function(s) that take a numeric
#'   vector with some NAs and return a single number, eg c('myFun1', 'myFun2')
#' @param plot if TRUE, plots the inflections for manual verification
#' @return Returns a dataframe with columns containing summaries of one or
#'   multiple inputs (one input per row). The descriptives are as follows:
#'   \describe{\item{duration}{total duration, s} \item{durDefined}{duration
#'   after omitting leading and trailing NAs} \item{propDefined}{proportion of
#'   input with non-NA value, eg proportion of voiced frames if the input is
#'   pitch} \item{start, start_oct, end, end_oct}{the first and last values on
#'   the original scale and in octaves above C0 (16.3516 Hz)} \item{mean,
#'   median, max, min}{average and extreme values on the original scale}
#'   \item{mean_oct, median_oct, min_oct, max_oct}{same in octaves above C0}
#'   \item{time_max, time_min}{the location of minimum and maximum relative to
#'   durDefined, 0 to 1} \item{range, range_sem, sd, sd_sem}{range and standard
#'   deviation on the original scale and in semitones} \item{CV}{coefficient of
#'   variation = sd/mean (provided for historical reasons)} \item{meanSlope,
#'   meanSlope_sem}{mean slope in Hz/s or semitones/s (NB: does not depend on
#'   duration or missing values)} \item{meanAbsSlope, meanAbsSlope_sem}{mean
#'   absolute slope (modulus, ie rising and falling sections no longer cancel
#'   out)} \item{maxAbsSlope, maxAbsSlope_sem}{the steepest slope}}
#' @export
#' @examples
#' x = c(NA, NA, 405, 441, 459, 459, 460, 462, 462, 458, 458, 445, 458, 451,
#' 444, 444, 430, 416, 409, 403, 403, 389, 375, NA, NA, NA, NA, NA, NA, NA, NA,
#' NA, 183, 677, 677, 846, 883, 886, 924, 938, 883, 946, 846, 911, 826, 826,
#' 788, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 307,
#' 307, 368, 377, 383, 383, 383, 380, 377, 377, 377, 374, 374, 375, 375, 375,
#' 375, 368, 371, 374, 375, 361, 375, 389, 375, 375, 375, 375, 375, 314, 169,
#' NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 238, 285, 361, 374, 375, 375,
#' 375, 375, 375, 389, 403, 389, 389, 375, 375, 389, 375, 348, 361, 375, 348,
#' 348, 361, 348, 342, 361, 361, 361, 365, 365, 361, 966, 966, 966, 959, 959,
#' 946, 1021, 1021, 1026, 1086, 1131, 1131, 1146, 1130, 1172, 1240, 1172, 1117,
#' 1103, 1026, 1026, 966, 919, 946, 882, 832, NA, NA, NA, NA, NA, NA, NA, NA,
#' NA, NA)
#' plot(x, type = 'b')
#' ci95 = function(x) diff(quantile(na.omit(x), probs = c(.025, .975)))
#' pd = pitchDescriptives(
#'   x, step = .025,
#'   smoothBW = c(NA, 10, 1),   # original + smoothed at 10 Hz and 1 Hz
#'   inflThres = c(NA, .2, .2), # different for each level of smoothing
#'   extraSummaryFun = 'ci95',  # user-defined, here 95% coverage interval
#'   plot = TRUE
#' )
#' pd
#'
#' \dontrun{
#' # a single file
#' data(sheep, package = 'seewave')
#' a = analyze(sheep)
#' pd1 = pitchDescriptives(a$detailed[, c('time', 'pitch')],
#'                         timeUnit = 'ms', inflThres = NA, plot = TRUE)
#' pd2 = pitchDescriptives(a$detailed[, c('time', 'pitch')],
#'                         timeUnit = 'ms', inflThres = c(0.1, .5), plot = TRUE)
#'
#' # multiple files returned by pitch_app()
#' pd = pitchDescriptives(
#'   '~/Downloads/pitch_manual_1708.csv',
#'   timeUnit = 'ms', smoothBW = c(NA, 2), inflThres = .25)
#'
#' # a single file, exported from Praat
#' par(mfrow = c(3, 1))
#' pd = pitchDescriptives(
#'   '~/Downloads/F-Hin-Om_jana.wav_F0contour.txt',
#'   timeUnit = 's', smoothBW = c(NA, 25, 2), inflThres = .25, plot = TRUE)
#' par(mfrow = c(1, 1))
#' }
pitchDescriptives = function(x,
                             step = NULL,
                             smoothBW = c(NA, 10, 1),
                             inflThres = .2,
                             timeUnit = c('s', 'ms')[1],
                             extraSummaryFun = c(),
                             ref = 16.35,
                             plot = FALSE) {
  orig = ''
  if (is.character(x)) {
    # full path to csv file
    orig = x
    if (file.exists(x)) {
      x = try(read.csv(x), silent = TRUE)
      if (class(x) == 'try-error')
        stop('Input not recognized')
    }
  }
  if (is.list(x) && !is.null(x$time) && !is.null(x$pitch)) {
    # dataframe with time/pitch
    if (is.numeric(x$time)) {
      data = suppressWarnings(list(list(file = basename(orig),
                                        time = x$time,
                                        pitch = as.numeric(x$pitch))))
    } else if (is.character(x$time)) {
      nr = nrow(x)
      data = vector('list', nr)
      for (i in 1:nr) {
        data[[i]] = try(list(
          file = x$file[i],
          time = suppressWarnings(as.numeric(unlist(strsplit(x$time[i], ',')))),
          pitch = suppressWarnings(as.numeric(unlist(strsplit(x$pitch[i], ','))))
        ), silent = TRUE)
        if (class(data[[i]]) == 'try-error')
          stop('Input not recognized')
      }
    }
  } else if (is.vector(x)) {
    if (length(x) < 2) return(NA)
    if (is.null(step))
      stop('If x is a vector, step must be provided')
    data = list(list(
      file = '',
      pitch = x,
      time = step * (1:length(x) - .5)
    ))
  } else {
    stop('Input not recognized')
  }

  # process one input at a time
  out = NULL
  len_data = length(data)
  time_start = proc.time()
  for (i in 1:len_data) {
    data_i = data[[i]]
    if (timeUnit == 'ms') data_i$time = data_i$time / 1000
    res = do.call(
      .pitchDescriptives,
      list(time = data_i$time,
           pitch = data_i$pitch,
           smoothBW = smoothBW,
           inflThres = inflThres,
           extraSummaryFun = extraSummaryFun,
           ref = ref,
           plot = plot,
           main = data_i$file)
    )
    if (!is.null(data_i$file)) res$file = data_i$file
    if (is.null(out)) {
      out = res
    } else {
      out = rbind(out, res)
    }
    reportTime(i = i, nIter = len_data, time_start = time_start)
    if (plot && i < len_data)
      invisible(readline(prompt="Press [enter] to continue"))
  }
  return(out)
}


#' Pitch descriptives per file
#'
#' Internal soundgen function.
#'
#' @param time a vector of time stamps in s
#' @param pitch a vector of pitch values in Hz
#' @inheritParams pitchDescriptives
#' @keywords internal
.pitchDescriptives = function(time,
                              pitch,
                              smoothBW,
                              inflThres,
                              extraSummaryFun = c(),
                              ref = 16.35,
                              plot = FALSE,
                              main = '') {
  if (plot) par(mfrow = c(length(smoothBW), 1))  # plot each smoothing separately
  len = length(time)
  if (len < 2) return(NA)
  if (length(pitch) != len) stop('time and pitch must have the same length')
  if (!any(!is.na(pitch))) return(NA)
  # plot(time, pitch, type = 'b')

  step = time[2] - time[1]
  samplingRate = 1 / step
  if (length(inflThres) == 1) {
    inflThres = rep(inflThres[1], length(smoothBW))
  } else if (length(inflThres) != length(smoothBW)) {
    stop('inflThres should be of length 1 or the same length as smoothBW')
  }

  # apply smoothing
  out = data.frame(file = NA)
  for (i in 1:length(smoothBW)) {
    if (is.finite(smoothBW[i])) {
      pitch_sm = pitchSmoothPraat(pitch, bandwidth = smoothBW[i],
                                  samplingRate = samplingRate)
    } else {
      pitch_sm = pitch
    }
    # plot(time, pitch_sm, type = 'b')
    out_i = timeSeriesSummary(pitch_sm,
                              step = step,
                              inflThres = inflThres[i],
                              extraSummaryFun = extraSummaryFun,
                              ref = ref,
                              plot = plot,
                              main = paste(main, 'smoothed at', smoothBW[i], 'Hz'))
    if (is.finite(smoothBW[i])) {
      if (i == 1) {
        colnames(out_i)[4:ncol(out_i)] = paste0(
          colnames(out_i)[4:ncol(out_i)], '_', smoothBW[i]
        )
      } else {
        # only save durDefined and propDefined once
        out_i = out_i[, 4:ncol(out_i)]
        colnames(out_i) = paste0(colnames(out_i), '_', smoothBW[i])
      }
    }
    out = cbind(out, out_i)
  }
  if (plot) par(mfrow = c(1, 1))
  return(out)
}


#' Time series summary
#'
#' Internal soundgen function
#'
#' A helper function called by .pitchDescriptives for each smoothing level.
#' @param x numeric vector
#' @param step time step in s
#' @inheritParams pitchDescriptives
#' @keywords internal
timeSeriesSummary = function(x,
                             step,
                             inflThres = NULL,
                             extraSummaryFun = c(),
                             ref = 16.35,
                             plot = FALSE,
                             main = '') {
  len = length(x)
  out = data.frame(duration = step * (len + 1))
  vars = c('durDefined', 'propDefined',
           'start', 'start_oct', 'end', 'end_oct',
           'mean', 'mean_oct', 'median', 'median_oct',
           'min', 'min_oct', 'time_min',
           'max', 'max_oct', 'time_max',
           'range', 'range_sem', 'sd', 'sd_sem', 'CV',
           'meanSlope', 'meanSlope_sem', 'meanAbsSlope', 'meanAbsSlope_sem',
           'maxAbsSlope', 'maxAbsSlope_sem')
  out[, vars] = NA

  # transform to semitones, drop NAs
  x_noNA = as.numeric(na.omit(x))
  x_sem = soundgen::HzToSemitones(x, ref = ref)
  x_sem_noNA = as.numeric(na.omit(x_sem))
  ran_not_NA = range(which(!is.na(x)))
  if (length(x_noNA) < 1) return(out)

  # user-defined function(x)
  lu = length(extraSummaryFun)
  if (lu > 0) {
    out[, extraSummaryFun] = NA
    for (f in 1:lu) {
      temp = try(do.call(extraSummaryFun[f], list(x)))
      if (class(temp) != 'try-error') out[extraSummaryFun[f]] = temp
    }
  }

  # basic descriptives
  out$durDefined = (diff(ran_not_NA) + 1) * step  # from first to last non-NA frame
  out$propDefined = sum(!is.na(x)) / len * 100
  out$start = x_noNA[1]
  out$end = tail(x_noNA, n = 1)
  out$mean = mean(x_noNA)
  out$median = median(x_noNA)

  # min, max
  idx_min = which.min(x)
  if (length(idx_min) > 0) {
    out$min = x[idx_min]
    out$time_min = (idx_min - ran_not_NA[1]) / diff(ran_not_NA)
  }
  idx_max = which.max(x)
  if (length(idx_max) > 0) {
    out$max = x[idx_max]
    out$time_max = (idx_max - ran_not_NA[1]) / diff(ran_not_NA)
  }

  # convert mean-related vars from Hz to octaves above C0
  vars_to_oct = c('start', 'end', 'mean', 'median', 'min', 'max')
  for (v in vars_to_oct) {
    out[, paste0(v, '_oct')] = soundgen::HzToSemitones(out[, v], ref = 16.3516) / 12
  }

  # range, sd
  out$range = out$max - out$min
  out$range_sem = diff(range(x_sem_noNA))
  out$sd = sd(x_noNA)
  out$sd_sem = sd(x_sem_noNA)
  out$CV = out$sd / out$mean

  # slope
  pd = as.numeric(na.omit(diff(x))) / step
  pd_abs = abs(pd)
  pd_sem = as.numeric(na.omit(diff(x_sem))) / step
  pd_sem_abs = abs(pd_sem)

  if (length(pd) > 0) {
    out$meanSlope = mean(pd)
    out$meanSlope_sem = mean(pd_sem)
    out$meanAbsSlope = mean(pd_abs)
    out$meanAbsSlope_sem = mean(pd_sem_abs)
    out$maxAbsSlope = max(pd_abs)
    out$maxAbsSlope_sem = max(pd_sem_abs)
    # note: sumvar incorrectly ignores NAs b/c it divides by dur instead of step
  }

  # inflections
  infl = findInflections(x_sem, thres = inflThres,
                         step = step, plot = plot, main = main)
  n_inflections = length(infl)
  out$inflex = n_inflections / out$durDefined
  return(out)
}


#' Find inflections
#'
#' Finds inflections in discrete time series such as pitch contours. When there
#' are no missing values and no thresholds, this can be accomplished with a fast
#' one-liner like \code{which(diff(diff(x) > 0) != 0) + 1}. Missing values are
#' interpolated by repeating the first and last non-missing values at the head
#' and tail, respectively, and by linear interpolation in the middle. Setting a
#' threshold means that small "wiggling" no longer counts. To use an analogy
#' with ocean waves, smoothing (low-pass filtering) removes the ripples and only
#' leaves the slow roll, while thresholding preserves only waves that are
#' sufficiently high, whatever their period.
#' @param x numeric vector with or without NAs
#' @param thres minimum vertical distance between two extrema for them to count
#'   as two independent inflections
#' @param step distance between values in s (only needed for plotting)
#' @param plot if TRUE, produces a simple plot
#' @param main plot title
#' @return Returns a vector of indices giving the location of inflections.
#' @export
#' @examples
#' x = sin(2 * pi * (1:100) / 15) * seq(1, 5, length.out = 100)
#' idx_na = c(1:4, 6, 7, 14, 25, 30:36, 39, 40, 42, 45:50,
#'            57, 59, 62, 66, 71:79, 98)
#' x[idx_na] = NA
#' soundgen:::findInflections(x, plot = TRUE)
#' soundgen:::findInflections(x, thres = 5, plot = TRUE)
#'
#' for (i in 1:10) {
#'   temp = soundgen:::getRandomWalk(len = runif(1, 10, 100), rw_range = 10,
#'                                   rw_smoothing = runif(1, 0, 1))
#'   soundgen:::findInflections(temp, thres = 1, plot = TRUE)
#'   invisible(readline(prompt="Press [enter] to continue"))
#' }
findInflections = function(x,
                           thres = NULL,
                           step = NULL,
                           plot = FALSE,
                           main = '') {
  # remove leading/trailing NAs
  orig = x  # for plotting
  r = rle(is.na(x))
  x = zoo::na.trim(x)
  if (r$values[1]) {
    shift = r$lengths[1]
  } else {
    shift = 0
  }

  len = length(x)
  if (len < 3) return(numeric(0))
  if (any(is.na(x))) {
    xInt = approx(x, n = len, na.rm = TRUE)$y  # soundgen:::intplNA(x)
  } else {
    xInt = x
  }
  extrema = which(diff(diff(xInt) > 0) != 0) + 1

  # threshold
  if (!is.null(thres) && is.finite(thres) && length(extrema) > 0) {
    de = abs(diff(c(xInt[1], xInt[extrema])))
    idx_keep = which(de > thres)
    extrema = extrema[idx_keep]

    # now get rid of "staircase effects" - successive extrema in the same direction
    de = c(xInt[1], xInt[extrema], xInt[len])
    extrema = extrema[which(diff(diff(de) > 0) != 0)]

    # check that the last extremum is >thres below or above the final point
    le = length(extrema)
    if (le > 0) {
      d_last = abs(tail(xInt, 1) - xInt[tail(extrema, 1)])
      if (d_last <= thres) extrema = extrema[-le]
    }
  }
  extrema = extrema + shift

  if (plot) {
    if (!is.null(step)) {
      time = ((1:length(orig)) - 0.5) * step
      xlab = 'Time, s'
      xaxt = 'n'
    } else {
      xlab = ''
      xaxt = 's'
    }
    plot(c(rep(NA, shift), xInt), type = 'b', col = 'gray70', main = main,
         pch = 16, cex = .5, ylab = '', xlab = xlab, xaxt = 'n')
    points(orig, pch = 16, type = 'b')
    le = length(extrema)
    if (le > 0) {
      points(extrema, orig[extrema], col = 'blue', pch = 18)
      for (i in 1:le)
        segments(x0 = extrema[i], y0 = -1e50, y1 = orig[extrema[i]],
                 lty = 1, lwd = .25, col = 'black')
    }
    if (!is.null(step)) {
      pt = pretty(time)
      axis(1, at = pt / step + .5, labels = pt)
    }
  }
  return(extrema)
}
