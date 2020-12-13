## UTILITIES FOR FINDING SYLLABLES AND VOCAL BURSTS ##

#' Find syllables
#'
#' Internal soundgen function.
#'
#' Called by \code{\link{segment}}.
#'
#' @param envelope downsampled amplitude envelope: dataframe with columns "time"
#'   and "value"
#' @param step time difference between two points in the envelope (ms)
#' @param threshold all continuous segments above this value are considered to
#'   be syllables
#' @inheritParams segment
#' @return Returns a dataframe with timing of syllables.
#' @keywords internal
findSyllables = function(ampl,
                         step,
                         windowLength,
                         threshold,
                         shortestSyl,
                         shortestPause) {
  # the smallest number of consecutive values above threshold; but at least 1
  nRequired = max(1, ceiling(shortestSyl / step))
  # the greatest number of sub-threshold values that we tolerate before we say a
  # new syllable begins
  toleratedGap = floor(shortestPause / step)

  aboveThres = ampl > threshold

  # find and save separately all supra-threshold segments
  start = numeric()
  end = numeric()
  i = 1
  while (i < (length(aboveThres) - nRequired + 1)) {
    # find beginning
    while (i < (length(aboveThres) - nRequired + 1)) {
      if (sum(aboveThres[i:(i + nRequired - 1)]) == nRequired) {
        start = c(start, i)
        break
      }
      i = i + 1
    }
    # find end
    if (length(end) < length(start)) {
      while (i < (length(aboveThres) - toleratedGap + 1)) {
        if (sum(aboveThres[i:(i + toleratedGap)]) == 0) {
          end = c(end, i - 1)
          i = i - 1
          break
        }
        i = i + 1
      }
      if (length(end) < length(start)) {
        # if the end is not found, take the last value above threshold
        end = c(end, tail(which(aboveThres), 1))
        break
      }
    }
    i = i + 1
  }

  if (length(start) > 0) {
    syllables = data.frame(
      syllable = 1:length(start),
      start_idx = start,
      end_idx = end,
      start = (start - 1) * step + windowLength / 2,
      end = (end - 1) * step + windowLength / 2
    )
    syllables$sylLen = syllables$end - syllables$start
    syllables$pauseLen[1] = syllables$start[1]
    if (nrow(syllables) > 1) {
      for (i in 2:nrow(syllables)) {
        syllables$pauseLen[i] = syllables$start[i] - syllables$end[i - 1]
      }
    }
  } else {
    syllables = data.frame(syllable = NA,
                           start_idx = NA, end_idx = NA,
                           start = NA, end = NA, dur = NA,
                           sylLen = NA, pauseLen = NA)
  }

  return(syllables)
}



#' Find bursts
#'
#' Internal soundgen function.
#'
#' Called by \code{\link{segment}}.
#'
#' @inheritParams findSyllables
#' @inheritParams segment
#' @return Returns a dataframe with timing of bursts
#' @keywords internal
findBursts = function(ampl,
                      step,
                      windowLength,
                      interburst,
                      burstThres,
                      peakToTrough,
                      troughLocation = 'either',
                      scale = c('dB', 'linear')[2]) {
  if (!is.numeric(interburst)) {
    stop(paste0('interburst is weird:', interburst))
  }
  if (interburst < 0) {
    stop('interburst is negative')
  }

  # we're basically going to look for local maxima within plus-minus n
  n = floor(interburst / step) / 2
  len = length(ampl)
  aboveThres = which(ampl > burstThres)
  burst_idx = numeric(0)

  for (i in aboveThres) {
    # for each datapoint, compare it with the local minima to the left/right
    # over plus-minus interburst_min ms
    if (i > n) {
      local_min_left = min(ampl[(i - n):i])
    } else {
      local_min_left = 0
    }
    # close to the beginning of the file, local_min_left = 0
    if (i < (len - n - 1)) {
      # lowest ampl over interburst_min ms on the right
      local_min_right = min(ampl[i:(i + n)])
    } else {
      # just in case we want to evaluate both sides of a peak
      local_min_right = 0
    }

    # define the window for analysis (differs from plus-minus interburst_min,
    # because we have to consider the beginning and end of file)
    if (i > n) {
      limit_left = i - n
    } else {
      limit_left = 1
    }
    if (i < (len - n - 1)) {
      limit_right = i + n
    } else {
      limit_right = len
    }

    # DEFINITION OF A BURST FOLLOWS!!!
    # (1) it is a local maximum over plus-minus interburst_min
    cond1 = ampl[i] == max(ampl[limit_left:limit_right])
    # (2) it is above burstThres - already guaranteed
    # (3) it exceeds the local minimum on the LEFT / RIGHT by a factor of peakToTrough
    if (troughLocation == 'none') {
      cond3 = TRUE
    } else {
      if (scale == 'dB') {
        cond3_left = ampl[i] - local_min_left > peakToTrough
        cond3_right = ampl[i] - local_min_right > peakToTrough
      } else if (scale == 'linear') {
        cond3_left = ampl[i] / local_min_left > peakToTrough
        cond3_right = ampl[i] / local_min_right > peakToTrough
      }
      if (troughLocation == 'left') {
        cond3 = cond3_left
      } else if (troughLocation == 'right') {
        cond3 = cond3_right
      } else if (troughLocation == 'both') {
        cond3 = cond3_left & cond3_right
      } else if (troughLocation == 'either') {
        cond3 = cond3_left | cond3_right
      }
    }
    if (cond1 & cond3) burst_idx = c(burst_idx, i)
  }

  if (length(burst_idx) > 0) {
    bursts = data.frame(
      time = (burst_idx - 1) * step + windowLength / 2,
      ampl = ampl[burst_idx],
      interburst = NA
    )
    if (nrow(bursts) > 1) bursts$interburst[2:nrow(bursts)] = diff(bursts$time)
  } else {
    bursts = data.frame(time = NA, ampl = NA, interburst = NA)
  }

  return (bursts)
}
