#' Kill DC
#'
#' Removes DC offset or similar disbalance in a waveform dynamically, by
#' subtracting a smoothed ~moving average. Simplified compared to a true moving
#' average, but very fast (a few ms per second of 44100 audio).
#' @inheritParams flatEnv
#' @param plot if TRUE, plots the original sound, smoothed moving average, and
#'   modified sound
#' @keywords internal
#' @examples
#' # remove static DC offset
#' a = rnorm(500) + .3
#' b = soundgen:::killDC(a, windowLength_points = 500, plot = TRUE)
#'
#' # remove trend
#' a = rnorm(500) + seq(0, 1, length.out = 500)
#' b = soundgen:::killDC(a, windowLength_points = 100, plot = TRUE)
#'
#' # can also be used as a high-pass filter
#' a = rnorm(500) + sin(1:500 / 50)
#' b = soundgen:::killDC(a, windowLength_points = 25, plot = TRUE)
killDC = function(sound,
                  windowLength = 200,
                  samplingRate = 16000,
                  windowLength_points = NULL,
                  plot = FALSE) {
  if (!is.numeric(windowLength_points)) {
    if (is.numeric(windowLength)) {
      if (is.numeric(samplingRate)) {
        windowLength_points = windowLength / 1000 * samplingRate
      } else {
        stop(paste('Please, specify either windowLength (ms) plus samplingRate (Hz)',
                   'or the length of smoothing window in points (windowLength_points)'))
      }
    }
  }

  env = getEnv(sound = sound,
               windowLength_points = windowLength_points,
               method = 'mean')
  soundNorm = sound - env

  if (plot) {
    op = par('mfrow')
    par(mfrow = c(1, 2))
    plot(sound, type = 'l', main = 'Original')
    points(env, type = 'l', lty = 1, col = 'blue')
    points(rep(0, length(sound)), type = 'l', lty = 2)
    plot(soundNorm, type = 'l', main = 'Env removed')
    points(rep(0, length(sound)), type = 'l', col = 'blue')
    par(mfrow = op)
  }
  return(soundNorm)
}


#' Get amplitude envelope
#'
#' Internal soundgen function
#'
#' Returns the smoothed amplitude envelope of a waveform on the original scale.
#' @inheritParams flatEnv
#' @param method 'peak' for peak amplitude per window, 'rms' for root mean
#'   square amplitude, 'mean' for mean (for DC offset removal)
#' @keywords internal
#' @examples
#' a = rnorm(500) * seq(1, 0, length.out = 500)
#' plot(soundgen:::getEnv(a, 20))
getEnv = function(sound,
                  windowLength_points,
                  method = c('rms', 'peak', 'mean')[1]) {
  len = length(sound)
  if (method == 'peak') sound_abs = abs(sound)  # avoid repeated calculations

  if (windowLength_points >= len / 2) {
    # short sound relative to window - just take beginning and end (2 points)
    s = c(1, len)
  } else {
    s = c(1,
          seq(from = floor(windowLength_points / 2),
              to = length(sound) - floor(windowLength_points / 2),
              by = windowLength_points),
          length(sound))
  }
  # s is a sequence of starting indices for windows over which we average
  envShort = rep(NA, length(s) - 1)
  for (i in 1:(length(s) - 1)) {
    seg = s[i] : s[i+1]
    if (method == 'peak') {
      # get moving peak amplitude
      envShort[i] = max(sound_abs[seg])
    } else if (method == 'rms') {
      # get moving RMS amplitude
      envShort[i] = sqrt(mean(sound[seg] ^ 2))
    } else if (method == 'mean') {
      envShort[i] = mean(sound[seg])
    }
  }

  # upsample and smooth
  env = getSmoothContour(
    anchors = data.frame(time = seq(0, 1, length.out = length(envShort)),
                         value = envShort),
    len = length(sound)
  )
  return(env)
}
