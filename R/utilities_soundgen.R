### UTILITIES FOR SOUND GENERATION ###

#' Report time
#'
#' Internal soundgen function.
#'
#' Based on the current iteration, total number of iterations, and time when the
#' loop started running, prints estimated time left or a summary upon
#' completion.
#' @param i current iteration
#' @param nIter total number of iterations
#' @param time_start time when the loop started running
#' @param jobs vector of length \code{nIter} specifying the relative difficulty
#'   of each iteration. If not NULL, estimated time left takes into account
#'   whether the jobs ahead will take more or less time than the jobs already
#'   completed
#' @keywords internal
#' @examples
#' time_start = proc.time()
#' for (i in 1:5) {
#'   Sys.sleep(i ^ 2 / 100)
#'   soundgen:::reportTime(i = i, nIter = 5, time_start = time_start, jobs = (1:5) ^ 2 / 10)
#' }
reportTime = function(i, nIter, time_start, jobs = NULL) {
  time_diff = as.numeric((proc.time() - time_start)[3])
  if (i == nIter) {
    time_total = convert_sec_to_hms(time_diff)
    print(paste0('Completed ', i, ' iterations in ', time_total, '.'))
  } else {
    if (is.null(jobs)) {
      # simply count iterations
      time_left = time_diff / i * (nIter - i)
    } else {
      # take into account the expected time for each iteration
      speed = time_diff / sum(jobs[1:i])
      time_left = speed * sum(jobs[min((i + 1), nIter):nIter])
    }
    time_left_hms = convert_sec_to_hms(time_left)
    print(paste0('Done ', i, ' / ', nIter, '; Estimated time left: ', time_left_hms))
  }
}


#' Print time
#'
#' Internal soundgen function.
#'
#' Converts time in seconds to time in hh:mm:ss for pretty printing.
#' @param time_s time (s)
#' @return Returns a character string like "1 h 20 min 3 s"
#' @keywords internal
#' @examples
#' time_start = proc.time()
#' Sys.sleep(1)
#' time_diff = as.numeric((proc.time() - time_start)[3])
#' soundgen:::convert_sec_to_hms(time_diff)
convert_sec_to_hms = function(time_s) {
  hours = time_s %/% 3600
  minutes = time_s %/% 60 - hours * 60
  seconds = round(time_s %% 60, 0)

  output = ''
  if (hours > 0) output = paste0(output, hours, ' h ')
  if (minutes > 0) output = paste0(output, minutes, ' min ')
  output = paste0(output, seconds, ' s')

  # remove the last space, if any
  if (substr(output, nchar(output), nchar(output)) == ' ') {
    output = substr(output, 1, nchar(output)-1)
  }
  return(output)
}


#' Play audio
#'
#' Plays an audio file or a numeric vector. This is a simple wrapper for the
#' functionality provided by \code{\link[tuneR]{play}}
#' @param sound a vector of numbers on any scale or a path to a .wav file
#' @param samplingRate sampling rate (only needed if sound is a vector)
#' @export
#' @examples
#' # playme('~/myfile.wav')
#' f0_Hz = 440
#' sound = sin(2 * pi * f0_Hz * (1:16000) / 16000)
#' # playme(sound, 16000)
#' # in case of errors, look into tuneR::play()
playme = function(sound, samplingRate = 16000) {
  # input: a vector of numbers on any scale or a path to a .wav file
  if (class(sound) == 'character') {
    soundWave = tuneR::readWave(sound)
  } else if (class(sound) == 'numeric' | class(sound) == 'integer') {
    soundWave = tuneR::Wave(
      left = sound,
      samp.rate = samplingRate,
      bit = 16,
      pcm = TRUE
    )
    soundWave = tuneR::normalize(soundWave, unit = '32') # / 2
  }

  os = Sys.info()[['sysname']]
  if (os == 'Linux' | os == 'linux') {
    p = tuneR::play(soundWave, 'play')
  } else {  # windows | darwin
    p = tuneR::play(soundWave)
  }
  if (p > 0) {  # error in sh
    warning("Error in tuneR::play. Try setting the default audio player. See http://music.informatics.indiana.edu/courses/I546/tuneR_play.pdf")
  }
  # can't get rid of printed output! sink(), capture.output, invisible don't work!!!
}


#' Find zero crossing
#'
#' Internal soundgen function.
#'
#' \code{findZeroCrossing} looks for the last negative point before a zero
#' crossing as close as possible to the specified location. Since this is
#' primarily intended for joining waveforms without a click, this function only
#' looks at upward segments of a waveform (see example).
#'
#' @param ampl a vector of amplitudes oscillating around zero, such as a sound
#'   waveform
#' @param location the index indicating the desired location of a zero crossing
#' @return Returns the index of the last negative value before zero crossing
#'   closest to specified location.
#' @keywords internal
#' @examples
#' ampl = sin(1:100/2)
#' plot(ampl, type = 'b')
#' lines(1:100, rep(0,100), lty = 2)
#' zc = vector()
#' for (i in 1:length(ampl)){
#'   zc[i] = soundgen:::findZeroCrossing (ampl, i)
#'   # find zc closest to each of 100 points
#' }
#' for (z in unique(zc)){
#'   points(z, ampl[z], col = 'red', pch = 17)
#'   # only on upward segments
#' }
#' zc # see which zc is closest to each point
findZeroCrossing = function(ampl, location) {
  len = length(ampl)
  if (len < 1 | location < 1 | location > len)
    return (NA)
  if (len == 1 & location == 1)
    return(location)
  zc_left = zc_right = NA

  # left of location
  if (location > 1) {
    i = location
    while (i > 1) {
      if (ampl[i] > 0 && ampl[i - 1] < 0) {
        zc_left = i - 1
        break
      }
      i = i - 1
    }
  }

  # right of location
  if (location < len)
    i = location
  while (i < (len - 1)) {
    if (ampl[i + 1] > 0 && ampl[i] < 0) {
      zc_right = i
      break
    }
    i = i + 1
  }

  if (is.na(zc_left) & is.na(zc_right)) return (NA)
  zc_nearest = which.min(c(abs(zc_left - location), abs(zc_right - location)))
  if (zc_nearest == 1) {
    return (zc_left)
  } else if (zc_nearest == 2) {
    return (zc_right)
  } else {
    return (NA) # zc not found
  }
}


#' Join two waveforms by cross-fading
#'
#' \code{crossFade} joins two input vectors (waveforms) by cross-fading. It
#' truncates both input vectors, so that \code{ampl1} ends with a zero crossing
#' and \code{ampl2} starts with a zero crossing, both on an upward portion of
#' the soundwave. Then it cross-fades both vectors linearly with an overlap of
#' crossLen or crossLenPoints. If the input vectors are too short for the
#' specified length of cross-faded region, the two vectors are concatenated at
#' zero crossings instead of cross-fading. Soundgen uses \code{crossFade} for
#' gluing together epochs with different regimes of pitch effects (see the
#' vignette on sound generation), but it can also be useful for joining two
#' separately generated sounds without audible artifacts.
#' @param ampl1,ampl2 two numeric vectors (waveforms) to be joined
#' @param samplingRate the sampling rate of input vectors, Hz
#' @param crossLen the length of overlap, in ms (doesn't need to be specified
#'   if crossLenPoints is not NULL)
#' @param crossLenPoints (optional) the length of overlap, in points (defaults to
#'   NULL)
#' @export
#' @return Returns a numeric vector.
#' @examples
#' sound1 = sin(1:100 / 9)
#' sound2 = sin(7:107 / 3)
#' plot(c(sound1, sound2), type = 'b')
#' # an ugly discontinuity at 100 that will make an audible click
#'
#' sound = crossFade(sound1, sound2, crossLenPoints = 5)
#' plot(sound, type = 'b') # a nice, smooth transition
#' length(sound) # but note that cross-fading costs us ~60 points
#' #  because of trimming to zero crossings
crossFade = function (ampl1,
                      ampl2,
                      samplingRate,
                      crossLen = 15,
                      crossLenPoints = NULL) {
  # cut to the nearest zero crossings
  zc1 = findZeroCrossing(ampl1, location = length(ampl1))
  if (!is.na(zc1)) {
    # up to the last point before the last zero-crossing in sound 1 on the
    # upward curve + one extra zero (to have a nice, smooth transition line:
    # last negative in s1 - zero - first positive in s2)
    ampl1 = c(ampl1[1:zc1], 0)
  }
  zc2 = findZeroCrossing(ampl2, location = 1)
  if (!is.na(zc2)) {
    ampl2 = ampl2[(zc2 + 1):length(ampl2)]
    # from the first positive point on the upward curve. Note the +1 - next
    # point after the first zero crossing in s2
  }

  # check whether there is enough data to cross-fade. Note that ampl1 or ampl2
  # may even become shorter than crossLenPoints after we shortened them to the
  # nearest zero crossing
  if (is.null(crossLenPoints)) {
    crossLenPoints = min(floor(crossLen * samplingRate / 1000),
                        length(ampl1) - 1,
                        length(ampl2) - 1)
  } else {
    crossLenPoints = min(crossLenPoints, length(ampl1) - 1, length(ampl2) - 1)
  }

  # concatenate or cross-fade
  if (crossLenPoints < 2) {
    # for segments that are too short,
    # just concatenate from zero crossing to zero crossing
    ampl = c(ampl1, ampl2)
  } else {
    # for segments that are long enough, cross-fade properly
    multipl = seq(0, 1, length.out = crossLenPoints)
    idx1 = length(ampl1) - crossLenPoints
    cross = rev(multipl) * ampl1[(idx1 + 1):length(ampl1)] +
      multipl * ampl2[1:crossLenPoints]
    ampl = c(ampl1[1:idx1],
             cross,
             ampl2[(crossLenPoints + 1):length(ampl2)])
  }
  return(ampl)
}


#' Upsample pitch contour
#'
#' Internal soundgen function.
#'
#' Upsamples a pitch contour to samplingRate through linear interpolation
#' between successive glottal cycles.
#' @param pitch_per_gc a vector of fundamental frequencies per glottal cycle
#' @param samplingRate target sampling rate after upsampling, in Hz
#' @return Returns a list of two vectors: pitch_upsampled (the upsampled version
#'   of the input) and gc_upsampled (new indices of glottal cycles on an
#'   upsampled scale)
#' @keywords internal
#' @examples
#' soundgen:::upsample(pitch_per_gc = c(100, 150, 130), samplingRate = 16000)
upsample = function(pitch_per_gc, samplingRate = 16000) {
  l = length(pitch_per_gc)
  gccrossLenPoints = round(samplingRate / pitch_per_gc)
  c = cumsum(gccrossLenPoints)
  gc_upsampled = c(1, c)

  if (l == 1) {
    pitch_upsampled = rep(pitch_per_gc, gccrossLenPoints)
  } else if (l == 2) {
    pitch_upsampled = seq(pitch_per_gc[1], pitch_per_gc[2], length.out = sum(gccrossLenPoints))
  } else {
    # find time stamps (in gc) corresponding to centers of each pitch value
    t = rep(1, l)
    t[1] = 1  # start at 1
    t[l] = sum(gccrossLenPoints)  # end at total number of gc
    for (i in 2:(l - 1)) {
      t[i] = c[i - 1] + round(gccrossLenPoints[i] / 2)
    }
    pitch_upsampled = spline(x = t,
                             y = pitch_per_gc,
                             n = tail(c, 1))$y
  }
  # plot(pitch_upsampled, type = 'l')
  return (list(pitch = pitch_upsampled, gc = gc_upsampled))
}


#' Fade-in and fade-out
#'
#' Internal soundgen function.
#'
#' Applies linear fade-in and fade-out of length 'length_fade' points to one or
#' both ends of input vector.
#' @param ampl numeric vector such as a waveform
#' @param do_fadeIn,do_fadeOut (logical) perform linear fade-in / fade-out?
#' @param length_fade the length of affected region, in points (expects an
#'   integer > 1, otherwise just returns the original vector with no
#'   modifications)
#' @return Returns a numeric vector of the same length as input
#' @keywords internal
#' @examples
#' ampl = sin(1:1000)
#' plot(soundgen:::fadeInOut(ampl, length_fade = 100), type = 'l')
#' plot(soundgen:::fadeInOut(ampl, length_fade = 300,
#'   do_fadeOut = FALSE), type = 'l')
#' # if the vector is shorter than twice the specified length_fade,
#' # fade-in/out regions overlap
#' plot(soundgen:::fadeInOut(ampl, length_fade = 700), type = 'l')
fadeInOut = function(ampl,
                     do_fadeIn = TRUE,
                     do_fadeOut = TRUE,
                     length_fade = 1000) {
  if ((!do_fadeIn & !do_fadeOut) | length_fade < 2) return(ampl)

  length_fade = min(length_fade, length(ampl))
  fadeIn = seq(0, 1, length.out = length_fade)
  if (do_fadeIn) {
    ampl[1:length_fade] = ampl[1:length_fade] * fadeIn
  }

  if (do_fadeOut) {
    fadeOut = rev(fadeIn)
    ampl[(length(ampl) - length_fade + 1):length(ampl)] =
      ampl[(length(ampl) - length_fade + 1):length(ampl)] * fadeOut
  }

  return (ampl)
}


#' Divide f0 contour into glottal cycles
#'
#' Internal soundgen function.
#'
#' Returns a vector of indices giving the borders between "glottal cycles",
#' assuming that we know the true f0 at each time point (as we do in synthesized
#' sounds) and that maximum amplitude gives us the center of a glottal cycle.
#' The first index is always 1.
#' @param pitch a vector of fundamental frequency values
#' @param samplingRate sampling rate at which f0 values are provided
#' @keywords internal
#' @examples
#' # 100 ms of audio with f0 steadily increasing from 150 to 200 Hz
#' soundgen:::getGlottalCycles (seq(150, 200, length.out = 350),
#'   samplingRate = 3500)
getGlottalCycles = function (pitch, samplingRate = 44100) {
  glottalCycles = numeric()
  i = 1 # the first border is the first time point
  while (i < length(pitch)) {
    glottalCycles = c(glottalCycles, i)
    # take steps proportionate to the current F0
    i = i + max(2, floor(samplingRate / pitch[i]))
  }
  return (glottalCycles)
}


#' Syllable structure of a bout
#'
#' Internal soundgen function.
#'
#' Stochastic generation of syllable structure of a bout. Calls
#' \code{\link{rnorm_bounded}} to vary the duration of each new syllable and of
#' pauses between syllables. Total bout duration will also vary, unless
#' temperature is zero.
#' @param nSyl the desired number of syllables
#' @param sylLen the desired mean syllable duration, in ms
#' @param pauseLen the desired mean pause between syllables, in ms
#' @param sylDur_min,sylDur_max the lower and upper bounds on possible syllable
#'   duration, in ms
#' @param pauseDur_min,pauseDur_max the lower and upper bounds on possible pause
#'   duration, in ms
#' @param temperature a non-negative float regulating the stochasticity of
#'   syllable segmentation; 0 = no stochasticity; 1 = sd of proposals is equal
#'   to sylLen (very strong stochasticity)
#' @param plot produce a plot of syllable structure?
#' @return Returns a matrix with a list of start-end points for syllables
#' @keywords internal
#' @examples
#' soundgen:::divideIntoSyllables (nSyl = 5, sylLen = 180,
#'   pauseLen = 55, temperature = 0.2, plot = TRUE)
#' soundgen:::divideIntoSyllables (nSyl = 5, sylLen = 180,
#'   pauseLen = 55, temperature = 0, plot = TRUE)
divideIntoSyllables = function (nSyl,
                                sylLen,
                                pauseLen,
                                sylDur_min = 20,
                                sylDur_max = 10000,
                                pauseDur_min = 20,
                                pauseDur_max = 1000,
                                temperature = 0.025,
                                plot = FALSE) {
  out = matrix(ncol = 2, nrow = 0)
  colnames(out) = c('start', 'end')
  if (nSyl == 1) {
    out = rbind(out, c(0, sylLen))
  } else {
    # generate random lengths while respecting the constraints
    c = 0
    while (nrow(out) < nSyl) {
      duration_ms_loop = rnorm_bounded(
        n = 1,
        mean = sylLen,
        low = sylDur_min,
        high = sylDur_max,
        sd = sylLen * temperature
      )
      pause_ms_loop = rnorm_bounded(
        n = 1,
        mean = pauseLen,
        low = pauseDur_min,
        high = pauseDur_max,
        sd = pauseLen * temperature
      )
      start = 1 + c # start time of syllable, in ms
      end = start + duration_ms_loop # end time of syllable, in ms
      out = rbind(out, c(start, end))
      c = end + pause_ms_loop
    }
  }

  if (plot) {
    # for the UI
    t = 1:max(out)
    plot(t, rep(1, length(t)), type = 'n', xlab = 'Time, ms', ylab = '',
         bty = 'n', yaxt = 'n', ylim = c(0.8, 1.2))
    for (i in 1:nrow(out)) {
      rect(xleft = out[i, 1], xright = out[i, 2], ybottom = .9, ytop = 1.1,
           col = 'blue')
      text(x = mean(c(out[i, 2], out[i, 1])), y = 1,
           col = 'yellow', cex = 5, labels = i)
    }
  }
  return(out)
}

#' sampleModif
#'
#' Internal soundgen function
#'
#' Same as \code{\link[base]{sample}}, but without defaulting to x = 1:x if
#' length(x) = 1. See
#' https://stackoverflow.com/questions/7547758/using-sample-with-sample-space-size-1
#' @param x vector
#' @param ... other arguments passed to \code{sample}
#' @keywords internal
#' @examples
#' soundgen:::sampleModif(x = 3, n = 1)
#' # never returns 1 or 2: cf. sample(x = 3, n = 1)
sampleModif = function(x, ...) x[sample.int(length(x), ...)]

#' Randomly modify anchors
#'
#' Internal soundgen function.
#'
#' A helper function for introducing random variation into any anchors (for
#' pitch / breathing / amplitude / ...). At higher temperatures can also add or
#' delete an anchor. NB: make sure the lower and upper bounds are reasonable
#' given the scale of df$value!
#' @param df dataframe of anchors, for ex. \code{data.frame(time = c(0, .1, .8,
#'   1), value = c(100, 230, 180, 90))}
#' @param temperature,temp_coef regulate the amount of stochasticity
#'   ("wiggling"). Since \code{temperature} is used in several functions,
#'   \code{temp_coef} gives more flexibility by controlling how much temperature
#'   affects this particular aspect, namely random variation in anchors. These
#'   two are multiplied, so \code{temp_coef} of 0.5 halves the effect of
#'   temperature.
#' @param low,high bounds on possible variation. Both \code{low} and \code{high}
#'   should be vectors of length 2: the first element specifies the boundary for
#'   \code{df$time} and the second for \code{df$value}. Ex.: low = c(0,1) - low
#'   bound on "time"=0, low bound on "value"=1
#' @param wiggleAllRows should the first and last time anchors be wiggled? (TRUE
#'   for breathing, FALSE for other anchors)
#' @return Modified original dataframe.
#' @keywords internal
#' @examples
#' soundgen:::wiggleAnchors(df = data.frame(
#'   time = c(0, .1, .8, 1), value = c(100, 230, 180, 90)),
#'   temperature = .2, temp_coef = .1, low = c(0, 50), high = c(1, 1000),
#'   wiggleAllRows = FALSE) # pitch
#' soundgen:::wiggleAnchors(df = data.frame(time = 0, value = 240),
#'   temperature = .2, temp_coef = .1, low = c(0, 50), high = c(1, 1000),
#'   wiggleAllRows = FALSE) # pitch, sinle anchor
#' soundgen:::wiggleAnchors(df = data.frame(
#'   time = c(-100, 100, 600, 900), value = c(-120, -80, 0, -120)),
#'   temperature = .4, temp_coef = .5, low = c(-Inf, -120), high = c(+Inf, 30),
#'   wiggleAllRows = TRUE) # noise

#' # formants
#' formants = list(f1 = list(time = 0, freq = 860, amp = 30, width = 120),
#'                 f2 = list(time = c(0,1), freq = 1280,
#'                 amp = c(10,40), width = 120))
#' for (f in 1:length(formants)) {
#'   formants[[f]] = soundgen:::wiggleAnchors(
#'     df = formants[[f]],
#'     temperature = .4, temp_coef = .5,
#'     low = c(0, 50, 0, 1),
#'     high = c(1, 8000, 120, 2000),
#'     wiggleAllRows = FALSE
#'   )
#' }
#' print(formants)
wiggleAnchors = function(df,
                         temperature,
                         temp_coef,
                         low,
                         high,
                         wiggleAllRows = FALSE) {
  if (any(is.na(df))) return(NA)
  if (class(df) != 'data.frame') df = as.data.frame(df)

  if (ncol(df) != length(low) |
      ncol(df) != length(high) |
      length(low) != length(high)) {
    warning('Vectors "low" and "high" should be the same length as ncol(df)')
  }

  # should we add a new anchor or remove one?
  action = sample(c('nothing', 'remove', 'add'),
                  size = 1,
                  prob = c(1 - temperature, temperature / 2, temperature / 2))
  if (action == 'add') {  # add an anchor
    if (nrow(df) == 1) {
      # the first anchor is the original, the second random
      idx = 2:ncol(df)
      newAnchor = try(rnorm_bounded(
        n = ncol(df) - 1,
        mean = as.numeric(df[1, idx]),
        sd = as.numeric(df[1, idx] * temperature * temp_coef),
        low = low[idx],
        high = high[idx]))
      if (class(newAnchor) == 'try-error') {
        stop(paste('Failed to add an anchor to df:', paste(df, collapse = ', ')))
      } else {
        df = rbind(df, c(1, newAnchor))
        df[1, 1] = 0  # make time c(0, 1)
      }
    } else {
      # insert between any two existing anchors
      a1 = sample(1:nrow(df), size = 1)
      direction = sample(c(-1, 1), size = 1)
      a2 = ifelse(a1 + direction < 1 || a1 + direction > nrow(df),
                  a1 - direction,
                  a1 + direction)
      i1 = min(a1, a2)
      i2 = max(a1, a2)  # insert between rows i1 and i2
      newAnchor = colMeans(df[i1:i2, ])
      df = rbind(df[1:i1, ],
                 newAnchor,
                 df[i2:nrow(df), ])
    }
  } else if (action == 'remove') {
    if (wiggleAllRows) {
      # we can remove any anchor
      idx = sample(1:nrow(df), 1)
      df = df[-idx, ]
    } else {
      # we don't touch the first and last anchors
      if (nrow(df) > 2) {
        # NB: sample() may return 1 if nrow(df) = 2, hence sampleModif()
        idx = sampleModif(x = (2:(nrow(df) - 1)), size = 1)
        df = df[-idx, ]
      }
    }
  }
  rownames(df) = 1:nrow(df)  # in case we added / removed an anchor

  # wiggle anchors
  if (wiggleAllRows) {
    orig = NULL
  } else {
    # save the original time values and put them back in later (usually 0 and 1)
    orig = c(df[1, 1], df[nrow(df), 1])
  }
  if (nrow(df) == 1) {
    ranges = as.numeric(df)
  } else {
    ranges = as.numeric(apply(df, 2, function(x) abs(diff(range(x)))))
    # if no variation in values, defaults to value
    z = which(ranges == 0)
    ranges[z] = abs(as.numeric(df[1, z]))
  }
  for (i in 1:ncol(df)) {
    w = try(rnorm_bounded(
      n = nrow(df),
      mean = as.numeric(df[, i]),
      sd = as.numeric(ranges[i] * temperature * temp_coef),
      low = low[i],
      high = high[i],
      roundToInteger = FALSE
    ))
    if (class(w) == 'try-error') {
      warning(paste('Failed to wiggle column', i, 'of df:',
                 paste(df, collapse = ', ')))
    } else {
      df[, i] = w
    }
  }
  if (is.numeric(orig)) {
    df[c(1, nrow(df)), 1] = orig
  }

  return(df)
}


#' Get amplitude envelope
#'
#' Internal soundgen function
#'
#' Returns the smoothed amplitude envelope of a waveform on the original scale.
#' @inheritParams flatEnv
#' @param method 'peak' for peak amplitude per window, 'rms' for root mean
#'   square amplitude, 'mean' for mean (for DC offset removal)
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




#' Flat envelope
#'
#' Flattens the amplitude envelope of a waveform. This is achieved by dividing
#' the waveform by some function of its smoothed rolling amplitude (peak or root
#' mean square).
#' @param sound input vector oscillating about zero
#' @param windowLength the length of smoothing window, ms
#' @param samplingRate the sampling rate, Hz. Only needed if the length of
#'   smoothing window is specified in ms rather than points
#' @param method 'peak' for peak amplitude per window, 'rms' for root mean
#'   square amplitude
#' @param windowLength_points the length of smoothing window, points. If
#'   specified, overrides both \code{windowLength} and \code{samplingRate}
#' @param killEnv if TRUE, dynamically removes DC offset or similar deviations of
#'   average waveform from zero
#' @param throwaway parts of sound quieter than \code{throwaway} dB will not be
#'   amplified
#' @param plot if TRUE, plots the original sound, smoothed envelope, and
#'   flattened sound
#' @export
#' @examples
#' a = rnorm(500) * seq(1, 0, length.out = 500)
#' b = flatEnv(a, plot = TRUE, killEnv = TRUE, method = 'peak',
#'             windowLength_points = 5)         # too short
#' c = flatEnv(a, plot = TRUE, killEnv = TRUE,
#'             windowLength_points = 250)       # too long
#' d = flatEnv(a, plot = TRUE, killEnv = TRUE,
#'             windowLength_points = 50)        # about right
flatEnv = function(sound,
                   windowLength = 200,
                   samplingRate = 16000,
                   method = c('rms', 'peak')[1],
                   windowLength_points = NULL,
                   killEnv = FALSE,
                   throwaway = -80,
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

  m = max(abs(sound))       # original scale (eg -1 to +1 gives m = 1)
  soundNorm = sound / m    # normalize
  throwaway_lin = 2 ^ (throwaway / 10)  # from dB to linear
  # get smoothed amplitude envelope
  env = getEnv(sound = soundNorm,
               windowLength_points = windowLength_points,
               method = method)
  # don't amplify very quiet sections
  env[env < throwaway_lin] = 1
  # flatten amplitude envelope
  soundFlat = soundNorm / env
  # re-normalize to original scale
  soundFlat = soundFlat / max(abs(soundFlat)) * m
  # remove DC offset
  if (killEnv) {
    sound_norm = killEnv(sound = soundFlat,
                         windowLength_points = windowLength_points,
                         plot = FALSE)
  }

  if (plot) {
    op = par('mfrow')
    par(mfrow = c(1, 2))
    plot(sound, type = 'l', main = 'Original')
    points(env * m, type = 'l', lty = 1, col = 'blue')
    plot(soundFlat, type = 'l', main = 'Flattened')
    par(mfrow = op)
  }
  return(soundFlat)
}


#' Kill envelope
#'
#' Removes DC offset or similar disbalance in a waveform dynamically, by
#' subtracting a smoothed ~moving average. Simplified compared to a true moving
#' average, but very fast (a few ms per second of 44100 audio).
#' @inheritParams flatEnv
#' @param plot if TRUE, plots the original sound, smoothed moving average, and
#'   modified sound
#' @examples
#' # remove static DC offset
#' a = rnorm(500) + .3
#' b = soundgen:::killEnv(a, windowLength_points = 500, plot = TRUE)
#'
#' # remove trend
#' a = rnorm(500) + seq(0, 1, length.out = 500)
#' b = soundgen:::killEnv(a, windowLength_points = 100, plot = TRUE)
#'
#' # can also be used as a high-pass filter
#' a = rnorm(500) + sin(1:500 / 50)
#' b = soundgen:::killEnv(a, windowLength_points = 25, plot = TRUE)
killEnv = function(sound,
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
