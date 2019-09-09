#' Play audio
#'
#' Plays an audio file or a numeric vector. This is a simple wrapper for the
#' functionality provided by \code{\link[tuneR]{play}}. Recommended players on
#' Linux: "play" from the "vox" library (default), "aplay".
#' @param sound a vector of numbers on any scale or a path to a .wav file
#' @param samplingRate sampling rate (only needed if sound is a vector)
#' @param player the name of player to use, eg "aplay", "play", "vlc", etc. In
#'   case of errors, try setting another default player for
#'   \code{\link[tuneR]{play}}
#' @param from,to play a selected time range (s)
#' @param ... additional parameters passed to \code{\link[tuneR]{play}}
#' @export
#' @examples
#' # Play an audio file:
#' # playme('pathToMyAudio/audio.wav')
#'
#' # Create and play a numeric vector:
#' f0_Hz = 440
#' sound = sin(2 * pi * f0_Hz * (1:16000) / 16000)
#' # playme(sound, 16000)
#' # playme(sound, 16000, from = .1, to = .5)  # play from 100 to 500 ms
#'
#' # In case of errors, look into tuneR::play(). For ex., you might need to
#' # specify which player to use:
#' # playme(sound, 16000, player = 'aplay')
#'
#' # To avoid doing it all the time, set the default player:
#' tuneR::setWavPlayer('aplay')
#' # playme(sound, 16000)  # should work without specifying the player
playme = function(sound,
                  samplingRate = 16000,
                  player = NULL,
                  from = NULL,
                  to = NULL) {
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

  # to play a selected time range
  if (!is.null(from) | !is.null(to)) {
    if (is.null(from)) from = 0
    if (is.null(to)) to = length(soundWave@left) / soundWave@samp.rate
    soundWave = tuneR::extractWave(object = soundWave,
                                   from = from,
                                   to = to,
                                   interact = FALSE,
                                   xunit = 'time')
  }

  if (!is.null(player)) {
    p = tuneR::play(soundWave, player = player)
  } else {
    # try to guess
    os = Sys.info()[['sysname']]
    if (os == 'Linux' | os == 'linux') {
      p = tuneR::play(soundWave, 'play')
    } else {  # windows | darwin
      p = tuneR::play(soundWave)
    }
  }
  if (p > 0) {  # error in sh
    warning(paste0(
      "Error in tuneR::play. Try setting the default audio player,",
      "eg tuneR::setWavPlayer('aplay'). See http://music.informatics.",
      "indiana.edu/courses/I546/tuneR_play.pdf"))
  }
  # can't get rid of printed output! sink(), capture.output, invisible don't work!!!
}


#' Fade
#'
#' Applies fade-in and/or fade-out of variable length, shape, and steepness. The
#' resulting effect softens the attack and release of a waveform.
#' @param x zero-centered (!) numeric vector such as a waveform
#' @param fadeIn,fadeOut length of segments for fading in and out, interpreted
#'   as points if \code{samplingRate = NULL} and as ms otherwise (0 = no fade)
#' @param samplingRate sampling rate of the input vector, Hz
#' @param shape controls the type of fade function: 'lin' = linear, 'exp' =
#'   exponential, 'log' = logarithmic, 'cos' = cosine, 'logistic' = logistic
#'   S-curve
#' @param steepness scaling factor regulating the steepness of fading curves if
#'   the shape is 'exp', 'log', or 'logistic' (0 = linear, >1 = steeper than
#'   default)
#' @param plot if TRUE, produces an oscillogram of the waveform after fading
#' @return Returns a numeric vector of the same length as input
#' @export
#' @examples
#' #' # Fading a real sound: say we want fast attack and slow release
#' s = soundgen(attack = 0, windowLength = 10,
#'              sylLen = 500, addSilence = 0)
#' # playme(s)
#' # plot(s, type = 'l')
#' s1 = fade(s, fadeIn = 10, fadeOut = 350,
#'           samplingRate = 16000, shape = 'cos')
#' # playme(s1)
#' # plot(s1, type = 'l')
#'
#'
#' # Illustration of fade shapes
#' x = runif(5000, min = -1, max = 1)  # make sure to zero-center input!!!
#' # plot(x, type = 'l')
#' y = fade(x, fadeIn = 1000, fadeOut = 0, plot = TRUE)
#' y = fade(x,
#'          fadeIn = 1000,
#'          fadeOut = 1500,
#'          shape = 'exp',
#'          plot = TRUE)
#' y = fade(x,
#'          fadeIn = 1500,
#'          fadeOut = 500,
#'          shape = 'log',
#'          plot = TRUE)
#' y = fade(x,
#'          fadeIn = 1500,
#'          fadeOut = 500,
#'          shape = 'log',
#'          steepness = 8,
#'          plot = TRUE)
#' y = fade(x,
#'          fadeIn = 1000,
#'          fadeOut = 1500,
#'          shape = 'cos',
#'          plot = TRUE)
#' y = fade(x,
#'          fadeIn = 1500,
#'          fadeOut = 500,
#'          shape = 'logistic',
#'          steepness = 4,
#'          plot = TRUE)
fade = function(x,
                fadeIn = 1000,
                fadeOut = 1000,
                samplingRate = NULL,
                shape = c('lin', 'exp', 'log', 'cos', 'logistic')[1],
                steepness = 1,
                plot = FALSE) {
  if ((!is.numeric(fadeIn) | fadeIn < 1) &
      (!is.numeric(fadeOut) | fadeOut < 1)) {
    return(x)
  }

  if (steepness < 0) {
    steepness = 1
    warning('steepness must be non-negative; resetting to 1')
  } else if (steepness == 0) {
    shape = 'lin'
  }

  if (is.numeric(samplingRate)) {
    fadeIn = round(fadeIn * samplingRate / 1000)
    fadeOut = round(fadeOut * samplingRate / 1000)
  }

  # round fading window just in case of non-integers, shorten if needed
  fadeIn = min(length(x), round(fadeIn))
  fadeOut = min(length(x), round(fadeOut))

  time_in = seq(0, 1, length.out = fadeIn)
  time_out = seq(1, 0, length.out = fadeOut)
  if (shape == 'lin') {
    fi = time_in
    fo = time_out
  } else if (shape == 'exp') {
    fi = zeroOne(exp(time_in * steepness / 3))
    fo = zeroOne(exp(time_out * steepness / 3))
  } else if (shape == 'log') {
    fi = 1 - rev(zeroOne(exp(time_in * steepness / 3)))
    fo = 1 - rev(zeroOne(exp(time_out * steepness / 3)))
  } else if (shape == 'cos') {
    fi = (1 - cos(time_in * pi)) / 2
    fo = (1 - cos(time_out * pi)) / 2
  } else if (shape == 'logistic') {
    fi = zeroOne(1 - 1 / (1 + exp(steepness * (time_in - .5))))
    fo = zeroOne(1 - 1 / (1 + exp(steepness * (time_out - .5))))
  }
  # plot(fi, type = 'l')
  # plot(fo, type = 'l')

  if (fadeIn > 0) {
    x[1:fadeIn] = x[1:fadeIn] * fi
  }
  if (fadeOut > 0) {
    x[(length(x) - fadeOut + 1):length(x)] =
      x[(length(x) - fadeOut + 1):length(x)] * fo
  }

  if (plot) {
    plot(x, type = 'l', xlab = '', ylab = '')
    abline(v = fadeIn, col = 'blue')
    abline(v = length(x) - fadeOut, col = 'blue')
  }
  return(x)
}


#' Flat envelope
#'
#' Flattens the amplitude envelope of a waveform. This is achieved by dividing
#' the waveform by some function of its smoothed amplitude envelope (Hilbert,
#' peak or root mean square).
#' @param sound input vector oscillating about zero
#' @param windowLength the length of smoothing window, ms
#' @param samplingRate the sampling rate, Hz. Only needed if the length of
#'   smoothing window is specified in ms rather than points
#' @param method 'hil' for Hilbert envelope, 'rms' for root mean square
#'   amplitude, 'peak' for peak amplitude per window
#' @param windowLength_points the length of smoothing window, points. If
#'   specified, overrides both \code{windowLength} and \code{samplingRate}
#' @param killDC if TRUE, dynamically removes DC offset or similar deviations of
#'   average waveform from zero
#' @param dynamicRange parts of sound quieter than \code{-dynamicRange} dB will
#'   not be amplified
#' @param plot if TRUE, plots the original sound, smoothed envelope, and
#'   flattened sound
#' @export
#' @examples
#' a = rnorm(500) * seq(1, 0, length.out = 500)
#' b = flatEnv(a, plot = TRUE, windowLength_points = 5)    # too short
#' c = flatEnv(a, plot = TRUE, windowLength_points = 250)  # too long
#' d = flatEnv(a, plot = TRUE, windowLength_points = 50)   # about right
#'
#' \dontrun{
#' s = soundgen(sylLen = 1000, ampl = c(0, -40, 0), plot = TRUE, osc = TRUE)
#' # playme(s)
#' s_flat1 = flatEnv(s, plot = TRUE, windowLength = 50, method = 'hil')
#' s_flat2 = flatEnv(s, plot = TRUE, windowLength = 10, method = 'rms')
#' # playme(s_flat2)
#'
#' # Remove DC offset
#' s1 = c(rep(0, 50), runif(1000, -1, 1), rep(0, 50)) +
#'      seq(.3, 1, length.out = 1100)
#' s2 = flatEnv(s1, plot = TRUE, windowLength_points = 50, killDC = FALSE)
#' s3 = flatEnv(s1, plot = TRUE, windowLength_points = 50, killDC = TRUE)
#' }
flatEnv = function(sound,
                   windowLength = 200,
                   samplingRate = 16000,
                   method = c('hil', 'rms', 'peak')[1],
                   windowLength_points = NULL,
                   killDC = FALSE,
                   dynamicRange = 80,
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
  throwaway_lin = 10 ^ (-dynamicRange / 20)  # from dB to linear
  # get smoothed amplitude envelope
  if (method == 'hil') {
    env = seewave::env(
      soundNorm,
      f = samplingRate,
      envt = 'hil',
      ssmooth = windowLength_points,
      fftw = FALSE,
      plot = FALSE
    )
    env = as.numeric(env)
  } else {
    env = getEnv(sound = soundNorm,
                 windowLength_points = windowLength_points,
                 method = method)
  }
  env = env / max(abs(env))

  # don't amplify very quiet sections
  env_cut = env
  env_cut[env_cut < throwaway_lin] = 1
  # flatten amplitude envelope
  soundFlat = soundNorm / env_cut
  # re-normalize to original scale
  soundFlat = soundFlat / max(abs(soundFlat)) * m
  # remove DC offset
  if (killDC) {
    soundFlat = killDC(sound = soundFlat,
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


#' Join two waveforms by cross-fading
#'
#' \code{crossFade} joins two input vectors (waveforms) by cross-fading. First
#' it truncates both input vectors, so that \code{ampl1} ends with a zero
#' crossing and \code{ampl2} starts with a zero crossing, both on an upward
#' portion of the soundwave. Then it cross-fades both vectors linearly with an
#' overlap of crossLen or crossLenPoints. If the input vectors are too short for
#' the specified length of cross-faded region, the two vectors are concatenated
#' at zero crossings instead of cross-fading. Soundgen uses \code{crossFade} for
#' gluing together epochs with different regimes of pitch effects (see the
#' vignette on sound generation), but it can also be useful for joining two
#' separately generated sounds without audible artifacts.
#' @param ampl1,ampl2 two numeric vectors (waveforms) to be joined
#' @param crossLenPoints (optional) the length of overlap in points
#' @param crossLen the length of overlap in ms (overrides crossLenPoints)
#' @param samplingRate the sampling rate of input vectors, Hz (needed only if
#'   crossLen is given in ms rather than points)
#' @inheritParams fade
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
#' #  because of trimming to zero crossings and then overlapping
#'
#' \dontrun{
#' # Actual sounds, alternative shapes of fade-in/out
#' sound3 = soundgen(formants = 'a', pitch = 200,
#'                   addSilence = 0, attackLen = c(50, 0))
#' sound4 = soundgen(formants = 'u', pitch = 200,
#'                   addSilence = 0, attackLen = c(0, 50))
#'
#' # simple concatenation (with a click)
#' playme(c(sound3, sound4), 16000)
#'
#' # concatentation from zc to zc (no click, but a rough transition)
#' playme(crossFade(sound3, sound4, crossLen = 0), 16000)
#'
#' # linear crossFade over 35 ms - brief, but smooth
#' playme(crossFade(sound3, sound4, crossLen = 35, samplingRate = 16000), 16000)
#'
#' # s-shaped cross-fade over 300 ms (shortens the sound by ~300 ms)
#' playme(crossFade(sound3, sound4, samplingRate = 16000,
#'                  crossLen = 300, shape = 'cos'), 16000)
#' }
crossFade = function(ampl1,
                     ampl2,
                     crossLenPoints = 240,
                     crossLen = NULL,
                     samplingRate = NULL,
                     shape = c('lin', 'exp', 'log', 'cos', 'logistic')[1],
                     steepness = 1) {
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
  if (is.null(crossLenPoints) |
      (!is.null(crossLen) & !is.null(samplingRate))) {
    crossLenPoints = floor(crossLen * samplingRate / 1000)
  }
  crossLenPoints = min(crossLenPoints, length(ampl1) - 1, length(ampl2) - 1)

  # concatenate or cross-fade
  if (crossLenPoints < 2) {
    # for segments that are too short,
    # just concatenate from zero crossing to zero crossing
    ampl = c(ampl1, ampl2)
  } else {
    # for segments that are long enough, cross-fade properly
    # multipl = seq(0, 1, length.out = crossLenPoints)
    multipl = fade(rep(1, crossLenPoints),
                   fadeIn = crossLenPoints,
                   fadeOut = 0,
                   shape = shape,
                   steepness = steepness)
    idx1 = length(ampl1) - crossLenPoints
    cross = rev(multipl) * ampl1[(idx1 + 1):length(ampl1)] +
      multipl * ampl2[1:crossLenPoints]
    ampl = c(ampl1[1:idx1],
             cross,
             ampl2[(crossLenPoints + 1):length(ampl2)])
  }
  return(ampl)
}


#' Flat spectrum
#'
#' Flattens the spectrum of a sound by smoothing in the frequency domain. Can be
#' used for removing formants without modifying pitch contour or voice quality
#' (the balance of harmonic and noise components), followed by the addition of a
#' new spectral envelope (cf. \code{\link{transplantFormants}}). Algorithm:
#' makes a spectrogram, flattens the real part of the smoothed spectrum of each
#' STFT frame, and transforms back into time domain with inverse STFT (see also
#' \code{\link{addFormants}}).
#'
#' @return Returns a numeric vector with the same sampling rate as the input.
#' @inheritParams spectrogram
#' @param freqWindow the width of smoothing window, Hz. Defaults to median
#'   pitch estimated by \code{\link{analyze}}
#' @export
#' @examples
#' sound_aii = soundgen(formants = 'aii')
#' # playme(sound_aii, 16000)
#' seewave::meanspec(sound_aii, f = 16000, dB = 'max0')
#'
#' sound_flat = flatSpectrum(sound_aii, freqWindow = 150, samplingRate = 16000)
#' # playme(sound_flat, 16000)
#' seewave::meanspec(sound_flat, f = 16000, dB = 'max0')
#' # harmonics are still there, but formants are gone and can be replaced
#'
#' \dontrun{
#' # Now let's make a sheep say "aii"
#' data(sheep, package = 'seewave')  # import a recording from seewave
#' sheep_orig = as.numeric(scale(sheep@left))
#' samplingRate = sheep@samp.rate
#' playme(sheep_orig, samplingRate)
#' # spectrogram(sheep_orig, samplingRate)
#' # seewave::spec(sheep_orig, f = samplingRate, dB = 'max0')
#'
#' sheep_flat = flatSpectrum(sheep_orig, freqWindow = 150,  # freqWindow ~f0
#'   samplingRate = samplingRate)
#' # playme(sheep_flat, samplingRate)
#' # spectrogram(sheep_flat, samplingRate)
#' # seewave::spec(sheep_flat, f = samplingRate, dB = 'max0')
#'
#' # So far we have a sheep bleating with a flat spectrum;
#' # now let's add new formants
#' sheep_aii = addFormants(sheep_flat,
#'   samplingRate = samplingRate,
#'   formants = 'aii',
#'   lipRad = -3)  # negative lipRad to counter unnatural flat source
#' playme(sheep_aii, samplingRate)
#' # spectrogram(sheep_aii, samplingRate)
#' # seewave::spec(sheep_aii, f = samplingRate, dB = 'max0')
#' }
flatSpectrum = function(x,
                        freqWindow = NULL,
                        samplingRate = NULL,
                        dynamicRange = 80,
                        windowLength = 50,
                        step = NULL,
                        overlap = 90,
                        wn = 'gaussian',
                        zp = 0) {
  if (is.character(x)) {
    extension = substr(x, nchar(x) - 2, nchar(x))
    if (extension == 'wav' | extension == 'WAV') {
      sound_wav = tuneR::readWave(x)
    } else if (extension == 'mp3' | extension == 'MP3') {
      sound_wav = tuneR::readMP3(x)
    } else {
      stop('Input not recognized: must be a numeric vector or wav/mp3 file')
    }
    samplingRate = sound_wav@samp.rate
    sound = as.numeric(sound_wav@left)
  } else {
    sound = x
  }

  # default freqWindow = f0
  if (!is.numeric(freqWindow)) {
    a = analyze(sound, samplingRate, plot = FALSE)
    freqWindow = median(a$pitch, na.rm = TRUE)
  }

  # get a spectrogram of the original sound
  spec = spectrogram(sound,
                     samplingRate = samplingRate,
                     dynamicRange = dynamicRange,
                     windowLength = windowLength,
                     step = step,
                     overlap = overlap,
                     wn = wn,
                     zp = zp,
                     output = 'complex',
                     plot = FALSE)

  # calculate the width of smoothing window in bins
  freqRange_kHz = diff(range(as.numeric(rownames(spec))))
  freqBin_Hz = freqRange_kHz * 1000 / nrow(spec)
  freqWindow_bins = round(freqWindow / freqBin_Hz, 0)
  if (freqWindow_bins < 3) {
    message(paste('freqWindow has to be at least 3 bins wide;
                  resetting to', ceiling(freqBin_Hz * 3)))
    freqWindow_bins = 3
  }

  # modify the spectrogram
  for (i in 1:ncol(spec)) {
    abs_s = abs(spec[, i])
    cor_coef = flatEnv(abs_s, method = 'peak',
                       windowLength_points = freqWindow_bins) / abs_s
    spec[, i] = complex(real = Re(spec[, i]) * cor_coef,
                        imaginary = Im(spec[, i]))
    # plot(abs(spec[, i]), type = 'b')
  }

  # recreate an audio from the modified spectrogram
  windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
  sound_new = as.numeric(
    seewave::istft(
      spec,
      f = samplingRate,
      ovlp = overlap,
      wl = windowLength_points,
      output = "matrix"
    )
  )
  # playme(sound_new, samplingRate)
  # spectrogram(sound_new, samplingRate)
  return(sound_new)
}

