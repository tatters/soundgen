#' Play audio
#'
#' Plays one or more sounds: wav/mp3 file(s), Wave objects, or numeric vectors.
#' This is a simple wrapper for the functionality provided by
#' \code{\link[tuneR]{play}}. Recommended players on Linux: "play" from the
#' "vox" library (default), "aplay".
#' @inheritParams spectrogram
#' @param player the name of player to use, eg "aplay", "play", "vlc", etc.
#'   Defaults to "play" on Linux, "afplay" on MacOS, and tuneR default on
#'   Windows. In case of errors, try setting another default player for
#'   \code{\link[tuneR]{play}}
#' @param from,to play a selected time range (s)
#' @export
#' @examples
#' \dontrun{
#' # Play an audio file:
#' playme('pathToMyAudio/audio.wav')
#'
#' # Create and play a numeric vector:
#' f0_Hz = 440
#' sound = sin(2 * pi * f0_Hz * (1:16000) / 16000)
#' playme(sound, 16000)
#' playme(sound, 16000, from = .1, to = .5)  # play from 100 to 500 ms
#'
#' # In case of errors, look into tuneR::play(). For ex., you might need to
#' # specify which player to use:
#' playme(sound, 16000, player = 'aplay')
#'
#' # To avoid doing it all the time, set the default player:
#' tuneR::setWavPlayer('aplay')
#' playme(sound, 16000)  # should now work without specifying the player
#' }
playme = function(x,
                  samplingRate = 16000,
                  player = NULL,
                  from = NULL,
                  to = NULL) {
  # try to guess what player to use
  os = Sys.info()[['sysname']]
  if (os == 'Linux' | os == 'linux') {
    player = 'play'
  } else if (os == 'Darwin' | os == 'darwin') {
    player = 'afplay'
  } else {
    # a good default on windows?
  }

  # check input type
  input = checkInputType(x)
  if (input$type[1] == 'file') x = rep(list(NULL), input$n)
  if (!is.list(x)) x = list(x)

  # play each input
  for (i in 1:length(x)) {
    # make input i into a Wave object
    if (input$type[i] == 'file') {
      fi = input$filenames[i]
      ext_i = substr(fi, nchar(fi) - 3, nchar(fi))
      if (ext_i %in% c('.wav', '.WAV')) {
        sound_wave = try(tuneR::readWave(fi))
      } else if (ext_i %in% c('mp3', 'MP3')) {
        sound_wave = try(tuneR::readMP3(fi))
      } else {
        warning(paste('Input', fi, 'not recognized: expected a wav/mp3 file'))
      }
    } else if (input$type[i] == 'vector') {
      sound_wave = tuneR::Wave(
        left = x[[i]],
        samp.rate = samplingRate,
        bit = 16,
        pcm = TRUE
      )
      sound_wave = tuneR::normalize(sound_wave, unit = '32') # / 2
    } else if (input$type[i] == 'Wave') {
      sound_wave = x[[i]]
    }

    if (class(sound_wave) != 'try-error') {
      # select time range
      if (!is.null(from) | !is.null(to)) {
        if (is.null(from)) from = 0
        if (is.null(to)) to = length(sound_wave@left) / sound_wave@samp.rate
        soundWave = tuneR::extractWave(object = sound_wave,
                                       from = from,
                                       to = to,
                                       interact = FALSE,
                                       xunit = 'time')
      }
      p = tuneR::play(sound_wave, player = player)
      if (p > 0) {  # error in sh
        warning(paste0(
          "Error in tuneR::play. Try setting the default audio player,",
          "eg tuneR::setWavPlayer('aplay'). See http://music.informatics.",
          "indiana.edu/courses/I546/tuneR_play.pdf"))
      }
    }
  }
  # can't get rid of printed output! sink(), capture.output, invisible don't work!!!
}


#' Fade
#'
#' Applies fade-in and/or fade-out of variable length, shape, and steepness. The
#' resulting effect softens the attack and release of a waveform.
#'
#' @seealso \code{\link{crossFade}}
#'
#' @param x zero-centered (!) numeric vector such as a waveform
#' @param fadeIn,fadeOut length of segments for fading in and out, interpreted
#'   as points if \code{samplingRate = NULL} and as ms otherwise (0 = no fade)
#' @param samplingRate sampling rate of the input vector, Hz
#' @param shape controls the type of fade function: 'lin' = linear, 'exp' =
#'   exponential, 'log' = logarithmic, 'cos' = cosine, 'logistic' = logistic
#'   S-curve
#' @param steepness scaling factor regulating the steepness of fading curves
#'   (except for shapes 'lin' and 'cos'): 0 = linear, >1 = steeper than default
#' @param plot if TRUE, produces an oscillogram of the waveform after fading
#' @return Returns a numeric vector of the same length as input
#' @export
#' @examples
#' #' # Fading a real sound: say we want fast attack and slow release
#' s = soundgen(attack = 0, windowLength = 10,
#'              sylLen = 500, addSilence = 0)
#' # playme(s)
#' s1 = fade(s, fadeIn = 40, fadeOut = 350,
#'           samplingRate = 16000, shape = 'cos', plot = TRUE)
#' # playme(s1)
#'
#' # Illustration of fade shapes
#' x = runif(5000, min = -1, max = 1)  # make sure to zero-center input!!!
#' # plot(x, type = 'l')
#' y = fade(x, fadeIn = 1000, fadeOut = 0, plot = TRUE)
#' y = fade(x, fadeIn = 1000, fadeOut = 1500,
#'          shape = 'exp', steepness = 1, plot = TRUE)
#' y = fade(x, fadeIn = 1500, fadeOut = 500,
#'          shape = 'log', steepness = 1, plot = TRUE)
#' y = fade(x, fadeIn = 1500, fadeOut = 500,
#'          shape = 'log', steepness = 3, plot = TRUE)
#' y = fade(x, fadeIn = 1500, fadeOut = 1500,
#'          shape = 'cos', plot = TRUE)
#' y = fade(x, fadeIn = 1500, fadeOut = 1500,
#'          shape = 'logistic', steepness = 1, plot = TRUE)
#' y = fade(x, fadeIn = 1500, fadeOut = 1500,
#'          shape = 'logistic', steepness = 3, plot = TRUE)
#' y = fade(x, fadeIn = 1500, fadeOut = 1500,
#'          shape = 'gaussian', steepness = 1.5, plot = TRUE)
fade = function(
  x,
  fadeIn = 1000,
  fadeOut = 1000,
  samplingRate = NULL,
  shape = c('lin', 'exp', 'log', 'cos', 'logistic', 'gaussian')[1],
  steepness = 1,
  plot = FALSE) {
  if ((!is.numeric(fadeIn) | fadeIn < 1) &
      (!is.numeric(fadeOut) | fadeOut < 1)) {
    return(x)
  }

  valid_shapes = c('lin', 'exp', 'log', 'cos', 'logistic', 'gaussian')
  if (!shape %in% valid_shapes) {
    warning(paste(
      shape, 'is not a valid shape, defaulting to "lin".',
      'Implemented shapes: "', paste(valid_shapes, collapse = ", "), '"'
    ))
    shape = 'lin'
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
    m = exp(steepness * 3)  # to avoid taking min/max within zeroOne
    fi = zeroOne(exp(time_in * steepness * 3), xmin = 1, xmax = m)
    fo = zeroOne(exp(time_out * steepness * 3), xmin = 1, xmax = m)
  } else if (shape == 'log') {
    m = exp(steepness * 3)
    fi = 1 - rev(zeroOne(exp(time_in * steepness * 3), xmin = 1, xmax = m))
    fo = 1 - rev(zeroOne(exp(time_out * steepness * 3), xmin = 1, xmax = m))
  } else if (shape == 'cos') {
    fi = (1 - cos(time_in * pi)) / 2
    fo = (1 - cos(time_out * pi)) / 2
  } else if (shape == 'logistic') {
    xmin = 1 - 1 / (1 + exp(6 * steepness * (0 - .5)))
    xmax = 1 - 1 / (1 + exp(6 * steepness * (1 - .5)))
    fi = zeroOne(1 - 1 / (1 + exp(6 * steepness * (time_in - .5))),
                 xmin = xmin, xmax = xmax)
    fo = zeroOne(1 - 1 / (1 + exp(6 * steepness * (time_out - .5))),
                 xmin = xmin, xmax = xmax)
  } else if (shape == 'gaussian') {
    xmin = dnorm(x = -1, mean = 0, sd = .4 / steepness)
    xmax = dnorm(x = 0, mean = 0, sd = .4 / steepness)
    fi = zeroOne(dnorm(x = -rev(time_in), mean = 0, sd = .4 / steepness),
                 xmin = xmin, xmax = xmax)
    fo = rev(zeroOne(dnorm(x = time_out, mean = 0, sd = .4 / steepness),
                     xmin = xmin, xmax = xmax))
  }
  # plot(fi, type = 'l', xlim = c(1, max(fadeIn, fadeOut)))
  # points(fo, type = 'l', col = 'red')

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
  invisible(x)
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
#'
#' @seealso \code{\link{fade}}
#'
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
crossFade = function(
  ampl1,
  ampl2,
  crossLenPoints = 240,
  crossLen = NULL,
  samplingRate = NULL,
  shape = c('lin', 'exp', 'log', 'cos', 'logistic', 'gaussian')[1],
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
#' @seealso \code{\link{addFormants}} \code{\link{transplantFormants}}
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
                     padWithSilence = FALSE,
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
    cor_coef = flatEnv(abs_s,
                       samplingRate = 1,  # not used, but can't be left NULL
                       method = 'peak',
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
  invisible(sound_new)
}


#' Reverb & echo
#'
#' Adds reverberation and/or echo to a sound. Algorithm for reverb: adds
#' time-shifted copies of the signal weighted by a decay function, which is
#' analogous to convoluting the input with a parametric model of some
#' hypothetical impulse response function. In simple terms: we specify how much
#' and when the sound rebounds back (as from a wall) and add these time-shifted
#' copies to the original, optionally with some spectral filtering.
#' @inheritParams spectrogram
#' @param filter (optional) a spectral filter to apply to the created reverb and
#'   echo (see \code{addFormants} for acceptable formats)
#' @param echoDelay the delay at which the echo appears, ms
#' @param echoLevel the rate at which the echo weakens at each repetition, dB
#' @param reverbDelay the time of maximum reverb density, ms
#' @param reverbSpread standard deviation of reverb spread around time
#'   \code{reverbDelay}, ms
#' @param reverbLevel the maximum amplitude of reverb, dB below input
#' @param reverbDensity the number of echos or "voices" added
#' @param reverbType not yet implemented
#' @param dynamicRange the precision with which the reverb and echo are
#'   calculated, dB
#' @param output "audio" = returns just the processed audio, "detailed" =
#'   returns a list with reverb window, the added reverb/echo, etc.
#' @param len (optional) the length of input vector (used internally for speed)
#' @export
#' @examples
#' s = soundgen()
#' s_rev = reverb(s, 16000)
#' # playme(s_rev)
#'
#' \dontrun{
#' # double echo, no reverb
#' s1 = reverb(s, samplingRate = 16000, reverbLevel = NULL,
#'             echoDelay = c(250, 800), echoLevel = c(-15, -25))
#' # playme(s1)
#' # spectrogram(s1, 16000, osc = TRUE, ylim = c(0, 4))
#'
#' # only reverb (indoors)
#' s2 = reverb(s, samplingRate = 16000, echoDelay = NULL,
#'             reverbDelay = 70, reverbSpread = 130,
#'             reverbLevel = -20, reverbDensity = 20)
#' # playme(s2)
#' # spectrogram(s2, 16000, osc = TRUE, ylim = c(0, 4))
#'
#' # reverb (caves)
#' s3 = reverb(s, samplingRate = 16000, echoDelay = NULL,
#'             reverbDelay = 600, reverbSpread = 1500,
#'             reverbLevel = -10, reverbDensity = 100)
#' # playme(s3)
#' # spectrogram(s3, 16000, osc = TRUE, ylim = c(0, 4))
#'
#' # both echo and reverb with high frequencies emphasized
#' s4 = reverb(s, samplingRate = 16000,
#'             echoDelay = 250, echoLevel = -20,
#'             reverbDelay = 70, reverbSpread = 120,
#'             reverbLevel = -25, reverbDensity = 50,
#'             filter = list(formants = NULL, lipRad = 3))
#' # playme(s4)
#' # spectrogram(s4, 16000, osc = TRUE, ylim = c(0, 4))
#'
#' # add reverb to a recording
#' s5 = reverb('~/Downloads/temp260/ut_fear_57-m-tone.wav',
#'             echoDelay = 850, echoLevel = -40)
#' # playme(s5, 44100)
#' }
reverb = function(x,
                  samplingRate = NULL,
                  echoDelay = 200,
                  echoLevel = -20,
                  reverbDelay = 70,
                  reverbSpread = 130,
                  reverbLevel = -25,
                  reverbDensity = 50,
                  reverbType = 'gaussian',
                  filter = list(),
                  dynamicRange = 80,
                  output = c('audio', 'detailed')[1],
                  len = NULL) {
  # import audio
  if (class(x)[1] == 'character') {
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
  } else if (class(x)[1] == 'numeric' & length(x) > 1) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    } else {
      sound = x
    }
  }
  if (is.null(len)) len = length(sound)

  ## reverb
  if (is.numeric(reverbDelay) & is.numeric(reverbLevel)) {
    if (reverbType == 'gaussian') {
      # Gaussian over 3 SD
      rvb_len_ms = reverbDelay + 3 * reverbSpread
      nFr_rvb = round(rvb_len_ms * samplingRate / 1000)
      win = dnorm(1:nFr_rvb,
                  mean = reverbDelay * samplingRate / 1000,
                  sd = reverbSpread * samplingRate / 1000)
      win = win / max(win) * dynamicRange - dynamicRange + reverbLevel
      idx_keep = sample(1:nFr_rvb, size = min(nFr_rvb, reverbDensity))
      win = 10 ^ (win / 20)
      win[-idx_keep] = 0
      # plot(win, type = 'l')
    } else {
      stop("Only reverbType = 'gaussian' has been implemented so far")
      # exponential decay: halves (-6 dB) every reverbDelay
      n_halves = min(4, round((dynamicRange + reverbLevel) / 6))
      rvb_len_ms = n_halves * reverbDelay
      nFr_rvb = ceiling(rvb_len_ms * samplingRate / 1000)
      len_halflife = ceiling(reverbDelay * samplingRate / 1000)
      reverbLevel_lin = 10 ^ (reverbLevel / 20)
      win = 2 ^ (-(1:nFr_rvb) / len_halflife) * reverbLevel_lin
      # plot(win, type = 'l')

      # add some noise
      if (FALSE) {
        # rvb_wiggle = getRandomWalk(len = nFr_rvb,
        #   rw_range = reverbSpread * reverbLevel_lin * 2) - reverbSpread * reverbLevel_lin
        lw = rvb_len_ms / 2  # relatively smooth wiggle (1 value per 2 ms)
        # sd_w = seq(reverbSpread, reverbSpread / n_halves, length.out = lw) * win[1]
        rvb_wiggle = rnorm(lw, mean = 0, sd = sd_w)
        rvb_wiggle = approx(rvb_wiggle, n = nFr_rvb)$y
        # plot(rvb_wiggle, type = 'l')
        win = win + rvb_wiggle
        # plot(win, type = 'l')
      }
    }

    # create reverb by adding up time-shifted signal weighted by decay window
    rvb = rep(0, len + nFr_rvb - 1)
    for (i in idx_keep) {
      idx_i = i:(i + len - 1)
      rvb[idx_i] = rvb[idx_i] + sound * win[i]
    }
    # playme(rvb)
    # spectrogram(rvb, 16000, ylim = c(0, 4), osc = TRUE)
    # rvb = rvb_pad[1:len]  # range(rvb)
  } else {
    rvb = 0
  }

  ## echo
  if (is.numeric(echoLevel) && echoLevel > (-dynamicRange) & any(echoDelay > 0)) {
    le = length(echoDelay)
    if (le > 1 & length(echoLevel) == 1) echoLevel = rep(echoLevel, le)
    echo = NULL
    for (e in 1:le) {
      nFr_echo = dynamicRange / (-echoLevel[e])
      step_echo = ceiling(echoDelay[e] * samplingRate / 1000)
      echo_e = rep(0, len + nFr_echo * step_echo - 1)
      for (i in 1:nFr_echo) {
        idx_start = i * step_echo
        idx_i = idx_start:(idx_start + len - 1)
        echo_e[idx_i] = echo_e[idx_i] + sound * 10 ^ (echoLevel[e] * i / 20)
      }
      if (is.null(echo)) {
        echo = echo_e
      } else {
        echo = addVectors(echo, echo_e, normalize = FALSE)
      }
    }
    # playme(echo)
  } else {
    echo = 0
  }

  effect = addVectors(rvb, echo, normalize = FALSE)
  if (length(filter) > 1) {
    scale = max(effect)
    rvb = do.call(addFormants, c(list(
      x = effect, samplingRate = samplingRate,
      normalize = TRUE),
      filter)
    ) * scale
    # playme(effect)
    # spectrogram(effect, 16000, ylim = c(0, 4), osc = TRUE)
  }

  out = addVectors(sound, effect)
  # playme(out)
  # spectrogram(out, 16000, ylim = c(0, 4), osc = TRUE)

  if (output == 'audio') {
    result = out
  } else if (output == 'detailed') {
    result = list(
      rvb_win = win,
      rvb = rvb,
      echo = echo,
      effect = effect,
      audio = out
    )
  }
  invisible(result)
}
