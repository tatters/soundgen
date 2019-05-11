# Functions for generating excitation source: either noise with generateNoise() or harmonics with generateHarmonics()

#' Generate noise
#'
#' Generates noise of length \code{len} and with spectrum defined by linear
#' decay of \code{rolloffNoise} dB/kHz above \code{noiseFlatSpec} Hz OR by a
#' specified filter \code{spectralEnvelope}. This function is called internally
#' by \code{\link{soundgen}}, but it may be more convenient to call it directly
#' when synthesizing non-biological noises defined by specific spectral and
#' amplitude envelopes rather than formants: the wind, whistles, impact noises,
#' etc. See \code{\link{fart}} and \code{\link{beat}} for similarly simplified
#' functions for tonal non-biological sounds.
#'
#' Algorithm: paints a spectrogram with desired characteristics, sets phase to
#' zero, and generates a time sequence via inverse FFT.
#' @param len length of output
#' @param spectralEnvelope (optional): as an alternative to using rolloffNoise,
#'   we can provide the exact filter - a vector of non-negative numbers
#'   specifying the power in each frequency bin on a linear scale (interpolated
#'   to length equal to windowLength_points/2). A matrix specifying the filter
#'   for each STFT step is also accepted. The easiest way to create this matrix
#'   is to call soundgen:::getSpectralEnvelope or to use the spectrum of a
#'   recorded sound
#' @inheritParams soundgen
#' @param windowLength_points the length of fft window, points
#' @export
#' @examples
#' # .5 s of white noise
#' samplingRate = 16000
#' noise1 = generateNoise(len = samplingRate * .5,
#'   samplingRate = samplingRate)
#' # playme(noise1, samplingRate)
#' # seewave::meanspec(noise1, f = samplingRate)
#'
#' # Percussion (run a few times to notice stochasticity due to temperature = .25)
#' noise2 = generateNoise(len = samplingRate * .15, noise = c(0, -80),
#'   rolloffNoise = c(4, -6), attackLen = 5, temperature = .25)
#' noise3 = generateNoise(len = samplingRate * .25, noise = c(0, -40),
#'   rolloffNoise = c(4, -20), attackLen = 5, temperature = .25)
#' # playme(c(noise2, noise3), samplingRate)
#'
#' \dontrun{
#' playback = c(TRUE, FALSE, 'aplay', 'vlc')[2]
#' # 1.2 s of noise with rolloff changing from 0 to -12 dB above 2 kHz
#' noise = generateNoise(len = samplingRate * 1.2,
#'   rolloffNoise = c(0, -12), noiseFlatSpec = 2000,
#'   samplingRate = samplingRate, play = playback)
#' # spectrogram(noise, samplingRate, osc = TRUE)
#'
#' # Similar, but using the dataframe format to specify a more complicated
#' # contour for rolloffNoise:
#' noise = generateNoise(len = samplingRate * 1.2,
#'   rolloffNoise = data.frame(time = c(0, .3, 1), value = c(-12, 0, -12)),
#'   noiseFlatSpec = 2000, samplingRate = samplingRate, play = playback)
#' # spectrogram(noise, samplingRate, osc = TRUE)
#'
#' # To create a sibilant [s], specify a single strong, broad formant at ~7 kHz:
#' windowLength_points = 1024
#' spectralEnvelope = soundgen:::getSpectralEnvelope(
#'   nr = windowLength_points / 2, nc = 1, samplingRate = samplingRate,
#'  formants = list('f1' = data.frame(time = 0, freq = 7000,
#'                                    amp = 50, width = 2000)))
#' noise = generateNoise(len = samplingRate,
#'   samplingRate = samplingRate, spectralEnvelope = as.numeric(spectralEnvelope),
#'   play = playback)
#' # plot(spectralEnvelope, type = 'l')
#'
#' # Low-frequency, wind-like noise
#' spectralEnvelope = soundgen:::getSpectralEnvelope(
#'   nr = windowLength_points / 2, nc = 1, lipRad = 0,
#'   samplingRate = samplingRate, formants = list('f1' = data.frame(
#'     time = 0, freq = 150, amp = 30, width = 90)))
#' noise = generateNoise(len = samplingRate,
#'   samplingRate = samplingRate, spectralEnvelope = as.numeric(spectralEnvelope),
#'   play = playback)
#'
#' # Manual filter, e.g. for a kettle-like whistle (narrow-band noise)
#' spectralEnvelope = c(rep(0, 100), 120, rep(0, 100))  # any length is fine
#' # plot(spectralEnvelope, type = 'b')  # notch filter at Nyquist / 2, here 4 kHz
#' noise = generateNoise(len = samplingRate, spectralEnvelope = spectralEnvelope,
#'   samplingRate = samplingRate, play = playback)
#'
#' # Compare to a similar sound created with soundgen()
#' # (unvoiced only, a single formant at 4 kHz)
#' noise_s = soundgen(pitch = NULL,
#'   noise = data.frame(time = c(0, 1000), value = c(0, 0)),
#'   formants = list(f1 = data.frame(freq = 4000, amp = 80, width = 20)),
#'   play = playback)
#'
#'
#' # Use the spectral envelope of an existing recording (bleating of a sheep)
#' # (see also the same example with tonal source in ?addFormants)
#' data(sheep, package = 'seewave')  # import a recording from seewave
#' sound_orig = as.numeric(sheep@left)
#' samplingRate = sheep@samp.rate
#' # playme(sound_orig, samplingRate)
#'
#' # extract the original spectrogram
#' windowLength = c(5, 10, 50, 100)[1]  # try both narrow-band (eg 100 ms)
#' # to get "harmonics" and wide-band (5 ms) to get only formants
#' spectralEnvelope = spectrogram(sound_orig, windowLength = windowLength,
#'   samplingRate = samplingRate, output = 'original')
#' sound_noise = generateNoise(len = length(sound_orig),
#'   spectralEnvelope = spectralEnvelope, rolloffNoise = 0,
#'   samplingRate = samplingRate, play = playback)
#' # playme(sound_noise, samplingRate)
#'
#' # The spectral envelope is similar to the original recording. Compare:
#' par(mfrow = c(1, 2))
#' seewave::meanspec(sound_orig, f = samplingRate, dB = 'max0')
#' seewave::meanspec(sound_noise, f = samplingRate, dB = 'max0')
#' par(mfrow = c(1, 1))
#' # However, the excitation source is now white noise
#' # (which sounds like noise if windowLength is ~5-10 ms,
#' # but becomes more and more like the original at longer window lengths)
#' }
generateNoise = function(len,
                         rolloffNoise = 0,
                         noiseFlatSpec = 1200,
                         spectralEnvelope = NULL,
                         noise = NULL,
                         temperature = .1,
                         attackLen = 10,
                         windowLength_points = 1024,
                         samplingRate = 16000,
                         overlap = 75,
                         dynamicRange = 80,
                         play = FALSE) {
  # wiggle pars
  if (temperature > 0) {  # set to 0 when called internally by soundgen()
    len = rnorm_bounded(n = 1,
                        mean = len,
                        sd = len * temperature * .5,
                        low = 0, high = samplingRate * 10,  # max 10 s
                        roundToInteger = TRUE)
    if (is.list(rolloffNoise)) {
      rolloffNoise = wiggleAnchors(
        rolloffNoise,
        temperature = temperature,
        temp_coef = .5,
        low = c(-Inf, permittedValues['rolloffNoise', 'low']),
        high = c(Inf, permittedValues['rolloffNoise', 'high']),
        wiggleAllRows = TRUE
      )
    } else {
      rolloffNoise = rnorm_bounded(
        n = length(rolloffNoise),
        mean = rolloffNoise,
        sd = abs(rolloffNoise) * temperature * .5,
        low = -20,
        high = 20
      )
    }
    noiseFlatSpec = rnorm_bounded(n = 1,
                                  mean = noiseFlatSpec,
                                  sd = noiseFlatSpec * temperature * .5,
                                  low = 0, high = samplingRate / 2)
    attackLen = rnorm_bounded(n = length(attackLen),
                              mean = attackLen,
                              sd = attackLen * temperature * .5,
                              low = 0, high = len / samplingRate * 1000 / 2)
    noise = wiggleAnchors(
      reformatAnchors(noise),
      temperature = temperature,
      temp_coef = .5,
      low = c(0, -dynamicRange),
      high = c(1, 0),
      wiggleAllRows = TRUE
    )
    if (is.vector(spectralEnvelope)) {
      spectralEnvelope = rnorm_bounded(
        n = length(spectralEnvelope),
        mean = spectralEnvelope + .1,  # to wiggle zeros
        sd = spectralEnvelope * temperature * .5,
        low = 0, high = Inf
      )
    }
  }

  # convert anchors to a smooth contour of breathing amplitudes
  if (is.list(noise)) {
    breathingStrength = getSmoothContour(
      len = len,
      anchors = noise,
      normalizeTime = FALSE,
      valueFloor = permittedValues['noiseAmpl', 'low'],
      valueCeiling = permittedValues['noiseAmpl', 'high'],
      samplingRate = samplingRate,
      plot = FALSE
    )
    # convert anchor amplitudes from dB to linear multipliers
    breathingStrength = 10 ^ (breathingStrength / 20)
    if (sum(is.na(breathingStrength)) > 0) {
      return(rep(0, len))
    }
    # plot(breathingStrength)
  } else {
    breathingStrength = rep(1, len)
  }

  # set up spectral filter
  step = seq(1,
             len + windowLength_points,
             by = windowLength_points - (overlap * windowLength_points / 100))
  # len + windowLength_points gives us two extra windows, since otherwise
  #   the sequence is a bit shorter than needed after i-fft
  nr = windowLength_points / 2
  nc = length(step)
  if (is.null(spectralEnvelope)) {
    # basic linear rolloff above noiseFlatSpec Hz
    bin = samplingRate / 2 / nr
    binsPerKHz = round(1000 / bin)
    flatBins = round(noiseFlatSpec / bin)
    idx = (flatBins + 1):nr  # the bins affected by rolloffNoise
    if (is.list(rolloffNoise) |
        (is.numeric(rolloffNoise) & length(rolloffNoise) > 1)) {
      rolloffNoise = getSmoothContour(
        anchors = rolloffNoise,
        len = nc,
        valueFloor = permittedValues['rolloffNoise', 'low'],
        valueCeiling = permittedValues['rolloffNoise', 'high']
      )
      spectralEnvelope = matrix(1, nrow = nr, ncol = nc)
      for (c in 1:nc) {
        spectralEnvelope[idx, c] = 10 ^ (rolloffNoise[c] / 20 * (idx - flatBins) / binsPerKHz)
      }
      # Johnson_2012_Acoustic-and-Auditory-Phonetics, Fig. 7.1: spectrum of turbulent noise
    } else {
      a = rep(1, nr)
      a[idx] = 10 ^ (rolloffNoise / 20 * (idx - flatBins) / binsPerKHz)
      spectralEnvelope = matrix(rep(a, nc), ncol = nc)
    }
    # image(t(spectralEnvelope))
  } else {
    # user-specified exact spectral envelope
    if (is.vector(spectralEnvelope)) {
      spectralEnvelope = spectralEnvelope[!is.na(spectralEnvelope)]
      if (length(spectralEnvelope) != nr) {
        # interpolate to correct freq resolution
        spectralEnvelope = getSmoothContour(spectralEnvelope, len = nr)
        # spectralEnvelope = approx(spectralEnvelope, n = nr, method = 'linear')$y
      }
      spectralEnvelope = matrix(rep(spectralEnvelope, nc), ncol = nc)
    } else {  # interpolate the matrix
      spectralEnvelope = na.omit(spectralEnvelope)
      if (ncol(spectralEnvelope) != nc | nrow(spectralEnvelope) != nr) {
        message('Incorrect dimensions of spectralEnvelope matrix. Interpolating...')
        spectralEnvelope = interpolMatrix(spectralEnvelope,
                                          nr = nr, nc = nc,
                                          interpol = 'approx')
      }
    }
  }
  # image(t(spectralEnvelope))
  # plot(spectralEnvelope[, 1], type = 'l')
  # plot(log10(spectralEnvelope[, 1]) * 20, type = 'l')

  if (sum(spectralEnvelope) == 0) {
    # zero filter - nothing to synthesize
    warning('These settings will result in silence!')
    breathing = rep(0, len)
  } else {
    ## instead of synthesizing the time series and then doing fft-ifft,
    #   we can simply synthesize spectral noise, convert to complex
    #   (setting imaginary=0), and then do inverse FFT just once
    # set up spectrum with white noise
    z1 = matrix(as.complex(runif(nr * nc)), nrow = nr, ncol = nc)
    # multiply by filter
    z1_filtered = z1 * spectralEnvelope
    # do inverse FFT
    breathing = as.numeric(
      seewave::istft(
        z1_filtered,
        f = samplingRate,
        ovlp = overlap,
        wl = windowLength_points,
        output = "matrix"
      )
    )
    breathing = matchLengths(breathing, len = len)  # pad with 0s or trim
    breathing = breathing / max(abs(breathing)) * breathingStrength # normalize

    # add attack
    if (is.numeric(attackLen)) {
      if (any(attackLen > 0)) {
        l = floor(attackLen * samplingRate / 1000)
        if (length(l) == 1) l = c(l, l)
        breathing = fade(
          breathing,
          fadeIn = l[1],
          fadeOut = l[2]
        )
      }
    }
  }
  # plot(breathing, type = 'l')
  if (play == TRUE) playme(breathing, samplingRate = samplingRate)
  if (is.character(play)) {
    playme(breathing, samplingRate = samplingRate, player = play)
  }
  # spectrogram(breathing, samplingRate = samplingRate)
  # seewave::meanspec(breathing, f = samplingRate, wl = windowLength_points, dB = 'max0')
  return(breathing)
}



#' Generate harmonics
#'
#' Internal soundgen function.
#'
#' Returns one continuous, unfiltered, voiced syllable consisting of several
#' sine waves.
#' @param pitch a contour of fundamental frequency (numeric vector). NB: for
#'   computational efficiency, provide the pitch contour at a reduced sampling
#'   rate pitchSamplingRate, eg 3500 points/s. The pitch contour will be
#'   upsampled before synthesis.
#' @inheritParams soundgen
#' @param pitchDriftDep scale factor regulating the effect of temperature on the
#'   amount of slow random drift of f0 (like jitter, but slower): the higher,
#'   the more f0 "wiggles" at a given temperature
#' @param pitchDriftFreq scale factor regulating the effect of temperature on
#'   the frequency of random drift of f0 (like jitter, but slower): the higher,
#'   the faster f0 "wiggles" at a given temperature
#' @param randomWalk_trendStrength try 0 to 1 - the higher, the more likely rw
#'   is to get high in the middle and low at the beginning and end (i.e. max
#'   effect amplitude in the middle of a sound)
#' @param rolloff_perAmpl as amplitude goes down from max to
#'   \code{-dynamicRange}, \code{rolloff} increases by \code{rolloff_perAmpl}
#'   dB/octave. The effect is to make loud parts brighter by increasing energy
#'   in higher frequencies
#' @param normalize if TRUE, normalizes to -1...+1 prior to applying attack and
#'   amplitude envelope. W/o this, sounds with stronger harmonics are louder
#' @keywords internal
#' @examples
#' rolloffExact1 = c(.2, .2, 1, .2, .2)
#' s1 = soundgen:::generateHarmonics(pitch = seq(400, 530, length.out = 1500),
#'                        rolloffExact = rolloffExact1)
#' spectrogram(s1, 16000, ylim = c(0, 4))
#' # playme(s1, 16000)
#'
#' rolloffExact2 = matrix(c(.2, .2, 1, .2, .2,
#'                          1, .5, .2, .1, .05), ncol = 2)
#' s2 = soundgen:::generateHarmonics(pitch = seq(400, 530, length.out = 1500),
#'                        rolloffExact = rolloffExact2)
#' spectrogram(s2, 16000, ylim = c(0, 4))
#' # playme(s2, 16000)
generateHarmonics = function(pitch,
                             glottis = 0,
                             attackLen = 50,
                             nonlinBalance = 0,
                             nonlinDep = 50,
                             nonlinRandomWalk = NULL,
                             jitterDep = 0,
                             jitterLen = 1,
                             vibratoFreq = 5,
                             vibratoDep = 0,
                             shimmerDep = 0,
                             shimmerLen = 1,
                             creakyBreathy = 0,
                             rolloff = -9,
                             rolloffOct = 0,
                             rolloffKHz = 0,
                             rolloffParab = 0,
                             rolloffParabHarm = 3,
                             rolloff_perAmpl = 0,
                             rolloffExact = NULL,
                             temperature = .025,
                             pitchDriftDep = .5,
                             pitchDriftFreq = .125,
                             amplDriftDep = 5,
                             subDriftDep = 4,
                             rolloffDriftDep = 3,
                             randomWalk_trendStrength = .5,
                             shortestEpoch = 300,
                             subFreq = 100,
                             subDep = 0,
                             ampl = NA,
                             normalize = TRUE,
                             interpol = c('approx', 'spline', 'loess')[3],
                             overlap = 75,
                             samplingRate = 16000,
                             pitchFloor = 75,
                             pitchCeiling = 3500,
                             pitchSamplingRate = 3500,
                             dynamicRange = 80) {
  ## PRE-SYNTHESIS EFFECTS (NB: the order in which effects are added is NOT arbitrary!)
  # vibrato (performed on pitch, not pitch_per_gc!)
  if (is.list(vibratoDep)) {
    if (any(vibratoDep$value > 0)) {
      lp = length(pitch)
      for (p in c('vibratoFreq', 'vibratoDep')) {
        old = get(p)
        if (length(old) > 1) {
          new = getSmoothContour(
            anchors = old,
            len = lp,
            valueFloor = permittedValues[p, 'low'],
            valueCeiling = permittedValues[p, 'high'],
            interpol = interpol)
          assign(p, new)
        }
      }
      vibrato = 2 ^ (sin(2 * pi * (1:length(pitch)) * vibratoFreq /
                           pitchSamplingRate) * vibratoDep / 12)
      # plot(vibrato[], type = 'l')
      pitch = pitch * vibrato  # plot (pitch, type = 'l')
    }
  }

  # transform f0 per s to f0 per glottal cycle
  gc = getGlottalCycles(pitch, samplingRate = pitchSamplingRate)  # our "glottal cycles"
  pitch_per_gc = pitch[gc]
  nGC = length(pitch_per_gc)
  # to avoid recalculating gc for each contour like jitterDep,
  # we can upsample them to length nGC and just take gc indices
  # scaled to length nGC instead of length(pitch)
  gc1 = ceiling(gc * nGC / length(pitch))

  # vectorized par-s should be upsampled and converted from ms to gc scale
  update_pars = c(
    'rolloff', 'rolloffOct', 'rolloffParab', 'rolloffParabHarm', 'rolloffKHz',
    'jitterDep', 'jitterLen', 'shimmerDep', 'shimmerLen', 'subFreq', 'subDep'
  )
  for (p in update_pars) {
    old = get(p)
    if (length(old) > 1) {
      new = getSmoothContour(
        anchors = old,
        len = nGC,
        valueFloor = permittedValues[p, 'low'],
        valueCeiling = permittedValues[p, 'high'],
        interpol = interpol)[gc1]
      assign(p, new)
    }
  }

  # generate a short amplitude contour to adjust rolloff per glottal cycle
  rolloffAmpl = rep(0, nGC)
  if (is.numeric(ampl) | is.list(ampl)) {
    if (any(ampl$value != 0)) {
      amplContour = getSmoothContour(
        anchors = ampl,
        len = nGC,
        valueFloor = -dynamicRange,
        valueCeiling = 0,
        samplingRate = samplingRate
      )
      # plot(amplContour, type='l')
      amplContour = (amplContour + dynamicRange) / dynamicRange - 1
      rolloffAmpl = amplContour * rolloff_perAmpl
    }
  }

  # get a random walk for intra-syllable variation
  if (temperature > 0 &
      (nonlinBalance < 100 | !is.null(nonlinRandomWalk))) {
    rw = getRandomWalk(
      len = nGC,
      rw_range = temperature,
      trend = c(randomWalk_trendStrength, -randomWalk_trendStrength),
      rw_smoothing = .3
    ) # plot(rw, type = 'l')
    rw = rw - mean(rw) + 1 # change mean(rw) to 1
    if (is.null(nonlinRandomWalk)) {
      rw_0_100 = zeroOne(rw) * 100
      # plot(rw_0_100, type = 'l')
      rw_bin = getIntegerRandomWalk(
        rw_0_100,
        nonlinBalance = nonlinBalance,
        minLength = ceiling(shortestEpoch / 1000 * pitch_per_gc)
        # minLength is shortestEpoch / period_ms, where
        #   period_ms = 1000 / pitch_per_gc
      )
    } else {
      # interpolate or downsample user-provided random walk by simple repetition,
      # so that its new length = nGC (overrides shortestEpoch)
      rw_bin = approx(nonlinRandomWalk, n = nGC, method = 'constant')$y
      # plot(rw_bin)
    }
    vocalFry_on = (rw_bin > 0) # when is vocal fry on? For ex., rw_bin==1
    #   sets vocal fry on only in regime 1, while rw_bin>0 sets vocal fry on
    #   in regimes 1 and 2 (i.e. together with jitter)
    jitter_on = shimmer_on = (rw_bin == 2)
  } else {
    rw = rep(1, nGC)
    vocalFry_on = jitter_on = shimmer_on = rep(TRUE, nGC)
  }

  # calculate jitter (random variation of F0)
  if (any(jitterDep > 0) & any(jitter_on)) {
    jitter_per_gc = wiggleGC(dep = jitterDep / 12,
                             len = jitterLen,
                             nGC = nGC,
                             pitch_per_gc = pitch_per_gc,
                             rw = rw,
                             effect_on = jitter_on)
    pitch_per_gc = pitch_per_gc * jitter_per_gc
    # plot(pitch_per_gc, type = 'l')
  }

  # calculate random drift of F0 (essentially the same as jitter but slow)
  if (temperature > 0) {
    # # illustration of the effects of temperature and number of gc's
    # #   on the amount of smoothing applied to the random drift of f0
    # library(reshape2)
    # library(plot3D)
    # df = expand.grid(temperature = seq(0, 1, length.out = 30), n_gc = seq(1, 1000, length.out = 30))
    # df$rw_smoothing = .9 - df$temperature / 8 - 1.2 / (1 + exp(-.008 * (df$n_gc - 10))) + .6 # 10 gc is "neutral"
    # out_pred = as.matrix(dcast(df, temperature~n_gc, value.var = "rw_smoothing"))
    # rownames(out_pred) = seq(0, 1, length.out = 30)
    # out_pred = out_pred[, -1]
    # persp3D (as.numeric(rownames(out_pred)), as.numeric(colnames(out_pred)), out_pred, theta=40, phi=50, zlab='rw_smoothing', xlab='Temperature', ylab='# of glottal cycles', colkey=FALSE, ticktype='detailed', cex.axis=0.75)
    rw_smoothing = .9 - temperature * pitchDriftFreq -
      1.2 / (1 + exp(-.008 * (length(pitch_per_gc) - 10))) + .6
    # rw_range is 1 semitone per second at temp = .05 and pitchDriftDep = .5 (defaults)
    rw_range = temperature * pitchDriftDep * 40 *  # 40 * .05 * .5 = 1
      length(pitch) / pitchSamplingRate / 12
    drift = getRandomWalk(
      len = length(pitch_per_gc),
      rw_range = rw_range,
      rw_smoothing = rw_smoothing,
      method = 'spline'
    )
    # we get a separate random walk for this slow drift of intonation.
    #   Its smoothness vs wiggleness depends on temperature and duration
    #   (in glottal cycles). For ex., temperature * 2 means that pitch will
    #   vary within one octave if temperature == 1
    drift = 2 ^ (drift - mean(drift)) # plot (drift, type = 'l')
    pitch_per_gc = pitch_per_gc * drift  # plot(pitch_per_gc, type = 'l')
  }

  # as a result of adding pitch effects, F0 might have dropped to indecent
  #   values, so we double-check and flatten
  pitch_per_gc[pitch_per_gc > pitchCeiling] = pitchCeiling
  pitch_per_gc[pitch_per_gc < pitchFloor] = pitchFloor

  ## prepare the harmonic stack
  if (is.null(rolloffExact)) {
    # calculate the number of harmonics to generate (from lowest pitch to Nyquist)
    nHarmonics = floor(samplingRate / 2 / min(pitch_per_gc))
    # get rolloff
    if (length(rolloff) < nGC) {
      rolloff = spline(rolloff, n = nGC)$y
    }
    rolloff_source = getRolloff(
      pitch_per_gc = pitch_per_gc,
      nHarmonics = nHarmonics,
      rolloff = (rolloff + rolloffAmpl) * rw ^ rolloffDriftDep,
      rolloffOct = rolloffOct,
      rolloffKHz = rolloffKHz,
      rolloffParab = rolloffParab,
      rolloffParabHarm = rolloffParabHarm,
      samplingRate = samplingRate,
      dynamicRange = dynamicRange
    )
  } else {
    rolloff_user = as.matrix(rolloffExact)
    rolloff_source = interpolMatrix(
      rolloff_user,
      nr = nrow(rolloff_user),  # don't change the number of harmonics
      nc = nGC,                 # interpolate over time
      interpol = 'approx'
    )
  }
  # NB: rolloff_source at this stage should be a matrix WITH NUMBERED ROWS

  # NB: this whole pitch_per_gc trick is purely for computational efficiency.
  #   The entire pitch contour can be fed in, but then it takes up to 1 s
  #   per s of audio
  # image(t(log(rolloff_source)))

  # add shimmer (random variation in amplitude)
  if (any(shimmerDep > 0) & any(shimmer_on)) {
    shimmer_per_gc = wiggleGC(dep = shimmerDep / 100,
                              len = shimmerLen,
                              nGC = nGC,
                              pitch_per_gc = pitch_per_gc,
                              rw = rw,
                              effect_on = shimmer_on)
    rolloff_source = t(t(rolloff_source) * shimmer_per_gc)  # multiplies the first
    # column of rolloff_source by shimmer_per_gc[1],
    # the second column by shimmer_per_gc[2], etc
  }

  # synthesize one glottal cycle at a time or a whole epoch at once?
  synthesize_per_gc = FALSE
  if (is.list(glottis)) {
    if (any(glottis$value > 0)) {
      synthesize_per_gc = TRUE
    }
  }

  # add vocal fry (subharmonics)
  if (!synthesize_per_gc &  # can't add subharmonics if doing one gc
      # at a time (one f0 period)
      any(subDep > 0) & any(vocalFry_on)) {
    vocalFry = getVocalFry(
      rolloff = rolloff_source,
      pitch_per_gc = pitch_per_gc,
      subFreq = subFreq * rw ^ subDriftDep,
      subDep = subDep * rw ^ subDriftDep * vocalFry_on,
      shortestEpoch = shortestEpoch,
      dynamicRange = dynamicRange
    )
    rolloff_source = vocalFry$rolloff # list of matrices
    epochs = vocalFry$epochs # dataframe
  } else {
    # if we don't need to add vocal fry
    rolloff_source = list(rolloff_source)
    epochs = data.frame ('start' = 1, 'end' = length(pitch_per_gc))
  }

  ## WAVEFORM GENERATION
  if (synthesize_per_gc) {
    # synthesize one glottal cycle at a time
    r = rolloff_source
    for (e in 1:length(rolloff_source)) {
      r[[e]] = rolloff_source[[e]]
      r[[e]] = as.list(as.data.frame(r[[e]]))
      for (i in 1:length(r[[e]])) {
        r[[e]][[i]] = matrix(r[[e]][[i]],
                             ncol = 1,
                             dimnames = list(rownames(rolloff_source[[e]])))
      }
    }
    r = unlist(r, recursive = FALSE)  # get rid of epochs
    glottisClosed_per_gc = getSmoothContour(anchors = glottis,
                                            len = nGC,
                                            valueFloor = 0)
    waveform = generateGC(pitch_per_gc = pitch_per_gc,
                          glottisClosed_per_gc = glottisClosed_per_gc,
                          rolloff_per_gc = r,
                          samplingRate = samplingRate)
  } else {
    # synthesize continuously
    waveform = generateEpoch(pitch_per_gc = pitch_per_gc,
                             epochs = epochs,
                             rolloff_per_epoch = rolloff_source,
                             samplingRate = samplingRate)
  }
  # sum(is.na(waveform))
  # plot(waveform[], type = 'l')
  # spectrogram(waveform, samplingRate = samplingRate, osc = TRUE)
  # playme(waveform, samplingRate = samplingRate)
  # seewave::meanspec(waveform, f = samplingRate)

  ## POST-SYNTHESIS EFFECTS
  # add attack
  if (is.numeric(attackLen)) {
    if (any(attackLen > 0)) {
      l = floor(attackLen * samplingRate / 1000)
      if (length(l) == 1) l = c(l, l)
      waveform = fade(waveform,
                      fadeIn = l[1],
                      fadeOut = l[2])
      # plot(waveform, type = 'l')
    }
  }

  # normalize to be on the same scale as breathing (NB: after adding attack,
  # b/c fading the ends can change the overall range if eg peak ampl is at the beg.)
  if (normalize) waveform = waveform / max(abs(waveform))

  # apply amplitude envelope
  if (is.numeric(ampl) | is.list(ampl)) {
    if (any(ampl$value != 0)) {
      amplEnvelope = getSmoothContour(
        anchors = ampl,
        len = length(waveform),
        valueFloor = -dynamicRange,
        samplingRate = samplingRate
      )
      # plot(amplEnvelope, type = 'l')
      # convert from dB to linear multiplier
      amplEnvelope = 10 ^ (amplEnvelope / 20)
      waveform = waveform * amplEnvelope
    }
  }

  # pitch drift is accompanied by amplitude drift
  if (temperature > 0 & amplDriftDep > 0) {
    gc_upsampled = upsample(pitch_per_gc, samplingRate = samplingRate)$gc
    drift_upsampled = approx(drift,
                             n = length(waveform),
                             x = gc_upsampled[-length(gc_upsampled)])$y
    waveform = waveform * drift_upsampled ^ amplDriftDep
    # plot(drift_upsampled, type = 'l')
  }
  # playme(waveform, samplingRate = samplingRate)
  # spectrogram(waveform, samplingRate = samplingRate)
  return(waveform)
}



#' Generate glottal cycles
#'
#' Internal soundgen function.
#' Takes descriptives of a number of glottal cycles (f0, closed phase, rolloff -
#' note that all three should be vectors of the same length, namely nGC) and
#' creates a waveform consisting of a string of these glottal cycles separated
#' by pauses (if there is a closed phase). The principle is to work with one
#' glottal cycle at a time and create a sine wave for each harmonic, with
#' amplitudes adjusted by rolloff.
#' @param pitch_per_gc pitch per glottal cycle, Hz
#' @param glottisClosed_per_gc proportion of closed phase per glottal cycle, \%
#' @param rolloff_per_gc a list of one-column matrices, one for each glottal
#'   cycle, specifying rolloff per harmonic (linear multiplier, ie NOT in dB)
#'   Each matrix has as many rows as there are harmonics, and rownames specify
#'   the ratio to F0 (eg 1.5 means it's a subharmonic added between f0 and its
#'   first harmonic)
#' @param samplingRate the sampling rate of generated sound, Hz
#' @param wn windowing function applied to each glottal cycle (see ftwindow_modif)
#' @return Returns a waveform as a non-normalized numeric vector centered at zero.
#' @keywords internal
#' @examples
#' pitch_per_gc = seq(100, 150, length.out = 25)
#' glottisClosed_per_gc = seq(0, 300, length.out = 25)
#' m = matrix(10 ^ (-6 * log2(1:200) / 20))
#' rownames(m) = 1:nrow(m)
#' rolloff_per_gc = rep(list(m), 25)
#' s = soundgen:::generateGC(pitch_per_gc, glottisClosed_per_gc,
#'                           rolloff_per_gc, samplingRate = 16000)
#' # plot(s, type = 'l')
#' # playme(s)
generateGC = function(pitch_per_gc,
                      glottisClosed_per_gc,
                      rolloff_per_gc,
                      samplingRate,
                      wn = 'none') {
  gc_len = round(samplingRate / pitch_per_gc)  # length of each gc, points
  gc_closed = round(gc_len * glottisClosed_per_gc / 100)  # length of each pause, points

  # adjust nGC to have ~the same duration as w/o closed phase
  nGC_orig = length(pitch_per_gc)
  dur_target = sum(gc_len)
  dur_with_closed = dur_target + sum(gc_closed)
  nGC = round(dur_target / dur_with_closed * length(pitch_per_gc))
  idx = round(seq(1, nGC_orig, length.out = nGC))
  pitch_per_gc_adj = pitch_per_gc[idx]
  glottisClosed_per_gc_adj = glottisClosed_per_gc[idx]
  rolloff_per_gc_adj = rolloff_per_gc[idx]
  gc_len_adj = round(samplingRate / pitch_per_gc_adj)
  gc_closed_adj = round(gc_len_adj * glottisClosed_per_gc_adj / 100)
  # gc_closed_adj[nGC] = 1  # don't add silence after the last gc

  # synthesize one glottal cycle at a time
  waveform = 0
  for (i in 1:nGC) {
    cycle = 0
    idx = 0:(gc_len_adj[i] - 1)  # count from zero, ensuring the gc starts at sin(0) = 0
    for (h in 1:nrow(rolloff_per_gc_adj[[i]])) {
      times_f0 = as.numeric(rownames(rolloff_per_gc_adj[[i]])[h]) # freq of harmonic h
      cycle = cycle +
        sin(2 * pi * pitch_per_gc_adj[i] * times_f0 * idx / samplingRate) *
        rolloff_per_gc_adj[[i]][h]
    }
    # plot(cycle, type = 'l')
    # spectrogram(cycle, samplingRate, ylim = c(0, 2))
    # seewave::spec(rep(cycle,10), f = samplingRate, flim = c(0, 2), alim = c(-50, 0), dB = 'max0')

    if (wn == 'none') {
      waveform = c(waveform, cycle, rep(0, gc_closed_adj[i]))
    } else {
      # window before glueing gc with pauses
      win = ftwindow_modif(wl = gc_len_adj[i], wn = wn)
      # plot(win, type = 'b')
      cycle_win = cycle * win
      # plot(cycle_win, type = 'l')
      # spectrogram(cycle_win, samplingRate, ylim = c(0, 10))
      # seewave::spec(rep(cycle_win,10), f = samplingRate, flim = c(0, 2), alim = c(-50, 0), dB = 'max0')
      waveform = c(waveform, cycle_win, rep(0, gc_closed_adj[i]))
    }
  }
  # plot(waveform, type = 'l')
  # playme(waveform, samplingRate)
  # spectrogram(waveform, samplingRate, ylim = c(0, 10))
  # seewave::spec(waveform, f = samplingRate, flim = c(0, 2), alim = c(-50, 0), dB = 'max0')
  return(waveform)
}


#' Generate an epoch
#'
#' Internal soundgen function.
#' Takes descriptives of a number of glottal cycles (f0, closed phase, rolloff)
#' and creates a continuous waveform. The principle is to work with one epoch
#' with stable regime of subharmonics at a time and create a sine wave for each
#' harmonic, with amplitudes adjusted by rolloff.
#' @param pitch_per_gc pitch per glottal cycle, Hz
#' @param rolloff_per_epoch a list of matrices with one matrix for each epoch; each
#'   matrix should contain one column for each glottal cycle and one row for
#'   each harmonic (linear multiplier, ie NOT in dB). Rownames specify the ratio
#'   to F0 (eg 1.5 means it's a subharmonic added between f0 and its first
#'   harmonic)
#' @param epochs a dataframe specifying the beginning and end of each epoch
#' @param samplingRate the sampling rate of generated sound, Hz
#' @return Returns a waveform as a non-normalized numeric vector centered at zero.
#' @keywords internal
#' @examples
#' pitch_per_gc = seq(100, 150, length.out = 90)
#' epochs = data.frame (start = c(1, 51),
#'                      end = c(50, 90))
#' m1 = matrix(rep(10 ^ (-6 * log2(1:200) / 20), 50), ncol = 50, byrow = FALSE)
#' m2 = matrix(rep(10 ^ (-12 * log2(1:200) / 20), 40), ncol = 40, byrow = FALSE)
#' rownames(m1) = 1:nrow(m1)
#' rownames(m2) = 1:nrow(m2)
#' rolloff_source = list(m1, m2)
#' s = soundgen:::generateEpoch(pitch_per_gc, epochs,
#'                              rolloff_source, samplingRate = 16000)
#' # plot(s, type = 'l')
#' # playme(s)
generateEpoch = function(pitch_per_gc,
                         epochs,
                         rolloff_per_epoch,
                         samplingRate) {
  # Upsample pitch contour to full resolution (samplingRate).
  #   Uses a more sophisticated but still very fast version of linear
  #   interpolation, which takes into account the variable length
  #   of glottal cycles
  up = upsample(pitch_per_gc, samplingRate = samplingRate)
  pitch_upsampled = up$pitch
  gc_upsampled = up$gc
  integr = cumsum(pitch_upsampled) / samplingRate
  waveform = 0

  # synthesize one epoch at a time and join by cross-fading
  for (e in 1:nrow(epochs)) {
    idx_gc_up = gc_upsampled[epochs$start[e]:(epochs$end[e] + 1)]
    idx_up = min(idx_gc_up):max(idx_gc_up)
    waveform_epoch = 0
    integr_epoch = integr[idx_up]
    rolloff_epoch = rolloff_per_epoch[[e]]  # rolloff_source MUST be a list!

    for (h in 1:nrow(rolloff_epoch)) {
      # NB: rolloff_source can have fewer
      # harmonics that nHarmonics, since weak harmonics are discarded for
      # computational efficiency. Or it can have a lot more than nHarmonics,
      # if we add vocal fry
      times_f0 = as.numeric(rownames(rolloff_epoch)[h]) # freq of harmonic h
      # as a multiple of f0
      am_upsampled = approx(rolloff_epoch[h, ],
                            n = length(idx_up),
                            x = idx_gc_up[-length(idx_gc_up)])$y
      # plot(am_upsampled, type = 'l')
      # the actual waveform synthesis happens HERE:
      waveform_epoch = waveform_epoch +
        sin(2 * pi * integr_epoch * times_f0) * am_upsampled
    }
    waveform = crossFade(waveform,
                         waveform_epoch,
                         samplingRate = samplingRate,
                         crossLen = 15) # longer crossLen
    # provides for smoother transitions, but it shortens the resulting sound.
    # Short transitions preserve sound duration but may create a click
    # where two epochs are joined
  }
  # plot(waveform, type = 'l')
  # playme(waveform, samplingRate)
  return(waveform)
}

#' Fart
#'
#' While the same sounds can be created with soundgen(), this facetious function
#' produces the same effect more efficiently and with very few control
#' parameters. With default settings, execution time is ~ 10 ms per second of
#' audio sampled at 16000 Hz. Principle: creates separate glottal cycles with
#' harmonics, but no formants. See \code{\link{soundgen}} for more details.
#' @inheritParams soundgen
#' @param sylLen syllable length, ms (not vectorized)
#' @param rolloff rolloff of harmonics in source spectrum, dB/octave (not
#'   vectorized)
#' @param plot if TRUE, plots the waveform
#' @return Returns a normalized waveform.
#' @export
#' @examples
#' f = fart()
#' # playme(f)
#'
#' \dontrun{
#' while (TRUE) {
#'   fart(sylLen = 300, temperature = .5, play = TRUE)
#'   Sys.sleep(rexp(1, rate = 1))
#' }
#' }
fart = function(glottis = c(50, 200),
                pitch = 65,
                temperature = 0.25,
                sylLen = 600,
                rolloff = -10,
                samplingRate = 16000,
                play = FALSE,
                plot = FALSE) {
  glottis = reformatAnchors(glottis)
  pitch = reformatAnchors(pitch)

  # wiggle pars
  if (temperature > 0) {
    rolloff = rnorm_bounded(n = 1,
                            mean = rolloff,
                            sd = rolloff * temperature * .5,
                            low = -50, high = 10)
    sylLen = rnorm_bounded(n = 1,
                           mean = sylLen,
                           sd = sylLen * temperature * .5,
                           low = 0, high = 10000)

    glottis = wiggleAnchors(
      glottis, temperature, temp_coef = .5,
      low = c(0, 0), high = c(1, 10000), wiggleAllRows = TRUE
      )
    pitch = wiggleAnchors(
      pitch, temperature, temp_coef = 1,
      low = c(0, 0), high = c(1, 10000), wiggleAllRows = TRUE
      )
  }

  # prepare pitch contour
  pitch = getSmoothContour(anchors = pitch,
                           len = round(sylLen * 3500 / 1000))

  # synthesize the sound
  s = generateHarmonics(
    pitch = pitch,
    glottis = glottis,
    rolloff = rolloff,
    samplingRate = samplingRate,
    pitchSamplingRate = 3500
  )
  # plot(s, type = 'l')
  # spectrogram(f, osc = T, samplingRate = samplingRate, ylim = c(0, 2))
  # seewave::spec(f, f = samplingRate, dB = 'max0')

  # amplitude drift
  if (temperature > 0) {
    drift = getRandomWalk(
      len = 100,
      rw_range = temperature * 10,
      rw_smoothing = .2,
      method = 'spline'
    ) + 1
    # plot(drift, type = 'l')
    drift_upsampled = approx(drift,
                             n = length(s))$y
    s = s * drift_upsampled
  }

  s = s / max(abs(s))

  if (play) playme(s, samplingRate)
  if (is.character(play)) {
    playme(s, samplingRate = samplingRate, player = play)
  }
  if (plot) {
    time = (1:length(s)) / samplingRate * 1000
    plot(time, s, type = 'l', xlab = 'Time, ms')
  }
  return(s)
}


#' Generate beat
#'
#' Generates percussive sounds from clicks through drum-like beats to sliding
#' tones. The principle is to create a sine wave with rapid frequency modulation
#' and to add a fade-out. No extra harmonics or formants are added. For this
#' specific purpose, this is vastly faster and easier than to tinker with
#' \code{\link{soundgen}} settings, especially since percussive syllables tend
#' to be very short.
#' @inheritParams soundgen
#' @param nSyl the number of syllables to generate
#' @param sylLen average duration of each syllable, ms
#' @param pauseLen average duration of pauses between syllables, ms
#' @param pitch fundamental frequency, Hz - a vector or data.frame(time = ...,
#'   value = ...)
#' @param fadeOut if TRUE, a linear fade-out is applied to the entire syllable
#' @return Returns a non-normalized waveform centered at zero.
#' @export
#' @examples
#' playback = c(TRUE, FALSE)[2]
#' # a drum-like sound
#' s = beat(nSyl = 1, sylLen = 200,
#'          pitch = c(200, 100), play = playback)
#' # plot(s, type = 'l')
#'
#' # a dry, muted drum
#' s = beat(nSyl = 1, sylLen = 200,
#'          pitch = c(200, 10), play = playback)
#'
#' # sci-fi laser guns
#' s = beat(nSyl = 3, sylLen = 300,
#'          pitch = c(1000, 50), play = playback)
#'
#' # machine guns
#' s = beat(nSyl = 10, sylLen = 10, pauseLen = 50,
#'          pitch = c(2300, 300), play = playback)
beat = function(nSyl = 10,
                sylLen = 200,
                pauseLen = 50,
                pitch = c(200, 10),
                samplingRate = 16000,
                fadeOut = TRUE,
                play = FALSE) {
  len = sylLen * samplingRate / 1000
  pitchContour = getSmoothContour(anchors = pitch,
                                  len = len,
                                  valueFloor = 0,
                                  thisIsPitch = TRUE)
  int = cumsum(pitchContour)
  beat = sin(2 * pi * int / samplingRate)
  if (fadeOut) {
    beat = fade(beat, fadeOut = length(beat))
  }
  # plot(beat, type = 'l')
  # spectrogram(beat, samplingRate, ylim = c(0, 1))
  if (nSyl > 1) {
    pause = rep(0, round(pauseLen / 1000 * samplingRate))
    beat = rep(c(beat, pause), nSyl)
  }
  beat = beat / max(abs(beat))  # normalize
  if (play == TRUE) playme(beat, samplingRate)
  if (is.character(play)) {
    playme(beat, samplingRate = samplingRate, player = play)
  }
  return(beat)
}
