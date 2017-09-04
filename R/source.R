# Functions for generating excitation source: either noise with generateNoise() or harmonics with generateHarmonics()

#' Generate noise
#'
#' Internal soundgen function.
#'
#' Generates white noise of length \code{len} and with spectrum defined by
#' exponential decay \code{rolloffNoise} and/or a specified filter
#' \code{filterNoise}. Algorithm: paints a spectrum with desired
#' characteristics, sets phase to zero, and generates a time sequence via
#' inverse FFT. Noise can then be used as an additional source to be added to
#' the glottal source AFTER the glottal source has been formant-filtered, or
#' BEFORE formant-filtering for glottal breathing noise.
#' @param len length of output
#' @param noiseAnchors a dataframe specifying the amplitude envelope of
#'   output. $time: timing of aspiration noise, ms c(start,finish) relative to
#'   voiced part, eg c(-100,500) means breathing starts 100 ms before the voiced
#'   part and lasts until 500 ms into the voiced part (eg total duration of
#'   breathing = 500 - (-100) = 600 ms). noiseAnchors$value: the amount of
#'   aspiration noise at the given time anchors (to be smoothed). throwaway =
#'   no breathing, 0 = as strong as the voiced (harmonic) part
#' @inheritParams soundgen
#' @param windowLength_points the length of fft window, points
#' @param filterNoise (optional): in addition to using rolloffNoise,
#'   we can provide the exact filter - a vector of length windowLength_points/2
#'   or, if we want moving formants, a matrix with windowLength_points/2 rows
#'   and an arbitrary number of columns
#' @keywords internal
#' @examples
#' # 1 s of white noise
#' samplingRate = 16000
#' noise = soundgen:::generateNoise(len = samplingRate,
#'   rolloffNoise = 0, samplingRate = samplingRate)
#' # playme(noise, samplingRate = samplingRate)
#' # 1 s of noise with rolloff -6 dB
#' noise = soundgen:::generateNoise(len = samplingRate,
#'   rolloffNoise = -6, samplingRate = samplingRate)
#'
#' # To create a sibilant [s], specify a single strong, broad formant at ~7 kHz:
#' windowLength_points = 1024
#' filterNoise = soundgen:::getSpectralEnvelope(
#'   nr = windowLength_points / 2, nc = 1, samplingRate = samplingRate,
#'  formants = list('f1' = data.frame(time = 0, freq = 7000,
#'                                    amp = 50, width = 2000)))
#' noise = soundgen:::generateNoise(len = samplingRate, rolloffNoise = -12,
#'   samplingRate = samplingRate, filterNoise = filterNoise)
#' # plot (filterNoise, type = 'l')
#' # playme(noise, samplingRate = samplingRate)
#'
#' # low-frequency, wind-like noise
#' filterNoise = soundgen:::getSpectralEnvelope(
#'   nr = windowLength_points / 2, nc = 1, rolloffLip = 0,
#'   samplingRate = samplingRate, formants = list('f1' = data.frame(
#'     time = 0, freq = 150, amp = 30, width = 90)))
#' noise = soundgen:::generateNoise(len = samplingRate, rolloffNoise = -12,
#'   samplingRate = samplingRate, filterNoise = filterNoise)
generateNoise = function(len,
                         noiseAnchors = data.frame(
                           'time' = c(0, 300),
                           'value' = c(-120, -120)
                         ),
                         rolloffNoise = -6,
                         attackLen = 10,
                         windowLength_points = 1024,
                         samplingRate = 16000,
                         overlap = 75,
                         throwaway = -120,
                         filterNoise = NA) {
  # convert anchors to a smooth contour of breathing amplitudes
  breathingStrength = getSmoothContour(
    len = len,
    anchors = noiseAnchors,
    valueFloor = permittedValues['noiseAmpl', 'low'],
    valueCeiling = permittedValues['noiseAmpl', 'high'],
    samplingRate = samplingRate,
    plot = FALSE
  )
  # plot(breathingStrength)

  # convert anchor amplitudes from dB to linear multipliers
  breathingStrength = 2 ^ (breathingStrength / 10)

  if (sum(is.na(breathingStrength)) > 0) {
    return(rep(0, len))
  }

  # set up spectral filter
  step = seq(1,
             len + windowLength_points,
             by = windowLength_points - (overlap * windowLength_points / 100))
  # len + windowLength_points gives us two extra windows, since otherwise
  #   the sequence is a bit shorter than needed after i-fft
  nr = windowLength_points / 2
  nc = length(step)
  if (is.na(filterNoise[1])) {
    filterNoise = matrix(rep (1, nr), nrow = 1)
    filterRowIdx = rep(1, nc)
  } else {
    filterRowIdx = round(seq(1, ncol(filterNoise), length.out = nc))
  }
  # modify the exact filter (if provided) by adding the specified
  #   basic exponential rolloff
  filterNoise = apply(filterNoise, 2, function(x) {
    x * 2 ^ (rolloffNoise / 10 * log2(1:nr))
  })
  # plot(filterNoise[,1], type = 'l')

  # instead of synthesizing the time series and then doing fft-ifft,
  #   we can simply synthesize spectral noise, convert to complex
  #   (setting imaginary=0), and then do inverse FFT just once
  z1 = matrix(as.complex(runif(nr * nc)), nrow = nr, ncol = nc)  # set up spectrum
  z1_filtered = apply(matrix(1:ncol(z1)), 1, function(x) {
    z1[, x] * filterNoise[, filterRowIdx[x]]
  })  # multiply by filter
  breathing = as.numeric (
    seewave::istft(
      z1_filtered,
      f = samplingRate,
      ovlp = overlap,
      wl = windowLength_points,
      output = "matrix"
    )
  )  # inverse FFT
  breathing = matchLengths(breathing, len = len)  # pad with 0s or trim
  breathing = breathing / max(breathing) * breathingStrength # normalize
  breathing = fadeInOut(
    breathing,
    do_fadeIn = TRUE,
    do_fadeOut = TRUE,
    length_fade = floor(attackLen * samplingRate / 1000)
  )  # add attack
  # plot(breathing, type = 'l')
  # playme(breathing, samplingRate = samplingRate)
  # spectrogram(breathing, samplingRate = samplingRate)
  # seewave::meanspectrogram(breathing, f = samplingRate, wl = windowLength_points,
  #   dB = 'max0')
  return (breathing)
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
#'   is to get high in the middle and low at the beginning and end (ie max
#'   effect amplitude in the middle of a sound)
#' @param rolloff_perAmpl as amplitude goes down from max to
#'   \code{throwaway}, \code{rolloff} increases by \code{rolloff_perAmpl}
#'   dB/octave. The effect is to make loud parts brighter by increasing energy
#'   in higher frequencies
#' @keywords internal
#' @examples
#' pitch=soundgen:::getSmoothContour(len = 3500,
#'   anchors = data.frame('time' = c(0, 1), 'value' = c(200, 300)))
#' plot(pitch)
#' sound = soundgen:::generateHarmonics(pitch, samplingRate = 16000)
#' # playme(sound, samplingRate = 16000) # no formants yet
generateHarmonics = function(pitch,
                             attackLen = 50,
                             nonlinBalance = 0,
                             nonlinDep = 0,
                             jitterDep = 0,
                             jitterLen = 1,
                             vibratoFreq = 100,
                             vibratoDep = 0,
                             shimmerDep = 0,
                             creakyBreathy = 0,
                             rolloff = -18,
                             rolloffOct = -2,
                             rolloffKHz = -6,
                             rolloffParab = 0,
                             rolloffParabHarm = 3,
                             rolloffLip = 6,
                             rolloff_perAmpl = 12,
                             temperature = 0,
                             pitchDriftDep = .5,
                             pitchDriftFreq = .125,
                             randomWalk_trendStrength = .5,
                             shortestEpoch = 300,
                             subFreq = 100,
                             subDep = 0,
                             amDep = 0,
                             amFreq = 30,
                             amplAnchors = NA,
                             overlap = 75,
                             samplingRate = 16000,
                             pitchFloor = 75,
                             pitchCeiling = 3500,
                             pitchSamplingRate = 3500,
                             throwaway = -120) {
  ## PRE-SYNTHESIS EFFECTS (NB: the order in which effects are added is NOT arbitrary!)
  # vibrato (performed on pitch, not pitch_per_gc!)
  if (vibratoDep > 0) {
    vibrato = 2 ^ (sin(2 * pi * (1:length(pitch)) * vibratoFreq /
                         pitchSamplingRate) * vibratoDep / 12)
    # plot(vibrato[], type = 'l')
    pitch = pitch * vibrato  # plot (pitch, type = 'l')
  }

  # transform f0 per s to f0 per glottal cycle
  gc = getGlottalCycles(pitch, samplingRate = pitchSamplingRate)  # our "glottal cycles"
  pitch_per_gc = pitch[gc]
  nGC = length(pitch_per_gc)

  # generate a short amplitude contour to adjust rolloff per glottal cycle
  if (!is.na(amplAnchors) &&
      length(which(amplAnchors$value < -throwaway)) > 0) {
    amplContour = getSmoothContour(
      anchors = amplAnchors,
      len = nGC,
      valueFloor = 0,
      valueCeiling = -throwaway,
      samplingRate = samplingRate
    )
    # plot (amplContour, type='l')
    amplContour = amplContour / abs(throwaway) - 1
    rolloffAmpl = amplContour * rolloff_perAmpl
  } else {
    rolloffAmpl = 0
  }

  # get a random walk for intra-syllable variation
  if (temperature > 0) {
    rw = getRandomWalk(
      len = nGC,
      rw_range = temperature,
      trend = c(randomWalk_trendStrength, -randomWalk_trendStrength),
      rw_smoothing = .3
    ) # plot (rw, type = 'l')
    rw_0_100 = zeroOne(rw) * 100
    # plot (rw_0_100, type = 'l')
    rw_bin = getIntegerRandomWalk(
      rw_0_100,
      nonlinBalance = nonlinBalance,
      minLength = ceiling(shortestEpoch / 1000 * pitch_per_gc)
    )
    # minLength is shortestEpoch / period_ms, where
    #   period_ms = 1000 / pitch_per_gc
    rw = rw - mean(rw) + 1 # change mean(rw) to 1
    vocalFry_on = (rw_bin > 0) # when is vocal fry on? For ex., rw_bin==1
    #   sets vocal fry on only in regime 1, while rw_bin>0 sets vocal fry on
    #   in regimes 1 and 2 (ie together with jitter)
    jitter_on = shimmer_on = (rw_bin == 2)
  } else {
    rw = rep(1, nGC)
    vocalFry_on = jitter_on = shimmer_on = rep(1, nGC)
  }

  # calculate jitter (random variation of F0)
  if (jitterDep > 0 & nonlinBalance > 0) {
    ratio = pitch_per_gc * jitterLen / 1000 # the number of gc that make
    #   up one jitter period (vector of length nGC)
    idx = 1
    i = 1
    while (i < nGC) {
      i = tail(idx, 1) + ratio[i]
      idx = c(idx, i)
    }
    idx = round(idx)
    idx = idx[idx <= nGC] # pitch for these gc will be wiggled
    idx = unique(idx)

    jitter = 2 ^ (rnorm(
      n = length(idx),
      mean = 0,
      sd = jitterDep / 12
    ) * rw[idx] * jitter_on[idx])
    # plot(jitter, type = 'l')
    # jitter_per_gc = approx(jitter, n = nGC, x = idx, method = 'constant')$y
    jitter_per_gc = spline(jitter, n = nGC, x = idx)$y
    # plot(jitter_per_gc, type = 'l')
    # a simpler alternative: jitter = 2^(rnorm(n=length(pitch_per_gc), mean=0, sd=jitterDep/12)*rw*jitter_on)
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
    rw_range = temperature * pitchDriftDep +
      length(pitch_per_gc) / 1000 / 12
    drift = getRandomWalk (
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
  # calculate the number of harmonics to generate (from lowest pitch to nyquist)
  nHarmonics = ceiling((samplingRate / 2 - min(pitch_per_gc)) / min(pitch_per_gc))
  # get rolloff
  rolloff_source = getRolloff(
    pitch_per_gc = pitch_per_gc,
    nHarmonics = nHarmonics,
    rolloff = (rolloff + rolloffAmpl) * rw ^ 3,
    rolloffOct = rolloffOct * rw ^ 3,
    rolloffKHz = rolloffKHz * rw,
    rolloffParab = rolloffParab,
    rolloffParabHarm = rolloffParabHarm,
    samplingRate = samplingRate,
    throwaway = throwaway
  )
  # NB: this whole pitch_per_gc trick is purely for computational efficiency.
  #   The entire pitch contour can be fed in, but then it takes up to 1 s
  #   per s of audio
  # image(t(rolloff_source))

  # add shimmer (random variation in amplitude)
  if (shimmerDep > 0 & nonlinBalance > 0) {
    shimmer = 2 ^ (rnorm (
      n = ncol(rolloff_source),
      mean = 0,
      sd = shimmerDep / 100
    ) * rw * shimmer_on)
    # plot(shimmer, type = 'l')
    rolloff_source = t(t(rolloff_source) * shimmer)  # multiplies the first
    # column of rolloff_source by shimmer[1], the second column by shimmer[2], etc
  }

  # add vocal fry (subharmonics)
  if (subDep > 0 & nonlinBalance > 0) {
    vocalFry = getVocalFry(
      rolloff = rolloff_source,
      pitch_per_gc = pitch_per_gc,
      subFreq = subFreq * rw ^ 4,
      subDep = subDep * rw ^ 4 * vocalFry_on,
      shortestEpoch = shortestEpoch,
      throwaway = throwaway
    )
    rolloff_source = vocalFry$rolloff # list of matrices
    epochs = vocalFry$epochs # dataframe
  } else {
    # if we don't need to add vocal fry
    rolloff_source = list(rolloff_source)
    epochs = data.frame ('start' = 1, 'end' = length(pitch_per_gc))
  }

  ## WAVEFORM GENERATION
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
    waveform_epoch = rep(0, length(idx_up))
    integr_epoch = integr[idx_up]
    rolloff_epoch = rolloff_source[[e]]  # rolloff_source MUST be a list!

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
      # # normalize amplitude to avoid abrupt amplitude jumps as subharmonics regime changes
      # if (e > 1) {
      #   max_prev = max(tail(ampl, samplingRate * .1)) # over the last ... ms of the previous epoch
      #   max_new = max(head(ampl_epoch, samplingRate * .1)) # over the first ... ms of the new epoch
      #   ampl_epoch = ampl_epoch * max_prev / max_new
      #   RMS_prev = sqrt(mean(tail(ampl, samplingRate * .1)^2)) # over the last ... ms of the previous epoch
      #   RMS_new = sqrt(mean(head(ampl_epoch, samplingRate * .1)^2)) # over the first ... ms of the new epoch
      #   ampl_epoch = ampl_epoch * RMS_prev / RMS_new
      # }
    }
    waveform = crossFade(waveform,
                         waveform_epoch,
                         samplingRate = samplingRate,
                         crossLen = 15) # longer crossLen
    # provides for smoother transitions, but it shortens the resulting sound.
    # Short transitions preserve sound duration but may create a click
    # where two epochs are joined
  }
  # sum(is.na(waveform))
  # plot(waveform[], type = 'l')
  # spectrogram(waveform, samplingRate = samplingRate, osc = TRUE)
  # playme(waveform, samplingRate = samplingRate)
  # seewave::meanspectrogram(waveform, f = samplingRate)

  ## POST-SYNTHESIS EFFECTS
  # apply amplitude envelope and normalize to be on the same scale as breathing
  if (!is.na(amplAnchors) &&
      length(which(amplAnchors$value < -throwaway)) > 0) {
    amplEnvelope = getSmoothContour(
      anchors = amplAnchors,
      len = length(waveform),
      valueFloor = 0,
      samplingRate = samplingRate
    )
    # plot (amplEnvelope, type = 'l')
    # convert from dB to linear multiplier
    amplEnvelope = 2 ^ (amplEnvelope / 10)
    waveform = waveform * amplEnvelope
  }
  waveform = waveform / max(waveform)

  # add attack
  if (attackLen > 0) {
    waveform = fadeInOut(waveform,
                         length_fade = floor(attackLen * samplingRate / 1000))
    # plot(waveform, type = 'l')
  }

  # pitch drift is accompanied by amplitude drift
  if (temperature > 0) {
    drift_upsampled = approx(drift,
                             n = length(waveform),
                             x = gc_upsampled[-length(gc_upsampled)])$y
    # plot(drift_upsampled, type = 'l')
  } else {
    drift_upsampled = 1
  }
  waveform = waveform * drift_upsampled
  # playme(waveform, samplingRate = samplingRate)
  # spectrogram(waveform, samplingRate = samplingRate)
  return(waveform)
}
