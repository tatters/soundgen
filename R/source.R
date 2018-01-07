# Functions for generating excitation source: either noise with generateNoise() or harmonics with generateHarmonics()

#' Generate noise
#'
#' Internal soundgen function.
#'
#' Generates white noise of length \code{len} and with spectrum defined by
#' linear decay \code{rolloffNoise} above \code{flatSpectrum} and/or a specified
#' filter \code{filterNoise}. Algorithm: paints a spectrum with desired
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
#' @param flatSpectrum keep spectrum flat up to ... Hz (ie rolloffNoise applies
#'   only above this frequency)
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
#' # 1 s of noise with rolloff changing from 0 to -6 dB
#' noise = soundgen:::generateNoise(len = samplingRate,
#'   rolloffNoise = c(0, -6), samplingRate = samplingRate)
#'
#' # To create a sibilant [s], specify a single strong, broad formant at ~7 kHz:
#' windowLength_points = 1024
#' filterNoise = soundgen:::getSpectralEnvelope(
#'   nr = windowLength_points / 2, nc = 1, samplingRate = samplingRate,
#'  formants = list('f1' = data.frame(time = 0, freq = 7000,
#'                                    amp = 50, width = 2000)))
#' noise = soundgen:::generateNoise(len = samplingRate, rolloffNoise = -4,
#'   samplingRate = samplingRate, filterNoise = filterNoise)
#' # plot(filterNoise, type = 'l')
#' # playme(noise, samplingRate = samplingRate)
#'
#' # low-frequency, wind-like noise
#' filterNoise = soundgen:::getSpectralEnvelope(
#'   nr = windowLength_points / 2, nc = 1, lipRad = 0,
#'   samplingRate = samplingRate, formants = list('f1' = data.frame(
#'     time = 0, freq = 150, amp = 30, width = 90)))
#' noise = soundgen:::generateNoise(len = samplingRate, rolloffNoise = -12,
#'   samplingRate = samplingRate, filterNoise = filterNoise)
#' # playme(noise, samplingRate = samplingRate)
generateNoise = function(len,
                         noiseAnchors = data.frame(
                           time = c(0, 300),
                           value = c(-80, -80)
                         ),
                         rolloffNoise = -4,
                         attackLen = 10,
                         flatSpectrum = 1200,
                         windowLength_points = 1024,
                         samplingRate = 16000,
                         overlap = 75,
                         throwaway = -80,
                         filterNoise = NA) {
  # convert anchors to a smooth contour of breathing amplitudes
  breathingStrength = getSmoothContour(
    len = len,
    anchors = noiseAnchors,
    normalizeTime = FALSE,
    valueFloor = permittedValues['noiseAmpl', 'low'],
    valueCeiling = permittedValues['noiseAmpl', 'high'],
    samplingRate = samplingRate,
    plot = FALSE
  )
  # plot(breathingStrength)

  # convert anchor amplitudes from dB to linear multipliers
  breathingStrength = 10 ^ (breathingStrength / 20)
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
  bin = samplingRate / 2 / nr
  binsPerKHz = round(1000 / bin)
  flatBins = round(flatSpectrum / bin)
  nc = length(step)
  if (is.na(filterNoise[1])) {
    filterNoise = matrix(1, nrow = nr, ncol = nc)
  } else {
    if (nrow(filterNoise) != nr | ncol(filterNoise) != nc) {
      # message('Incorrect dimensions of filterNoise. Interpolating...')
      filterRowIdx = round(seq(1, nrow(filterNoise), length.out = nr))
      filterColIdx = round(seq(1, ncol(filterNoise), length.out = nc))
      filterNoise = filterNoise[filterRowIdx, filterColIdx]
    }
  }
  # modify the exact filter (if provided) by adding the specified
  #   basic linear rolloff
  idx = (flatBins + 1):nr  # the bins that will be modified
  if (length(rolloffNoise) > 1) {
    rolloffNoise = getSmoothContour(anchors = rolloffNoise,
                                    len = nc)
    mult = matrix(apply(
      matrix(1:nc, ncol = 1),
      1,
      function(x) 10 ^ (rolloffNoise[x] / 20 * (idx - flatBins) / binsPerKHz)
    ), nrow = length(idx), ncol = nc)
    # Johnson_2012_Acoustic-and-Auditory-Phonetics, Fig. 7.1: spectrum of turbulent noise
  } else {
    a = 10 ^ (rolloffNoise / 20 * (idx - flatBins) / binsPerKHz)
    mult = matrix(rep(a, nc),
                  ncol = nc)
  }
  # image(t(mult))

  filterNoise[idx, ] = filterNoise[idx, ] * mult
  # image(t(filterNoise))
  # plot(filterNoise[, 1], type = 'l')
  # plot(log10(filterNoise[, 1]) * 20, type = 'l')

  ## instead of synthesizing the time series and then doing fft-ifft,
  #   we can simply synthesize spectral noise, convert to complex
  #   (setting imaginary=0), and then do inverse FFT just once
  # set up spectrum with white noise
  z1 = matrix(as.complex(runif(nr * nc)), nrow = nr, ncol = nc)
  # multiply by filter
  z1_filtered = z1 * filterNoise
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
  breathing = breathing / max(breathing) * breathingStrength # normalize
  # add attack
  if (is.numeric(attackLen) && attackLen > 0) {
    l = floor(attackLen * samplingRate / 1000)
    breathing = fade(
      breathing,
      fadeIn = l,
      fadeOut = l
    )
  }
  # plot(breathing, type = 'l')
  # playme(breathing, samplingRate = samplingRate)
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
#'   \code{throwaway}, \code{rolloff} increases by \code{rolloff_perAmpl}
#'   dB/octave. The effect is to make loud parts brighter by increasing energy
#'   in higher frequencies
#' @keywords internal
generateHarmonics = function(pitch,
                             glottisAnchors = 0,
                             attackLen = 50,
                             nonlinBalance = 0,
                             nonlinDep = 50,
                             jitterDep = 0,
                             jitterLen = 1,
                             vibratoFreq = 5,
                             vibratoDep = 0,
                             shimmerDep = 0,
                             creakyBreathy = 0,
                             rolloff = -9,
                             rolloffOct = -3,
                             rolloffKHz = -3,
                             rolloffParab = 0,
                             rolloffParabHarm = 3,
                             rolloff_perAmpl = 3,
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
                             amplAnchors = NA,
                             overlap = 75,
                             samplingRate = 16000,
                             pitchFloor = 75,
                             pitchCeiling = 3500,
                             pitchSamplingRate = 3500,
                             throwaway = -80) {
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
    # plot(amplContour, type='l')
    amplContour = amplContour / abs(throwaway) - 1
    rolloffAmpl = amplContour * rolloff_perAmpl
  } else {
    rolloffAmpl = rep(0, nGC)
  }

  # get a random walk for intra-syllable variation
  if (temperature > 0 && nonlinBalance < 100) {
    rw = getRandomWalk(
      len = nGC,
      rw_range = temperature,
      trend = c(randomWalk_trendStrength, -randomWalk_trendStrength),
      rw_smoothing = .3
    ) # plot(rw, type = 'l')
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
    #   in regimes 1 and 2 (i.e. together with jitter)
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
    throwaway = throwaway
  )
  # NB: this whole pitch_per_gc trick is purely for computational efficiency.
  #   The entire pitch contour can be fed in, but then it takes up to 1 s
  #   per s of audio
  # image(t(log(rolloff_source)))

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

  # synthesize one glottal cycle at a time or a whole epoch at once?
  synthesize_per_gc = is.list(glottisAnchors) && any(glottisAnchors$value > 0)

  # add vocal fry (subharmonics)
  if (!synthesize_per_gc &&  # can't add subharmonics if doing one gc at a time (one f0 period)
      subDep > 0 & nonlinBalance > 0) {
    vocalFry = getVocalFry(
      rolloff = rolloff_source,
      pitch_per_gc = pitch_per_gc,
      subFreq = subFreq * rw ^ subDriftDep,
      subDep = subDep * rw ^ subDriftDep * vocalFry_on,
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
    glottisClosed_per_gc = getSmoothContour(anchors = glottisAnchors,
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
    amplEnvelope = 10 ^ (amplEnvelope / 20)
    waveform = waveform * amplEnvelope
  }

  # add attack
  if (attackLen > 0) {
    l = floor(attackLen * samplingRate / 1000)
    waveform = fade(waveform,
                    fadeIn = l,
                    fadeOut = l)
    # plot(waveform, type = 'l')
  }

  # pitch drift is accompanied by amplitude drift
  if (temperature > 0 && amplDriftDep > 0) {
    gc_upsampled = upsample(pitch_per_gc, samplingRate = samplingRate)$gc
    drift_upsampled = approx(drift,
                             n = length(waveform),
                             x = gc_upsampled[-length(gc_upsampled)])$y
    waveform = waveform * drift_upsampled ^ amplDriftDep
    # plot(drift_upsampled, type = 'l')
  }
  waveform = waveform / max(waveform)
  # playme(waveform, samplingRate = samplingRate)
  # spectrogram(waveform, samplingRate = samplingRate)
  return(waveform)
}



#' Generate glottal cycles
#'
#' Internal soundgen function.
#' Takes descriptives of a number of glottal cycles (f0, closed phase, rolloff)
#' and creates a waveform consisting of a string of these glottal cycles
#' separated by pauses (if there is a closed phase). The principle is to work
#' with one glottal cycle at a time and create a sine wave for each harmonic,
#' with amplitudes adjusted by rolloff.
#' @param pitch_per_gc pitch per glottal cycle, Hz
#' @param glottisClosed_per_gc proportion of closed phase per glottal cycle, \%
#' @param rolloff_per_gc a list of one-column matrices, one for each glottal
#'   cycle, specifying rolloff per harmonic (linear multiplier, ie NOT in dB)
#'   Each matrix has as many rows as there are harmonics, and rownames specify
#'   the ratio to F0 (eg 1.5 means it's a subharmonic added between f0 and its
#'   first harmonic)
#' @param samplingRate the sampling rate of generated sound, Hz
#' @return Returns a waveform as a non-normalized numeric vector centered at zero.
#' @keywords internal
#' @examples
#' pitch_per_gc = seq(100, 150, length.out = 25)
#' glottisClosed_per_gc = seq(0, 300, length.out = 25)
#' m = matrix(10 ^ (-6 * log2(1:200) / 20))
#' rownames(m) = 1:nrow(m)
#' rolloff_source = rep(list(m), 25)
#' s = soundgen:::generateGC(pitch_per_gc,  glottisClosed_per_gc,
#'                           rolloff_source,  samplingRate = 16000)
#' # plot(s, type = 'l')
#' # playme(s)
generateGC = function(pitch_per_gc,
                      glottisClosed_per_gc,
                      rolloff_per_gc,
                      samplingRate) {
  gc_len = round(samplingRate / pitch_per_gc)  # length of each gc, points
  pause_len = round(gc_len * glottisClosed_per_gc / 100)  # length of each pause, points

  # synthesize one glottal cycle at a time
  waveform = 0
  for (i in 1:length(gc_len)) {
    cycle = 0
    for (h in 1:nrow(rolloff_per_gc[[i]])) {
      times_f0 = as.numeric(rownames(rolloff_per_gc[[i]])[h]) # freq of harmonic h
      idx = 0:(gc_len[i] - 1)  # count from zero, ensuring the gc starts at sin(0) = 0
      cycle = cycle +
        sin(2 * pi * pitch_per_gc[i] * times_f0 * idx / samplingRate) *
        rolloff_per_gc[[i]][h]
    }
    # plot(cycle, type = 'l')
    waveform = c(waveform, cycle, rep(0, pause_len[i]))
  }
  # plot(waveform, type = 'l')
  # playme(waveform, samplingRate)
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
#' While the same sounds can be created with soundgen(), this dedicated
#' facetious function produces the same effect more efficiently and with very
#' few control parameters. With default settings, execution time is ~ 10 ms per
#' second of audio sampled at 16000 Hz. Principle: creates separate glottal
#' cycles with harmonics, but no formants. See \code{\link{soundgen}} for more
#' details.
#' @inheritParams soundgen
#' @param plot if TRUE, plots the waveform
#' @return Returns a normalized waveform.
#' @export
#' @examples
#' f = fart()
#' # playme(f)
fart = function(glottisAnchors = c(350, 700),
                pitchAnchors = 75,
                temperature = 0.25,
                sylLen = 600,
                rolloff = -20,
                samplingRate = 16000,
                play = FALSE,
                plot = FALSE) {
  glottisAnchors = reformatAnchors(glottisAnchors)
  pitchAnchors = reformatAnchors(pitchAnchors)

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

    glottisAnchors = wiggleAnchors(glottisAnchors, temperature, temp_coef = .5, low = c(0, 0), high = c(1, 10000), wiggleAllRows = TRUE)
    pitchAnchors = wiggleAnchors(pitchAnchors, temperature, temp_coef = 1, low = c(0, 0), high = c(1, 10000), wiggleAllRows = TRUE)
  }

  # preliminary glottis contour
  glottisClosed = getSmoothContour(anchors = glottisAnchors, len = 100)

  # adjust length based on proportion of closed glottis (pauses added)
  mean_closed = mean(glottisClosed) / 100
  sylLen = sylLen / (mean_closed + 1)
  pitchAnchors = pitchAnchors * (mean_closed + 1)

  # prepare pitch contour
  pitch = getSmoothContour(anchors = pitchAnchors, len = sylLen * samplingRate / 1000)

  # get pitch per glottal cycle
  gc = getGlottalCycles(pitch, samplingRate = samplingRate)
  pitch_per_gc = pitch[gc]
  gc_len = round(samplingRate / pitch_per_gc)

  # final glottis contour
  glottisClosed = getSmoothContour(anchors = glottisAnchors, len = length(gc))

  # prepare rolloff
  nHarmonics = ceiling((samplingRate / 2 - min(pitch)) / min(pitch))
  roll_dB = log2(1:nHarmonics) * rolloff
  roll_dB = roll_dB[roll_dB > -80]
  roll = 10 ^ (roll_dB / 20)

  # synthesize the sound
  pause_len = round(gc_len * glottisClosed / 100)
  s = vector()
  for (i in 1:length(gc)) {
    cycle = 0
    for (h in 1:length(roll)) {
      cycle = cycle +
        sin(2 * pi * pitch_per_gc[i] * (0:(gc_len[i] - 1)) *
              h / samplingRate) * roll[h]
    }  # plot(cycle, type = 'l')
    s = c(s, cycle, rep(0, pause_len[i]))
  }

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
#' @param fadeOut if TRUE, a linear fade-out is applied to the entire syllable
#' @return Returns a non-normalized waveform centered at zero.
#' @export
#' @examples
#' play = c(TRUE, FALSE)[2]
#' # a drum-like sound
#' s = beat(nSyl = 1, sylLen = 200,
#'                  pitchAnchors = c(200, 100), play = play)
#' # plot(s, type = 'l')
#'
#' # a dry, muted drum
#' s = beat(nSyl = 1, sylLen = 200,
#'                  pitchAnchors = c(200, 10), play = play)
#'
#' # sci-fi laser guns
#' s = beat(nSyl = 3, sylLen = 300,
#'                  pitchAnchors = c(1000, 50), play = play)
#'
#' # machine guns
#' s = beat(nSyl = 10, sylLen = 10, pauseLen = 50,
#'                  pitchAnchors = c(2300, 300), play = play)
beat = function(nSyl = 10,
                sylLen = 200,
                pauseLen = 50,
                pitchAnchors = c(200, 10),
                samplingRate = 16000,
                fadeOut = TRUE,
                play = FALSE) {
  len = sylLen * samplingRate / 1000
  pitchContour = getSmoothContour(anchors = pitchAnchors,
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
  if (play) playme(beat, samplingRate)
  return(beat)
}
