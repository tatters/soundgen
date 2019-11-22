#' Spectral envelope
#'
#' Prepares a spectral envelope for filtering a sound to add formants, lip
#' radiation, and some stochastic component regulated by temperature. Formants
#' are specified as a list containing time, frequency, amplitude, and width
#' values for each formant (see examples). See vignette('sound_generation',
#' package = 'soundgen') for more information.
#' @param nr the number of frequency bins = windowLength_points/2, where
#'   windowLength_points is the size of window for Fourier transform
#' @param nc the number of time steps for Fourier transform
#' @inheritParams soundgen
#' @param formants a character string like "aaui" referring to default presets
#'   for speaker "M1"; a vector of formant frequencies; or a list of formant
#'   times, frequencies, amplitudes, and bandwidths, with a single value of each
#'   for static or multiple values of each for moving formants. \code{formants =
#'   NA} defaults to schwa. Time stamps for formants and mouthOpening can be
#'   specified in ms or an any other arbitrary scale.
#' @param interpol the method of smoothing envelopes based on provided mouth
#'   anchors: 'approx' = linear interpolation, 'spline' = cubic spline, 'loess'
#'   (default) = polynomial local smoothing function. NB: this does NOT affect
#'   the smoothing of formant anchors
#' @param formDrift scale factor regulating the effect of temperature on the
#'   depth of random drift of all formants (user-defined and stochastic): the
#'   higher, the more formants drift at a given temperature
#' @param formDisp scale factor regulating the effect of temperature on the
#'   irregularity of the dispersion of stochastic formants: the higher, the more
#'   unevenly stochastic formants are spaced at a given temperature
#' @param speedSound speed of sound in warm air, cm/s. Stevens (2000) "Acoustic
#'   phonetics", p. 138
#' @param openMouthBoost amplify the voice when the mouth is open by
#'   \code{openMouthBoost} dB
#' @param formantDepStoch the amplitude of additional formants added above
#'   the highest specified formant (only if temperature > 0)
#' @param smoothLinearFactor regulates smoothing of formant anchors (0 to +Inf)
#'   as they are upsampled to the number of fft steps \code{nc}. This is
#'   necessary because the input \code{formants} normally contains fewer
#'   sets of formant values than the number of fft steps.
#'   \code{smoothLinearFactor} = 0: close to default spline; >3: approaches
#'   linear extrapolation
#' @param plot if TRUE, produces a plot of the spectral envelope
#' @param duration duration of the sound, ms (for plotting purposes only)
#' @param colorTheme black and white ('bw'), as in seewave package ('seewave'),
#'   or another color theme (e.g. 'heat.colors')
#' @param nCols number of colors in the palette
#' @param xlab,ylab labels of axes
#' @param ... other graphical parameters passed on to \code{image()}
#' @export
#' @return Returns a spectral filter (matrix nr x nc, where nr is the number of
#'   frequency bins and nc is the number of time steps). Accordingly, rownames
#'   of the output give central frequency of each bin(in kHz), while colnames
#'   give time values (in ms if duration is specified, otherwise 0 to 1).
#' @examples
#' # [a] with F1-F3 visible
#' e = getSpectralEnvelope(nr = 512, nc = 50, duration = 300,
#'   formants = soundgen:::convertStringToFormants('a'),
#'   temperature = 0, plot = TRUE)
#' # image(t(e))  # to plot the output on a linear scale instead of dB
#'
#' # some "wiggling" of specified formants plus extra formants on top
#' e = getSpectralEnvelope(nr = 512, nc = 50,
#'   formants = soundgen:::convertStringToFormants('a'),
#'   temperature = 0.1, formantDepStoch = 20, plot = TRUE)
#'
#' # a schwa based on variable length of vocal tract
#' e = getSpectralEnvelope(nr = 512, nc = 100, formants = NA,
#'   vocalTract = list(time = c(0, .4, 1), value = c(13, 18, 17)),
#'   temperature = .1, plot = TRUE)
#'
#' # no formants at all, only lip radiation
#' e = getSpectralEnvelope(nr = 512, nc = 50, lipRad = 6,
#'   formants = NA, temperature = 0, plot = FALSE)
#' plot(e[, 1], type = 'l')              # linear scale
#' plot(20 * log10(e[, 1]), type = 'l')  # dB scale - 6 dB/oct
#'
#' # mouth opening
#' e = getSpectralEnvelope(nr = 512, nc = 50,
#'   vocalTract = 16, plot = TRUE, lipRad = 6, noseRad = 4,
#'   mouth = data.frame(time = c(0, .5, 1), value = c(0, 0, .5)))
#'
#' # scale formant amplitude and/or bandwidth
#' e1 = getSpectralEnvelope(nr = 512, nc = 50,
#'   formants = soundgen:::convertStringToFormants('a'),
#'   formantWidth = 1, formantDep = 1)  # defaults
#' e2 = getSpectralEnvelope(nr = 512, nc = 50,
#'   formants = soundgen:::convertStringToFormants('a'),
#'   formantWidth = 1.5, formantDep = 1.5)
#' plot(e2[, 1], type = 'l', col = 'red', lty = 2)
#' points(e1[, 1], type = 'l')
#'
#' # manual specification of formants
#' e = getSpectralEnvelope(nr = 512, nc = 50, plot = TRUE, samplingRate = 16000,
#'   formants = list(f1 = data.frame(time = c(0, 1), freq = c(900, 500),
#'                                   amp = 20, width = c(80, 50)),
#'                   f2 = data.frame(time = c(0, 1), freq = c(1200, 2500),
#'                                   amp = 20, width = 100),
#'                   f3 = data.frame(time = 0, freq = 2900,
#'                                   amp = 20, width = 120)))
getSpectralEnvelope = function(nr,
                               nc,
                               formants = NA,
                               formantDep = 1,
                               formantWidth = 1,
                               lipRad = 6,
                               noseRad = 4,
                               mouth = NA,
                               interpol = c('approx', 'spline', 'loess')[3],
                               mouthOpenThres = 0.2,
                               openMouthBoost = 0,
                               vocalTract = NULL,
                               temperature = 0.05,
                               formDrift = .3,
                               formDisp = .2,
                               formantDepStoch = 20,
                               smoothLinearFactor = 1,
                               formantCeiling = 2,
                               samplingRate = 16000,
                               speedSound = 35400,
                               plot = FALSE,
                               duration = NULL,
                               colorTheme = c('bw', 'seewave', '...')[1],
                               nCols = 100,
                               xlab = 'Time',
                               ylab = 'Frequency, kHz',
                               ...) {
  # standard formatting
  formants = reformatFormants(formants)
  if (!is.null(vocalTract)) {
    if (!any(is.na(vocalTract))) {
      if (is.list(vocalTract) |
          (is.numeric(vocalTract) & length(vocalTract) > 1)) {
        vocalTract = getSmoothContour(
          vocalTract,
          len = nc,
          interpol = interpol,
          valueFloor = permittedValues['vocalTract', 'low'],
          valueCeiling = permittedValues['vocalTract', 'high'],
          plot = FALSE
        )  # vocalTract is now either NULL/NA or numeric of length nc
      }
    }
  }

  ## estimate vocal tract length
  if (!is.list(vocalTract) & !is.numeric(vocalTract) & is.list(formants)) {
    # if we don't know vocalTract, but at least one formant is defined,
    # we guess the length of vocal tract
    vocalTract = estimateVTL(formants = formants,
                             speedSound = speedSound,
                             checkFormat = FALSE)  # already checked
  }

  # if is.na(formants) or if there's something wrong with it,
  # we fall back on vocalTract to make a schwa
  if (length(formants[[1]]) < 2 & is.numeric(vocalTract) &
      temperature > 0 & formantDep > 0 & formantDepStoch > 0) {
    freq = speedSound / 4 / vocalTract
    formants = list('f1' = data.frame(
      'time' = seq(0, 1, length.out = length(freq)),
      'freq' = freq,
      'amp' = NA,
      'width' = getBandwidth(freq)  # corrected Tappert, Martony, and Fant (TMF)-1963
    ))
  }

  # if there are zeros, set all NA amps to formantDepStoch
  if (is.list(formants)) {
    any_zeros = any(sapply(formants, function(f) {
      # check if there are any negative amp values, excluding NA's
      a = f$amp
      any(is.numeric(a)) & any(a < 0, na.rm = TRUE)
    }))
    if (any_zeros) {
      for (f in 1:length(formants)) {
        if (!is.numeric(formants[[f]]$amp)) {
          formants[[f]]$amp = formantDepStoch
        }
      }
    }
  }

  # create a "spectrogram"-shaped filter matrix
  spectralEnvelope = matrix(0, nrow = nr, ncol = nc)

  ### START OF FORMANTS
  if (is.list(formants)) {
    # upsample to the length of fft steps
    nPoints = max(unlist(lapply(formants, nrow)))
    formants_upsampled = vector('list', length = length(formants))
    for (f in 1:length(formants)) {
      formant_f = data.frame(time = seq(0, 1, length.out = nc))
      for (v in c('freq', 'amp')) {
        if (length(formants[[f]][, v]) > 1 & !any(is.na(formants[[f]][, v]))) {
          # just spline produces imprecise, overly smoothed curves. Loess is just
          # too slow for this. So we apply linear extrapolation to formant values
          # first, to get a fairly straight line between anchors, and THEN smooth
          # it out with spline
          formant_f[, v] = spline(approx(
            formants[[f]][, v],
            n = nPoints + 2 ^ smoothLinearFactor,
            x = formants[[f]]$time)$y, n = nc
          )$y
        } else {
          formant_f[, v] = rep(formants[[f]][1, v], nc)
        }
      }
      formant_f$freq = formant_f$freq * vocalTract[1] / vocalTract
      formant_f$width = getBandwidth(formant_f$freq)
      formants_upsampled[[f]] = formant_f
    }
    names(formants_upsampled) = names(formants)

    ## Stochastic part (only for temperature > 0)
    if (temperature > 0) {
      # non-integer formants like "f1.4" refer to extra zero-pole pairs.
      # They should not be considered for VTL estimation or for adding formants
      non_integer_formants = apply(as.matrix(names(formants_upsampled)),
                                   1,
                                   function(x) {
                                     grepl('.', x, fixed = TRUE)
                                   })
      # create a few new, relatively high-frequency extra formants
      if (!is.numeric(vocalTract) & length(formants) > 1) {
        ff = unlist(lapply(formants[!non_integer_formants], function(x) x$freq[1]))
        formantDispersion = getFormantDispersion(ff,
                                                 speedSound = speedSound,
                                                 method = 'regression')
      } else if (is.numeric(vocalTract)) {
        formantDispersion = speedSound / (2 * vocalTract)
      } else {
        formantDispersion = NA # making sdG also NA, ie extra formants not added
      }
      sdG = formantDispersion * temperature * formDisp
      nFormants = length(formants_upsampled)
      nFormants_integer = length(formants_upsampled) - sum(non_integer_formants)
      freq_max = max(formants_upsampled[[nFormants]][, 'freq'])

      if (!any(is.na(sdG)) & formantDepStoch > 0) {
        # formant_f = (2 * f - 1) / 2 * formantDispersion,
        # therefore, to generate formants to 2 * Nyquist
        # (to compensate for downward drag of lower formants)
        # 2 * nyquist = (2 * nExtraFormants - 1) / 2 * formantDispersion
        # Solving for nExtraFormants gives (nyquist * 4 / formantDispersion + 1) / 2:
        nExtraFormants = round((samplingRate * formantCeiling / min(formantDispersion) + 1) / 2) - nFormants
        if (is.numeric(nExtraFormants) && nExtraFormants > 0) {
          nf = length(formantDispersion)
          extraFreqs = extraWidths = matrix(NA, nrow = nf, ncol = nExtraFormants)
          extraAmps = rgamma(
            nExtraFormants,
            # mean = formantDepStoch, sd = formantDepStoch * temperature
            1 / temperature ^ 2,
            1 / (formantDepStoch * temperature ^ 2)
          )
          for (frame in 1:nf) {
            # once for static vtl, for each frame in 1:nc otherwise
            idx = (nFormants_integer + 1) : (nFormants_integer + nExtraFormants)
            extraFreqs_regular = (2 * idx - 1) / 2 * formantDispersion[frame]
            extraFreqs[frame, ] = rgamma(
              nExtraFormants,
              # mean = extraFreqs_regular, sd = sdG
              extraFreqs_regular ^ 2 / sdG[frame] ^ 2,
              extraFreqs_regular / sdG[frame] ^ 2
            )
            extraWidths[frame, ] = getBandwidth(extraFreqs[[frame]])
          }

          for (f in 1:nExtraFormants) {
            formants_upsampled[[nFormants + 1]] = data.frame (
              'time' = formants_upsampled[[1]][, 'time'],
              'freq' = extraFreqs[, f],
              'amp' = ifelse(any_zeros, extraAmps[f], NA),
              'width' = extraWidths[, f]
            )
            nFormants = length(formants_upsampled)
          }
        }
      }

      # wiggle both user-specified and stochastic formants
      for (f in 1:nFormants) {
        for (c in 2:4) {
          # wiggle freq, ampl and bandwidth independently
          rw = getRandomWalk(
            len = nc,
            rw_range = temperature * formDrift,
            rw_smoothing = 0.3,
            trend = rnorm(1)
          )
          # if nc == 1, returns one number close to 1
          if (length(rw) > 1) {
            # for actual random walks, make sure mean is 1
            rw = rw - mean(rw) + 1
          }
          formants_upsampled[[f]][, c] = formants_upsampled[[f]][, c] * rw
        }
      } # end of wiggling formants
    } # end of if temperature > 0

    ## Deterministic part
    # convert formant freqs and widths from Hz to bins
    bin_width = samplingRate / 2 / nr # Hz
    bin_freqs = seq(bin_width / 2, samplingRate / 2, length.out = nr) # Hz
    for (f in 1:length(formants_upsampled)) {
      formants_upsampled[[f]][, 'freq'] =
        (formants_upsampled[[f]][, 'freq'] - bin_width / 2) / bin_width + 1
      # frequencies expressed in bin indices (how many bin widths above the
      # central frequency of the first bin)
      formants_upsampled[[f]][, 'width'] =
        formants_upsampled[[f]][, 'width'] / bin_width * formantWidth
    }

    # mouth opening
    if (length(mouth) < 1 | any(is.na(mouth))) {
      mouthOpening_upsampled = rep(0.5, nc) # defaults to mouth half-open the
      # whole time - sort of hanging loosely agape ;))
      mouthOpen_binary = rep(1, nc)
    } else {
      mouthOpening_upsampled = getSmoothContour(
        len = nc,
        anchors = mouth,
        interpol = interpol,
        valueFloor = permittedValues['mouthOpening', 'low'],
        valueCeiling = permittedValues['mouthOpening', 'high'],
        plot = FALSE
      )
      # mouthOpening_upsampled[mouthOpening_upsampled < mouthOpenThres] = 0
      mouthOpen_binary = ifelse(mouthOpening_upsampled > mouthOpenThres, 1, 0)
    }
    # plot(mouthOpening_upsampled, type = 'l')

    # adjust formants for mouth opening
    adjustment_bins = 0
    if (!is.null(vocalTract)) {
      if (!any(is.na(vocalTract))) {
        # is.finite() returns F for NaN, NA, inf, etc
        adjustment_hz = (mouthOpening_upsampled - 0.5) * speedSound /
          (4 * vocalTract) # speedSound = 35400 cm/s, speed of sound in warm
        # air. The formula for mouth opening is adapted from Moore (2016)
        # "A Real-Time Parametric General-Purpose Mammalian Vocal Synthesiser".
        # mouthOpening = .5 gives no modification (neutral, "default" position).
        # Basically we assume a closed-closed tube for closed mouth and a
        # closed-open tube for open mouth, but since formants can be specified
        # rather than calculated based on vocalTract, we just subtract half the
        # total difference between open and closed tubes in Hz from each formant
        # value as the mouth goes from half-open (neutral) to fully closed, or we
        # add half that value as the mouth goes from neutral to max open
        adjustment_bins = (adjustment_hz - bin_width / 2) / bin_width + 1
      }
    }
    for (f in 1:length(formants_upsampled)) {
      formants_upsampled[[f]][, 'freq'] =
        formants_upsampled[[f]][, 'freq'] + adjustment_bins
      # force each formant frequency to be positive
      formants_upsampled[[f]][, 'freq'] [formants_upsampled[[f]][, 'freq'] < 1] = 1
    }

    # nasalize the parts with closed mouth: see Hawkins & Stevens (1985);
    # http://www.cslu.ogi.edu/tutordemos/SpectrogramReading/cse551html/cse551/node35.html
    nasalizedIdx = which(mouthOpen_binary == 0) # or specify a separate
    # increase F1 bandwidth to 175 Hz
    formants_upsampled$f1[nasalizedIdx, 'width'] = 175 / bin_width
    # nasalization contour
    if (length(nasalizedIdx) > 0) {
      # add a pole
      formants_upsampled$fnp = formants_upsampled$f1
      formants_upsampled$fnp[, 'amp'] = 0
      formants_upsampled$fnp[nasalizedIdx, 'amp'] = formantDepStoch
      formants_upsampled$fnp[nasalizedIdx, 'width'] =
        formants_upsampled$f1[nasalizedIdx, 'width'] * 2 / 3
      formants_upsampled$fnp[nasalizedIdx, 'freq'] =
        ifelse(
          formants_upsampled$f1[nasalizedIdx, 'freq'] > 550 / bin_width,
          formants_upsampled$f1[nasalizedIdx, 'freq'] - 250 / bin_width,
          formants_upsampled$f1[nasalizedIdx, 'freq'] + 250 / bin_width
        )
      # 250 Hz below or above F1, depending on whether F1 is above or below
      # 550 Hz

      # add a zero
      formants_upsampled$fnz = formants_upsampled$f1
      formants_upsampled$fnz[, 'amp'] = 0
      formants_upsampled$fnz[nasalizedIdx, 'amp'] = -formantDepStoch
      formants_upsampled$fnz[nasalizedIdx, 'freq'] =
        (formants_upsampled$fnp[nasalizedIdx, 'freq'] +
           formants_upsampled$f1[nasalizedIdx, 'freq']) / 2  # midway between
      # f1 and fnp
      formants_upsampled$fnz[nasalizedIdx, 'width'] =
        formants_upsampled$fnp[nasalizedIdx, 'width']
      # modify f1
      formants_upsampled$f1[nasalizedIdx, 'amp'] =
        formants_upsampled$f1[nasalizedIdx, 'amp'] * 4 / 5
      formants_upsampled$f1[nasalizedIdx, 'width'] =
        formants_upsampled$f1[nasalizedIdx, 'width'] * 5 / 4
    }

    # Add formants to spectrogram
    freqs_bins = 1:nr
    poles = 1:length(formants_upsampled)
    zeros = as.numeric(which(sapply(formants_upsampled, function(x) any(x[, 'amp'] < 0))))
    if (length(zeros) > 0) poles = poles[-zeros]
    s = complex(real = 0, imaginary = 2 * pi * freqs_bins)
    na_amp = sapply(formants_upsampled, function(x) is.na(x[1, 'amp']))  # check only the first frame

    if (length(zeros) == 0) {  # all-pole
      # special case for faster computing (saves ~10 to 30 ms). Stevens 2000 p. 131
      for (f in 1:length(formants_upsampled)) {
        pf = 2 * pi * formants_upsampled[[f]][, 'freq']
        bp = -formants_upsampled[[f]][, 'width'] * pi
        s1 = complex(real = bp, imaginary = pf)
        s1c = Conj(s1)
        formant = matrix(0, nrow = nr, ncol = nc)
        for (c in 1:nc) {
          # actually much faster w/o log-transform, and numbers don't get very large anyhow
          tns = s1[c] * s1c[c] / (s - s1[c]) / (s - s1c[c])
          formant[, c] = log10(abs(tns))
          if (na_amp[f]) {
            formant[, c] = formant[, c] * 20  # formantDepStoch can be 0
          } else {
            formant[, c] = formant[, c] / max(formant[, c]) * formants_upsampled[[f]][c, 'amp']
          }
          # plot(bin_freqs, formant[, c], type = 'l')
        }
        spectralEnvelope = spectralEnvelope + formant
      }
    } else {  # both zeros and poles
      # General case. Stevens 2000 p. 137
      n = length(formants_upsampled)
      freqs = matrix(sapply(formants_upsampled[1:n], function(x) x[, 'freq']), ncol = n)
      widths = matrix(sapply(formants_upsampled[1:n], function(x) x[, 'width']), ncol = n)
      amps = matrix(sapply(formants_upsampled[1:n], function(x) x[, 'amp']), ncol = n)
      amps_norm = abs(amps / 20)  # we know which ones are zeros, but their amps must be positive
      if (any(na_amp)) {
        amps_norm[is.na(amps_norm)] = formantDepStoch / 20
      }

      for (c in 1:nc) {
        # instead of looping through formants (all-pole), we loop through frames
        pf = 2 * pi * freqs[c, ]
        bp = widths[c, ] * pi
        s1 = complex(real = bp[poles], imaginary = pf[poles])
        s1c = Conj(s1)
        s0 = complex(real = bp[zeros], imaginary = pf[zeros])
        s0c = Conj(s0)
        # numerator = prod(s1 ^ amps_norm[c, poles]) * prod(s1c ^ amps_norm[c, poles])  # can exceed 10^150
        log_numerator = sum(amps_norm[c, poles] * log10(s1)) +
          sum(amps_norm[c, poles] * log10(s1c))
        if (length(zeros) > 0) {
          for (z in 1:length(zeros)) {
            # numerator = numerator * ((s - s0[z]) * (s - s0c[z])) ^ amps_norm[c, zeros[z]]
            log_numerator = log_numerator + amps_norm[c, zeros[z]] *
              log10((s - s0[z]) * (s - s0c[z]))
          }
          # denominator = prod(s0 ^ amps_norm[c, zeros]) * prod(s0c ^ amps_norm[c, zeros])
          log_denominator = sum(amps_norm[c, zeros] * log10(s0)) +
            sum(amps_norm[c, zeros] * log10(s0c))
        } else {
          # denominator = 1
          log_denominator = 0
        }
        for (p in 1:length(poles)) {
          # denominator = denominator * ((s - s1[p]) * (s - s1c[p])) ^ amps_norm[c, poles[p]]
          log_denominator = log_denominator + amps_norm[c, poles[p]] *
            log10((s - s1[p]) * (s - s1c[p]))
        }
        # tns = numerator / denominator
        log_tns = log_numerator - log_denominator
        # formants_per_bin = 20 * log10(abs(tns))
        formants_per_bin = 20 * Re(log_tns)
        # plot(bin_freqs, formants_per_bin, type = 'l')
        spectralEnvelope[, c] = spectralEnvelope[, c] + formants_per_bin
      }
    }
    spectralEnvelope = spectralEnvelope * formantDep
  } else {
    mouthOpen_binary = rep(1, nc)
    mouthOpening_upsampled = rep(0.5, nc)
    bin_width = samplingRate / 2 / nr # otherwise it's not defined if formants = NULL
  }
  # plot(spectralEnvelope[, 1], type = 'l')
  # image(t(spectralEnvelope))

  # save frequency and time stamps
  freqs = seq(bin_width / 2,
              samplingRate / 2 - bin_width / 2,
              length.out = nr) / 1000
  rownames(spectralEnvelope) = freqs
  if (is.numeric(duration)) {
    colnames(spectralEnvelope) = seq(0, duration, length.out = nc)
  } else {
    colnames(spectralEnvelope) = seq(0, 1, length.out = nc)
  }

  # add correction for not adding higher formants
  if (FALSE) {
    # add correction for not adding higher formants
    # rolloffAdjust = 0 - 12 * log2(((nr*1):1)) [1:nr]
    # rolloffAdjust = rolloffAdjust - min(rolloffAdjust)
    # spectralEnvelope = apply(spectralEnvelope,
    #                          2,
    #                          function(x) x + rolloffAdjust)

    # or:
    # s = as.matrix(data.frame(freq = freqs,
    #                          amp = spectralEnvelope[, 1]))
    # peaks = as.data.frame(seewave::fpeaks(s, f = samplingRate, plot = FALSE))
    # mod = nls(amp ~ a + b * freq ^ c, peaks, start = list(a = 0, b = -1, c = 1))
    # if (FALSE) {
    #   peaks$pred = predict(mod)
    #   plot(peaks$freq, peaks$amp, type = 'b')
    #   points(peaks$freq, peaks$pred, type = 'l', col = 'red')
    # }
    #
    # ms = summary(mod)
    # msc = ms$coefficients[, 1]
    # specAdjust = as.numeric(-(msc[1] + msc[2] * s[, 'freq'] ^ msc[3]))
    # specAdjust = specAdjust - min(specAdjust)
    # for (c in 1:ncol(spectralEnvelope)) {
    #   spectralEnvelope[, c] = spectralEnvelope[, c] + specAdjust
    # }
  }
  # image(t(spectralEnvelope))
  # END OF FORMANTS

  # add lip radiation when the mouth is open and nose radiation when the mouth is closed
  lip_dB = lipRad * log2(1:nr) # vector of length nr
  nose_dB = noseRad * log2(1:nr)
  # plot(lip_dB, type = 'l'); plot(nose_dB, type = 'l')
  for (c in 1:nc) {
    spectralEnvelope[, c] = spectralEnvelope[, c] +
      lip_dB * mouthOpen_binary[c] +
      nose_dB * (1 - mouthOpen_binary[c]) +
      mouthOpen_binary[c] * openMouthBoost
  }
  # plot(spectralEnvelope[, 1], type = 'l')

  # convert from dB to linear multiplier of power spectrum
  spectralEnvelope_lin = 10 ^ (spectralEnvelope / 20)
  # plot(spectralEnvelope_lin[, 1], type = 'l')

  if (plot) {
    if (colorTheme == 'bw') {
      col = gray(seq(from = 1, to = 0, length = nCols))
    } else if (colorTheme == 'seewave') {
      col = seewave::spectro.colors(nCols)
    } else {
      colFun = match.fun(colorTheme)
      col = rev(colFun(nCols))
    }
    image(x = as.numeric(colnames(spectralEnvelope)),
          y = as.numeric(rownames(spectralEnvelope)),
          z = t(spectralEnvelope),
          xlab = xlab,
          ylab = ylab,
          col = col,
          ...)
  }

  invisible(spectralEnvelope_lin)
}


#' Reformat formants
#'
#' Internal soundgen function.
#'
#' Checks that the formants are formatted in a valid way and expands them to a
#' standard list of dataframes with time, frequency, amplitude, and bandwidth of
#' each formant specified explicitly.
#' @param formants either a character string like "aoiu" or a list with an entry
#'   for each formant
#' @keywords internal
#' @examples
#' formants = soundgen:::reformatFormants('aau')
#' formants = soundgen:::reformatFormants(c(500, 1500, 2500))
#' formants = soundgen:::reformatFormants(list(f1 = 500, f2 = c(1500, 1700)))
#' formants = soundgen:::reformatFormants(list(
#'      f1 = list(freq = 800, amp = 30),
#'      f2 = list(freq = c(1500, 1700, 2200), width = c(100, 150, 175))
#' ))
reformatFormants = function(formants) {
  if (class(formants) == 'character') {
    # "aui" etc - read off values from presets$M1
    formants = convertStringToFormants(formants)
  } else if (is.numeric(formants)) {
    # expand to full format from e.g. "formants = c(500, 1500, 2500)"
    freqs = formants
    formants = vector('list', length(freqs))
    names(formants) = paste0('f', 1:length(freqs))
    for (f in 1:length(freqs)) {
      formants[[f]] = data.frame(time = 0,
                                 freq = freqs[f],
                                 amp = NA,
                                 width = getBandwidth(freqs[f]))
    }
  }
  if (is.list(formants)) {
    if (is.null(names(formants))) {
      names(formants) = paste0('f', 1:length(formants))
    }
    for (f in 1:length(formants)) {
      formant = formants[[f]]
      if (is.list(formant)) {
        if ('freq' %in% names(formant)) {
          # expand to full format from e.g. f1 = list(freq = 550, amp = 30)
          formant = as.data.frame(formant)
          if (is.null(formant$time)) {
            formant$time = seq(0, 1, length.out = nrow(formant))
          }
          if (is.null(formant$amp)) {
            formant$amp = NA
          }
          if (is.null(formant$width)) {
            formant$width = getBandwidth(formant$freq)
          }
        }
      } else if (is.numeric(formant)) {
        # expand to full format from e.g. f1 = 550
        # (numbers are assumed to represent frequency)
        formant = data.frame(time = seq(0, 1, length.out = length(formant)),
                             freq = formant,
                             amp = rep(NA, length(formant)),
                             width = getBandwidth(formant))
      }
      # make sure columns are in the right order (for esthetics & debugging)
      formants[[f]] = formant[, c('time', 'freq', 'amp', 'width')]
    }
  } else if (!is.null(formants)) {
    if (!is.na(formants)) {
      stop('If defined, formants must be either a list or a string of characters
         from dictionary presets: a, o, i, e, u, 0 (schwa)')
    }
  }
  return(formants)
}


#' Get bandwidth
#'
#' Internal soundgen function.
#'
#' Calculates formant bandwidth as a function of formant frequencies using a
#' modified version of TMF-63 formula. Namely, above 500 Hz it follows the
#' original formula from Tappert, Martony, and Fant (TMF)-1963, and below 500 Hz
#' it applies a correction to allow for energy losses at low frequencies. See
#' Khodai-Joopari & Clermont (2002), "Comparison of formulae for estimating
#' formant bandwidths". Below 250 Hz the bandwidth is forces to drop again to
#' avoid very large values near zero (just guesswork!)
#' @param f a vector of formant frequencies, Hz
#' @keywords internal
#' @examples
#' f = 1:5000
#' plot(f, soundgen:::getBandwidth(f), type = 'l',
#'   xlab = 'Formant frequency, Hz', ylab = 'Estimated bandwidth, Hz')
getBandwidth = function(f) {
  b = rep(NA, length(f))
  f1 = which(f < 250)
  f2 = which(f >= 250 & f < 500)
  f3 = which(f >= 500)
  # just guesswork below 250 Hz - no data for such low freqs,
  # apart from elephant rumbles etc.
  b[f1] = 26.38541 - .042 * f[f1] + .0011 * f[f1] ^ 2
  # see Khodai-Joopari & Clermont 2002
  b[f2] =  52.08333 * (1 + (f[f2]-500) ^ 2/ 10 ^ 5)
  b[f3] = 50 * (1 + f[f3] ^ 2 / 6 / 10 ^ 6)
  # plot(f, b, type = 'l')
  return(b)
}


#' Prepare a list of formants
#'
#' Internal soundgen function.
#'
#' Takes a string of phonemes entered WITHOUT ANY BREAKS. Recognized phonemes in
#' the human preset dictionary: vowels "a" "o" "i" "e" "u" "0" (schwa);
#' consonants "s" "x" "j".
#' @param phonemeString a string of characters from the dictionary of phoneme
#'   presets, e.g., uaaaaii (short u - longer a - medium-long i)
#' @param speaker name of the preset dictionary to use
#' @return Returns a list of formant values, which can be fed directly into
#'   \code{\link{getSpectralEnvelope}}
#' @keywords internal
#' @examples
#' formants = soundgen:::convertStringToFormants(phonemeString = 'a')
#' formants = soundgen:::convertStringToFormants(
#'   phonemeString = 'au', speaker = 'M1')
#' formants = soundgen:::convertStringToFormants(
#'   phonemeString = 'aeui', speaker = 'F1')
#' formants = soundgen:::convertStringToFormants(
#'   phonemeString = 'aaeuiiiii', speaker = 'Chimpanzee')
convertStringToFormants = function(phonemeString, speaker = 'M1') {
  availablePresets = names(presets[[speaker]]$Formants$vowels)
  if (length(availablePresets) < 1) {
    warning(paste0('No phoneme presets found for speaker ', speaker,
                   '. Defaulting to M1'))
    speaker = 'M1'
    availablePresets = names(presets[[speaker]]$Formants$vowels)
  }
  input_phonemes = strsplit(phonemeString, "")[[1]]
  valid_phonemes = input_phonemes[input_phonemes %in% availablePresets]
  unique_phonemes = unique(valid_phonemes)
  if (length(valid_phonemes) < 1)
    return(NA)

  # for each input vowel, look up the corresponding formant values
  # in the presets dictionary and append to formants
  vowels = list()
  formantNames = character()
  for (v in 1:length(unique_phonemes)) {
    vowels[[v]] = presets[[speaker]]$Formants$vowels[unique_phonemes[v]][[1]]
    formantNames = c(formantNames, names(vowels[[v]]))
  }
  formantNames = sort(unique(formantNames))
  names(vowels) = unique_phonemes

  # make sure we have filled in info on all formants from the entire
  # sequence of vowels for each individual vowel
  for (v in 1:length(vowels)) {
    absentFormants = formantNames[!formantNames %in% names(vowels[[v]])]
    for (f in absentFormants) {
      closestFreq = unlist(sapply(vowels, function(x) x[f]))
      # names_stripped = substr(names(closestFreq),
      #                         nchar(names(closestFreq)) - 3,
      #                         nchar(names(closestFreq)))
      # closestFreq = closestFreq[which(names_stripped == 'freq')]
      vowels[[v]] [[f]] = as.numeric(na.omit(closestFreq))[1]
      # NB: instead of the last [1], ideally we should specify some intelligent
      # way to pick up the closest vowel with this missing formant, not just the
      # first one, but that's only a problem in long sequences of vowels with
      # really different numbers of formants (nasalization)
    }
  }

  # initialize a common list of exact formants
  formants = vector("list", length(formantNames))
  names(formants) = formantNames

  # for each vowel, append its formants to the common list
  for (v in 1:length(valid_phonemes)) {
    vowel = vowels[[valid_phonemes[v]]]
    for (f in 1:length(vowel)) {
      formantName = names(vowel)[f]
      formants[[formantName]] = c(formants[[formantName]], vowel[[f]])
    }
  }

  return(formants)
}


#' Add formants
#'
#' A spectral filter that either adds or removes formants from a sound - that
#' is, amplifies or dampens certain frequency bands, as in human vowels. See
#' \code{\link{soundgen}} and \code{\link{getSpectralEnvelope}} for more
#' information. With \code{action = 'remove'} this function can perform inverse
#' filtering to remove formants and obtain raw glottal output, provided that you
#' can specify the correct formant structure.
#'
#' Algorithm: converts input from a time series (time domain) to a spectrogram
#' (frequency domain) through short-term Fourier transform (STFT), multiples by
#' the spectral filter containing the specified formants, and transforms back to
#' a time series via inverse STFT. This is a subroutine in
#' \code{\link{soundgen}}, but it can also be used on any existing sound.
#'
#' @seealso \code{\link{getSpectralEnvelope}} \code{\link{transplantFormants}}
#'   \code{\link{soundgen}}
#'
#' @param sound numeric vector with \code{samplingRate}
#' @param action 'add' = add formants to the sound, 'remove' = remove formants
#'   (inverse filtering)
#' @param spectralEnvelope (optional): as an alternative to specifying formant
#'   frequencies, we can provide the exact filter - a vector of non-negative
#'   numbers specifying the power in each frequency bin on a linear scale
#'   (interpolated to length equal to windowLength_points/2). A matrix
#'   specifying the filter for each STFT step is also accepted. The easiest way
#'   to create this matrix is to call soundgen:::getSpectralEnvelope or to use
#'   the spectrum of a recorded sound
#' @param formDrift,formDisp scaling factors for the effect of temperature on
#'   formant drift and dispersal, respectively
#' @param windowLength_points length of FFT window, points
#' @param interpol the method of smoothing envelopes based on provided mouth
#'   anchors: 'approx' = linear interpolation, 'spline' = cubic spline, 'loess'
#'   (default) = polynomial local smoothing function. NB: this does NOT affect
#'   the smoothing of formant anchors
#' @param normalize if TRUE, normalizes the output to range from -1 to +1
#' @inheritParams soundgen
#' @export
#' @examples
#' sound = c(rep(0, 1000), runif(16000), rep(0, 1000))  # white noise
#' # NB: pad with silence to avoid artefacts if removing formants
#' # playme(sound)
#' # spectrogram(sound, samplingRate = 16000)
#'
#' # add F1 = 900, F2 = 1300 Hz
#' sound_filtered = addFormants(sound, formants = c(900, 1300))
#' # playme(sound_filtered)
#' # spectrogram(sound_filtered, samplingRate = 16000)
#'
#' # ...and remove them again (assuming we know what the formants are)
#' sound_inverse_filt = addFormants(sound_filtered,
#'                                  formants = c(900, 1300),
#'                                  action = 'remove')
#' # playme(sound_inverse_filt)
#' # spectrogram(sound_inverse_filt, samplingRate = 16000)
#'
#' \dontrun{
#' # Use the spectral envelope of an existing recording (bleating of a sheep)
#' # (see also the same example with noise as source in ?generateNoise)
#' data(sheep, package = 'seewave')  # import a recording from seewave
#' sound_orig = as.numeric(scale(sheep@left))
#' samplingRate = sheep@samp.rate
#' sound_orig = sound_orig / max(abs(sound_orig))  # range -1 to +1
#' # playme(sound_orig, samplingRate)
#'
#' # get a few pitch anchors to reproduce the original intonation
#' pitch = analyze(sound_orig, samplingRate = samplingRate,
#'   pitchMethod = c('autocor', 'dom'))$pitch
#' pitch = pitch[!is.na(pitch)]
#' pitch = pitch[seq(1, length(pitch), length.out = 10)]
#'
#' # extract a frequency-smoothed version of the original spectrogram
#' # to use as filter
#' specEnv_bleating = spectrogram(sound_orig, windowLength = 5,
#'  samplingRate = samplingRate, output = 'original', plot = FALSE)
#' # image(t(log(specEnv_bleating)))
#'
#' # Synthesize source only, with flat spectrum
#' sound_unfilt = soundgen(sylLen = 2500, pitch = pitch,
#'   rolloff = 0, rolloffOct = 0, rolloffKHz = 0,
#'   temperature = 0, jitterDep = 0, subDep = 0,
#'   formants = NULL, lipRad = 0, samplingRate = samplingRate)
#' # playme(sound_unfilt, samplingRate)
#' # seewave::meanspec(sound_unfilt, f = samplingRate, dB = 'max0')  # ~flat
#'
#' # Force spectral envelope to the shape of target
#' sound_filt = addFormants(sound_unfilt, formants = NULL,
#'   spectralEnvelope = specEnv_bleating, samplingRate = samplingRate)
#' # playme(sound_filt, samplingRate)  # playme(sound_orig, samplingRate)
#' # spectrogram(sound_filt, samplingRate)  # spectrogram(sound_orig, samplingRate)
#'
#' # The spectral envelope is now similar to the original recording. Compare:
#' par(mfrow = c(1, 2))
#' seewave::meanspec(sound_orig, f = samplingRate, dB = 'max0', alim = c(-50, 20))
#' seewave::meanspec(sound_filt, f = samplingRate, dB = 'max0', alim = c(-50, 20))
#' par(mfrow = c(1, 1))
#' # NB: but the source of excitation in the original is actually a mix of
#' # harmonics and noise, while the new sound is purely tonal
#' }
addFormants = function(sound,
                       formants,
                       spectralEnvelope = NULL,
                       action = c('add', 'remove')[1],
                       vocalTract = NA,
                       formantDep = 1,
                       formantDepStoch = 20,
                       formantWidth = 1,
                       formantCeiling = 2,
                       lipRad = 6,
                       noseRad = 4,
                       mouthOpenThres = 0,
                       mouth = NA,
                       interpol = c('approx', 'spline', 'loess')[3],
                       temperature = 0.025,
                       formDrift = 0.3,
                       formDisp = 0.2,
                       samplingRate = 16000,
                       windowLength_points = 800,
                       overlap = 75,
                       normalize = TRUE) {
  formants = reformatFormants(formants)
  # prepare vocal tract filter (formants + some spectral noise + lip radiation)
  if (sum(sound) == 0) {
    # otherwise fft glitches
    soundFiltered = sound
  } else {
    # pad input with one windowLength_points of 0 to avoid softening the attack
    sound = c(rep(0, windowLength_points),
              sound,
              rep(0, windowLength_points))

    # for very short sounds, make sure the analysis window is no more
    #   than half the sound's length
    windowLength_points = min(windowLength_points, floor(length(sound) / 2))
    step = seq(1,
               max(1, (length(sound) - windowLength_points)),
               windowLength_points - (overlap * windowLength_points / 100))
    nc = length(step) # number of windows for fft
    nr = windowLength_points / 2 # number of frequency bins for fft

    # are formants moving or stationary?
    if (is.null(spectralEnvelope)) {
      if (is.list(formants)) {
        movingFormants = max(sapply(formants, function(x) sapply(x, length))) > 1
      } else {
        movingFormants = FALSE
      }
      if (is.list(mouth)) {
        if (sum(mouth$value != .5) > 0) {
          movingFormants = TRUE
        }
      }
      if (is.list(vocalTract)) {
        if (length(vocalTract$value) > 1) {
          movingFormants = TRUE
        }
      }
      nInt = ifelse(movingFormants, nc, 1)

      # prepare the filter
      spectralEnvelope = getSpectralEnvelope(
        nr = nr,
        nc = nInt,
        formants = formants,
        formantDep = formantDep,
        formantDepStoch = formantDepStoch,
        formantWidth = formantWidth,
        formantCeiling = formantCeiling,
        lipRad = lipRad,
        noseRad = noseRad,
        mouthOpenThres = mouthOpenThres,
        mouth = mouth,
        interpol = interpol,
        temperature = temperature,
        formDrift = formDrift,
        formDisp = formDisp,
        samplingRate = samplingRate,
        vocalTract = vocalTract
      )
    } else {  # user-provided spectralEnvelope
      spectralEnvelope = as.matrix(spectralEnvelope)  # if a vector,
      # becomes a matrix with one row
      if (nrow(spectralEnvelope) > 1) {
        nInt = nc
        movingFormants = TRUE
      } else {  # vector
        nInt = 1
        vingFormants = FALSE
      }
      spectralEnvelope = interpolMatrix(
        spectralEnvelope,
        nr = nr,
        nc = nInt,
        interpol = 'approx'
      )
    }
    # image(t(spectralEnvelope))

    # fft and filtering
    z = seewave::stdft(
      wave = as.matrix(sound),
      f = samplingRate,
      wl = windowLength_points,
      zp = 0,
      step = step,
      wn = 'hamming',
      fftw = FALSE,
      scale = TRUE,
      complex = TRUE
    )

    if (action == 'add') {
      if (movingFormants) {
        z = z * spectralEnvelope
      } else {
        z = apply(z, 2, function(x)
          x * spectralEnvelope)
      }
    } else if (action == 'remove') {
      if (movingFormants) {
        z = z / spectralEnvelope
      } else {
        z = apply(z, 2, function(x)
          x / spectralEnvelope)
      }
    }

    # inverse fft
    soundFiltered = as.numeric(
      seewave::istft(
        z,
        f = samplingRate,
        ovlp = overlap,
        wl = windowLength_points,
        output = "matrix"
      )
    )
    # normalize
    if (normalize) {
      soundFiltered = soundFiltered - mean(soundFiltered)
      soundFiltered = soundFiltered / max(abs(soundFiltered))
    }
  }

  # remove zero padding
  l = length(soundFiltered)
  tailIdx = suppressWarnings(max(which(soundFiltered[(l - windowLength_points):l] > .03)))
  idx = l - windowLength_points + tailIdx - 1
  if(!is.finite(idx)) idx = l - windowLength_points
  soundFiltered = soundFiltered[(windowLength_points + 1):idx]

  # spectrogram(soundFiltered, samplingRate = samplingRate)
  # playme(soundFiltered, samplingRate = samplingRate)
  return(soundFiltered)
}


#' Transplant formants
#'
#' Takes the general spectral envelope of one sound (\code{donor}) and
#' "transplants" it onto another sound (\code{recipient}). For biological sounds
#' like speech or animal vocalizations, this has the effect of replacing the
#' formants in the recipient sound while preserving the original intonation and
#' (to some extent) voice quality. Note that \code{freqWindow_donor} and
#' \code{freqWindow_recipient} are crucial parameters that regulate the amount
#' of spectral smoothing in both sounds. The default is to set them to the
#' estimated median pitch, but this is time-consuming and error-prone, so set
#' them to reasonable values manually if possible. Also ensure that both sounds
#' have the same sampling rate.
#'
#' Algorithm: makes spectrograms of both sounds, interpolates and smoothes the
#' donor spectrogram, flattens the recipient spectrogram, multiplies the
#' spectrograms, and transforms back into time domain with inverse STFT.
#'
#' @seealso \code{\link{transplantEnv}} \code{\link{getSpectralEnvelope}}
#'   \code{\link{addFormants}} \code{\link{soundgen}}
#'
#' @inheritParams spectrogram
#' @param donor the sound that provides the formants or the desired spectral
#'   filter as returned by \code{\link{getSpectralEnvelope}}
#' @param recipient the sound that receives the formants
#' @param freqWindow_donor,freqWindow_recipient the width of smoothing window.
#'   Defaults to median pitch of each respective sound estimated by
#'   \code{\link{analyze}}
#' @export
#' @examples
#' \dontrun{
#' # Objective: take formants from the bleating of a sheep and apply them to a
#' # synthetic sound with any arbitrary duration, intonation, nonlinearities etc
#' data(sheep, package = 'seewave')  # import a recording from seewave
#' donor = as.numeric(scale(sheep@left))  # source of formants
#' samplingRate = sheep@samp.rate
#' playme(donor, samplingRate)
#' spectrogram(donor, samplingRate, osc = TRUE)
#' seewave::meanspec(donor, f = samplingRate, dB = 'max0')
#'
#' recipient = soundgen(sylLen = 1200,
#'                      pitch = c(100, 300, 250, 200),
#'                      vibratoFreq = 9, vibratoDep = 1,
#'                      addSilence = 180,
#'                      samplingRate = samplingRate)
#' playme(recipient, samplingRate)
#' spectrogram(recipient, samplingRate, osc = TRUE)
#'
#' s1 = transplantFormants(
#'   donor = donor,
#'   recipient = recipient,
#'   samplingRate = samplingRate)
#' playme(s1, samplingRate)
#' spectrogram(s1, samplingRate, osc = TRUE)
#' seewave::meanspec(s1, f = samplingRate, dB = 'max0')
#'
#' # if needed, transplant amplitude envelopes as well:
#' s2 = transplantEnv(donor = donor, samplingRateD = samplingRate,
#'                    recipient = s1, windowLength = 10)
#' playme(s2, samplingRate)
#' spectrogram(s2, samplingRate, osc = TRUE)
#'
#' # Now we use human formants on sheep source: the sheep says "why?"
#' s2 = transplantFormants(
#'   donor = soundgen(formants = 'uaaai',
#'                    samplingRate = samplingRate),
#'   recipient = donor,
#'   samplingRate = samplingRate)
#' playme(s2, samplingRate)
#' spectrogram(s2, samplingRate, osc = TRUE)
#' seewave::meanspec(s2, f = samplingRate, dB = 'max0')
#'
#' # We can also transplant synthetic formants w/o synthesizing a donor sound to
#' save time
#' s3 = transplantFormants(
#'   donor = getSpectralEnvelope(
#'             nr = 512, nc = 100,  # fairly arbitrary dimensions
#'             formants = 'uaaai',
#'             samplingRate = samplingRate),
#'   recipient = donor,
#'   samplingRate = samplingRate)
#' playme(s3, samplingRate)
#' spectrogram(s3, samplingRate, osc = TRUE)
#' }
transplantFormants = function(donor,
                              freqWindow_donor = NULL,
                              recipient,
                              freqWindow_recipient = NULL,
                              samplingRate = NULL,
                              dynamicRange = 80,
                              windowLength = 50,
                              step = NULL,
                              overlap = 90,
                              wn = 'gaussian',
                              zp = 0) {
  # First check that both sounds have the same sampling rate
  samplingRate_donor = samplingRate_recipient = 0
  if (is.character(donor)) {
    extension = substr(donor, nchar(donor) - 2, nchar(donor))
    if (extension == 'wav' | extension == 'WAV') {
      donor_wav = tuneR::readWave(donor)
    } else if (extension == 'mp3' | extension == 'MP3') {
      donor_wav = tuneR::readMP3(donor)
    } else {
      stop('Donor not recognized: must be a numeric vector or wav/mp3 file')
    }
    samplingRate_donor = donor_wav@samp.rate
    donor = as.numeric(donor_wav@left)
  }

  if (is.character(recipient)) {
    extension = substr(recipient, nchar(recipient) - 2, nchar(recipient))
    if (extension == 'wav' | extension == 'WAV') {
      recipient_wav = tuneR::readWave(recipient)
    } else if (extension == 'mp3' | extension == 'MP3') {
      recipient_wav = tuneR::readMP3(recipient)
    } else {
      stop('recipient not recognized: must be a numeric vector or wav/mp3 file')
    }
    samplingRate_recipient = recipient_wav@samp.rate
    recipient = as.numeric(recipient_wav@left)
  }

  if (samplingRate_donor > 0 & samplingRate_recipient > 0) {
    # two audio files
    if (samplingRate_donor != samplingRate_recipient) {
      stop('Please use two sounds with the same sampling rate')
    } else {
      samplingRate = samplingRate_donor  # or recipient - they are the same
    }
  } else if (samplingRate_donor == 0 & samplingRate_recipient == 0) {
    # two vectors
    if (!is.numeric(samplingRate)) {
      stop('Please specify sampling rate')
    }
  } else {
    # one audio file, one vector
    if (!is.numeric(samplingRate)) {
      samplingRate = max(samplingRate_donor, samplingRate_recipient)
      message(paste('Sampling rate not specified; assuming the same',
                    'as for the audio file, namely', samplingRate, 'Hz'))
    } else if (samplingRate != max(samplingRate_donor, samplingRate_recipient)) {
      stop('Please use two sounds with the same sampling rate')
    }
  }

  windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
  spec_recipient = spectrogram(
    recipient,
    samplingRate = samplingRate,
    dynamicRange = dynamicRange,
    windowLength = windowLength,
    step = step,
    overlap = overlap,
    wn = wn,
    zp = zp,
    output = 'complex',
    padWithSilence = FALSE,
    plot = FALSE
  )
  if (!is.matrix(donor)) {
    # donor is a sound
    spec_donor = spectrogram(
      donor,
      samplingRate = samplingRate,
      dynamicRange = dynamicRange,
      windowLength = windowLength,
      step = step,
      overlap = overlap,
      wn = wn,
      zp = zp,
      output = 'original',
      padWithSilence = FALSE,
      plot = FALSE
    )
    # Make sure the donor spec has the same dimensions as the recipient spec
    spec_donor_rightDim = interpolMatrix(m = spec_donor,
                                         nr = nrow(spec_recipient),
                                         nc = ncol(spec_recipient))
  } else {
    # donor is a matrix (spectrogram giving the desired formant structure)
    spec_donor_rightDim = interpolMatrix(m = donor,
                                         nr = nrow(spec_recipient),
                                         nc = ncol(spec_recipient))
  }
  rownames(spec_donor_rightDim) = rownames(spec_recipient)

  # Flatten the recipient spectrogram
  if (!is.numeric(freqWindow_recipient)) {
    anal_recipient = analyze(recipient, samplingRate, plot = FALSE)
    freqWindow_recipient = median(anal_recipient$pitch, na.rm = TRUE)
  }
  freqRange_kHz_rec = diff(range(as.numeric(rownames(spec_recipient))))
  freqBin_Hz_rec = freqRange_kHz_rec * 1000 / nrow(spec_recipient)
  freqWindow_rec_bins = round(freqWindow_recipient / freqBin_Hz_rec, 0)
  if (freqWindow_rec_bins < 3) {
    message(paste('freqWindow_recipient has to be at least 3 bins wide;
                  resetting to', ceiling(freqBin_Hz_rec * 3)))
    freqWindow_rec_bins = 3
  }
  for (i in 1:ncol(spec_recipient)) {
    abs_s = abs(spec_recipient[, i])
    # plot(abs_s, type = 'l')
    cor_coef = flatEnv(abs_s, method = 'peak',
                       windowLength_points = freqWindow_rec_bins) / abs_s
    # plot(Re(spec_recipient[, i]) * cor_coef, type = 'l')
    spec_recipient[, i] = complex(
      real = Re(spec_recipient[, i]) * cor_coef,
      imaginary = Im(spec_recipient[, i])
    )
    # plot(abs(spec_recipient[, i]), type = 'l')
  }

  # Smooth the donor spectrogram
  if (!is.numeric(freqWindow_donor)) {
    if (is.matrix(donor)) {
      freqWindow_donor = freqWindow_recipient
    } else {
      anal_donor = analyze(donor, samplingRate, plot = FALSE)
      freqWindow_donor = median(anal_donor$pitch, na.rm = TRUE)
    }
  }
  freqRange_kHz_donor = diff(range(as.numeric(rownames(spec_donor_rightDim))))
  freqBin_Hz_donor = freqRange_kHz_donor * 1000 / nrow(spec_donor_rightDim)
  freqWindow_donor_bins = round(freqWindow_donor / freqBin_Hz_donor, 0)
  if (freqWindow_donor_bins < 3) {
    message(paste('freqWindow_donor has to be at least 3 bins wide;
                  resetting to', ceiling(freqBin_Hz_donor * 3)))
    freqWindow_donor_bins = 3
  }
  # plot(spec_donor_rightDim[, 10], type = 'l')
  for (i in 1:ncol(spec_donor_rightDim)) {
    spec_donor_rightDim[, i] = getEnv(
      sound = spec_donor_rightDim[, i],
      windowLength_points = freqWindow_donor_bins,
      method = 'peak'
    )
  }

  # Multiply the spectrograms and reconstruct the audio
  spec_recipient_new = spec_recipient * spec_donor_rightDim
  recipient_new = as.numeric(
    seewave::istft(
      spec_recipient_new,
      f = samplingRate,
      ovlp = overlap,
      wl = windowLength_points,
      output = "matrix"
    )
  )
  # spectrogram(donor, samplingRate)
  # spectrogram(recipient_new, samplingRate)
  return(recipient_new)
}
