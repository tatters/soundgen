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
#' @param formantDepStoch multiplication factor for the amplitude of additional
#'   formants added above the highest specified formant (0 = none, 1 = default)
#' @param smoothLinearFactor regulates smoothing of formant anchors (0 to +Inf)
#'   as they are upsampled to the number of fft steps \code{nc}. This is
#'   necessary because the input \code{formants} normally contains fewer
#'   sets of formant values than the number of fft steps.
#'   \code{smoothLinearFactor} = 0: close to default spline; >3: approaches
#'   linear extrapolation
#' @param output "simple" returns just the spectral filter, while "detailed"
#'   also returns a data.frame of formant frequencies over time (needed for
#'   internal purposes such as formant locking)
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
#' # [a] with only F1-F3 visible, with no stochasticity
#' e = getSpectralEnvelope(nr = 512, nc = 50, duration = 300,
#'   formants = soundgen:::convertStringToFormants('a'),
#'   temperature = 0, plot = TRUE)
#' # image(t(e))  # to plot the output on a linear scale instead of dB
#'
#' # some "wiggling" of specified formants plus extra formants on top
#' e = getSpectralEnvelope(nr = 512, nc = 50,
#'   formants = c(860, 1430, 2900),
#'   temperature = 0.1, formantDepStoch = 1, plot = TRUE)
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
#' plot(as.numeric(rownames(e2)), 20 * log10(e2[, 1]),
#'      type = 'l', xlab = 'KHz', ylab = 'dB', col = 'red', lty = 2)
#' points(as.numeric(rownames(e1)), 20 * log10(e1[, 1]), type = 'l')
#'
#' # manual specification of formants
#' e3 = getSpectralEnvelope(
#'   nr = 512, nc = 50, samplingRate = 16000, plot = TRUE,
#'   formants = list(
#'     f1 = list(freq = c(900, 500), amp = c(30, 35), width = c(80, 50)),
#'     f2 = list(freq = c(1900, 2500), amp = c(25, 30), width = 100),
#'     f3 = list(freq = 3400, amp = 30, width = 120)
#' ))
#'
#' # extra zero-pole pair (doesn't affect estimated VTL and thus the extra
#' # formants added on top)
#' e4 = getSpectralEnvelope(
#'   nr = 512, nc = 50, samplingRate = 16000, plot = TRUE,
#'   formants = list(
#'     f1 = list(freq = c(900, 500), amp = c(30, 35), width = c(80, 50)),
#'     f1.5 = list(freq = 1300, amp = -15),
#'     f1.7 = list(freq = 1500, amp = 15),
#'     f2 = list(freq = c(1900, 2500), amp = c(25, 30), width = 100),
#'     f3 = list(freq = 3400, amp = 30, width = 120)
#' ))
#' plot(as.numeric(rownames(e4)), 20 * log10(e3[, ncol(e3)]),
#'      type = 'l', xlab = 'KHz', ylab = 'dB')
#' points(as.numeric(rownames(e4)), 20 * log10(e4[, ncol(e4)]),
#'        type = 'l', col = 'red', lty = 2)
getSpectralEnvelope = function(
  nr,
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
  formantDepStoch = 1,
  smoothLinearFactor = 1,
  formantCeiling = 2,
  samplingRate = 16000,
  speedSound = 35400,
  output = c('simple', 'detailed')[1],
  plot = FALSE,
  duration = NULL,
  colorTheme = c('bw', 'seewave', '...')[1],
  nCols = 100,
  xlab = 'Time',
  ylab = 'Frequency, kHz',
  ...
) {
  # standard formatting
  formants = reformatFormants(formants)
  if (is.list(formants)) {
    bandwidth_specified = as.numeric(which(unlist(
      lapply(formants, function(x) 'width' %in% names(x))
    )))
  } else {
    bandwidth_specified = numeric(0)
  }

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
                             checkFormat = TRUE)  # may need to remove non-integer
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

  # create a "spectrogram"-shaped filter matrix
  spectralEnvelope = matrix(0, nrow = nr, ncol = nc)

  ### START OF FORMANTS
  if (is.list(formants)) {
    # upsample to the length of fft steps
    nPoints = max(unlist(lapply(formants, nrow)))
    formants_upsampled = vector('list', length = length(formants))
    for (f in 1:length(formants)) {
      formant_f = data.frame(time = seq(0, 1, length.out = nc))
      for (v in c('freq', 'amp', 'width')) {
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
      if (!f %in% bandwidth_specified) {
        formant_f$width = getBandwidth(formant_f$freq)
      }
      formants_upsampled[[f]] = formant_f
    }
    names(formants_upsampled) = names(formants)
    nFormants = length(formants)
    amplScaleFactor = rep(1, nFormants)

    ## Stochastic part (only for temperature > 0)
    if (temperature > 0) {
      # non-integer formants like "f1.4" refer to extra zero-pole pairs.
      # They should not be considered for VTL estimation or for adding formants
      non_integer_formants = apply(
        as.matrix(names(formants_upsampled)), 1, function(x) {
          grepl('.', x, fixed = TRUE)
        })
      # create a few new, relatively high-frequency extra formants
      if(!is.numeric(formantDepStoch)) formantDepStoch = 1
      if (!is.numeric(vocalTract) & length(formants) > 1 &
          formantDepStoch > 0 & formantDep > 0) {
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
      nFormants_integer = length(formants_upsampled) - sum(non_integer_formants)
      freq_max = max(formants_upsampled[[nFormants]][, 'freq'])

      if (!any(is.na(sdG))) {
        # formant_f = (2 * f - 1) / 2 * formantDispersion,
        # therefore, to generate formants to 2 * Nyquist
        # (to compensate for downward drag of lower formants)
        # 2 * nyquist = (2 * nExtraFormants - 1) / 2 * formantDispersion
        # Solving for nExtraFormants gives (nyquist * 4 / formantDispersion + 1) / 2:
        nExtraFormants = round(
          (samplingRate * formantCeiling / min(formantDispersion) + 1) / 2
        ) - nFormants
        if (is.numeric(nExtraFormants) && nExtraFormants > 0) {
          # if we are going to add extra formants
          nf = length(formantDispersion)
          extraFreqs = extraWidths = matrix(NA, nrow = nf, ncol = nExtraFormants)
          extraAmps = rgamma(
            nExtraFormants,
            # mean = formantDepStoch, sd = formantDepStoch * temperature
            1 / temperature ^ 2,
            1 / (formantDepStoch * temperature ^ 2)
          )
          amplScaleFactor = c(amplScaleFactor, extraAmps)
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
            extraWidths[frame, ] = getBandwidth(extraFreqs[frame, ])
          }

          formants_upsampled = c(formants_upsampled, vector('list', nExtraFormants))
          for (f in 1:nExtraFormants) {
            formants_upsampled[[nFormants + f]] = data.frame (
              'time' = formants_upsampled[[1]][, 'time'],
              'freq' = extraFreqs[, f],
              'amp' = NA,
              'width' = extraWidths[, f]
            )
          }
        }
      }

      # wiggle both user-specified and stochastic formants
      nFormants = length(formants_upsampled)
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
        # air. The formula for mouth opening is adapted from Moore (2016) "A
        # Real-Time Parametric General-Purpose Mammalian Vocal Synthesiser".
        # mouthOpening = .5 gives no modification (neutral, "default" position).
        # Basically we could assume a closed-closed tube for closed mouth and a
        # closed-open tube for open mouth, but since formants can be specified
        # rather than calculated based on vocalTract, we just subtract half the
        # total difference between open and closed tubes in Hz from each formant
        # value as the mouth goes from half-open (neutral) to fully closed, or
        # we add half that value as the mouth goes from neutral to max open. NB:
        # so "closed" is actually "half-closed", and we assume that nostrils are
        # always open (so not really a closed-closed tube)
        adjustment_bins = adjustment_hz / bin_width
      }
    }
    for (f in 1:nFormants) {
      formants_upsampled[[f]][, 'freq'] =
        formants_upsampled[[f]][, 'freq'] + adjustment_bins
      # force each formant frequency to be positive (min 1 bin)
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
      formants_upsampled$fnp[nasalizedIdx, 'amp'] = NA
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
      formants_upsampled$fnz[nasalizedIdx, 'amp'] = NA
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
      nFormants = length(formants_upsampled)
      amplScaleFactor = c(amplScaleFactor, .5, .5)
      # make the added zero-pole half as strong as ordinary formants
    }

    # Add formants to spectrogram (Stevens 2000, Ch. 3, ~p. 137)
    freqs_bins = 1:nr
    poles = 1:nFormants
    zeros = as.numeric(which(sapply(
      formants_upsampled, function(x) any(x[, 'amp'] < 0)
    )))
    if (length(zeros) > 0) {
      poles = poles[-zeros]
      for (z in zeros)
        formants_upsampled[[z]]$amp = -formants_upsampled[[z]]$amp
      # need to have positive amp values (we know which ones are zeros)
    }
    s = complex(real = 0, imaginary = 2 * pi * freqs_bins)
    for (f in 1:nFormants) {
      pf = 2 * pi * formants_upsampled[[f]][, 'freq']
      bp = -formants_upsampled[[f]][, 'width'] * pi
      sf = complex(real = bp, imaginary = pf)
      sfc = Conj(sf)
      formant = matrix(0, nrow = nr, ncol = nc)
      for (c in 1:nc) {
        pole = (f %in% poles)
        numerator = sf[c] * sfc[c]
        denominator = (s - sf[c]) * (s - sfc[c])
        if (pole) {
          tns =  numerator / denominator  # pole
        } else {
          tns = denominator / numerator   # zero
        }
        formant[, c] = log10(abs(tns))
        if (is.na(formants_upsampled[[f]][c, 'amp'])) {
          # just convert to dB
          formant[, c] = formant[, c] * 20 * amplScaleFactor[f]
        } else {
          # normalize ampl to be exactly as specified in dB
          if (pole) m = max(formant[, c]) else m = -min(formant[, c])
          formant[, c] = formant[, c] / m *
            formants_upsampled[[f]][c, 'amp'] * amplScaleFactor[f]
          # amplScaleFactor is 1 for user-specified and formantDepStoch otherwise
        }
      }
      # plot(formant[, c], type = 'l')
      spectralEnvelope = spectralEnvelope + formant
    }
    spectralEnvelope = spectralEnvelope * formantDep
  } else {
    mouthOpen_binary = rep(1, nc)
    mouthOpening_upsampled = rep(0.5, nc)
    bin_width = samplingRate / 2 / nr # otherwise it's not defined if formants = NULL
  }

  # save frequency and time stamps
  freqs = (0:(nr - 1)) * bin_width / 1000
  rownames(spectralEnvelope) = freqs
  if (is.numeric(duration)) {
    colnames(spectralEnvelope) = seq(0, duration, length.out = nc)
  } else {
    colnames(spectralEnvelope) = seq(0, 1, length.out = nc)
  }
  # plot(freqs, spectralEnvelope[, 1], type = 'l')
  # image(t(spectralEnvelope))

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
  # END OF FORMANTS

  # add lip radiation when the mouth is open and nose radiation when the mouth
  # is closed
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

  if (output == 'detailed') {
    if (exists('formants_upsampled')) {
      formantSummary = as.data.frame(matrix(NA, nrow = nFormants, ncol = nc))
      for (i in 1:nFormants) {
        formantSummary[i, ] = (formants_upsampled[[i]]$freq - 1) *
          bin_width + bin_width / 2  # from bins back to Hz
      }
      max_freqs = apply(formantSummary, 1, max)  # save only to Nyquist
      formantSummary = formantSummary[which(max_freqs < (samplingRate / 2)), ]
      colnames(formantSummary) = colnames(spectralEnvelope)
    } else {
      formantSummary = NULL
    }
    invisible(list(formantSummary = formantSummary,
                   specEnv = spectralEnvelope_lin))
  } else {
    invisible(spectralEnvelope_lin)
  }
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
#' @inheritParams soundgen
#' @inheritParams spectrogram
#' @inheritParams segment
#' @param action 'add' = add formants to the sound, 'remove' = remove formants
#'   (inverse filtering)
#' @param spectralEnvelope (optional): as an alternative to specifying formant
#'   frequencies, we can provide the exact filter - a vector of non-negative
#'   numbers specifying the power in each frequency bin on a linear scale
#'   (interpolated to length equal to windowLength_points/2). A matrix
#'   specifying the filter for each STFT step is also accepted. The easiest way
#'   to create this matrix is to call soundgen:::getSpectralEnvelope or to use
#'   the spectrum of a recorded sound
#' @param zFun (optional) an arbitrary function to apply to the spectrogram
#'   prior to iSTFT, where "z" is the spectrogram - a matrix of complex values
#'   (see examples)
#' @param formDrift,formDisp scaling factors for the effect of temperature on
#'   formant drift and dispersal, respectively
#' @param windowLength_points length of FFT window, points
#' @param interpol the method of smoothing envelopes based on provided mouth
#'   anchors: 'approx' = linear interpolation, 'spline' = cubic spline, 'loess'
#'   (default) = polynomial local smoothing function. NB: this does NOT affect
#'   the smoothing of formant anchors
#' @param normalize if TRUE, normalizes the output to range from -1 to +1
#' @export
#' @examples
#' sound = c(rep(0, 1000), runif(8000) * 2 - 1, rep(0, 1000))  # white noise
#' # NB: pad with silence to avoid artefacts if removing formants
#' # playme(sound)
#' # spectrogram(sound, samplingRate = 16000)
#'
#' # add F1 = 900, F2 = 1300 Hz
#' sound_filtered = addFormants(sound, samplingRate = 16000,
#'                              formants = c(900, 1300))
#' # playme(sound_filtered)
#' # spectrogram(sound_filtered, samplingRate = 16000)
#'
#' # ...and remove them again (assuming we know what the formants are)
#' sound_inverse_filt = addFormants(sound_filtered,
#'                                  samplingRate = 16000,
#'                                  formants = c(900, 1300),
#'                                  action = 'remove')
#' # playme(sound_inverse_filt)
#' # spectrogram(sound_inverse_filt, samplingRate = 16000)
#'
#' \dontrun{
#' ## Perform some user-defined manipulation of the spectrogram with zFun
#' # Ex.: noise removal - silence all bins under threshold,
#' # say -0 dB below the max value
#' s_noisy = soundgen(sylLen = 200, addSilence = 0,
#'                    noise = list(time = c(-100, 300), value = -20))
#' spectrogram(s_noisy, 16000)
#' # playme(s_noisy)
#' zFun = function(z, cutoff = -50) {
#'   az = abs(z)
#'   thres = max(az) * 10 ^ (cutoff / 20)
#'   z[which(az < thres)] = 0
#'   return(z)
#' }
#' s_denoised = addFormants(s_noisy, samplingRate = 16000,
#'                          formants = NA, zFun = zFun, cutoff = -40)
#' spectrogram(s_denoised, 16000)
#' # playme(s_denoised)
#'
#' ## Use the spectral envelope of an existing recording (bleating of a sheep)
#' # (see also the same example with noise as source in ?generateNoise)
#' # (NB: this can also be achieved with a single call to transplantFormants)
#' data(sheep, package = 'seewave')  # import a recording from seewave
#' sound_orig = as.numeric(scale(sheep@left))
#' samplingRate = sheep@samp.rate
#' sound_orig = sound_orig / max(abs(sound_orig))  # range -1 to +1
#' # playme(sound_orig, samplingRate)
#'
#' # get a few pitch anchors to reproduce the original intonation
#' pitch = analyze(sound_orig, samplingRate = samplingRate,
#'   pitchMethod = c('autocor', 'dom'))$detailed$pitch
#' pitch = pitch[!is.na(pitch)]
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
#'   formants = NULL, lipRad = 0, samplingRate = samplingRate,
#'   invalidArgAction = 'ignore')  # prevent soundgen from increasing samplingRate
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
addFormants = function(
  x,
  samplingRate = NULL,
  formants,
  spectralEnvelope = NULL,
  zFun = NULL,
  action = c('add', 'remove')[1],
  vocalTract = NA,
  formantDep = 1,
  formantDepStoch = 1,
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
  windowLength_points = 800,
  overlap = 75,
  normalize = TRUE,
  play = FALSE,
  saveAudio = NULL,
  reportEvery = NULL,
  ...
) {
  formants = reformatFormants(formants)
  mouth = reformatFormants(mouth)

  # match args
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'saveAudio',
    'formants', 'mouth')]
  myPars$formants = formants
  myPars$mouth = mouth

  pa = processAudio(x,
                    samplingRate = samplingRate,
                    saveAudio = saveAudio,
                    funToCall = '.addFormants',
                    myPars = myPars,
                    reportEvery = reportEvery
  )
  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Add formants per sound
#'
#' Internal soundgen function called by \code{\link{addFormants}}
#' @inheritParams addFormants
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.addFormants = function(
  audio,
  formants,
  spectralEnvelope = NULL,
  zFun = NULL,
  action = c('add', 'remove')[1],
  vocalTract = NA,
  formantDep = 1,
  formantDepStoch = 1,
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
  windowLength_points = 800,
  overlap = 75,
  normalize = TRUE,
  play = FALSE,
  ...
) {
  # prepare vocal tract filter (formants + some spectral noise + lip radiation)
  if (sum(audio$sound) == 0) {
    # otherwise fft glitches
    soundFiltered = audio$sound
  } else {
    # pad input with one windowLength_points of 0 to avoid softening the attack
    sound = c(rep(0, windowLength_points),
              audio$sound,
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
    # (basically always moving, unless temperature = 0 and nothing else changes)
    if (is.null(spectralEnvelope)) {
      if (temperature > 0) {
        movingFormants = TRUE
      } else {
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
        samplingRate = audio$samplingRate,
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
        movingFormants = FALSE
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
      f = audio$samplingRate,
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

    # apply some arbitrary function to the spectrogram before iSTFT
    if (!is.null(zFun) && class(zFun) == 'function')
      z = do.call(zFun, list(z = z, ...))

    # inverse fft
    soundFiltered = as.numeric(
      seewave::istft(
        z,
        f = audio$samplingRate,
        ovlp = overlap,
        wl = windowLength_points,
        output = "matrix"
      )
    )
    # osc(soundFiltered, samplingRate = samplingRate)

    # normalize
    if (normalize) {
      soundFiltered = soundFiltered - mean(soundFiltered)
      soundFiltered = soundFiltered / max(abs(soundFiltered))
    }
  }

  # remove zero padding
  l = length(soundFiltered)
  hl = seewave::env(soundFiltered[(l - windowLength_points + 1):l],
                    f = audio$samplingRate, envt = 'hil', plot = FALSE)
  tailIdx = suppressWarnings(min(which(hl < (.01 * max(hl)))))
  idx = l - windowLength_points + tailIdx
  if(!is.finite(idx)) idx = l # l - windowLength_points
  soundFiltered = soundFiltered[(windowLength_points + 1):idx]
  # osc(soundFiltered, samplingRate = samplingRate)

  if (play) playme(soundFiltered, audio$samplingRate)
  if (is.character(audio$saveAudio)) {
    seewave::savewav(
      soundFiltered, f = audio$samplingRate,
      filename = paste0(audio$saveAudio, audio$filename_noExt, '.wav'))
  }

  # spectrogram(soundFiltered, samplingRate = samplingRate, ylim = c(0, 4))
  # playme(soundFiltered, samplingRate = samplingRate)
  return(soundFiltered)
}


#' Transplant formants
#'
#' Takes the general spectral envelope of one sound (\code{donor}) and
#' "transplants" it onto another sound (\code{recipient}). For biological sounds
#' like speech or animal vocalizations, this has the effect of replacing the
#' formants in the recipient sound while preserving the original intonation and
#' (to some extent) voice quality. Note that \code{freqWindow} is a crucial
#' parameter: too narrow, and noise between harmonics will be amplified,
#' creasing artifacts; too wide, and formants may be missed. The default is to
#' set \code{freqWindow} to the estimated median pitch, but this is
#' time-consuming and error-prone, so set it to a reasonable value manually if
#' possible. Also ensure that both sounds have the same sampling rate.
#'
#' Algorithm: makes spectrograms of both sounds, interpolates and smoothes the
#' donor spectrogram, flattens the recipient spectrogram, multiplies the
#' spectrograms, and transforms back into time domain with inverse STFT.
#'
#' @seealso \code{\link{transplantEnv}} \code{\link{getSpectralEnvelope}}
#'   \code{\link{addFormants}} \code{\link{soundgen}}
#'
#' @inheritParams spectrogram
#' @param donor the sound that provides the formants (vector, Wave, or file) or
#'   the desired spectral filter (matrix) as returned by
#'   \code{\link{getSpectralEnvelope}}
#' @param recipient the sound that receives the formants (vector, Wave, or file)
#' @param freqWindow the width of smoothing window. Defaults to median pitch of
#'   the donor (or of the recipient if donor is a filter matrix)
#' @export
#' @examples
#' \dontrun{
#' # Objective: take formants from the bleating of a sheep and apply them to a
#' # synthetic sound with any arbitrary duration, intonation, nonlinearities etc
#' data(sheep, package = 'seewave')  # import a recording from seewave
#' playme(sheep)
#' spectrogram(sheep, osc = TRUE)
#'
#' recipient = soundgen(
#'   sylLen = 1200,
#'   pitch = c(100, 300, 250, 200),
#'   vibratoFreq = 9, vibratoDep = 1,
#'   addSilence = 180,
#'   samplingRate = sheep@samp.rate,  # same as donor
#'   invalidArgAction = 'ignore')  # force to keep the low samplingRate
#' playme(recipient, sheep@samp.rate)
#' spectrogram(recipient, sheep@samp.rate, osc = TRUE)
#'
#' s1 = transplantFormants(
#'   donor = sheep,
#'   recipient = recipient,
#'   samplingRate = sheep@samp.rate)
#' playme(s1, sheep@samp.rate)
#' spectrogram(s1, sheep@samp.rate, osc = TRUE)
#'
#' # The spectral envelope of s1 will be similar to sheep's on a frequency scale
#' # determined by freqWindow. Compare the spectra:
#' par(mfrow = c(1, 2))
#' seewave::meanspec(sheep, dB = 'max0', alim = c(-50, 20), main = 'Donor')
#' seewave::meanspec(s1, f = sheep@samp.rate, dB = 'max0',
#'                   alim = c(-50, 20), main = 'Processed recipient')
#' par(mfrow = c(1, 1))
#'
#' # if needed, transplant amplitude envelopes as well:
#' s2 = transplantEnv(donor = sheep, samplingRateD = sheep@samp.rate,
#'                    recipient = s1, windowLength = 10)
#' playme(s2, sheep@samp.rate)
#' spectrogram(s2, sheep@samp.rate, osc = TRUE)
#'
#' # Now we use human formants on sheep source: the sheep asks "why?"
#' s3 = transplantFormants(
#'   donor = getSpectralEnvelope(
#'             nr = 512, nc = 100,  # fairly arbitrary dimensions
#'             formants = 'uaaai',
#'             samplingRate = sheep@samp.rate),
#'   recipient = sheep,
#'   samplingRate = sheep@samp.rate)
#' playme(s3, sheep@samp.rate)
#' spectrogram(s3, sheep@samp.rate, osc = TRUE)
#' }
transplantFormants = function(donor,
                              recipient,
                              samplingRate = NULL,
                              freqWindow = NULL,
                              dynamicRange = 80,
                              windowLength = 50,
                              step = NULL,
                              overlap = 90,
                              wn = 'gaussian',
                              zp = 0) {
  if (!is.null(step)) {
    overlap = (1 - step / windowLength) * 100  # for istft
  }

  # Read inputs
  recipient = readAudio(
    recipient,
    input = checkInputType(recipient),
    samplingRate = samplingRate
  )
  if (!is.matrix(donor)) {
    # donor is a sound
    donor = readAudio(
      donor,
      input = checkInputType(donor),
      samplingRate = samplingRate
    )
    # Check that both sounds have the same sampling rate
    if (donor$samplingRate != recipient$samplingRate) {
      stop('Please use two sounds with the same sampling rate')
    }
  }
  samplingRate = recipient$samplingRate  # donor may be a matrix (filter)

  windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
  spec_recipient = spectrogram(
    recipient$sound,
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
      donor$sound,
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
  if (!is.numeric(freqWindow)) {
    if (is.matrix(donor)) {
      # set freqWindow to the median pitch of recipient
      anal_recipient = analyze(recipient$sound, samplingRate, plot = FALSE)
      freqWindow = median(anal_recipient$detailed$pitch, na.rm = TRUE)
    } else {
      # set freqWindow to the median pitch of donor
      anal_donor = analyze(donor$sound, samplingRate, plot = FALSE)
      freqWindow = median(anal_donor$detailed$pitch, na.rm = TRUE)
    }
  }
  freqRange_kHz = diff(range(as.numeric(rownames(spec_recipient))))
  freqBin_Hz = freqRange_kHz * 1000 / nrow(spec_recipient)
  freqWindow_bins = round(freqWindow / freqBin_Hz, 0)
  if (freqWindow_bins < 3) {
    message(paste('freqWindow has to be at least 3 bins wide;
                  resetting to', ceiling(freqBin_Hz * 3)))
    freqWindow_rec_bins = 3
  }
  for (i in 1:ncol(spec_recipient)) {
    abs_s = abs(spec_recipient[, i])
    # plot(abs_s, type = 'l')
    cor_coef = flatEnv(
      abs_s, samplingRate = 1, method = 'peak',
      windowLength_points = freqWindow_bins) / abs_s
    # plot(Re(spec_recipient[, i]) * cor_coef, type = 'l')
    spec_recipient[, i] = complex(
      real = Re(spec_recipient[, i]) * cor_coef,
      imaginary = Im(spec_recipient[, i])
    )
    # plot(abs(spec_recipient[, i]), type = 'l')
  }

  # Smooth the donor spectrogram
  # plot(spec_donor_rightDim[, 10], type = 'l')
  for (i in 1:ncol(spec_donor_rightDim)) {
    spec_donor_rightDim[, i] = getEnv(
      sound = spec_donor_rightDim[, i],
      windowLength_points = freqWindow_bins,
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
  # spectrogram(donor$sound, samplingRate)
  # spectrogram(recipient_new, samplingRate)
  return(recipient_new)
}
