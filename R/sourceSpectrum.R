# Functions for controlling the spectrum of generated sounds (rolloff and formants).

#' Control rolloff of harmonics
#'
#' Harmonics are generated as separate sine waves. But we don't want each
#' harmonic to be equally strong, so we normally specify some rolloff function
#' that describes the loss of energy in upper harmonics relative to the
#' fundamental frequency (f0). \code{\link{getRolloff}} provides flexible
#' control over this rolloff function, going beyond simple exponential decay
#' (\code{rolloff}). Use quadratic terms to modify the behavior of a few lower
#' harmonics, \code{rolloffOct} to adjust the rate of decay per
#' octave, and \code{rolloffKHz} for rolloff correction depending on
#' f0. Plot the output with different parameter values and see examples below
#' and the vignette to get a feel for how to use \code{\link{getRolloff}}
#' effectively.
#' @param pitch_per_gc a vector of f0 per glottal cycle, Hz
#' @param nHarmonics maximum number of harmonics to generate (very weak
#'   harmonics with amplitude < \code{throwaway} will be discarded)
#' @inheritParams soundgen
#' @param rolloffParabCeiling quadratic adjustment is applied only up to
#'   \code{rolloffParabCeiling}, Hz. If not NULL, it overrides
#'   \code{rolloffParabHarm}
#' @param baseline The "neutral" frequency, at which no adjustment of rolloff
#'   takes place regardless of \code{rolloffKHz}
#' @param samplingRate sampling rate (needed to stop at Nyquist frequency and
#'   for plotting purposes)
#' @param plot if TRUE, produces a plot
#' @return Returns a matrix of amplitude multiplication factors for adjusting
#'   the amplitude of harmonics relative to f0. Each row of output contains one
#'   harmonic, and each column contains one glottal cycle.
#' @export
#' @examples
#' # steady exponential rolloff of -12 dB per octave
#' rolloff = getRolloff(pitch_per_gc = 150, rolloff = -12,
#'   rolloffOct = 0, plot = TRUE)
#' # the rate of rolloff slows down with each octave
#' rolloff = getRolloff(pitch_per_gc = 150, rolloff = -12,
#'   rolloffOct = 2, plot = TRUE)
#' # the rate of rolloff increases with each octave
#' rolloff = getRolloff(pitch_per_gc = 150, rolloff = -12,
#'   rolloffOct = -2, plot = TRUE)
#'
#' # variable f0: the lower f0, the more harmonics are non-zero
#' rolloff = getRolloff(pitch_per_gc = c(150, 800, 3000),
#'   rolloffOct = 0, plot = TRUE)
#' # without the correction for f0 (rolloffKHz),
#'   # high-pitched sounds have the same rolloff as low-pitched sounds,
#'   # producing unnaturally strong high-frequency harmonics
#' rolloff = getRolloff(pitch_per_gc = c(150, 800, 3000),
#'   rolloffOct = 0, rolloffKHz = 0, plot = TRUE)
#'
#' # parabolic adjustment of lower harmonics
#' rolloff = getRolloff(pitch_per_gc = 350, rolloffParab = 0,
#'   rolloffParabHarm = 2, plot = TRUE)
#' # rolloffParabHarm = 1 affects only f0
#' rolloff = getRolloff(pitch_per_gc = 150, rolloffParab = 30,
#'   rolloffParabHarm = 1, plot = TRUE)
#' # rolloffParabHarm=2 or 3 affects only h1
#' rolloff = getRolloff(pitch_per_gc = 150, rolloffParab = 30,
#'   rolloffParabHarm = 2, plot = TRUE)
#' # rolloffParabHarm = 4 affects h1 and h2, etc
#' rolloff = getRolloff(pitch_per_gc = 150, rolloffParab = 30,
#'   rolloffParabHarm = 4, plot = TRUE)
#' # negative rolloffParab weakens lower harmonics
#' rolloff = getRolloff(pitch_per_gc = 150, rolloffParab = -20,
#'   rolloffParabHarm = 7, plot = TRUE)
#' # only harmonics below 2000 Hz are affected
#' rolloff = getRolloff(pitch_per_gc = c(150, 600),
#'   rolloffParab = -20, rolloffParabCeiling = 2000,
#'   plot = TRUE)
getRolloff = function(pitch_per_gc = c(440),
                      nHarmonics = 100,
                      rolloff = -12,
                      rolloffOct = -2,
                      rolloffParab = 0,
                      rolloffParabHarm = 2,
                      rolloffParabCeiling = NULL,
                      rolloffKHz = -6,
                      baseline = 200,
                      throwaway = -120,
                      samplingRate = 16000,
                      plot = FALSE) {
  ## Exponential decay
  deltas = matrix(0, nrow = nHarmonics, ncol = length(pitch_per_gc))
  if (sum(rolloffOct != 0) > 0) {
    for (h in 2:nHarmonics) {
      deltas[h, ] = rolloffOct * (pitch_per_gc * h - baseline) / 1000
      # rolloff changes by rolloffOct per octave for each octave above H2
    }
  }
  # plot(deltas[, 1])

  r = matrix(0, nrow = nHarmonics, ncol = length(pitch_per_gc))
  for (h in 1:nHarmonics) {
    r[h,] = ((rolloff + rolloffKHz *
                (pitch_per_gc - baseline) / 1000) * log2(h)) + deltas[h,]
    # note that rolloff is here adjusted as a linear function of
    #   the difference between current f0 and baseline
    r[h, which(h * pitch_per_gc >= samplingRate / 2)] = -Inf # to avoid
    # aliasing, we discard all harmonics above Nyquist frequency
  }

  ## QUADRATIC term affecting the first rolloffParabHarm harmonics only
  if (rolloffParab != 0) {
    if (!is.null(rolloffParabCeiling)) {
      rolloffParabHarm = round(rolloffParabCeiling / pitch_per_gc)  # vector of
      # length pitch_per_gc specifying the number of harmonics whose amplitude
      # is to be adjusted
    } else {
      rolloffParabHarm = rep(round(rolloffParabHarm), length(pitch_per_gc))
    }
    rolloffParabHarm[rolloffParabHarm == 2] = 3 # will have the effect of boosting
    # H1 (2 * F0)
    # parabola ax^2+bx+c
    # 0 at h=1 and at h=rolloffParabHarm; a parabola up/down in between. We have the following constraints on the parabola: f(1)=0; f(rolloffParabHarm)=0; f'((1+rolloffParabHarm)/2)=0; and f((1+rolloffParabHarm)/2)=rolloffParab.
    ## Solving for a,b,c
    # f'(middle) = 2a*(1+rolloffParabHarm)/2+b = a*(1+rolloffParabHarm)+b = 0, so b = -a*(1+rolloffParabHarm).
    # f(1) = a+b+c = 0, so c = -a+a*(1+rolloffParabHarm) = a*rolloffParabHarm.
    # f(middle)=rolloffParab. middle is (1+rolloffParabHarm)/2, and f( (1+rolloffParabHarm)/2 ) = a*(1+rolloffParabHarm)^2/4 + b*(1+rolloffParabHarm)/2 + c = (substituting above expressions for b and c) = a*(1+rolloffParabHarm)^2/4 - a*(1+rolloffParabHarm)*(1+rolloffParabHarm)/2 + a*rolloffParabHarm = -a*(1+rolloffParabHarm)^2/4 + a*rolloffParabHarm = -a/4*(1 + rolloffParabHarm^2 + 2*rolloffParabHarm - 4*rolloffParabHarm) = -a/4*(1-rolloffParabHarm)^2. And we want this to equal rolloffParab. Solving for a, we have a = -4*rolloffParab/(rolloffParabHarm-1)^2
    a = -4 * rolloffParab / (rolloffParabHarm - 1) ^ 2
    b = -a * (1 + rolloffParabHarm)
    c = a * rolloffParabHarm
    # # verify:
    # myf = function(s, a, b, c) {return(a * s^2 + b * s + c)}
    # s = seq(1, rolloffParabHarm[1], by = .5)
    # plot (s, myf(s, a, b, c))

    # for a single affected harmonic, just change the amplitude of F0
    r[1, which(rolloffParabHarm < 3)] =
      r[1, which(rolloffParabHarm < 2)] + rolloffParab
    # if at least 2 harmonics are to be adjusted, calculate a parabola
    for (i in which(rolloffParabHarm >= 3)) {
      rowIdx = 1:rolloffParabHarm[i]
      r[rowIdx, i] = r[rowIdx, i] + a[i] * rowIdx ^ 2 +
        b[i] * rowIdx + c[i]   # plot (r[, 1])
    }
  }

  # set values under throwaway to zero
  if (is.numeric(throwaway)) {
    # if not null and not NA
    r[r < throwaway] = -Inf
  }

  # normalize so the amplitude of F0 is always 0
  r = apply (r, 2, function(x) x - max(x))

  # plotting
  if (plot) {
    x_max = samplingRate / 2 / 1000
    if (length(pitch_per_gc) == 1 | var(pitch_per_gc) == 0) {
      idx = which(r[, 1] > -Inf)
      plot ( idx * pitch_per_gc[1] / 1000, r[idx, 1],
             type = 'b', xlim = c(0, x_max), xlab = 'Frequency, Hz',
             ylab = 'Amplitude, dB', main = 'Glottal source rolloff')
    } else {
      pitch_min = min(pitch_per_gc)
      pitch_max = max(pitch_per_gc)
      idx_min = which.min(pitch_per_gc)
      idx_max = which.max(pitch_per_gc)
      rows_min = 1:tail(which(r[, idx_min] > -Inf), 1)
      rows_max = 1:tail(which(r[, idx_max] > -Inf), 1)
      freqs_min = rows_min * pitch_min / 1000
      freqs_max = rows_max * pitch_max / 1000
      rolloff_min = r[rows_min, idx_min]
      rolloff_max = r[rows_max, idx_max]
      plot (freqs_min, rolloff_min, type = 'b', col = 'blue',
            xlim = c(0, x_max), xlab = 'Frequency, Hz',
            ylab = 'Amplitude, dB', main = 'Glottal source rolloff')
      text (x = x_max, y = -10, labels = 'Lowest pitch',
            col = 'blue', pos = 2)
      points (freqs_max, rolloff_max, type = 'b', col = 'red')
      text (x = x_max, y = 0, labels = 'Highest pitch',
            col = 'red', pos = 2)
    }
  }

  # convert from dB to linear amplitude multipliers
  r = 2 ^ (r / 10)

  # shorten by discarding harmonics that are 0 throughout the sound
  r = r[which(apply(r, 1, sum) > 0), , drop = FALSE]
  rownames(r) = 1:nrow(r) # helpful for adding vocal fry

  return (r)
}


#' Spectral envelope
#'
#' Prepares a spectral envelope for filtering a sound to add formants, lip
#' radiation, and some stochastic component regulated by temperature.
#' Formants are specified as a list containing time, frequency, amplitude,
#' and width values for each formant (see examples). NB: each formant is
#' generated as a gamma distribution with mean = freq and SD = width. Formant
#' bandwidths in soundgen are therefore NOT compatible with formant bandwidths
#' used in Klatt synthesizer and other algorithms that rely on FIR instead of
#' FFT.
#' @param nr the number of frequency bins = windowLength_points/2, where
#'   windowLength_points is the size of window for Fourier transform
#' @param nc the number of time steps for Fourier transform
#' @inheritParams soundgen
#' @param formants either a character string like "aaui" referring to default
#'   presets for speaker "M1" or a list of formant times, frequencies,
#'   amplitudes, and bandwidths. \code{formants = NA} defaults to schwa. Time
#'   stamps for formants and mouthOpening can be specified in ms or an any other
#'   arbitrary scale.
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
#' @param mouthOpenThres the mouth is considered to be open when its
#'   opening is greater than \code{mouthOpenThres}. Defaults to 0
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
#'   frequency bins = windowLength_points/2 and nc is the number of time steps)
#' @examples
#' # [a] with F1-F3 visible
#' image(t(getSpectralEnvelope(nr = 512, nc = 50,
#'   formants = soundgen:::convertStringToFormants('a'),
#'   temperature = 0)))
#' # some "wiggling" of specified formants plus extra formants on top
#' image(t(getSpectralEnvelope(nr = 512, nc = 50,
#'   formants = soundgen:::convertStringToFormants('a'),
#'   temperature = 0.1, formantDepStoch = 10)))
#' # stronger extra formants
#' image(t(getSpectralEnvelope(nr = 512, nc = 50,
#'   formants = soundgen:::convertStringToFormants('a'),
#'   temperature = 0.1, formantDepStoch = 30)))
#' # a schwa based on the length of vocal tract = 15.5 cm
#' image(t(getSpectralEnvelope(nr = 512, nc = 50, formants = NA,
#'   temperature = .1, vocalTract = 15.5)))
#'
#' # no formants at all
#' image(t(getSpectralEnvelope(nr = 512, nc = 50,
#'   formants = NA, temperature = 0)))
#'
#' # manual specification of formants
#' image(t(getSpectralEnvelope(nr = 512, nc = 50,
#' samplingRate = 16000, formants = list(
#'   'f1' = data.frame('time' = 0, 'freq' = 900, 'amp' = 30, 'width' = 120),
#'   'f2' = data.frame('time' = 0, 'freq' = 1300, 'amp' = 30, 'width' = 120),
#'   'f3' = data.frame('time' = 0, 'freq' = 3200, 'amp' = 20, 'width' = 200)))))
getSpectralEnvelope = function(nr,
                               nc,
                               formants = NA,
                               formantDep = 1,
                               rolloffLip = 6,
                               mouthAnchors = NA,
                               mouthOpenThres = 0,
                               openMouthBoost = 0,
                               vocalTract = NULL,
                               temperature = 0,
                               formDrift = .3,
                               formDisp = .2,
                               formantDepStoch = 30,
                               smoothLinearFactor = 1,
                               samplingRate = 16000,
                               speedSound = 35400,
                               plot = FALSE,
                               duration = NULL,
                               colorTheme = c('bw', 'seewave', '...')[1],
                               nCols = 100,
                               xlab = 'Time',
                               ylab = 'Frequency, kHz',
                               ...) {
  if (class(formants) == 'character') {
    formants = convertStringToFormants(formants)
  } else if (is.list(formants)) {
    if (is.list(formants[[1]])) {
      formants = lapply(formants, as.data.frame)
    }
  } else if (!is.null(formants) && !is.na(formants)) {
    stop('If defined, formants must be a list or a string of characters
          from dictionary presets: a, o, i, e, u, 0 (schwa)')
  }
  if (!is.numeric(vocalTract) & length(formants[[1]]) > 2) {
    # if we don't know vocalTract, but at least one formant is defined,
    # we guess the length of vocal tract
    formantDispersion = mean(diff(unlist(lapply(formants, function(f) f$freq))))
    vocalTract = ifelse(
      is.numeric(formantDispersion),
      speedSound / 2 / formantDispersion,
      speedSound / 4 / formants$f1$freq
    )
  }
  if (length(formants[[1]]) < 2 & is.numeric(vocalTract)) {
    # ie if is.na(formants) or if there's something wrong with it,
    # we fall back on vocalTract to make a schwa
    freq = speedSound / 4 / vocalTract
    formants = list('f1' = data.frame(
      'time' = 0,
      'freq' = freq,
      'amp' = 30,
      'width' = 50 * (1 + freq^2 / 6 / 10^6)  # Tappert, Martony, and Fant (TMF)-1963
    ))
    # freq = 50:5000; a = 50 * (1 + freq^2 / 6 / 10^6); plot(freq, a)
  }

  # create a "spectrogram"-shaped filter matrix
  spectralEnvelope = matrix(0, nrow = nr, ncol = nc)

  # START OF FORMANTS
  if (is.list(formants)) {
    # upsample to the length of fft steps
    nPoints = max(unlist(lapply(formants, nrow)))
    formants_upsampled = lapply(formants, function(f) {
      temp = apply(f, 2, function(y) {
        if (nrow(f) > 1) {
          # just spline produces imprecise, overly smoothed curves. Loess is just
          # too slow for this. So we apply linear extrapolation to formant values
          # first, to get a fairly straight line between anchors, and THEN smooth
          # it out with spline
          out = spline(approx(y, n = nPoints + 2 ^ smoothLinearFactor,
                              x = f$time)$y, n = nc)$y
        } else {
          out = rep(y, nc)
        }
        out
      })
      if (class(temp) == 'numeric') {
        # if nc==1, we get numeric instead of
        # matrix and need to convert
        temp = t(as.matrix(temp))
      }
      temp
    }) # check that class(formants_upsampled[[1]]) == 'matrix'

    ## Stochastic part (only for temperature > 0)
    if (temperature > 0) {
      # create a few new, relatively high-frequency "pseudo-formants" moving
      # together with the real formants
      if (is.null(vocalTract) && length(formants) > 1) {
        ff = unlist(lapply(formants, function(x) x$freq[1]))
        formantDispersion = mean(c(ff[1], diff(ff)))
      } else if (!is.null(vocalTract)) {
        formantDispersion = 2 * speedSound / (4 * vocalTract)
      } else {
        formantDispersion = NA # making sdG also NA, ie extra formants not added
      }
      sdG = formantDispersion * temperature * formDisp
      nFormants = length(formants_upsampled)
      freq_max = max(formants_upsampled[[nFormants]][, 'freq'])

      if (!is.na(sdG) && formantDepStoch > 0) {
        while (freq_max < (samplingRate / 2 - 1000)) {
          # don't add extra formants close to Nyquist to avoid artifacts
          rw = getRandomWalk(
            len = nc,
            rw_range = temperature * formDrift,
            rw_smoothing = 0,
            trend = 0
          )
          # for nPoints == 1, returns one number close to 1
          if (length(rw) > 1) {
            rw = rw - mean(rw) + 1
          } # for actual random walks, make sure mean is 1
          temp = data.frame (
            'time' = formants_upsampled[[1]][, 'time'],
            'freq' = formants_upsampled[[nFormants]][, 'freq'] +
              round(rgamma(1,
                           formantDispersion ^ 2 / sdG ^ 2,
                           formantDispersion / sdG ^ 2
              ) * rw
              ))
          # rgamma: mean = formantDepStoch, sd = formantDepStoch*temperature
          temp$amp = round(rgamma(
            1,
            (formantDep / temperature) ^ 2,
            formantDepStoch * formantDep /
              (formantDepStoch * temperature) ^ 2 ) * rw)
          temp$width = 50 + (log2(temp$freq) - 5) * 20
          # visualize: freq=50:8000; plot(freq, 50+(log2(freq)-5)*20)
          formants_upsampled[[nFormants + 1]] = temp
          nFormants = nFormants + 1
          freq_max = max(formants_upsampled[[nFormants]]$freq)
        }
      }

      # wiggle both user-specified and stochastically added formants
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
      } # end of wiggling existing formants
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
        formants_upsampled[[f]][, 'width'] / bin_width
    }

    # mouth opening
    if (length(mouthAnchors) < 1 | sum(is.na(mouthAnchors)) > 0) {
      mouthOpening_upsampled = rep(0.5, nc) # defaults to mouth half-open the
      # whole time - sort of hanging loosely agape ;))
      mouthOpen_binary = rep(1, nc)
    } else {
      mouthOpening_upsampled = getSmoothContour(
        len = nc,
        anchors = mouthAnchors,
        valueFloor = permittedValues['mouthOpening', 'low'],
        valueCeiling = permittedValues['mouthOpening', 'high'],
        plot = FALSE
      )
      mouthOpening_upsampled[mouthOpening_upsampled < mouthOpenThres] = 0
      mouthOpen_binary = ifelse(mouthOpening_upsampled > 0, 1, 0)
    }
    # plot(mouthOpening_upsampled, type = 'l')

    # adjust formants for mouth opening
    if (!is.null(vocalTract) && is.finite(vocalTract)) {
      # is.finite() returns F for NaN, NA, ±inf, etc
      adjustment_hz = (mouthOpening_upsampled - 0.5) * speedSound /
        (4 * vocalTract) # speedSound = 35400 cm/s, speed of sound in warm
      # air. The formula for mouth opening is adapted from Moore (2016)
      # "A Real-Time Parametric General-Purpose Mammalian Vocal Synthesiser".
      # mouthOpening = .5 gives no modification (neutral, "default" position)
      adjustment_bins = (adjustment_hz - bin_width / 2) / bin_width + 1
    } else {
      adjustment_bins = 0
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
    # nasalization contour
    if (length(nasalizedIdx) > 0) {
      # add a pole
      formants_upsampled$fnp = formants_upsampled$f1
      formants_upsampled$fnp[, 'amp'] = 0
      formants_upsampled$fnp[nasalizedIdx, 'amp'] =
        formants_upsampled$f1[nasalizedIdx, 'amp'] * 2 / 3
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
      formants_upsampled$fnz[nasalizedIdx, 'amp'] =
        -formants_upsampled$f1[nasalizedIdx, 'amp'] * 2 / 3
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

    # add formants to spectrogram
    for (f in 1:length(formants_upsampled)) {
      mg = formants_upsampled[[f]][, 'freq']  # mean of gamma distribution
      # (vector of length nc)
      sdg = formants_upsampled[[f]][, 'width']  # sd of gamma distribution
      # (vector of length nc)
      sdg[sdg == 0] = 1  # otherwise division by 0
      shape = mg ^ 2 / sdg ^ 2
      rate = mg / sdg ^ 2
      formant = matrix(0, nrow = nr, ncol = nc)
      for (c in 1:nc) {
        formant[, c] = dgamma (1:nr, shape[c], rate[c])
        formant[, c] = formant[, c] / max(formant[, c]) *
          formants_upsampled[[f]][c, 'amp']
      }
      spectralEnvelope = spectralEnvelope + formant
    }
    spectralEnvelope = spectralEnvelope * formantDep
    # plot(spectralEnvelope[, 1], type = 'l')
  } else {
    mouthOpen_binary = rep(1, nc)
    mouthOpening_upsampled = rep(0.5, nc)
  }
  # END OF FORMANTS

  # add lip radiation when the mouth is open
  lip_dB = rolloffLip * log2(1:nr) # vector of length nr
  for (c in 1:nc) {
    spectralEnvelope[, c] = (spectralEnvelope[, c] +
                               lip_dB * mouthOpen_binary[c]) *
      2 ^ (mouthOpening_upsampled[c] * openMouthBoost / 10)
  }

  # convert from dB to linear multiplier
  spectralEnvelope = 2 ^ (spectralEnvelope / 10)

  if (plot) {
    if (is.numeric(duration)) {
      x = seq(0, duration, length.out = nc)
    } else {
      x = seq(0, 1, length.out = nc)
    }
    if (colorTheme == 'bw') {
      col = gray(seq(from = 1, to = 0, length = nCols))
    } else if (colorTheme == 'seewave') {
      col = seewave::spectro.colors(nCols)
    } else {
      colFun = match.fun(colorTheme)
      col = rev(colFun(nCols))
    }
    image(x = x,
          y = seq(0, samplingRate /2, length.out = nr)/ 1000,
          z = t(spectralEnvelope),
          xlab = xlab,
          ylab = ylab,
          col = col,
          ...)
  }

  return (spectralEnvelope)
}
