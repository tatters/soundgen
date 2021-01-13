#' Get lowest dominant frequency band
#'
#' Internal soundgen function.
#'
#' Calculate the lowest frequency band in the spectrum above pitchFloor whose
#' amplitude exceeds a certain threshold.
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @param domThres (0 to 1) to find the lowest dominant frequency band, we
#'   do short-term FFT and take the lowest frequency with amplitude at least
#'   domThres
#' @param domSmooth the width of smoothing interval (Hz) for finding
#'   \code{dom}
#' @return Returns a list of $dom (NA or numeric) and $dom_array
#'   (either NULL or a dataframe of pitch candidates).
#' @keywords internal
getDom = function(frame,
                  bin,
                  freqs,
                  domSmooth,
                  domThres,
                  pitchFloor,
                  pitchCeiling
) {
  dom_array = data.frame(
    'pitchCand' = numeric(),
    'pitchCert' = numeric(),
    'pitchSource' = character(),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  dom = NA
  # width of smoothing interval (in bins), forced to be an odd number
  domSmooth_bins = 2 * ceiling(domSmooth / bin / 2) - 1

  # find peaks in the smoothed spectrum (much faster than seewave::fpeaks)
  temp = zoo::rollapply(zoo::as.zoo(frame),
                        width = domSmooth_bins,
                        align = 'center',
                        function(x) {
                          isCentral.localMax(x, threshold = domThres)
                        })
  idx = zoo::index(temp)[zoo::coredata(temp)]
  pitchFloor_idx = which(freqs > pitchFloor)[1]
  pitchCeiling_idx = which(freqs > pitchCeiling)[1]
  idx_peak = idx[which(idx > pitchFloor_idx & idx < pitchCeiling_idx)[1]]
  # parabolic interpolation to get closer to the true peak
  applyCorrection = !is.na(idx_peak) &&
    length(idx_peak) == 1 &&
    (idx_peak > 1 & idx_peak < length(frame))
  if (applyCorrection) {
    threePoints = log10(frame[(idx_peak - 1) : (idx_peak + 1)])
    parabCor = parabPeakInterpol(threePoints)
    dom = freqs[idx_peak] + bin * parabCor$p
    dom_ampl = 10 ^ parabCor$ampl_p
    if (dom_ampl > 1) dom_ampl = 1  # cap at 1
  } else {
    dom = freqs[idx_peak]
    dom_ampl = frame[idx_peak]
  }

  if (length(idx) > 0) {
    # lowest dominant freq band - we take the first frequency in the spectrum at
    # least /domThres/ % of the amplitude of peak frequency, but high
    # enough to be above pitchFloor (and below pitchCeiling)
    dom_array = data.frame(
      'pitchCand' = dom,
      'pitchCert' = dom_ampl,
      'pitchSource' = 'dom',
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  return(list(dom_array = dom_array, dom = dom))
}


#' Autocorrelation pitch tracker
#'
#' Internal soundgen function.
#'
#' Attempts to find F0 of a frame by looking for peaks in the autocorrelation
#' function (time domain analysis). Modified PRAAT's algorithm. See Boersma, P.
#' (1993). Accurate short-term analysis of the fundamental frequency and the
#' harmonics-to-noise ratio of a sampled sound. In Proceedings of the institute
#' of phonetic sciences (Vol. 17, No. 1193, pp. 97-110).
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @param autocorThres voicing threshold (unitless, ~0 to 1)
#' @param autocorSmooth the width of smoothing interval (in bins) for
#'   finding peaks in the autocorrelation function. Defaults to 7 for sampling
#'   rate 44100 and smaller odd numbers for lower values of sampling rate
#' @param autocorUpsample upsamples acf to this resolution (Hz) to improve
#'   accuracy in high frequencies
#' @param autocorBestPeak amplitude of the lowest best candidate relative to the
#'   absolute max of the acf
#' @return Returns a list of $HNR (NA or numeric) and $pitchAutocor_array
#'   (either NULL or a dataframe of pitch candidates).
#' @keywords internal
getPitchAutocor = function(autoCorrelation,
                           samplingRate,
                           nCands,
                           autocorThres,
                           autocorSmooth = NULL,
                           autocorUpsample,
                           autocorBestPeak,
                           pitchFloor,
                           pitchCeiling) {
  # autoCorrelation = autocorBank[, 13]
  pitchAutocor_array = NULL

  # don't consider candidates above nyquist / 2 b/c there's not enough
  # resolution in acf that high up
  pitchCeiling = min(pitchCeiling, samplingRate / 4)

  orig = data.frame('freq' = as.numeric(names(autoCorrelation)),
                    'amp' = autoCorrelation)
  rownames(orig) = NULL
  a = orig[orig$freq > pitchFloor &
             orig$freq < pitchCeiling, , drop = FALSE]
  # plot(a$freq, a$amp, type='b')

  # upsample to improve resolution in higher frequencies
  if (autocorUpsample > 0) {
    upsample_from_bin = 1  # in Hz, it's samplingRate / (upsample_from_bin + 1)
    # same as which(a$freq < samplingRate / 4)[1], etc.
    upsample_to_bin = which(diff(a$freq) > -autocorUpsample)[1]
    upsample_len = round((a$freq[upsample_from_bin] - a$freq[upsample_to_bin]) /
                           autocorUpsample)
    if (!is.na(upsample_len) &&
        pitchCeiling > a$freq[upsample_to_bin] & upsample_len > 1) {
      temp = spline(a$amp[upsample_from_bin:upsample_to_bin],
                    n = upsample_len,
                    x = a$freq[upsample_from_bin:upsample_to_bin])
      # points(temp$x, temp$y, type = 'p', cex = .25, col = 'red')
      a = rbind(a[1:upsample_from_bin, ],
                data.frame(freq = rev(temp$x), amp = rev(temp$y)),
                a[(upsample_to_bin + 1):nrow(a), ])
    }
  }

  HNR = max(a$amp) # HNR is here defined as the maximum autocorrelation
  # within the specified pitch range. It is also measured for the frames which
  # are later classified as unvoiced (i.e. HNR can be <voicedThres)

  # find peaks in the corrected autocorrelation function
  a_zoo = zoo::as.zoo(a$amp)
  temp = zoo::rollapply(a_zoo,
                        width = autocorSmooth,
                        align = 'center',
                        function(x) {
                          isCentral.localMax(x, threshold = autocorThres)
                          # width = 7 chosen by optimization, but it doesn't make that much difference anyhow
                        })
  idx = zoo::index(temp)[zoo::coredata(temp)]
  autocorPeaks = a[idx, ]

  if (nrow(autocorPeaks) > 0) {
    # if some peaks are found...
    # we are only interested in frequencies above half of the best candidate
    # (b/c otherwise we get false subharmonics)
    bestFreq = autocorPeaks$freq[which(autocorPeaks$amp >
                                         autocorBestPeak * max(autocorPeaks$amp))[1]]
    # bestFreq = autocorPeaks$freq[which.max(autocorPeaks$amp)]
    if (!is.na(bestFreq)) {
      autocorPeaks = try(autocorPeaks[autocorPeaks$freq > bestFreq / 1.8,
                                      , drop = FALSE], silent = TRUE)
      # otherwise we get false subharmonics
      autocorPeaks = try(autocorPeaks[order(autocorPeaks$amp, decreasing = TRUE),
                                      , drop = FALSE], silent = TRUE)
    }
    if (class(autocorPeaks)[1] != 'try-error') {
      if (nrow(autocorPeaks) > 0) {
        # if some peaks satisfy all criteria, return them:
        pitchAutocor_array = data.frame (
          'pitchCand' = autocorPeaks [1:min(nrow(autocorPeaks), nCands), 1],
          # save n candidates of pitchAutocor, convert to Hz
          'pitchCert' = autocorPeaks[1:min(nrow(autocorPeaks), nCands), 2],
          # save amplitudes corresponding to each pitchAutocor candidate
          'pitchSource' = 'autocor',
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }
    }
  }
  # very occasionally HNR can be calculated as 1.01 etc. To prevent this nonsense:
  if (!is.na(HNR) & HNR >= 1) {
    HNR = 1 / HNR  # See Boersma, 1993. Up to soundgen 1.2.1, it was just set to 0.9999 (40 dB)
  }

  return(list(pitchAutocor_array = pitchAutocor_array,
              HNR = HNR))
}


#' Cepstral pitch tracker
#'
#' Internal soundgen function.
#'
#' Attempts to find F0 of a frame by looking for peaks in the cepstrum.
#' See http://www.phon.ucl.ac.uk/courses/spsci/matlab/lect10.html
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @param cepThres voicing threshold (unitless, ~0 to 1)
#' @param cepSmooth the width of smoothing interval (Hz) for finding peaks in
#'   the cepstrum
#' @param cepZp zero-padding of the spectrum used for cepstral pitch detection
#'   (final length of spectrum after zero-padding in points, e.g. 2 ^ 13)
#' @return Returns either NULL or a dataframe of pitch candidates.
#' @keywords internal
getPitchCep = function(frame,
                       samplingRate,
                       bin,
                       nCands,
                       cepThres,
                       cepSmooth,
                       cepZp,
                       pitchFloor,
                       pitchCeiling,
                       cepPenalty = 1,
                       logSpec = FALSE) {
  pitchCep_array = NULL

  if (cepZp < length(frame)) {
    frameZP = frame
  } else {
    zp = rep(0, (cepZp - length(frame)) / 2)
    frameZP = c(zp, frame, zp)
    cepSmooth = cepSmooth * round(cepZp / length(frame))
  }
  # plot(frameZP, type = 'l')
  if (logSpec) frameZP = log(frameZP + 1e-6)  # 1e-6 is -120 dB

  # fft of fft, whatever you call it - cepstrum or smth else
  cepstrum = abs(fft(as.numeric(frameZP)))
  # normalize to make cert more comparable to other methods
  cepstrum = cepstrum / max(cepstrum)
  # plot(cepstrum, type = 'l')
  l = length(cepstrum) %/% 2
  zp_corr = (length(frameZP) / length(frame))
  cep_freqs = samplingRate / (1:l) / 2 * zp_corr
  b = data.frame(
    # NB: divide by 2 because it's another fft, not inverse fft (cf. pitchAutocor)
    idx = 1:l,
    freq = cep_freqs,
    cep = cepstrum[1:l]
  )
  bin_width_Hz = samplingRate / 2 / l
  cepSmooth_bins = max(1, 2 * ceiling(cepSmooth / bin_width_Hz / 2) - 1)
  b = b[b$freq > pitchFloor & b$freq < pitchCeiling, ]
  # plot(b$freq, b$cep, type = 'l', log = 'x')

  # find peaks
  a_zoo = zoo::as.zoo(b$cep)
  temp = zoo::rollapply(a_zoo,
                        width = 3, # cepSmooth_bins,
                        align = 'center',
                        function(x)
                          isCentral.localMax(x, threshold = cepThres))
  idx = zoo::index(temp)[zoo::coredata(temp)]

  if (length(idx) > 0) {
    # if some peaks are found...
    cepPeaks = b[idx, ]

    # parabolic interpolation to improve resolution
    for (i in 1:nrow(cepPeaks)) {
      idx_peak = which(b$idx == cepPeaks$idx[i])
      applyCorrecton = idx_peak > 1 & idx_peak < l
      if (applyCorrecton) {
        threePoints = b$cep[(idx_peak - 1) : (idx_peak + 1)]
        parabCor = parabPeakInterpol(threePoints)
        # actually on inverted scale (1/x), but just a liner correction for now
        cepPeaks$freq[i] = samplingRate / 2 / (cepPeaks$idx[i] + parabCor$p) * zp_corr
        cepPeaks$cep[i] = parabCor$ampl_p
      }
    }
    pitchCep_array = data.frame(
      'pitchCand' = cepPeaks$freq,
      'pitchCert' = cepPeaks$cep,
      'pitchSource' = 'cep',
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    # because cepstrum really stinks for frequencies above ~1 kHz, mostly
    # picking up formants or just plain noise, we discount confidence in
    # high-pitch cepstral estimates
    corFactor = 1 + cepPenalty / (1 + exp(-.005 * (pitchCep_array$pitchCand - 100 * bin)))
    pitchCep_array$pitchCert = pitchCep_array$pitchCert / corFactor
    # visualization: a = seq(pitchFloor, pitchCeiling, length.out = 100)
    # b = 1 + cepPenalty / (1 + exp(-.005 * (a - 100 * bin)))
    # plot(a, b, type = 'l')
    ord = order(pitchCep_array$pitchCert, decreasing = TRUE)
    pitchCep_array = pitchCep_array[ord, ]
    pitchCep_array = pitchCep_array[1:nCands, ]

    # to avoid false subharmonics:
    # absCepPeak = idx[which.max(b$cep[idx])]
    # idx = idx[b$freq[idx] > b$freq[absCepPeak] / 1.8]
    # plot(b$freq, b$cep, type = 'l', log = 'x')
    # points(b$freq[idx], b$cep[idx], log = 'x')
  }
  return(pitchCep_array)
}


#' BaNa pitch tracker
#'
#' Internal soundgen function.
#'
#' Attempts to find F0 of a frame by calculating ratios of putative harmonics
#' (frequency domain analysis, ~ modified BaNa algorithm). See Ba et al. (2012)
#' "BaNa: A hybrid approach for noise resilient pitch detection." Statistical
#' Signal Processing Workshop (SSP), 2012 IEEE.
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @param bin the width of spectral bin in \code{frame}, Hz
#' @param HNR harmonics-to-noise ratio returned by \code{\link{getPitchAutocor}}
#' @param specThres voicing threshold (unitless, ~0 to 1)
#' @param specPeak,specHNRslope when looking for putative harmonics in
#'   the spectrum, the threshold for peak detection is calculated as
#'   \code{specPeak * (1 - HNR * specHNRslope)}
#' @param specSmooth the width of window for detecting peaks in the spectrum, Hz
#' @param specMerge pitch candidates within \code{specMerge} semitones are
#'   merged with boosted certainty
#' @param specSinglePeakCert (0 to 1) if F0 is calculated based on a single
#'   harmonic ratio (as opposed to several ratios converging on the same
#'   candidate), its certainty is taken to be \code{specSinglePeakCert}
#' @return Returns either NULL or a dataframe of pitch candidates.
#' @keywords internal
getPitchSpec = function(frame,
                        bin,
                        freqs,
                        specSmooth,
                        specHNRslope,
                        HNR = NULL,
                        specThres,
                        specPeak,
                        specSinglePeakCert,
                        pitchFloor,
                        pitchCeiling,
                        specMerge,
                        nCands
) {
  pitchSpec_array = NULL
  n = length(frame)
  width = 2 * ceiling((specSmooth / bin + 1) * 20 / bin / 2) - 1 # to be always ~100 Hz,
  # regardless of bin, but an odd number
  if (!is.numeric(HNR)) {
    specPitchThreshold = specPeak # if HNR is NA, the sound is
    # probably a mess, so we play safe by only looking at very strong harmonics
  } else {
    # for noisy sounds the threshold is high to avoid false sumharmonics etc,
    # for tonal sounds it is low to catch weak harmonics
    specPitchThreshold = specPeak * (1 - HNR * specHNRslope)
  }

  # find peaks in the spectrum (hopefully harmonics)
  temp = zoo::rollapply(zoo::as.zoo(frame),
                        width = width,
                        align = 'center',
                        function(x) {
                          isCentral.localMax(x, threshold = specPitchThreshold)
                          # plot(zoo::as.zoo(frame), type='l')
                        })
  idx = zoo::index(temp)[zoo::coredata(temp)]
  specPeaks = data.frame('idx' = idx)
  nr = nrow(specPeaks)

  # parabolic interpolation to get closer to the true peak
  if (nr > 0) {
    for (i in 1:nr) {
      idx_peak = specPeaks$idx[i]
      applyCorrecton = idx_peak > 1 & idx_peak < n
      if (applyCorrecton) {
        threePoints = log10(frame[(idx_peak - 1) : (idx_peak + 1)])
        parabCor = parabPeakInterpol(threePoints)
        specPeaks$freq[i] = freqs[idx_peak] + bin * parabCor$p
        specPeaks$amp[i] = 10 ^ parabCor$ampl_p
      } else {
        specPeaks$freq[i] = freqs[idx_peak]
        specPeaks$amp[i] = frame[idx_peak]
      }
    }
  }

  if (nr == 1) {
    if (specPeaks[1, 1] < pitchCeiling & specPeaks[1, 1] > pitchFloor) {
      pitchSpec = specPeaks[1, 1]
      pitchCert = specSinglePeakCert
      pitchSpec_array = data.frame(
        'pitchCand' = pitchSpec,
        'pitchCert' = specSinglePeakCert,
        'pitchSource' = 'spec',
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  } else if (nr > 1) {
    # analyze five lowest harmonics
    specPeaks = specPeaks[1:min(5, nrow(specPeaks)), ]

    # A modified version of BaNa algorithm follows
    seq1 = 2:nrow(specPeaks)
    seq2 = 1:(nrow(specPeaks) - 1)
    n = length(seq1) * length(seq2)
    temp = data.frame (
      'harmonicA' = rep(0, n),
      'harmonicB' = rep(0, n),
      'AtoB_ratio' = rep(0, n)
    )
    counter = 1
    for (i in seq1) {
      for (j in seq2) {
        temp$harmonicA[counter] = i
        temp$harmonicB[counter] = j
        # get ratios of discovered harmonics to each other
        temp$AtoB_ratio[counter] = specPeaks$freq[i] / specPeaks$freq[j]
        counter = counter + 1
      }
    }
    temp = temp[temp$harmonicA > temp$harmonicB, ]

    pitchCand = numeric()
    for (i in 1:nrow(temp)) {
      # for each ratio that falls within the limits specified outside this
      # function in a dataframe called "ratios", calculate the corresponding
      # pitch. If several ratios suggest the same pitch, that's our best guess
      divLow = BaNaRatios$divide_lower_by[temp$AtoB_ratio[i] > BaNaRatios$value_low &
                                            temp$AtoB_ratio[i] < BaNaRatios$value_high]
      pitchCand = c(pitchCand,
                    as.numeric(specPeaks$freq[temp$harmonicB[i]] / divLow))
    }
    # add pitchCand based on the most common distances between harmonics
    # pitchCand = c(pitchCand, diff(specPeaks[,1]))

    pitchCand = sort(pitchCand[pitchCand > pitchFloor &
                                 pitchCand < pitchCeiling])
    if (length(pitchCand) > 0) {
      pitchSpec_array = data.frame(
        'pitchCand' = pitchCand,
        'specAmplIdx' = 1,
        'pitchSource' = 'spec',
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      c = 1
      while (c + 1 <= nrow(pitchSpec_array)) {
        if (abs(log2(pitchSpec_array$pitchCand[c] /
                     pitchSpec_array$pitchCand[c + 1])) < specMerge / 12) {
          # merge cands within specMerge into one "super-candidate"
          # and give this new super-candidate a certainty boost
          pitchSpec_array$specAmplIdx[c] = pitchSpec_array$specAmplIdx[c] + 1
          pitchSpec_array$pitchCand[c] = mean(c(
            pitchSpec_array$pitchCand[c],
            pitchSpec_array$pitchCand[c + 1]
          ))
          pitchSpec_array = pitchSpec_array[-(c + 1), ]
        } else {
          c = c + 1
        }
      }
      pitchSpec_array$pitchCert = specSinglePeakCert +
        (1 / (1 + exp(-(pitchSpec_array$specAmplIdx - 1))) - 0.5) * 2 *
        (1 - specSinglePeakCert) # normalization. Visualization:
      # a = 1:15
      # b = specSinglePeakCert + (1 / (1 + exp(-(a - 1))) - 0.5) * 2 *
      # (1 - specSinglePeakCert)
      # plot(a, b, type = 'l')
      pitchSpec_array = pitchSpec_array[
        order(pitchSpec_array$pitchCert, decreasing = TRUE),
        c('pitchCand', 'pitchCert', 'pitchSource')
      ]
    }
  }
  if (!is.null(pitchSpec_array)) {
    if (sum(!is.na(pitchSpec_array)) > 0) {
      pitchSpec_array = pitchSpec_array[pitchSpec_array$pitchCert > specThres,
                                        , drop = FALSE]
      # how many pitchSpec candidates to use (max)
      pitchSpec_array = pitchSpec_array[1:min(nrow(pitchSpec_array), nCands), ]
    }
  }

  return(pitchSpec_array)
}


#' Harmonic product spectrum
#'
#' Internal soundgen function.
#'
#' Estimates pitch per frame using the harmonic product spectrum. Algorithm:
#' downsample the spectrum repeatedly padding with 0 to the original length,
#' then multiply the resulting scaled spectra. This has the effect of
#' emphasizing f0, which should hopefully become the highest spectral peak. See
#' https://cnx.org/contents/i5AAkZCP@2/Pitch-Detection-Algorithms
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @param hpsThres voicing threshold (unitless, ~0 to 1)
#' @param hpsNum the number of times the spectrum is downsampled
#' @param hpsNorm the amount of inflation of hps pitch certainty (0 = none)
#' @param hpsPenalty the amount of penalizing hps candidates in low frequencies
#'   (0 = none)
#' @return Returns either NULL or a dataframe of pitch candidates.
#' @keywords internal
getPitchHps = function(frame,
                       freqs,
                       bin,
                       hpsThres,
                       hpsNum,
                       hpsNorm,
                       hpsPenalty,
                       pitchFloor,
                       pitchCeiling) {
  pitchHps_array = NULL
  n = length(frame)
  # take log to avoid multiplying large numbers
  frame_log = log(frame)  # NB: frame is normalize (max 1)
  # plot(freqs, frame_log, type = 'l')

  # Downsample the spectrum and pad with log(0) to the old length
  spectra = data.frame(s1 = frame_log)
  for (i in 2:hpsNum) {
    n1 = round(n / i)
    idx = seq(1, n, length.out = n1)
    spectra[, i] = c(frame_log[idx], rep(-Inf, (n - n1)))
  }

  # Multiply the spectra (add logs, then exponentiate)
  spec_hps = exp(apply(spectra, 1, sum) / hpsNum ^ hpsNorm)
  # Note: the /hpsNum part is normalization to make pitchCert more
  # reasonable for hps method. Alternative: simply normalize to 1:
  # spec_hps = spec_hps / max(spec_hps)
  # plot(freqs, spec_hps, type = 'l')

  # Focus on the area within [pitchFloor, pitchCeiling]
  # NB: if this is done before downsampling & multiplying the spectra,
  # resolution seems to suffer
  idx = which(freqs > pitchFloor & freqs < pitchCeiling)
  spec_hps = spec_hps[idx]
  freqs_hps = freqs[idx]
  # plot(freqs_hps, spec_hps, type = 'l')

  # Find peaks in the spectrum (hopefully harmonics)
  temp = zoo::rollapply(zoo::as.zoo(spec_hps),
                        width = 3,  # any peak will do
                        align = 'center',
                        function(x) {
                          isCentral.localMax(x, threshold = hpsThres)
                        })
  idx = zoo::index(temp)[zoo::coredata(temp)]

  if (length(idx) > 0) {
    idx = idx[order(spec_hps[idx], decreasing = TRUE)]
    acceptedHpsPeaks = idx[1]
    # acceptedHpsPeaks = idx[1:min(length(idx), nCands)]  # bad results
    if (length(acceptedHpsPeaks) > 0) {
      # if some peaks are found...
      hpsPeaks = data.frame(idx = acceptedHpsPeaks)
      # parabolic interpolation to get closer to the true peak
      for (i in 1:nrow(hpsPeaks)) {
        idx_peak = idx[i]
        applyCorrecton = idx_peak > 1 & idx_peak < n
        if (applyCorrecton) {
          threePoints = as.numeric(log10(spec_hps[(idx_peak - 1) : (idx_peak + 1)]))
          parabCor = parabPeakInterpol(threePoints)
          hpsPeaks$freq[i] = freqs_hps[idx_peak] + bin * parabCor$p
          hpsPeaks$amp[i] = 10 ^ parabCor$ampl_p
        } else {
          hpsPeaks$freq[i] = freqs_hps[idx_peak]
          hpsPeaks$amp[i] = spec_hps[idx_peak]
        }
      }
    }

    # Penalize low-frequency candidates b/c hps is more accurate for f0 values
    # that are high relative to spectral resolution. Illustration:
    # fr = seq(pitchFloor, pitchCeiling, length.out = n)
    # b = 1:n
    # coef = 1 - 1/exp((b - 1) / hpsPenalty)
    # plot(b, coef, type = 'l', log = 'x')
    # plot(fr, coef, type = 'l', log = 'x')
    coef = 1 - 1 / exp((hpsPeaks$idx - 1) / hpsPenalty)
    hpsPeaks$amp = hpsPeaks$amp * coef

    # Format the output
    pitchHps_array = data.frame(
      'pitchCand' = hpsPeaks$freq,
      'pitchCert' = hpsPeaks$amp,
      'pitchSource' = 'hps',
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  return(pitchHps_array)
}
