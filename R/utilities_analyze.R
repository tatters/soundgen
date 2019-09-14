### UTILITIES FOR ACOUSTIC ANALYSIS ###

#' Analyze fft frame
#'
#' Internal soundgen function.
#'
#' This function performs the heavy lifting of pitch tracking and acoustic
#' analysis in general: it takes the spectrum of a single fft frame as input and
#' analyzes it.
#' @param frame the real part of the spectrum of a frame, as returned by
#'   \code{\link[stats]{fft}}
#' @param autoCorrelation pre-calculated autocorrelation of the input frame
#'   (computationally more efficient than to do it here)
#' @param samplingRate sampling rate (Hz)
#' @param trackPitch if TRUE, attempt to find F0 in this frame (FALSE if entropy
#'   is above some threshold - specified in \code{\link{analyze}})
#' @inheritParams analyze
#' @return Returns a list with two components: $pitchCands_frame contains pitch
#'   candidates for the frame, and $summaries contains other acoustic predictors
#'   like HNR, specSlope, etc.
#' @keywords internal
analyzeFrame = function(frame,
                        autoCorrelation = NULL,
                        samplingRate = 44100,
                        scaleCorrection = 1,
                        trackPitch = TRUE,
                        pitchMethods = c('autocor', 'cep', 'spec', 'dom'),
                        cutFreq = 6000,
                        domThres = 0.1,
                        domSmooth = 220,
                        autocorThres = 0.75,
                        autocorSmooth = NULL,
                        cepThres = 0.45,
                        cepSmooth = 3,
                        cepZp = 2 ^ 13,
                        specThres = 0.45,
                        specPeak = 0.8,
                        specSinglePeakCert = 0.6,
                        specSmooth = 100,
                        specHNRslope = .1,
                        specMerge = 1,
                        pitchFloor = 75,
                        pitchCeiling = 3500,
                        nCands = 1) {
  ## DESCRIPTIVES
  absSpec = data.frame('freq' = 1000 * as.numeric(names(frame)),
                       'amp' = frame)
  amplitude = sum(frame)
  absSpec$w = absSpec$amp / amplitude
  specCentroid = sum(absSpec$freq * absSpec$w)
  peakFreq = absSpec$freq[which.max(frame)]
  medianFreq = absSpec$freq[min(which(cumsum(frame) > amplitude / 2))]
  if (is.numeric(scaleCorrection)) {
    loudness = getLoudnessPerFrame(
      spec = frame * scaleCorrection,
      samplingRate = samplingRate
    )  # in sone, assuming scaling by SPL_measured in analyze()
  } else {
    loudness = NA
  }

  # Cut spectral band from pitchFloor to cutFreq Hz
  absSpec_cut = absSpec[absSpec$freq > pitchFloor &
                          absSpec$freq < cutFreq,] # Above 5-6 kHz or so,
  # spectral energy depends too much on the original sampling rate, noises etc.
  # Besides, those frequencies are not super relevant to human vocalizations in
  # any case. So we cut away all info above 5 kHz before we calculate quartiles
  # of spectral energy
  peakFreqCut = absSpec_cut$freq[which.max(frame)] # peakFreq under cutFreq
  amplitude_cut = sum(absSpec_cut$amp)
  absSpec_cut$w = absSpec_cut$amp / amplitude_cut
  # spectral centroid under cutFreq
  specCentroidCut = sum(absSpec_cut$freq * absSpec_cut$w)
  # first quartile of spectral energy distribution in the band from pitchFloor
  # to cutFreq kHz
  cum_cut = cumsum(absSpec_cut$amp)
  quartile25 = absSpec_cut$freq[min(which(cum_cut >= 0.25 * amplitude_cut))]
  # second quartile (same as medianFreq within this spectral band)
  quartile50 = absSpec_cut$freq[min(which(cum_cut >= 0.5 * amplitude_cut))]
  # third quartile. Note: half the energy in the band from pitchFloor to
  # cutFreq kHz lies between quartile25 and quartile75
  quartile75 = absSpec_cut$freq[min(which(cum_cut >= 0.75 * amplitude_cut))]
  specSlope = summary(lm(amp ~ freq, data = absSpec_cut))$coef[2, 1]

  ## PITCH TRACKING
  frame = frame / max(frame) # plot (frame, type='l')
  bin = samplingRate / 2 / length(frame) # the width of one bin in spectrogram,
  # in Hz (~20 Hz for 44100 Hz with 50 ms window and zp = 0)

  # lowest dominant frequency band
  if (trackPitch & 'dom' %in% pitchMethods) {
    d = getDom(frame = frame,
               samplingRate = samplingRate,
               bin = bin,
               domSmooth = domSmooth,
               domThres = domThres,
               pitchFloor = pitchFloor,
               pitchCeiling = pitchCeiling
    )
    pitchCands_frame = d$dom_array
    dom = d$dom
  } else {
    pitchCands_frame = data.frame(
      'pitchCand' = numeric(),
      'pitchCert' = numeric(),
      'pitchSource' = character(),
      stringsAsFactors = FALSE,
      row.names = NULL
    )    # initialize an empty dataframe
    dom = NA
  }

  # autocorrelation (PRAAT)
  if (trackPitch & 'autocor' %in% pitchMethods) {
    pa = getPitchAutocor(autoCorrelation = autoCorrelation,
                         autocorThres = autocorThres,
                         autocorSmooth = autocorSmooth,
                         pitchFloor = pitchFloor,
                         pitchCeiling = pitchCeiling,
                         samplingRate = samplingRate,
                         nCands = nCands)
    if(!is.null(pa$pitchAutocor_array)) {
      pitchCands_frame = rbind(pitchCands_frame, pa$pitchAutocor_array)
    }
    HNR = pa$HNR
  } else {
    HNR = NA
  }

  # cepstrum
  if (trackPitch & 'cep' %in% pitchMethods) {
    pitchCep_array = getPitchCep(frame = frame,
                                 cepZp = cepZp,
                                 samplingRate = samplingRate,
                                 pitchFloor = pitchFloor,
                                 pitchCeiling = pitchCeiling,
                                 cepThres = cepThres,
                                 cepSmooth = cepSmooth,
                                 nCands = nCands)
    if(!is.null(pitchCep_array)) pitchCands_frame = rbind(pitchCands_frame, pitchCep_array)
  }

  # spectral: ratios of harmonics (BaNa)
  if (trackPitch & 'spec' %in% pitchMethods) {
    pitchSpec_array = getPitchSpec(frame = frame,
                                   specSmooth = specSmooth,
                                   specHNRslope = specHNRslope,
                                   bin = bin,
                                   HNR = NULL,
                                   specThres = specThres,
                                   specPeak = specPeak,
                                   specSinglePeakCert = specSinglePeakCert,
                                   pitchFloor = pitchFloor,
                                   pitchCeiling = pitchCeiling,
                                   specMerge = specMerge,
                                   nCands = nCands
    )
    if(!is.null(pitchSpec_array)) pitchCands_frame = rbind(pitchCands_frame, pitchSpec_array)
  }

  # some adjustments of pitch candidates
  if (nrow(pitchCands_frame) > 0) {
    pitchCands_frame[, 1:2] = apply(pitchCands_frame[, 1:2], 2, function(x) as.numeric(x))
    # otherwise they become characters after rbind
  }
  if (nrow(pitchCands_frame[pitchCands_frame$pitchSource == 'dom', ]) > 0 & !is.na(HNR)) {
    pitchCands_frame$pitchCert[pitchCands_frame$pitchSource == 'dom'] =
      1 / (1 + exp(3 * HNR - 1)) # dom is worth more for noisy sounds,
    # but its weight approaches ~0.2 as HNR approaches 1
    # (NB: this is before HNR is converted to dB). Visualization:
    # a = seq(0, 1, length.out = 100)
    # b = 1 / (1 + exp(3 * a - 1))
    # plot (a, b, ylim = c(0, 1))
  }

  return(list(
    'pitchCands_frame' = pitchCands_frame,
    'summaries' = data.frame(
      loudness = loudness,
      HNR = HNR,
      dom = dom,
      specCentroid = specCentroid,
      specCentroidCut = specCentroidCut,
      peakFreq = peakFreq,
      peakFreqCut = peakFreqCut,
      medianFreq = medianFreq,
      quartile25 = quartile25,
      quartile50 = quartile50,
      quartile75 = quartile75,
      specSlope = specSlope
    )
  ))
}


#' Get lowest dominant frequency band
#'
#' Internal soundgen function.
#'
#' Calculate the lowest frequency band in the spectrum above pitchFloor whose
#' power exceeds a certain threshold.
#' @inheritParams analyzeFrame
#' @inheritParams analyze
#' @param bin the width of one bin in spectrogram, Hz
#' @return Returns a list of $dom (NA or numeric) and $dom_array
#'   (either NULL or a dataframe of pitch candidates).
#' @keywords internal
getDom = function(frame,
                  samplingRate,
                  bin,
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
  pitchFloor_idx = which(as.numeric(names(frame)) > pitchFloor / 1000)[1]
  pitchCeiling_idx = which(as.numeric(names(frame)) > pitchCeiling / 1000)[1]
  idx = idx[idx > pitchFloor_idx & idx < pitchCeiling_idx]

  if (length(idx) > 0) {
    # lowest dominant freq band - we take the first frequency in the spectrum at
    # least /domThres/ % of the amplitude of peak frequency, but high
    # enough to be above pitchFloor (and below pitchCeiling)
    dom = as.numeric(names(frame)[idx[1]]) * 1000
    dom_array = data.frame(
      'pitchCand' = dom,
      'pitchCert' = frame[idx[1]],
      'pitchSource' = 'dom',
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  return (list(dom_array = dom_array, dom = dom))
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
#' @return Returns a list of $HNR (NA or numeric) and $pitchAutocor_array
#'   (either NULL or a dataframe of pitch candidates).
#' @keywords internal
getPitchAutocor = function(autoCorrelation,
                           autocorSmooth,
                           autocorThres,
                           pitchFloor,
                           pitchCeiling,
                           samplingRate,
                           nCands) {
  # autoCorrelation = autocorBank[, 13]
  pitchAutocor_array = NULL
  a = data.frame('freq' = as.numeric(names(autoCorrelation)),
                 'amp' = autoCorrelation)
  rownames(a) = NULL
  a = a[a$freq > pitchFloor &
          a$freq < pitchCeiling, , drop = FALSE] # plot(a[,2], type='l')
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
                                         0.975 * max(autocorPeaks$amp))[1]]
    # bestFreq = autocorPeaks$freq[which.max(autocorPeaks$amp)]
    if (!is.na(bestFreq)) {
      autocorPeaks = try(autocorPeaks[autocorPeaks$freq > bestFreq / 1.8,
                                      , drop = FALSE], silent = TRUE)
      # otherwise we get false subharmonics
      autocorPeaks = try(autocorPeaks[order(autocorPeaks$amp, decreasing = TRUE),
                                      , drop = FALSE], silent = TRUE)
    }
    if (class(autocorPeaks) != 'try-error') {
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
#' @return Returns either NULL or a dataframe of pitch candidates.
#' @keywords internal
getPitchCep = function(frame,
                       cepZp,
                       samplingRate,
                       pitchFloor,
                       pitchCeiling,
                       cepThres,
                       cepSmooth,
                       nCands) {
  pitchCep_array = NULL

  if (cepZp < length(frame)) {
    frameZP = frame
  } else {
    zp = rep(0, (cepZp - length(frame)) / 2)
    frameZP = c(zp, frame, zp)
    cepSmooth = cepSmooth * round(cepZp / length(frame))
  }

  # fft of fft, whatever you call it - cepstrum or smth else
  cepstrum = abs(fft(frameZP)) # plot(frameZP, type = 'l')
  cepstrum = cepstrum / max(cepstrum) # plot (cepstrum, type = 'l')
  l = length(cepstrum) %/% 2
  b = data.frame(
    # NB: divide by 2 because it's another fft, not inverse fft (cf. pitchAutocor)
    freq = samplingRate / (1:l) / 2 * (length(frameZP) / length(frame)),
    cep = cepstrum[1:l]
  )
  bin_width_Hz = samplingRate / 2 / l
  cepSmooth_bins = max(1, 2 * ceiling(cepSmooth / bin_width_Hz / 2) - 1)
  b = b[b$freq > pitchFloor & b$freq < pitchCeiling, ]
  # plot(b, type = 'l')

  # find peaks
  a_zoo = zoo::as.zoo(b$cep)
  temp = zoo::rollapply(a_zoo,
                        width = cepSmooth_bins,
                        align = 'center',
                        function(x)
                          isCentral.localMax(x, threshold = cepThres))
  idx = zoo::index(temp)[zoo::coredata(temp)]

  absCepPeak = try(idx[which.max(b$cep[idx])], silent = TRUE)
  if (class(absCepPeak) != 'try-error') {
    idx = idx[b$freq[idx] > b$freq[absCepPeak] / 1.8]
  } # to avoid false subharmonics
  # plot (b, type = 'l')
  # points(b$freq[idx], b$cep[idx])
  idx = idx[order(b$cep[idx], decreasing = TRUE)]
  acceptedCepPeaks = idx[1:min(length(idx), nCands)]

  if (length(acceptedCepPeaks) > 0) {
    # if some peaks are found...
    pitchCep_array = data.frame(
      'pitchCand' = b$freq[acceptedCepPeaks],
      'pitchCert' = b$cep[acceptedCepPeaks],
      'pitchSource' = 'cep',
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    # because cepstrum really stinks for frequencies above ~1 kHz, mostly
    # picking up formants or just plain noise, we discount confidence in
    # high-pitch cepstral estimates
    pitchCep_array$pitchCert = pitchCep_array$pitchCert /
      (1 + 5 / (1 + exp(2 - .25 * HzToSemitones(pitchCep_array$pitchCand / pitchFloor))))
    # visualization: a = seq(pitchFloor, pitchCeiling, length.out = 100)
    # b = 1 + 5 / (1 + exp(2 - .25 * HzToSemitones(a / pitchFloor)))
    # plot (a, b, type = 'l')
  }
  return (pitchCep_array)
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
#' @return Returns either NULL or a dataframe of pitch candidates.
#' @keywords internal
getPitchSpec = function(frame,
                        specSmooth,
                        specHNRslope,
                        bin,
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
  specPeaks = data.frame ('freq' = as.numeric(names(frame)[idx]) * 1000,
                          'amp' = frame[idx])

  if (nrow(specPeaks) == 1) {
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
  } else if (nrow(specPeaks) > 1) {
    # analyze five lowest harmonics
    specPeaks = specPeaks [1:min(5, nrow(specPeaks)), ]

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

    pitchCand = sort (pitchCand [pitchCand > pitchFloor &
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


#' Get prior for pitch candidates
#'
#' Internal soundgen function.
#'
#' Prior for adjusting the estimated pitch certainties. For ex., if primarily
#' working with speech, we could prioritize pitch candidates in the expected
#' pitch range (100-1000 Hz) and dampen candidates with very high or very low
#' frequency as unlikely but still remotely possible in everyday vocalizing
#' contexts (think a soft pitch ceiling). Algorithm: the multiplier for each
#' pitch candidate is the density of gamma distribution with mean = priorMean
#' (Hz) and sd = priorSD (semitones) normalized so max = 1 over [pitchFloor,
#' pitchCeiling]. Called by analyze().
#' @return Returns a numeric matrix of the same dimensions as pitchCands for
#'   multiplying the matrix of certainty of in pitch values.
#' @inheritParams analyze
#' @param pitchCands a matrix of pitch candidate frequencies
#' @param plot if TRUE, produces a separate plot of the prior
#' @param ... additional graphical parameters passed on to plot()
#' @keywords internal
#' @examples
#' soundgen:::getPrior(150, 2, pitchCands = NULL, plot = TRUE)
getPrior = function(priorMean,
                    priorSD,
                    pitchCands = NULL,
                    pitchFloor = 75,
                    pitchCeiling = 3000,
                    plot = FALSE,
                    ...) {
  priorMean_semitones = HzToSemitones(priorMean)
  shape = priorMean_semitones ^ 2 / priorSD ^ 2
  rate = priorMean_semitones / priorSD ^ 2
  freqs = seq(HzToSemitones(pitchFloor), HzToSemitones(pitchCeiling), length.out = 100)
  prior_normalizer = dgamma(
    freqs,
    shape = shape,
    rate = rate
  )
  prior_norm_max = max(prior_normalizer)
  if (plot) {
    plot(
      x = semitonesToHz(freqs),
      y = prior_normalizer / prior_norm_max,
      type = 'l',
      log = 'x',
      xlab = 'Frequency, Hz',
      ylab = 'Multiplier of certainty', main = 'Prior belief in pitch values',
      ...
    )
  }
  if (!is.null(pitchCands)) {
    pitchCert_multiplier = dgamma(
      HzToSemitones(pitchCands),
      shape = shape,
      rate = rate
    ) / prior_norm_max
    return(pitchCert_multiplier)
  }
}
