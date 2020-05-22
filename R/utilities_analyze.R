### UTILITIES FOR ACOUSTIC ANALYSIS ###

#' Analyze fft frame
#'
#' Internal soundgen function.
#'
#' This function performs the heavy lifting of pitch tracking and acoustic
#' analysis in general: it takes the spectrum of a single fft frame as input and
#' analyzes it.
#' @param frame the abs spectrum of a frame, as returned by
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
analyzeFrame = function(frame, bin, freqs,
                        autoCorrelation = NULL,
                        samplingRate = 44100,
                        scaleCorrection = 1,
                        cutFreq = 6000,
                        trackPitch = TRUE,
                        pitchMethods = c('autocor', 'cep', 'spec', 'dom'),
                        nCands = 1,
                        pitchDom = list(),
                        pitchAutocor = list(),
                        pitchCep = list(),
                        pitchSpec = list(),
                        pitchHps = list(),
                        pitchFloor = 75,
                        pitchCeiling = 3500) {
  ## DESCRIPTIVES
  absSpec = data.frame(freq = freqs,
                       amp = frame)
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
  frame = frame / max(frame) # plot(frame, type='l')

  # lowest dominant frequency band
  if (trackPitch & 'dom' %in% pitchMethods) {
    d = do.call(getDom,
                c(pitchDom,
                  list(frame = frame,
                       bin = bin,
                       freqs = freqs,
                       pitchFloor = pitchFloor,
                       pitchCeiling = pitchCeiling)))
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
    pa = do.call(getPitchAutocor,
                 c(pitchAutocor,
                   list(autoCorrelation = autoCorrelation,
                        samplingRate = samplingRate,
                        nCands = nCands,
                        pitchFloor = pitchFloor,
                        pitchCeiling = pitchCeiling)))
    if(!is.null(pa$pitchAutocor_array)) {
      pitchCands_frame = rbind(pitchCands_frame, pa$pitchAutocor_array)
    }
    HNR = pa$HNR
  } else {
    HNR = NA
  }

  # cepstrum
  if (trackPitch & 'cep' %in% pitchMethods) {
    pitchCep_array = do.call(getPitchCep,
                             c(pitchCep,
                               list(frame = frame,
                                    samplingRate = samplingRate,
                                    nCands = nCands,
                                    pitchFloor = pitchFloor,
                                    pitchCeiling = pitchCeiling)))
    if(!is.null(pitchCep_array))
      pitchCands_frame = rbind(pitchCands_frame, pitchCep_array)
  }

  # spectral: ratios of harmonics (BaNa)
  if (trackPitch & 'spec' %in% pitchMethods) {
    pitchSpec_array = do.call(getPitchSpec,
                              c(pitchSpec,
                                list(frame = frame,
                                     bin = bin,
                                     freqs = freqs,
                                     HNR = NULL,
                                     nCands = nCands,
                                     pitchFloor = pitchFloor,
                                     pitchCeiling = pitchCeiling)))
    if(!is.null(pitchSpec_array))
      pitchCands_frame = rbind(pitchCands_frame, pitchSpec_array)
  }

  # harmonic product spectrum (hps)
  if (trackPitch & 'hps' %in% pitchMethods) {
    pitchHps_array = do.call(getPitchHps,
                             c(pitchHps,
                               list(frame = frame,
                                    freqs = freqs,
                                    bin = bin,
                                    # nCands = nCands,
                                    pitchFloor = pitchFloor,
                                    pitchCeiling = pitchCeiling)))
    if(!is.null(pitchHps_array))
      pitchCands_frame = rbind(pitchCands_frame, pitchHps_array)
  }

  # some adjustments of pitch candidates
  if (nrow(pitchCands_frame) > 0) {
    pitchCands_frame[, 1:2] = apply(pitchCands_frame[, 1:2],
                                    2,
                                    function(x) as.numeric(x))
    # otherwise they become characters after rbind
  }
  if (nrow(pitchCands_frame[pitchCands_frame$pitchSource == 'dom', ]) > 0 &
      !is.na(HNR)) {
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

#' Get prior for pitch candidates
#'
#' Prior for adjusting the estimated pitch certainties in \code{\link{analyze}}.
#' For ex., if primarily working with speech, we could prioritize pitch
#' candidates in the expected pitch range (100-1000 Hz) and dampen candidates
#' with very high or very low frequency as unlikely but still remotely possible
#' in everyday vocalizing contexts (think a soft pitch ceiling). Algorithm: the
#' multiplier for each pitch candidate is the density of gamma distribution with
#' mean = priorMean (Hz) and sd = priorSD (semitones) normalized so max = 1 over
#' [pitchFloor, pitchCeiling]. Useful for previewing the prior given to
#' \code{\link{analyze}}.
#'
#' @seealso \code{\link{analyze}} \code{\link{pitch_app}}
#'
#' @return Returns a numeric vector of certainties of length \code{len} if
#'   pitchCands is NULL and a numeric matrix of the same dimensions as
#'   pitchCands otherwise.
#' @inheritParams analyze
#' @param len the required length of output vector (resolution)
#' @param plot if TRUE, plots the prior
#' @param ... additional graphical parameters passed on to plot()
#' @param pitchCands a matrix of pitch candidate frequencies (for internal
#'   soundgen use)
#' @export
#' @examples
#' soundgen:::getPrior(priorMean = 150,  # Hz
#'                     priorSD = 2)      # semitones
#' soundgen:::getPrior(150, 6)
#' s = soundgen:::getPrior(450, 24, pitchCeiling = 6000)
#' plot(s, type = 'l')
getPrior = function(priorMean,
                    priorSD,
                    pitchFloor = 75,
                    pitchCeiling = 3000,
                    len = 100,
                    plot = TRUE,
                    pitchCands = NULL,
                    ...) {
  priorMean_semitones = HzToSemitones(priorMean)
  shape = priorMean_semitones ^ 2 / priorSD ^ 2
  rate = priorMean_semitones / priorSD ^ 2
  freqs = seq(HzToSemitones(pitchFloor),
              HzToSemitones(pitchCeiling),
              length.out = len)
  prior_normalizer = dgamma(
    freqs,
    shape = shape,
    rate = rate
  )
  prior_norm_max = max(prior_normalizer)
  prior = prior_normalizer / prior_norm_max
  out = data.frame(freq = semitonesToHz(freqs),
                   prob = prior)
  if (plot) {
    plot(out, type = 'l', log = 'x',
         xlab = 'Frequency, Hz',
         ylab = 'Multiplier of certainty',
         main = 'Prior belief in pitch values',
         ...
    )
  }
  if (!is.null(pitchCands)) {
    pitchCert_multiplier = dgamma(
      HzToSemitones(pitchCands),
      shape = shape,
      rate = rate
    ) / prior_norm_max
    invisible(pitchCert_multiplier)
  } else {
    invisible(out)
  }
}

#' Summarize the output of analyze()
#'
#' Internal soundgen function
#' @param result dataframe returned by analyze(summary = FALSE)
#' @param summaryFun summary functions
#' @param var_noSummary variables that should not be summarized
#' @keywords internal
summarizeAnalyze = function(
  result,
  summaryFun = c('mean', 'sd'),
  var_noSummary = c('duration', 'duration_noSilence', 'voiced', 'time')
) {
  ls = length(summaryFun)
  vars = colnames(result)[!colnames(result) %in% var_noSummary]
  vars_f = paste0(rep(vars, each = ls), '_', rep(summaryFun, ls))

  # specify how to summarize pitch etc values for each frame within each file
  # - for ex., save mean, median, sd, ...
  out = result[1, c('duration', 'duration_noSilence')]
  out$voiced = mean(!is.na(result$pitch))
  out_sum = as.data.frame(matrix(ncol = length(vars_f)))
  colnames(out_sum) = vars_f
  out = cbind(out, out_sum)

  # remove non-summarizable vars from result
  for (v in var_noSummary) {
    result[, v] = NULL
  }

  # pre-parse summary function names to speed things up
  functions = vector('list', length(summaryFun))
  for (f in 1:length(summaryFun)) {
    functions[[f]] = eval(parse(text = summaryFun[f]))
  }

  # apply the specified summary function to each column of result
  for (v in vars) {
    for (s in 1:length(summaryFun)) {
      var_values = na.omit(result[, v])
      var_f_name = paste0(v, '_', summaryFun[s])
      if (any(is.finite(var_values))) {
        mySummary = do.call(functions[[s]], list(var_values))  # NAs already removed
        # for smth like range, collapse and convert to character
        if (length(mySummary) > 1) {
          mySummary = paste0(mySummary, collapse = ', ')
        }
        out[1, var_f_name] = mySummary
      } else {  # not finite, eg NA or -Inf - don't bother to calculate
        out[1, var_f_name] = NA
      }
    }
  }
  return(out)
}

#' Update analyze
#'
#' Internal soundgen function
#'
#' Updates the output of analyze using manual pitch. Called by pitch_app().
#' @param result the matrix of results returned by analyze()
#' @param pitch_true manual pitch contour of length nrow(result), with NAs
#' @param spectrogram spectrogram with ncol = nrow(result)
#' @param harmHeight_pars same as argument "harmHeight" to analyze() - a list of
#'   settings passed to soundgen:::harmHeight()
#' @keywords internal
updateAnalyze = function(result,
                         pitch_true,
                         spectrogram,
                         freqs = NULL,
                         bin = NULL,
                         harmHeight_pars,
                         smooth,
                         smoothing_ww,
                         smoothingThres) {
  # remove all pitch-related columns except dom
  result = result[-which(grepl('pitch', colnames(result)))]
  result$pitch = pitch_true

  # Finalize voicing (some measures are only reported for voiced frames)
  result$voiced = !is.na(pitch_true)
  unvoiced_idx = which(!result$voiced)
  result$amplVoiced = result$ampl
  result$amplVoiced[unvoiced_idx] = NA
  result[unvoiced_idx, c('quartile25', 'quartile50', 'quartile75')] = NA

  # Calculate how far harmonics reach in the spectrum and how strong they are
  # relative to f0
  result$harmEnergy = NA
  result$harmHeight = NA
  if (any(result$voiced)) {
    if (is.null(freqs)) freqs = as.numeric(rownames(spectrogram)) * 1000
    if (is.null(bin)) bin = freqs[2] - freqs[1]

    # Re-calculate the % of energy in harmonics based on the manual pitch estimates
    result$harmEnergy = to_dB(harmEnergy(
      pitch = result$pitch,
      s = spectrogram,
      freqs = freqs))
    # Re-calculate how high harmonics reach in the spectrum
    for (f in which(result$voiced)) {
      result$harmHeight[f] = do.call('harmHeight', c(
        harmHeight_pars,
        list(frame = spectrogram[, f],
             bin = bin,
             freqs = freqs,
             pitch = result$pitch[f]
        )))$harmHeight
    }
    if (smooth > 0) {
      result$harmHeight = medianSmoother(
        result[, 'harmHeight', drop = FALSE],
        smoothing_ww = smoothing_ww,
        smoothingThres = smoothingThres)[, 1]
    }
  }

  # Arrange columns in alphabetical order (except the first three)
  result = result[, c(1:3, 3 + order(colnames(result)[4:ncol(result)]))]
  return(result)
}


#' Upsample pitch contour
#'
#' Internal soundgen function
#'
#' Intended to up- or downsample pitch contours containing NA values using
#' linear interpolation ("approx"). The problem is that NA segments should also
#' be expanded when upsampling, and approx() doesn't do that. Algorithm: when
#' upsampling, first interpolates NAs (constant at beg/end, linear in the
#' middle), then runs approx(), and finally puts NAs back in where they belong.
#' @param pitch numeric vector of pitch values, including NAs (as returned by
#'   pitch_app)
#' @param len required length
#' @param plot if TRUE, plots the old and new pitch contours
#' @keywords internal
#' @examples
#' pitchManual = c(130, 150, 250, 290, 320, 300, 280, 270, 220)
#' soundgen:::upsamplePitchContour(pitchManual, len = 5, plot = TRUE)
#' soundgen:::upsamplePitchContour(pitchManual, len = 25, plot = TRUE)
#'
#' pitchManual = c(NA, 150, 250, NA, NA, 300, 280, 270, NA)
#' soundgen:::upsamplePitchContour(pitchManual, len = 5, plot = TRUE)
#' soundgen:::upsamplePitchContour(pitchManual, len = 25, plot = TRUE)
#'
#' soundgen:::upsamplePitchContour(c(NA, NA), len = 5)
upsamplePitchContour = function(pitch, len, plot = FALSE) {
  if (!any(!is.na(pitch))) return(rep(NA, len))
  len_orig = length(pitch)
  time_stamps1 = seq(0, 1, length.out = len_orig)

  if (len_orig == len) {
    return(pitch)
  } else if (len_orig > len) {
    # downsample
    idx = round(seq(1, len_orig, length.out = len))
    pitch1 = pitch[idx]
  } else {
    # upsample
    # interpolate NAs, otherwise approx doesn't work correctly
    # (esp. with NAs at beg/end)
    idx_unv = which(is.na(pitch))  # remember where NAs were
    pitch_int = intplPitch(pitch, idx_unv = idx_unv)
    pitch1 = approx(x = pitch_int, n = len)$y

    # find NA positions in the new sound
    d = diff(is.na(pitch))  # 1 = beginning of NA episode, -1 = end of NA episode
    beg = which(d == 1) + 1
    end = which(d == -1) + 1
    if (is.na(pitch[1])) beg = c(1, beg)
    if (is.na(pitch[len_orig])) end = c(end, len_orig)
    na_pos_01 = data.frame(beg = time_stamps1[beg], end = time_stamps1[end])
    na_pos2 = round(na_pos_01 * len)  # from % to position indices
    na_pos2_vector = unlist(apply(na_pos2, 1, function(x) x[1]:x[2]))

    # put NAs back in
    pitch1[na_pos2_vector] = NA
  }

  if (plot) {
    plot(time_stamps1, pitch, type = 'p', log = 'y',
         xlab = 'Relative position', ylab = 'Pitch')
    points(x = seq(0, 1, length.out = len), y = pitch1,
           type = 'b', col = 'red', pch = 3)
  }
  return(pitch1)
}

#' Height of harmonics
#'
#' Internal soundgen function
#'
#' Attempts to estimate how high harmonics reach in the spectrum - that is, at
#' what frequency we can still discern peaks at multiples of f0 or, for
#' low-pitched sounds, regularly spaced peaks separated by ~f0.
#' @inheritParams analyzeFrame
#' @param pitch the final pitch estimate for the current frame
#' @param harmThres minimum height of spectral peak, dB
#' @param harmPerSel the number of harmonics per sliding selection
#' @param harmTol maximum tolerated deviation of peak frequency from multiples
#'   of f0, proportion of f0
#' @return Returns the frequency (Hz) up to which we find harmonics
#' @keywords internal
#' @examples
#' s = soundgen(sylLen = 400, addSilence = 0, pitch = 400, noise = -10,
#'   rolloff = -15, jitterDep = .1, shimmerDep = 5, temperature = .001)
#' sp = spectrogram(s, samplingRate = 16000)
#' hh = soundgen:::harmHeight(sp[, 5], pitch = 400,
#'   freqs = as.numeric(rownames(sp)) * 1000, bin = 16000 / 2 / nrow(sp))
harmHeight = function(frame,
                      pitch,
                      bin,
                      freqs,
                      harmThres = 3,
                      harmTol = 0.25,
                      harmPerSel = 5) {
  frame_dB = 20 * log10(frame)

  # METHOD 1: look for peaks at multiples of f0
  lh_peaks = harmHeight_peaks(frame_dB, pitch, bin, freqs,
                              harmThres = harmThres,
                              harmTol = harmTol,
                              plot = FALSE)

  # METHODS 2 & 3: look for peaks separated by f0
  lh2 = harmHeight_dif(frame_dB, pitch, bin, freqs,
                       harmThres = harmThres,
                       harmTol = harmTol,
                       harmPerSel = harmPerSel,
                       plot = FALSE)
  lh = median(c(lh_peaks, lh2$lastHarm_dif, lh2$lastHarm_cep), na.rm = TRUE)
  return(list(harmHeight = lh,
              harmHeight_peaks = lh_peaks,
              harmHeight_dif = lh2$lastHarm_dif,
              harmHeight_cep = lh2$lastHarm_cep))
}

#' Height of harmonics: peaks method
#'
#' Internal soundgen function
#'
#' Estimates how far harmonics reach in the spectrum by checking how many
#' spectral peaks we can find close to multiples of f0.
#' @inheritParams harmHeight
#' @param plot if TRUE, produces a plot of spectral peaks
#' @keywords internal
harmHeight_peaks = function(frame_dB,
                            pitch,
                            bin,
                            freqs,
                            harmThres = 3,
                            harmTol = 0.25,
                            plot = FALSE) {
  harmSmooth = round(harmTol * pitch / bin)  # from prop of f0 to bins
  nHarm = floor((max(freqs) - harmSmooth * bin) / pitch)
  peakFound = rep(FALSE, nHarm)
  if (plot) plot(freqs, frame_dB, type = 'l')
  for (h in 1:nHarm) {
    # check f0 as well, otherwise may get 2 * f0 although f0 is also below thres
    bin_h = round(pitch * h / bin)
    # b/c of rounding error, and b/c pitch estimates are often slightly off, the
    # true harmonic may lie a bit above or below this bin, so we search for a
    # peak within harmSmooth of where we expect to find it
    idx_peak = which.max(frame_dB[(bin_h - harmSmooth) : (bin_h + harmSmooth)])
    bin_peak = bin_h + idx_peak - harmSmooth - 1
    # should be higher than both adjacent points
    left_over_zero = frame_dB[bin_peak] - frame_dB[bin_peak - 1] > 0
    right_over_zero = frame_dB[bin_peak] - frame_dB[bin_peak + 1] > 0
    # should be higher than either of the adjacent points by harmThres
    left_over_thres = frame_dB[bin_peak] - frame_dB[bin_peak - 1] > harmThres
    right_over_thres = frame_dB[bin_peak] - frame_dB[bin_peak + 1] > harmThres
    peakFound[h] = left_over_zero & right_over_zero &
      (left_over_thres | right_over_thres)
    if (plot) {  # plot for debugging
      if (peakFound[h]) {
        text(freqs[bin_peak], frame_dB[bin_peak],
             labels = h, pch = 5, col = 'blue')
      } else {
        text(freqs[bin_peak], frame_dB[bin_peak],
             labels = h, pch = 5, col = 'red')
      }
    }
  }
  first_absent_harm = which(!peakFound)[1]
  if (length(first_absent_harm) > 0) {
    lastHarm = pitch * (first_absent_harm - 1)
  } else {
    lastHarm = NA
  }
  return(lastHarm)
}

#' Height of harmonics: difference method
#'
#' Internal soundgen function
#'
#' Estimates how far harmonics reach in the spectrum by analyzing the typical
#' distances between spectral peaks in different frequency regions.
#' @inheritParams harmHeight
#' @param plot if TRUE, produces a plot of spectral peaks
#' @keywords internal
harmHeight_dif = function(frame_dB,
                          pitch,
                          bin,
                          freqs,
                          harmThres = 3,
                          harmTol = 0.25,
                          harmPerSel = 5,
                          plot = FALSE) {
  # width of smoothing interval (in bins), forced to be an odd number
  harmSmooth_bins = 2 * ceiling(pitch / bin / 2) - 1

  # find peaks in the smoothed spectrum (much faster than seewave::fpeaks)
  temp = zoo::rollapply(
    zoo::as.zoo(frame_dB),
    width = harmSmooth_bins,
    align = 'center',
    function(x) {
      middle = ceiling(length(x) / 2)
      which.max(x) == middle &   # peak in the middle
        any(x[middle] - x[1:(middle - 1)] > harmThres) &  # a deep drop on the left
        any(x[middle] - x[(middle + 1):length(x)] > harmThres)  # ...or on the right
    }
  )
  idx = zoo::index(temp)[zoo::coredata(temp)]

  if (plot) {
    plot(freqs, frame_dB, type = 'l')
    points(freqs[idx], frame_dB[idx], pch = 5, col = 'blue')
  }

  # slide a selection along the spectrum starting from f0
  pitch_bins = pitch / bin  # f0 location in bins
  # width of selection in bins (no more than half the frame len)
  sel_bins = min(round(pitch_bins * harmPerSel), length(frame_dB) / 2)
  harmTol_bins = round(pitch_bins * harmTol)  # tolerated deviance in bins
  i = pitch_bins  # start at f0
  pitch_bin_cep = pitch_bin_peaks = c()
  while (i + sel_bins < length(frame_dB)) {
    end = i + sel_bins - 1

    # count intervals b/w spectral peaks
    d = diff(idx[idx >= i & idx <= end])  # distances b/w peaks
    # median deviation of these distances from expected (f0)
    dp = abs(median(d, na.rm = TRUE) - pitch_bins)
    dp_within_tol = (dp < harmTol_bins)
    pitch_bin_peaks = c(pitch_bin_peaks, dp_within_tol)

    # cepstrum
    sel = as.numeric(frame_dB[i:(i + sel_bins - 1)])
    cep = abs(fft(sel))
    # plot(sel, type = 'l')
    l = length(cep) %/% 2
    cep = cep[1:l]
    # plot(cep, type = 'l')
    bin_at_pitch = harmPerSel + 1
    # Is there a local max at bin_at_pitch? Any height will do
    peak_at_pitch = (cep[bin_at_pitch] > cep[bin_at_pitch - 1]) &
      (cep[bin_at_pitch] > cep[bin_at_pitch + 1])
    pitch_bin_cep = c(pitch_bin_cep, peak_at_pitch)

    i = round(i + pitch_bins)  # move the sel by one harmonic (f0)
  }

  # Find the central frequency of the first bin w/o harmonics
  fbwh_peaks = which(!pitch_bin_peaks)[1]
  if (is.na(fbwh_peaks)) {
    lastHarm_dif = tail(freqs, 1)
  } else {
    lastHarm_dif = (pitch_bins * (fbwh_peaks - 1) - sel_bins / 2) * bin
    if (!is.na(lastHarm_dif) && lastHarm_dif < pitch) lastHarm_dif = NA
  }


  fbwh_cep = which(!pitch_bin_cep)[1]
  if (is.na(fbwh_cep)) {
    lastHarm_cep = tail(freqs, 1)
  } else {
    lastHarm_cep = (pitch_bins * (fbwh_cep - 1) - sel_bins / 2) * bin
    if (!is.na(lastHarm_cep) && lastHarm_cep < pitch) lastHarm_cep = NA
  }
  lastHarm_cep = (pitch_bins * (fbwh_cep - 1) - sel_bins / 2) * bin
  if (!is.na(lastHarm_cep) && lastHarm_cep < pitch) lastHarm_cep = NA

  return(list(lastHarm_cep = lastHarm_cep,
              lastHarm_dif = lastHarm_dif))
}

#' Energy in harmonics
#'
#' Internal soundgun function
#'
#' Calculates the % of energy in harmonics based on the provided pitch estimate
#' @param pitch pitch estimates, Hz (vector)
#' @param s spectrogram (ncol = length(pitch))
#' @param coef calculate above pitch * coef
#' @param freqs as.numeric(rownames(s)) * 1000
#' @keywords internal
harmEnergy = function(pitch, s, freqs = NULL, coef = 1.25) {
  if (is.null(freqs)) freqs = as.numeric(rownames(s)) * 1000
  threshold = coef * pitch
  he = apply(matrix(1:ncol(s)), 1, function(x) {
    ifelse(is.na(threshold[x]),
           NA,
           sum(s[freqs > threshold[x], x]) / sum(s[, x]))
  })
  return(he)
}
