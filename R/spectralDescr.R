
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
    # left
    if (bin_peak == 1) {
      left_over_zero = left_over_thres = TRUE
    } else {
      # should be higher than both adjacent points
      left_over_zero = frame_dB[bin_peak] - frame_dB[bin_peak - 1] > 0
      # should be higher than either of the adjacent points by harmThres
      left_over_thres = frame_dB[bin_peak] - frame_dB[bin_peak - 1] > harmThres
    }
    # right
    if (bin_peak == length(frame_dB)) {
      right_over_zero = right_over_thres = TRUE
    } else {
      right_over_zero = frame_dB[bin_peak] - frame_dB[bin_peak + 1] > 0
      right_over_thres = frame_dB[bin_peak] - frame_dB[bin_peak + 1] > harmThres
    }
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
  pitch_bin_cep = pitch_bin_peaks = vector('logical', 0)
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


#' Subharmonics-to-harmonics ratio
#'
#' Internal soundgen function
#' @keywords internal
subhToHarm = function(
  frame,
  bin,
  freqs,
  pitch,
  samplingRate,
  tol = .05,
  nHarm = 5,
  harmThres = 3,
  harmTol = 0.25,
  method = c('cep', 'harm')[1]
) {
  # plot(frame, type = 'l')
  best_subh = NA
  subDep = 0
  if (method == 'cep') {
    # cepstrum
    cep = abs(fft(as.numeric(log(frame))))
    l = length(cep) %/% 2
    cep = cep[1:l]
    cep[1] = 0
    freqs_cep = samplingRate / (1:l) / 2
    # plot(freqs_cep, cep, type = 'l', log = 'x')
    bin_at_pitch = which.min(abs(freqs_cep - pitch))
    nToTry = min(nHarm, floor(l / bin_at_pitch))
    ratios = data.frame(r = 1:nHarm, energy = NA)
    for (r in 1:nToTry) {
      ratios$energy[r] = max(cep[(bin_at_pitch * r - 1) : (bin_at_pitch * r + 1)])
    }
    ratios$extraEnergy = ratios$energy - ratios$energy[1] / ratios$r
    subR = na.omit(ratios[ratios$extraEnergy > 0, ])
    if (nrow(subR) > 0) {
      best_subh = subR$r[which.max(subR$extraEnergy)]
      subDep = ratios$extraEnergy[best_subh] / ratios$energy[best_subh]
    }
  } else if (method == 'harm') {
    keep_idx = which(freqs < (pitch * nHarm))
    frame = frame[keep_idx]
    frame_dB = 20 * log10(frame[keep_idx])
    freqs = freqs[keep_idx]
    n = length(keep_idx)

    # look for spectral peaks
    temp = zoo::rollapply(zoo::as.zoo(frame_dB),
                          width = 3,  # parameter - see hps or smth
                          align = 'center',
                          function(x) {
                            isCentral.localMax(x, threshold = harmThres)  # another par
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
      # specPeaks[1:10, ]

      idx_pitch = rep(NA, nHarm)
      for (h in 1:nHarm) {
        idx_range = pitch * h * c(1 - tol, 1 + tol)
        peaks_range = which(specPeaks$freq > idx_range[1] &
                              specPeaks$freq < idx_range[2])
        lp = length(peaks_range)
        if (lp == 1) {
          idx_pitch[h] = specPeaks$idx[peaks_range]
        } else if (lp > 1) {
          it = which.min(abs(specPeaks$freq[peaks_range] - pitch * h))
          idx_pitch[h] = specPeaks$idx[peaks_range][it]
        }
      }
      idx_pitch = as.numeric(na.omit(idx_pitch))
      # plot(freqs, frame_dB, type = 'l')
      # points(freqs[idx_pitch], frame_dB[idx_pitch], col = 'red', pch = 3)

      # now repeat for different f0/g0 ratios
      ratios = data.frame(r = 1:nHarm, energy = NA)
      ratios$energy[1] = sum(frame[idx_pitch])
      for (r in 2:nrow(ratios)) {
        freq_max = pitch * nHarm * r
        i_max = which(specPeaks$freq > freq_max)[1] - 1
        if (!is.finite(i_max)) i_max = nrow(specPeaks)
        h_max = floor(specPeaks$freq[i_max] / pitch * r)
        idx_pitch_r = rep(NA, h_max)
        for (h in 1:h_max) {
          pitch_h = pitch / r * h
          idx_range = pitch_h * c(1 - tol, 1 + tol)
          peaks_range = which(specPeaks$freq > idx_range[1] &
                                specPeaks$freq < idx_range[2])
          lp = length(peaks_range)
          if (lp == 1) {
            idx_pitch_r[h] = specPeaks$idx[peaks_range]
          } else if (lp > 1) {
            it = which.min(abs(specPeaks$freq[peaks_range] - pitch_h))
            idx_pitch_r[h] = specPeaks$idx[peaks_range][it]
          }
        }
        idx_pitch_r = as.numeric(na.omit(idx_pitch_r))
        ratios$energy[r] = sum(frame[idx_pitch_r])
        # plot(freqs, frame_dB, type = 'l')
        # points(freqs[idx_pitch_r], frame_dB[idx_pitch_r], col = 'red', pch = 3)
      }
      # some harmonics repeat, eg for g0/2 and g0/4 - think about how to take this into account
      ratios$extraEnergy = ratios$energy - ratios$energy[1]
      ratios$extraEnergy[4] = ratios$extraEnergy[4] - ratios$extraEnergy[2]
      # now we divide by subRatio b/c otherwise high subRatios are privileged (many
      # more potential harmonics)
      subR = na.omit(ratios[ratios$extraEnergy > 0, ])
      if (nrow(subR) > 0) {
        best_subh = subR$r[which.max(subR$extraEnergy)]
        subDep = ratios$extraEnergy[best_subh] / ratios$energy[best_subh]
      }
    }
  }
  return(list(subRatio = best_subh, subDep = subDep))
}
