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
#' @param bin spectrogram bin width, Hz
#' @param freqs frequency per bin of spectrogram
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
                        samplingRate,
                        scaleCorrection,
                        loudness,
                        cutFreq,
                        trackPitch = TRUE,
                        pitchMethods = c('dom', 'autocor'),
                        nCands,
                        pitchDom = list(),
                        pitchAutocor = list(),
                        pitchCep = list(),
                        pitchSpec = list(),
                        pitchHps = list(),
                        pitchFloor,
                        pitchCeiling) {
  absSpec = data.frame(freq = freqs,
                       amp = frame)
  # Cut spectral band from pitchFloor to cutFreq Hz (used for spectral
  # descriptives only - pitch tracking is always done with the full spectrum)
  if (is.null(cutFreq)) {
    absSpec_cut = absSpec
  } else {
    absSpec_cut = absSpec[absSpec$freq > cutFreq[1] &
                            absSpec$freq < cutFreq[2], ]
    # Above 5-6 kHz or so, spectral energy depends too much on the original
    # sampling rate, noises etc. Besides, those frequencies are not super
    # relevant to human vocalizations in any case. So we cut away all info above
    # 5 kHz before we calculate quartiles of spectral energy
  }

  ## DESCRIPTIVES
  # plot(absSpec_cut$freq, absSpec_cut$amp, type = 'l')
  amplitude = sum(absSpec_cut$amp)
  absSpec_cut$w = absSpec_cut$amp / amplitude
  specCentroid = sum(absSpec_cut$freq * absSpec_cut$w)
  peakFreq = absSpec_cut$freq[which.max(absSpec_cut$amp)]
  cums = cumsum(absSpec_cut$amp)
  # first quartile of spectral energy distribution
  quartile25 = absSpec_cut$freq[min(which(cums >= 0.25 * amplitude))]
  # second quartile (same as medianFreq within this spectral band)
  quartile50 = absSpec_cut$freq[min(which(cums >= 0.5 * amplitude))]
  # third quartile. Note: half the energy in the band from pitchFloor to
  # cutFreq kHz lies between quartile25 and quartile75
  quartile75 = absSpec_cut$freq[min(which(cums >= 0.75 * amplitude))]

  # get spectral slope in dB/kHz
  absSpec_cut$amp_dB = 20 * log10(absSpec_cut$amp)
  absSpec_cut$amp_dB = absSpec_cut$amp_dB - max(absSpec_cut$amp_dB)
  # plot(absSpec_cut$freq, absSpec_cut$amp_dB, type = 'l')
  specSlope = summary(lm(amp_dB ~ freq, data = absSpec_cut))$coef[2, 1] * 1000

  # scale correction for loudness estimation
  if (is.numeric(scaleCorrection)) {
    loudness = getLoudnessPerFrame(
      spec = frame * scaleCorrection,
      samplingRate = samplingRate,
      spreadSpectrum = loudness$spreadSpectrum
    )  # in sone, assuming scaling by SPL_measured in analyze()
  } else {
    loudness = NA
  }

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
                                    bin = bin,
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
      peakFreq = peakFreq,
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
#' candidates in the expected pitch range (100-1000 Hz) and decrease our
#' confidence in candidates with very high or very low frequency as unlikely but
#' still remotely possible. You can think of this as a "soft" alternative to
#' setting absolute pitch floor and ceiling. Algorithm: the multiplier for each
#' pitch candidate is the density of gamma distribution with mean = priorMean
#' (Hz) and sd = priorSD (semitones) normalized so max = 1 over [pitchFloor,
#' pitchCeiling]. Useful for previewing the prior given to
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
  freqs = seq(HzToSemitones(pitchFloor),
              HzToSemitones(pitchCeiling),
              length.out = len)
  if (is.numeric(priorMean) & is.numeric(priorSD)) {
    priorMean_semitones = HzToSemitones(priorMean)
    shape = priorMean_semitones ^ 2 / priorSD ^ 2
    rate = priorMean_semitones / priorSD ^ 2
    prior_normalizer = dgamma(
      freqs,
      shape = shape,
      rate = rate
    )
    prior_norm_max = max(prior_normalizer)
    prior = prior_normalizer / prior_norm_max
  } else {
    # flat prior
    prior = rep(1, len)
  }

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
    if (is.numeric(priorMean) & is.numeric(priorSD)) {
      pitchCert_multiplier = dgamma(
        HzToSemitones(pitchCands),
        shape = shape,
        rate = rate
      ) / prior_norm_max
    } else {
      pitchCert_multiplier = matrix(1, nrow = nrow(pitchCands),
                                    ncol = ncol(pitchCands))
    }
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
  if (is.character(var_noSummary)) {
    vars = colnames(result)[!colnames(result) %in% var_noSummary]
  } else {
    vars = colnames(result)
  }
  ls = length(summaryFun)
  lv = length(vars)
  vars_f = paste0(rep(vars, each = ls), '_', rep(summaryFun, each = lv))

  # pre-parse summary function names to speed things up
  functions = vector('list', length(summaryFun))
  for (f in 1:length(summaryFun)) {
    functions[[f]] = eval(parse(text = summaryFun[f]))
  }

  # apply the specified summary function to each column of result
  out = list()
  for (v in vars) {
    for (s in 1:ls) {
      # remove NAs for the most common summary functions
      if (summaryFun[s] %in% c('mean', 'median', 'sd', 'min', 'max', 'range', 'sum')) {
        var_values = na.omit(result[, v])
      } else {
        var_values = result[, v]
      }
      var_f_name = paste0(v, '_', summaryFun[s])
      if (any(is.finite(var_values))) {
        # not finite, eg NA or -Inf - don't bother to calculate
        mySummary = do.call(functions[[s]], list(var_values))  # NAs already removed
        # for smth like range, collapse and convert to character
        if (length(mySummary) > 1) {
          mySummary = paste0(mySummary, collapse = ', ')
        }
        out[[var_f_name]] = mySummary
      } else {
        out[[var_f_name]] = NA
      }
    }
  }

  if (is.character(var_noSummary)) {
    # called from analyze()
    temp = result[1, c('duration', 'duration_noSilence')]
    temp$voiced = mean(!is.na(result$pitch))
    out = c(temp, out)
  }

  return(as.data.frame(out))
}

#' Update analyze
#'
#' Internal soundgen function
#'
#' Updates the output of analyze using manual pitch. Called by pitch_app().
#' @param result the matrix of results returned by analyze()
#' @param pitch_true manual pitch contour of length nrow(result), with NAs
#' @param spectrogram spectrogram with ncol = nrow(result)
#' @param freqs frequency labels of spectrogram bins
#' @param bin spectrogram bin width
#' @param harmHeight_pars same as argument "harmHeight" to analyze() - a list of
#'   settings passed to soundgen:::harmHeight()
#' @param smooth,smoothing_ww,smoothingThres smoothing parameters
#' @param varsToUnv set these variables to NA in unvoiced frames
#' @keywords internal
updateAnalyze = function(
  result,
  pitch_true,
  pitchCands_list = NULL,
  spectrogram,
  freqs = NULL,
  bin = NULL,
  samplingRate = NULL,
  harmHeight_pars = list(),
  subh_pars = list(),
  smooth,
  smoothing_ww,
  smoothingThres,
  varsToUnv = NULL
) {
  # remove all pitch-related columns except dom
  result = result[-which(grepl('pitch', colnames(result)))]
  result$pitch = pitch_true

  # Finalize voicing (some measures are only reported for voiced frames)
  result$voiced = !is.na(pitch_true)
  unvoiced_idx = which(!result$voiced)
  result[unvoiced_idx, varsToUnv] = NA

  # Calculate how far harmonics reach in the spectrum and how strong they are
  # relative to f0
  result[, c('harmEnergy', 'harmHeight', 'subRatio', 'subDep')] = NA
  if (any(result$voiced)) {
    if (is.null(freqs)) freqs = as.numeric(rownames(spectrogram)) * 1000
    if (is.null(bin)) bin = freqs[2] - freqs[1]

    # Calculate the % of energy in harmonics based on the final pitch estimates
    result$harmEnergy = to_dB(harmEnergy(
      pitch = result$pitch,
      s = spectrogram,
      freqs = freqs))
    # Calculate how high harmonics reach in the spectrum
    for (f in which(result$voiced)) {
      temp = try(do.call('harmHeight', c(
        harmHeight_pars,
        list(frame = spectrogram[, f],
             bin = bin,
             freqs = freqs,
             pitch = result$pitch[f]
        )))$harmHeight, silent = TRUE)
      if (class(temp) != 'try-error') {
        result$harmHeight[f] = temp
      }
    }
    # Calculate subharmonics-to-harmonics ratio
    for (f in which(result$voiced)) {
      temp = try(do.call('subhToHarm', c(
        subh_pars,
        list(frame = spectrogram[, f],
             bin = bin,
             freqs = freqs,
             samplingRate = samplingRate,
             pitch = result$pitch[f],
             pitchCands = data.frame(freq = pitchCands_list$freq[, f],
                                     cert = pitchCands_list$cert[, f])
        ))), silent = TRUE)
      if (class(temp) != 'class-error') {
        result[f, c('subRatio', 'subDep')] = temp[c('subRatio', 'subDep')]
      }
    }
    # # result[, c('subRatio', 'subDep')]
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
  if (length(pitch) == 1) return(rep(pitch, len))
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


#' Format pitchManual
#'
#' Internal soundgen function
#'
#' @param pitchManual dataframe produced by analyze() or pitch_app(), path to a
#'   .csv file in which this dataframe is stored, a named list with a numeric
#'   vector of pitch values per sound, or a numeric vector
#' @return A named list of pitch contours.
#' @keywords internal
#' @examples
#' soundgen:::formatPitchManual(c(NA, 120, 180, NA))
#' soundgen:::formatPitchManual('NA, 120, 180, NA')
#' soundgen:::formatPitchManual(list('myfile.wav' = c(NA, 120, 180, NA)))
#' soundgen:::formatPitchManual(data.frame(file = c('file1.wav', 'file2.wav'),
#'                                         pitch = c('NA, 120', '180, NA')))
#' soundgen:::formatPitchManual('adja')
formatPitchManual = function(pitchManual) {
  pitchManual_list = NULL
  failed = FALSE
  if (is.character(pitchManual)) {
    if (file.exists(pitchManual)) {
      # path to csv
      pitchManual_df = try(read.csv(pitchManual)[, c('file', 'pitch')])
      if (class(pitchManual_df) == 'type-error') {
        # problem opening file
        failed = TRUE
      } else {
        # file OK
        pitchManual_list = vector('list', nrow(pitchManual_df))
        names(pitchManual_list) = pitchManual_df$file
        for (i in 1:nrow(pitchManual_df)) {
          pitchManual_list[[i]] = suppressWarnings(as.numeric(unlist(strsplit(
            as.character(pitchManual_df$pitch[[i]]), ','))))
        }
      }
    } else {
      # just a string - try to convert to numeric
      temp = try(suppressWarnings(as.numeric(unlist(strsplit(
        as.character(pitchManual), ',')))))
      if (class(temp) == 'try-error' || !any(!is.na(temp))) {
        failed = TRUE
        pitchManual_list = NULL
      } else {
        pitchManual_list = list(sound = temp)
      }
    }
  } else if (is.list(pitchManual)) {
    # list or dataframe
    if (!is.null(pitchManual$file) && !is.null(pitchManual$pitch) &&
        is.character(pitchManual$pitch[1])) {
      # output of analyze() imported as dataframe
      pitchManual_df = as.data.frame(pitchManual)
      pitchManual_list = vector('list', nrow(pitchManual_df))
      names(pitchManual_list) = pitchManual_df$file
      for (i in 1:nrow(pitchManual_df)) {
        pitchManual_list[[i]] = suppressWarnings(as.numeric(unlist(strsplit(
          as.character(pitchManual_df$pitch[[i]]), ','))))
      }
    } else {
      # preformatted list - return as is
      pitchManual_list = pitchManual
    }
  } else if (is.numeric(pitchManual)) {
    # numeric vector (pitch contour of a single sound)
    pitchManual_list = list(sound = pitchManual)
  } else {
    failed = TRUE
  }

  if (failed) {
    warning(paste(
      "pitchManual not recognized; should be a numeric vector, named list,",
      "csv file or dataframe containing the output of analyze() with columns",
      "'file' and 'pitch', or a named list with pitch contours per file"))
  }

  return(pitchManual_list)
}



#' Check audio input type
#'
#' Internal soundgen function.
#'
#' Checks the types of audio input to another function, which could be a folder
#' with audio files, a single file, a Wave object, or a numeric vector. The
#' purposes of this helper function are to ascertain that there are some valid
#' inputs and to make a list of valid audio files, if any.
#' @param x path to a .wav or .mp3 file, Wave object, or a numeric vector
#'   representing the waveform with specified samplingRate
#' @keywords internal
checkInputType = function(x) {
  if (is.character(x)) {
    # character means file or folder
    if (length(x) == 1 && dir.exists(x)) {
      # input is a folder
      x = dirname(paste0(x, '/arbitrary'))  # strips terminal '/', if any
      filenames = list.files(x, pattern = "*.wav|.mp3|.WAV|.MP3", full.names = TRUE)
      if (length(filenames) < 1)
        stop(paste('No wav/mp3 files found in', x))
    } else {
      # input is one or more audio files
      for (f in 1:length(x)) {
        if (!file.exists(x) ||
            !substr(x, nchar(x) - 3, nchar(x)) %in% c('.wav', '.mp3', '.WAV', '.MP3')) {
          stop('Input not recognized - must be a folder, wav/mp3 file(s), or numeric vector(s)')
        }
      }
      filenames = x
    }
    n = length(filenames)
    type = rep('file', n)
    filenames_base = filenames_noExt = basename(filenames)

    for (f in 1:n) {
      # strip extension
      filenames_noExt[f] = substr(filenames_base[f], 1, nchar(filenames_base[f]) - 4)
      filesizes = file.info(filenames)$size
      # expand from relative to full path (useful for functions that save audio
      # separately from plots)
      filenames[f] = normalizePath(filenames[f])
    }
  } else {
    # not file(s), but one or more objects (Wave / numeric)
    if (!is.list(x)) x = list(x)
    n = length(x)
    if (n == 1) {
      filenames_base = filenames_noExt = 'sound'
    } else {
      filenames_base = filenames_noExt = paste0('sound', 1:n)
    }
    filenames = NULL
    filesizes = NULL
    type = rep(NA, n)
    for (i in 1:n) {
      if (is.numeric(x[[i]])) {
        type[i] = 'vector'
      } else if (class(x[[i]]) == 'Wave') {
        # input is a Wave object
        type[i] = 'Wave'
      } else {
        stop(paste('Input not recognized - must be a folder, wav/mp3 file,',
                   'Wave object, or numeric vector'))
      }
    }
  }
  return(list(
    type = type,
    n = n,
    filenames = filenames,
    filenames_base = filenames_base,
    filenames_noExt = filenames_noExt,
    filesizes = filesizes
  ))
}


#' Read audio
#'
#' Internal soundgen function.
#'
#' @param x audio input (only used for Wave objects or numeric vectors)
#' @param input a list returned by \code{\link{checkInputType}}
#' @param i iteration
#' @param samplingRate sampling rate of \code{x} (only needed if \code{x} is a
#'   numeric vector, rather than an audio file or Wave object)
#' @param scale maximum possible amplitude of input used for normalization of
#'   input vector (only needed if \code{x} is a numeric vector, rather than an
#'   audio file or Wave object)
#' @param from,to if NULL (default), analyzes the whole sound, otherwise
#'   from...to (s)
#' @keywords internal
readAudio = function(x,
                     input = checkInputType(x),
                     i,
                     samplingRate = NULL,
                     scale = NULL,
                     from = NULL,
                     to = NULL) {
  failed = FALSE
  if (input$type[i] == 'file') {
    fi = input$filenames[i]
    ext_i = substr(fi, nchar(fi) - 3, nchar(fi))
    if (ext_i %in% c('.wav', '.WAV')) {
      sound_wave = try(tuneR::readWave(fi))
    } else if (ext_i %in% c('.mp3', '.MP3')) {
      sound_wave = try(tuneR::readMP3(fi))
    } else {
      warning(paste('Input', fi, 'not recognized: expected a wav/mp3 file'))
    }
    if (class(sound_wave) == 'try-error') {
      failed = TRUE
      sound = samplingRate = scale = NULL
    } else {
      sound = as.numeric(sound_wave@left)
      samplingRate = sound_wave@samp.rate
      scale = 2 ^ (sound_wave@bit - 1)
    }
  } else if (input$type[i] == 'vector') {
    if (is.null(samplingRate)) {
      samplingRate = 16000
      message('samplingRate not specified; defaulting to 16000')
    }
    sound = x
    m = max(abs(sound))
    if (is.null(scale)) {
      scale = max(m, 1)
      # message(paste('Scale not specified. Assuming that max amplitude is', scale))
    } else if (is.numeric(scale)) {
      if (scale < m) {
        scale = m
        warning(paste('Scale cannot be smaller than observed max;',
                      'resetting to', m))
      }
    }
  } else if (input$type[i] == 'Wave') {
    sound = x@left
    samplingRate = x@samp.rate
    scale = 2 ^ (x@bit - 1)
  }

  # from...to
  # from...to selection
  ls = length(sound)
  if (any(is.numeric(c(from, to)))) {
    if (!is.numeric(from)) {
      from_points = 1
    } else {
      from_points = max(1, round(from * samplingRate))
    }
    if (!is.numeric(to)) {
      to_points = ls
    }  else {
      to_points = min(ls, round(to * samplingRate))
    }
    sound = sound[from_points:to_points]
    timeShift = from_points / samplingRate
    ls = length(sound)
  } else {
    timeShift = 0
  }
  duration = ls / samplingRate

  return(list(
    sound = sound,
    samplingRate = samplingRate,
    scale = scale,
    failed = failed,
    ls = ls,
    duration = duration,
    timeShift = timeShift,
    filename = input$filenames[i],
    filename_base = input$filenames_base[i],
    filename_noExt = input$filenames_noExt[i]
  ))
}


#' Process audio
#'
#' Internal soundgen function.
#'
#' @inheritParams spectrogram
#' @param funToCall function to call (specify what to do with each audio input)
#' @param myPars a list of parameters to pass on to `funToCall`
#' @param summaryFun function(s) used to summarize the output per input
#' @param var_noSummary names of output variables that should not be summarized
#' @param reportEvery report estimated time left every ... iterations (NA = no
#'   reporting, NULL = default frequency)
#' @keywords internal
processAudio = function(x,
                        samplingRate = NULL,
                        scale = NULL,
                        from = NULL,
                        to = NULL,
                        funToCall,
                        myPars = list(),
                        var_noSummary = NULL,
                        reportEvery = NULL,
                        savePlots = NULL,
                        saveAudio = NULL) {
  input = checkInputType(x)
  input$failed = rep(FALSE, input$n)

  # savePlots
  if (is.character(savePlots)) {
    if (savePlots == '') {
      # same as the folder where the audio input lives
      if (input$type[1] == 'file') {
        savePlots = paste0(dirname(input$filenames[1]), '/')
      } else {
        savePlots = paste0(getwd(), '/')
      }
    } else {
      # make sure the last character of savePath is "/" and expand ~
      savePlots = paste0(
        dirname(paste0(savePlots, '/arbitrary')),
        '/'
      )
    }
    if (!dir.exists(savePlots)) dir.create(savePlots)
  } else {
    savePlots = NULL
  }
  input$savePlots = savePlots  # to pass on to top function like analyze()

  # saveAudio
  if (is.character(saveAudio)) {
    if (saveAudio == '') {
      # same as the folder where the audio input lives
      keypr = readline(prompt = paste(
        "NB: saveAudio='' will overwrite the originals. Proceed? (yes/no) "))
      if (substr(keypr, 1, 1) != 'y') stop('Aborting...')
      if (input$type[1] == 'file') {
        saveAudio = paste0(dirname(input$filenames[1]), '/')
      } else {
        saveAudio = paste0(getwd(), '/')
      }
    } else {
      # make sure the last character of savePath is "/" and expand ~
      saveAudio = paste0(
        dirname(paste0(saveAudio, '/arbitrary')),
        '/'
      )
    }
    if (!dir.exists(saveAudio)) dir.create(saveAudio)
  } else {
    saveAudio = NULL
  }
  input$saveAudio = saveAudio  # to pass on to top function like analyze()

  result = vector('list', input$n)
  names(result) = input$filenames_base
  if (input$type[1] == 'file') x = rep(list(NULL), input$n)
  if (!is.list(x)) x = list(x)
  time_start = proc.time()  # timing
  for (i in 1:input$n) {
    audio = readAudio(x[[i]], input, i,
                      samplingRate = samplingRate,
                      scale = scale,
                      from = from, to = to)
    # to pass savePlots and saveAudio on to funToCall without adding extra
    # args, put them in "audio"
    audio$savePlots = savePlots
    audio$saveAudio = saveAudio

    # analyze file
    if (!audio$failed) {
      an_i = try(do.call(funToCall, c(list(audio = audio), myPars)))
      if (class(an_i)[1] == 'try-error') audio$failed = TRUE
    }
    if (audio$failed) {
      if (input$n > 1) {
        warning(paste('Failed to process file', input$filenames[i]))
      } else {
        warning('Failed to process the input')
      }
      an_i = numeric(0)
      input$failed[i] = TRUE
    }
    result[[i]] = an_i

    # report time
    if ((is.null(reportEvery) || is.finite(reportEvery)) & input$n > 1) {
      reportTime(i = i, nIter = input$n, reportEvery = reportEvery,
                 time_start = time_start, jobs = input$filesizes)
    }
  }

  return(list(
    input = input,
    result = result
  ))
}
