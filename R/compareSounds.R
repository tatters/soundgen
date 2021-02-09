#' Compare two sounds
#'
#' Computes similarity between two sounds based on comparing their
#' spectrogram-like representations. The input can be the actual audio or two
#' pre-extracted spectrograms (matrices produced by any function of your
#' choice). If the input is audio, two methods of producing spectrograms are
#' available: \code{specType = 'linear'} calls \code{\link[tuneR]{powspec}} for
#' an power spectrogram with frequencies in Hz, and \code{specType = 'mel'}
#' calls \code{\link[tuneR]{melfcc}} for an auditory spectrogram with
#' frequencies in Mel. For more customized options, just produce your
#' spectrograms with your favorite function before calling \code{compareSounds}
#' because it also accepts matrices with pre-extracted spectrograms as its
#' input. To be directly comparable, the two spectrograms are made into matrices
#' of the same size. In case of differences in sampling rates, only frequencies
#' below the lower Nyquist frequency or below \code{maxFreq} are kept. In case
#' of differences in duration, the shorter sound is padded with 0 (silence) or
#' NA, as controlled by arguments \code{padWith, padDir}. Then the matrices are
#' compared using methods like bin-by-bin difference, correlation, or Dynamic
#' Time Warp.
#'
#' @param x,y either two spectrograms or two sounds to be compared (numeric
#'   vectors, Wave objects, or paths to wav/mp3 files)
#' @param samplingRate if one or both inputs are numeric vectors, specify
#'   sampling rate, Hz. A vector of length 2 means the two inputs have different
#'   sampling rates, in which case spectrograms are compared only up to the
#'   lower Nyquist frequency
#' @inheritParams spectrogram
#' @param specType "linear" = power spectrogram with
#'   \code{\link[tuneR]{powspec}}, "mel" = mel-frequency spectrogram with
#'   \code{\link[tuneR]{melfcc}}
#' @param specPars a list of parameters passed to \code{\link[tuneR]{melfcc}}
#' @param dtwPars a list of parameters passed to \code{\link[dtw]{dtw}}
#' @param method method of comparing mel-transformed spectra of two sounds:
#'   "cor" = Pearson's correlation; "cosine" = cosine similarity; "diff" =
#'   absolute difference between each bin in the two spectrograms; "dtw" =
#'   multivariate Dynamic Time Warp with \code{\link[dtw]{dtw}}
#' @param padWith if the duration of x and y is not identical, the compared
#'   spectrograms are padded with either silence (\code{padWith = 0}) or with
#'   NA's (\code{padWith = NA}) to have the same number of columns. Padding with
#'   NA implies that only the overlapping part is of relevance, whereas padding
#'   with 0 means that the added silent part is also compared with the longer
#'   sound, usually resulting in lower similarity (see examples)
#' @param padDir if padding, specify where to add zeros or NAs: before the sound
#'   ('left'), after the sound ('right'), or on both sides ('central')
#' @param dynamicRange parts of the spectra quieter than \code{-dynamicRange} dB
#'   are not compared
#' @param maxFreq parts of the spectra above \code{maxFreq} Hz are not compared
#' @return Returns a dataframe with two columns: "method" for the method(s)
#'   used, and "sim" for the similarity between the two sounds calculated with
#'   that method. The range of similarity measures is [-1, 1] for "cor",
#'   [0, 1] for "cosine" and "diff", and (-Inf, Inf) for "dtw".
#' @export
#' @examples
#' data(orni, peewit, package = 'seewave')
#' compareSounds(orni, peewit)
#' # spectrogram(orni); playme(orni)
#' # spectrogram(peewit); playme(peewit)
#'
#' \dontrun{
#' s1 = soundgen(formants = 'a', play = TRUE)
#' s2 = soundgen(formants = 'ae', play = TRUE)
#' s3 = soundgen(formants = 'eae', sylLen = 700, play = TRUE)
#' s4 = runif(8000, -1, 1)  # white noise
#' compareSounds(s1, s2, samplingRate = 16000)
#' compareSounds(s1, s4, samplingRate = 16000)
#'
#' # the central section of s3 is more similar to s1 than is the beg/eng of s3
#' compareSounds(s1, s3, samplingRate = 16000, padDir = 'left')
#' compareSounds(s1, s3, samplingRate = 16000, padDir = 'central')
#'
#' # padding with 0 penalized differences in duration, whereas padding with NA
#' # is like saying we only care about the overlapping part
#' compareSounds(s1, s3, samplingRate = 16000, padWith = 0)
#' compareSounds(s1, s3, samplingRate = 16000, padWith = NA)
#'
#' # comparing linear (Hz) vs mel-spectrograms produces quite different results
#' compareSounds(s1, s3, samplingRate = 16000, specType = 'linear')
#' compareSounds(s1, s3, samplingRate = 16000, specType = 'mel')
#'
#' # pass additional control parameters to dtw and melfcc
#' compareSounds(s1, s3, samplingRate = 16000,
#'               specPars = list(nbands = 128),
#'               dtwPars = list(dist.method = "Manhattan"))
#'
#' # a demo for comparing different similarity metrics
#' target = soundgen(sylLen = 500, formants = 'a',
#'                   pitch = data.frame(time = c(0, 0.1, 0.9, 1),
#'                                      value = c(100, 150, 135, 100)),
#'                   temperature = 0.001)
#' spec1 = soundgen:::getMelSpec(target, samplingRate = 16000)
#'
#' parsToTry = list(
#'   list(formants = 'i',                                            # wrong
#'        pitch = data.frame(time = c(0, 1),                         # wrong
#'                           value = c(200, 300))),
#'   list(formants = 'i',                                            # wrong
#'        pitch = data.frame(time = c(0, 0.1, 0.9, 1),               # right
#'                                  value = c(100, 150, 135, 100))),
#'   list(formants = 'a',                                            # right
#'        pitch = data.frame(time = c(0,1),                          # wrong
#'                                  value = c(200, 300))),
#'   list(formants = 'a',
#'        pitch = data.frame(time = c(0, 0.1, 0.9, 1),               # right
#'                                  value = c(100, 150, 135, 100)))  # right
#' )
#'
#' sounds = list()
#' for (s in 1:length(parsToTry)) {
#'   sounds[[length(sounds) + 1]] =  do.call(soundgen,
#'     c(parsToTry[[s]], list(temperature = 0.001, sylLen = 500)))
#' }
#' lapply(sounds, playme)
#'
#' method = c('cor', 'cosine', 'diff', 'dtw')
#' df = matrix(NA, nrow = length(parsToTry), ncol = length(method))
#' colnames(df) = method
#' df = as.data.frame(df)
#' for (i in 1:nrow(df)) {
#'   df[i, ] = compareSounds(
#'     x = spec1,  # faster to calculate spec1 once
#'     y = sounds[[i]],
#'     samplingRate = 16000,
#'     method = method
#'   )[, 2]
#' }
#' df$av = rowMeans(df, na.rm = TRUE)
#' # row 1 = wrong pitch & formants, ..., row 4 = right pitch & formants
#' df$formants = c('wrong', 'wrong', 'right', 'right')
#' df$pitch = c('wrong', 'right', 'wrong', 'right')
#' df
#' }
compareSounds = function(
  x, y,
  samplingRate = NULL,
  windowLength = 40,
  overlap = 50,
  step = NULL,
  dynamicRange = 80,
  method = c('cor', 'cosine', 'diff', 'dtw'),
  specType = c('linear', 'mel')[2],
  specPars = list(),
  dtwPars = list(),
  padWith = NA,
  padDir = c('central', 'left', 'right')[1],
  maxFreq = NULL) {
  # check sampling rates
  if (!is.null(samplingRate)) {
    if (length(samplingRate) == 1) {
      samplingRate = rep(samplingRate, 2)
    } else if (samplingRate[1] != samplingRate[2]) {
      nq_min = min(samplingRate) / 2
      if (is.null(maxFreq) || maxFreq > nq_min) maxFreq = nq_min
    }
  }

  # extract spectrograms
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  throwaway01 = 2 ^ (-dynamicRange / 20)
  if (class(x)[1] == 'matrix') {
    spec1 = x
  } else {
    x_audio = readAudio(x, input = checkInputType(x), samplingRate = samplingRate[1])
    samplingRate[1] = x_audio$samplingRate
    if (specType == 'linear') {
      spec1 = tuneR::powspec(x_audio$sound,
                             sr = samplingRate[1],
                             wintime = windowLength / 1000,
                             steptime = step / 1000,
                             dither = FALSE)
      # strip empty frames
      spec1 = spec1[, colMeans(spec1, na.rm = TRUE) > throwaway01, drop = FALSE]
      # log-transform and normalize
      spec1 = log01(spec1)
    } else if (specType == 'mel') {
      spec1 = getMelSpec(x_audio$sound,
                         samplingRate = samplingRate[1],
                         windowLength = windowLength,
                         step = step,
                         dynamicRange = dynamicRange,
                         maxFreq = maxFreq,
                         specPars = specPars)
    }
  }
  if (class(y)[1] == 'matrix') {
    spec2 = y
  } else {
    y_audio = readAudio(y, input = checkInputType(y), samplingRate = samplingRate[2])
    samplingRate[2] = y_audio$samplingRate
    if (specType == 'linear') {
      spec2 = tuneR::powspec(y_audio$sound,
                             sr = samplingRate[2],
                             wintime = windowLength / 1000,
                             steptime = step / 1000,
                             dither = FALSE)
      # strip empty frames
      spec2 = spec2[, colMeans(spec2, na.rm = TRUE) > throwaway01, drop = FALSE]
      # log-transform and normalize
      spec2 = log01(spec2)
    } else if (specType == 'mel') {
      spec2 = getMelSpec(y_audio$sound,
                         samplingRate = samplingRate[2],
                         windowLength = windowLength,
                         step = step,
                         dynamicRange = dynamicRange,
                         maxFreq = maxFreq,
                         specPars = specPars)
    }
  }

  # make sure the number of rows (frequency bins) is the same (only an issue if
  # sampling rates differ - basically, ignore frequencies only present in one
  # sound, as if downsampling the sound with the higher sampling rate)
  nr1 = nrow(spec1); nr2 = nrow(spec2)
  if (nr1 != nr2) {
    if (nr1 < nr2) {
      spec2 = spec2[1:nr1, ]
    } else if (nr1 > nr2) {
      spec1 = spec1[1:nr2]
    }
  }

  # make sure the number of columns (STFT frames) is the same by padding with
  # zeros (silence). This is not needed for "dtw"
  sim = data.frame(method = method, sim = NA)
  if (length(method[method != 'dtw']) > 0) {
    spec1_resized = spec1
    spec2_resized = spec2
    if (ncol(spec1) < ncol(spec2)) {
      if (is.na(padWith)) {
        # simply discard non-overlapping sections (shorten the long sound)
        col_long = matchLengths(1:ncol(spec1), ncol(spec2),
                                padDir = padDir, padWith = NA)
        spec2_resized = spec2[, which(!is.na(col_long))]
      } else {
        # pad the shorter sound
        spec1_resized = matchColumns(matrix_short = spec1,
                                     nCol = ncol(spec2),
                                     padWith = padWith,
                                     padDir = padDir)
      }
    } else if (ncol(spec1) > ncol(spec2)) {
      if (is.na(padWith)) {
        # simply discard non-overlapping sections (shorten the long sound)
        col_long = matchLengths(1:ncol(spec2), ncol(spec1),
                                padDir = padDir, padWith = NA)
        spec1_resized = spec1[, which(!is.na(col_long))]
      } else {
        # pad the shorter sound
        spec2_resized = matchColumns(matrix_short = spec2,
                                     nCol = ncol(spec1),
                                     padWith = padWith,
                                     padDir = padDir)
      }
    }
    if (is.na(padWith)) {
      # simply remove non-overlapping sections
      na_tgt = as.numeric(which(apply(spec1, 2, function(x) any(is.na(x)))))
      na_cnd = as.numeric(which(apply(spec2, 2, function(x) any(is.na(x)))))
      na_idx = c(na_tgt, na_cnd)
      if (length(na_idx) > 0) {
        spec1_resized = spec1[, -na_idx]
        spec2_resized = spec2[, -na_idx]
      }
    }
    # correlate the equal-sized matrices
    tgt = as.numeric(spec1_resized)  # range 01
    cnd = as.numeric(spec2_resized)
    if ('cor' %in% method) {
      sim$sim[sim$method == 'cor'] = cor(tgt, cnd, use = 'na.or.complete')
    }
    if ('cosine' %in% method) {
      # sim$sim[sim$method == 'cosine'] = crossprod(tgt, cnd) / sqrt(
      #   crossprod(tgt, tgt) * crossprod(cnd, cnd)
      # )
      sim$sim[sim$method == 'cosine'] = sum(tgt * cnd, na.rm = TRUE) / sqrt(
        sum(tgt * tgt, na.rm = TRUE) * sum(cnd * cnd, na.rm = TRUE)
      )
      # same as replacing NAs with 0 and running crossprod instead of sum
    }
    if ('diff' %in% method) {
      sim$sim[sim$method == 'diff'] = 1 - sum(abs(tgt - cnd), na.rm = TRUE) /
        (sum(tgt, na.rm = TRUE) + sum(cnd, na.rm = TRUE))
      # range(0, 1); or could do 1 - 2 * ... if we want a sim measure (-1, 1)
    }
  }

  if ('dtw' %in% method) {
    d = try(do.call(dtw::dtw, c(list(
      x = t(spec1), y = t(spec2),  # before resizing; t() b/c need time in columns
      distance.only = TRUE),
      dtwPars)),
      silent = TRUE)
    if (class(d)[1] == 'try-error') {
      dist_dtw = NA
    } else {
      dist_dtw = d$normalizedDistance
    }
    sim$sim[sim$method == 'dtw'] = 1 - dist_dtw
  }
  return(sim)
}


#' Mel-transformed spectrogram
#'
#' Internal soundgen function
#'
#' Takes a .wav file or a waveform as numeric vector + samplingRate and returns
#' mel-transformed spectrum (auditory spectrum). Calls
#' \code{\link[tuneR]{melfcc}}. See \code{\link{compareSounds}}.
#' @param s input sound (path to a .wav file or numeric vector)
#' @inheritParams compareSounds
#' @param plot if TRUE, plots the spectrum
#' @keywords internal
getMelSpec = function(s,
                      samplingRate = NULL,
                      windowLength = 40,
                      overlap = 50,
                      step = NULL,
                      dynamicRange = 80,
                      maxFreq = NULL,
                      specPars = list(),
                      plot = FALSE) {
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  throwaway01 = 2 ^ (-dynamicRange / 20)
  if (is.null(specPars$nbands)) specPars$nbands = 100 * windowLength / 20

  if (is.character(s)) {
    sWave = tuneR::readWave(s)
    samplingRate = sWave@samp.rate
  } else if (is.numeric(s)) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    }
    sWave = tuneR::Wave(s, samp.rate = samplingRate, bit = 16)
  } else if (class(s)[1] == 'Wave') {
    sWave = s
    samplingRate = sWave@samp.rate
  }

  if (is.null(maxFreq)) maxFreq = samplingRate / 2
  spec = t(do.call(tuneR::melfcc, c(
    list(
      sWave,
      wintime = windowLength / 1000,
      hoptime = step / 1000,
      maxfreq = maxFreq,
      spec_out = TRUE
    ), specPars
  ))$aspectrum)
  # strip empty frames
  spec = spec[, colMeans(spec, na.rm = TRUE) > throwaway01, drop = FALSE]
  # log-transform and normalize
  spec = log01(spec)

  if (plot) {
    # show the spectrogram of the target
    filled.contour.mod(
      x = seq(1, ncol(spec) * step,
              length.out = ncol(spec)),
      y = 1:nrow(spec),
      z = t(spec),
      levels = seq(0, 1, length = 30),
      color.palette = function(x) gray(seq(1, 0, length.out = x))
    )
  }
  return(spec)
}
