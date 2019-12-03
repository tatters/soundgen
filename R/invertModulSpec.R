#' Invert modulation spectrum
#'
#' samplingRate = 16000
#' s = soundgen(sylLen = 1500, pitch = 200, amFreq = 25, amDep = 50, samplingRate = samplingRate)
#' playme(s, samplingRate)
#' spec = spectrogram(s, samplingRate, windowLength = 25, overlap = 80, wn = 'hanning', osc = TRUE, padWithSilence = FALSE)
#' s_rev = invertSpectrogram(spec, samplingRate = samplingRate, windowLength = 25, overlap = 80, wn = 'hanning')
#'   playme(s_rev, samplingRate)
#'   spectrogram(s_rev, samplingRate, osc = TRUE)
#'
#' ms = modulationSpectrum(s, samplingRate = samplingRate, windowLength = 25, overlap = 80, wn = 'hanning', maxDur = Inf, logSpec = FALSE, power = NA, logWarp = NA, returnComplex = TRUE, kernelSize = 1)$complex
#' ms = specToMS(spec, samplingRate = samplingRate)
#' image(x = as.numeric(colnames(ms)), y = as.numeric(rownames(ms)), z = t(log(abs(ms))))
#'
#' # Filter as needed - for ex., remove AM > 3 Hz
#' am = as.numeric(colnames(ms))
#' fm = as.numeric(rownames(ms))
#' idx_row = 1:nrow(ms)
#' idx_col = which(abs(am) > 5)
#' ms_filt = ms
#' ms_filt[idx_row, idx_col] = 0
#' image(t(log(abs(ms_filt))))
#'
#' # Convert back to a spectrogram
#' spec_filt = msToSpec(ms_filt, samplingRate = samplingRate)
#' image(t(log(abs(spec_filt))))
#'
#' Invert the spectrogram
#' s_filt = invertSpectrogram(abs(spec_filt), samplingRate = samplingRate, windowLength = 25, overlap = 80, initialPhase = 'spsi', nIter = 50)
#'
#' # Compare with the original
#' playme(s, samplingRate)
#' spectrogram(s, samplingRate, osc = TRUE)
#' playme(s_filt, samplingRate)
#' spectrogram(s_filt, samplingRate, osc = TRUE)
invertModulationSpectrum = function(ms) {
  # ms[1:5, 1:5]
  # s1 = msToSpec(t(ms), samplingRate, windowLength = NULL, step = NULL)
  # # image(t(log(s1)))
  #
  # # inverse to go back to time domain and thus reconstruct the (modified) sound
  # s2 = fft(t(ms), inverse = TRUE) # s2_new[1:3, 1:3] - s2[1:3, 1:3]
  # # image(t(log(abs(s2))))
  # s1 = s2 / (-1)^(row(s2) + col(s2))
  # image(t(log(abs(s1))))
  # t(s1)[1:3, 1:3]
  # abs(s2_new2)[1:3, 1:3]
  # # image(log(abs(s1)))
  # dim(s1)
  #
  # s_new = invertSpectrogram(s1, samplingRate = samplingRate, windowLength = 25, overlap = 80, wn = 'hanning')
  # s_new = invertSpectrogram(abs(s2_new2), samplingRate = samplingRate, windowLength = 25, overlap = 80, wn = 'hanning')
  #
  # s_new = invertSpectrogram(temp1, samplingRate = samplingRate, windowLength = 25, overlap = 80, wn = 'hanning')
  # s_new = invertSpectrogram(abs(temp2_new2), samplingRate = samplingRate, windowLength = 25, overlap = 80, wn = 'hanning')
  #
  # playme(s_new, samplingRate)
  # spectrogram(s_new, samplingRate, osc = TRUE)
}


#' Spectrogram to modulation spectrum
#'
#' Internal soundgen function.
#'
#' Takes a spectrogram (either complex or magnitude) and returns a MS with
#' proper row and column labels.
#' @return Returns a MS - matrix of complex values of the same dimension as
#'   spec, with AM in rows and FM in columns.
#' @param spec target spectrogram (numeric matrix, frequency in rows, time in
#'   columns)
#' @inheritParams spectrogram
#' @keywords internal
specToMS = function(spec, samplingRate = NULL, step = NULL) {
  if ((is.null(colnames(spec)) & is.null(step)) |
      (is.null(rownames(spec)) & is.null(samplingRate))) {
    addNames = FALSE
    message(paste("If spec doesn't have rownames/colnames,",
                  "you have to specify STFT step and samplingRate,",
                  "otherwise AM and FM stamps can't be",
                  "added to the modulation spectrum"))
  } else {
    addNames = TRUE
  }

  # Center - see spec.fft function in "spectral" package
  spec_centered = spec * (-1)^(row(spec) + col(spec))  # checkerboard of ±1

  # 2D fft
  ms = fft(spec_centered, inverse = FALSE) / length(spec_centered)

  # Add labels
  if (addNames) {
    if (is.null(step)) step = diff(as.numeric(colnames(spec)))[1]
    max_am = 1000 / step / 2
    colnames(ms) = seq(-max_am, max_am, length.out = ncol(ms))   # AM
    nr = nrow(ms)
    max_fm = nr / (samplingRate / 1000)
    rownames(ms) = seq(-max_fm, max_fm, length.out = nr)     # FM
  }
  return(ms)
}


#' Modulation spectrum to spectrogram
#'
#' Internal soundgen function.
#'
#' Takes a complex MS and transforms it to a complex spectrogram with proper row
#' (frequency) and column (time) labels.
#' @return Returns a spectrogram - a numeric matrix of complex numbers of
#'   the same dimensions as ms.
#' @param ms target modulation spectrum (matrix of complex numbers)
#' @inheritParams spectrogram
#' @keywords internal
msToSpec = function(ms, samplingRate, windowLength = NULL, step = NULL) {
  if ((is.null(rownames(ms)) | is.null(colnames(ms))) &
      (is.null(windowLength) | is.null(step))) {
    addNames = FALSE
    message(paste("If ms doesn't have rownames/colnames,",
                  "you have to specify windowLength and step,",
                  "otherwise frequency and time stamps can't be",
                  "added to the spectrogram"))
  } else {
    addNames = TRUE
  }

  # Inverse FFT
  s1 = fft(ms, inverse = TRUE) / length(ms)

  # Undo centering
  s2 = s1 / (-1)^(row(s1) + col(s1))

  # Add rownames & colnames
  if (addNames) {
    step = 1000 / 2 / abs(as.numeric(rownames(ms)[1]))
    windowLength = abs(as.numeric(colnames(ms)[1])) * 2
    windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
    bin_width = samplingRate / 2 / windowLength_points
    rownames(s2) = seq(bin_width / 2,
                       samplingRate / 2 - bin_width / 2,
                       length.out = nrow(s2)) / 1000  # frequency stamp

    step_points = round(step / 1000 * samplingRate)
    myseq = (0:(ncol(s2) - 1)) * step_points + 1
    colnames(s2) = (myseq - 1 + windowLength_points / 2) *
      1000 / samplingRate
  }
  return(s2)
}

