#' Filter sound by modulation spectrum
#'
#' Manipulates the modulation spectrum (MS) of a sound so as to remove certain
#' frequencies of amplitude modulation (AM) and frequency modulation (FM).
#' Algorithm: produces a modulation spectrum with
#' \code{\link{modulationSpectrum}}, modifies it with \code{\link{filterMS}},
#' converts the modified MS to a spectrogram with \code{\link{msToSpec}}, and
#' finally inverts the spectrogram with \code{\link{invertSpectrogram}}, thus
#' producing a sound with (approximately) the desired characteristics of the MS.
#' Note that the last step of inverting the spectrogram introduces some noise,
#' so the resulting MS is not precisely the same as the intermediate filtered
#' version. In practice this means that some residual energy will still be
#' present in the filtered-out frequency range (see examples).
#'
#' @seealso \code{\link{invertSpectrogram}} \code{\link{filterMS}}
#'
#' @return Returns the filtered audio as a numeric vector normalized to [-1, 1]
#'   with the same sampling rate as input.
#' @inheritParams modulationSpectrum
#' @inheritParams filterMS
#' @inheritParams invertSpectrogram
#' @param play if TRUE, plays back the output
#' @param plot if TRUE, produces a triple plot: original MS, filtered MS, and
#'   the MS of the output sound
#' @export
#' @examples
#' # Create a sound to be filtered
#' samplingRate = 16000
#' s = soundgen(sylLen = 500, pitch = rnorm(n = 20, mean = 200, sd = 25),
#'   amFreq = 25, amDep = 50, samplingRate = samplingRate,
#'   plot = TRUE, osc = TRUE)
#' # playme(s, samplingRate)
#'
#' # Filter
#' s_filt = filterSoundByMS(s, samplingRate = samplingRate,
#'   amCond = 'abs(am) > 15', fmCond = 'abs(fm) > 5',
#'   action = 'remove', nIter = 15)
#' # playme(s_filt, samplingRate)
#'
#' # add exs with an audio file as an input, etc
#' \dontrun{
#' # You can also use manual filters w/o calling filterSoundByMS
#'
#' #' # Check that the spectrogram can be successfully inverted
#' s_rev = invertSpectrogram(spec, samplingRate = samplingRate,
#'   windowLength = 25, overlap = 80, wn = 'hamming', play = FALSE)
#' # playme(s_rev, samplingRate)
#' spectrogram(s_rev, samplingRate, osc = TRUE)
#'
#' }
filterSoundByMS = function(
  x,
  samplingRate = NULL,
  logSpec = FALSE,
  windowLength = 25,
  step = NULL,
  overlap = 80,
  wn = 'hamming',
  zp = 0,
  amCond = NULL,
  fmCond = NULL,
  jointCond = NULL,
  action = c('remove', 'preserve')[1],
  initialPhase = c('zero', 'random', 'spsi')[3],
  nIter = 50,
  play = FALSE,
  plot = TRUE,
  savePath = NA) {

  # Get a modulation spectrum
  ms = modulationSpectrum(
    x,
    samplingRate = samplingRate,
    windowLength = windowLength,
    step = step, overlap = overlap, wn = wn,
    maxDur = Inf, logSpec = logSpec,
    power = NA, returnComplex = TRUE,
    plot = FALSE
  )$complex
  # image(x = as.numeric(colnames(ms)), y = as.numeric(rownames(ms)), z = t(log(abs(ms))))

  # Filter as needed
  ms_filt = filterMS(ms, amCond = amCond, fmCond = fmCond,
                     jointCond = jointCond,
                     action = 'remove', plot = FALSE)

  # Convert back to a spectrogram
  spec_filt = msToSpec(ms_filt, windowLength = windowLength, step = step)
  # image(x = as.numeric(colnames(spec_filt)), y = as.numeric(rownames(spec_filt)), z = t(log(abs(spec_filt))))

  # Invert the spectrogram
  s_new = invertSpectrogram(
    abs(spec_filt), samplingRate = samplingRate,
    windowLength = windowLength, wn = wn,
    overlap = overlap, step = step,
    specType = ifelse(logSpec, 'log', 'abs'),
    initialPhase = initialPhase,
    nIter = nIter,
    normalize = TRUE,
    play = FALSE,
    verbose = FALSE,
    plotError = FALSE
  )

  if (play) playme(s_filt, samplingRate)

  if (plot) {
    # Get an MS of the new sound
    ms_actual = modulationSpectrum(
      s_new,
      samplingRate = samplingRate,
      windowLength = windowLength,
      step = step, overlap = overlap, wn = wn,
      maxDur = Inf, logSpec = logSpec,
      power = NA, returnComplex = TRUE,
      plot = FALSE
    )$complex

    par(mfrow = c(1, 3))
    image(x = as.numeric(colnames(ms)),
          y = as.numeric(rownames(ms)),
          z = t(log(abs(ms))),
          main = 'Original MS',
          xlab = '', ylab = 'FM, cycle/kHz')
    image(x = as.numeric(colnames(ms_filt)),
          y = as.numeric(rownames(ms_filt)),
          z = t(log(abs(ms_filt))),
          main = 'Filtered MS',
          xlab = 'AM, Hz', ylab = '')
    image(x = as.numeric(colnames(ms_actual)),
          y = as.numeric(rownames(ms_actual)),
          z = t(log(abs(ms_actual))),
          main = 'Achieved MS',
          xlab = '', ylab = '')
    par(mfrow = c(1, 1))
  }
  return(s_new)
}

#' Filter modulation spectrum
#'
#' Filters a modulation spectrum by removing a certain range of amplitude
#' modulation (AM) and frequency modulation (FM) frequencies.
#' @param ms a modulation spectrum as returned by
#'   \code{\link{modulationSpectrum}} - a matrix of real or complex values, AM
#'   in columns, FM in rows
#' @param amCond,fmCond character strings with valid conditions on AM or FM (see
#'   examples)
#' @param jointCond character string with a valid joint condition on AM and FM
#' @param action should the defined AM-FM region be removed ('remove') or
#'   preserved, while everything else is removed ('preserve')?
#' @param plot if TRUE, plots the filtered modulation spectrum
#' @return Returns the filtered modulation spectrum - a matrix of the original
#'   dimensions, real or complex.
#' @export
#' @examples
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000,
#'                         returnComplex = TRUE)$complex
#' # Remove all AM over 25 Hz
#' ms_filt = filterMS(ms, amCond = 'abs(am) > 25')
#'
#' # amCond and fmCond are OR-conditions
#' filterMS(ms, amCond = 'abs(am) > 15', fmCond = 'abs(fm) > 5', action = 'remove')
#' filterMS(ms, amCond = 'abs(am) > 15', fmCond = 'abs(fm) > 5', action = 'preserve')
#' filterMS(ms, amCond = 'abs(am) > 10 & abs(am) < 25', action = 'remove')
#'
#' # jointCond is an AND-condition
#' filterMS(ms, jointCond = 'am * fm < 5', action = 'remove')
#' filterMS(ms, jointCond = 'am^2 + fm^2 < 100', action = 'preserve')
#'
#' # So:
#' filterMS(ms, jointCond = 'abs(am) > 5 | abs(fm) < 5')
#' # ...is the same as:
#' filterMS(ms, amCond = 'abs(am) > 5', fmCond = 'abs(fm) < 5')
#'
#' \dontrun{
#' }
filterMS = function(ms,
                    amCond = NULL,
                    fmCond = NULL,
                    jointCond = NULL,
                    action = c('remove', 'preserve')[1],
                    plot = TRUE) {
  nr = nrow(ms)
  nc = ncol(ms)
  am = as.numeric(colnames(ms))
  fm = as.numeric(rownames(ms))

  # Set up an empty filter matrix
  if (action == 'remove') {
    filter = matrix(1, nrow = nr, ncol = nc)
  } else if (action == 'preserve') {
    filter = matrix(0, nrow = nr, ncol = nc)
  }

  # Calculate the affected region
  if (is.character(jointCond)) {  # use only jointCond
    joint_cond = gsub('am', 'am[j]', jointCond)
    joint_cond = gsub('fm', 'fm[i]', joint_cond)
    affectedRegion = matrix(FALSE, nrow = nr, ncol = nc)
    for (i in 1:nr) {
      for (j in 1:nc) {
        affectedRegion[i, j] = try(eval(parse(text = joint_cond)), silent = TRUE)
        if (class(affectedRegion[i, j])[1] == 'try-error') {
          stop('jointCond is invalid - see examples')
        }
      }
    }
    if (action == 'remove') {
      filter[which(affectedRegion == TRUE)] = 0
    } else if (action == 'preserve') {
      filter[which(affectedRegion == TRUE)] = 1
    }
  } else {  # use only separate conditions for am & fm
    if (is.character(amCond)) {
      am_cond = paste0('which(', amCond, ')')
      idx_col = try(eval(parse(text = am_cond)), silent = TRUE)
      if (class(idx_col)[1] == 'try-error') {
        stop('amCond must be a valid expression to pass to which() - see examples')
      }
    } else {
      idx_col = logical(0)
    }

    if (is.character(fmCond)) {
      fm_cond = paste0('which(', fmCond, ')')
      idx_row = try(eval(parse(text = fm_cond)), silent = TRUE)
      if (class(idx_row)[1] == 'try-error') {
        stop('fmCond must be a valid expression to pass to which() - see examples')
      }
    } else {
      idx_row = logical(0)
    }

    if (action == 'remove') {
      filter[idx_row, ] = 0
      filter[, idx_col] = 0
    } else if (action == 'preserve') {
      filter[idx_row, ] = 1
      filter[, idx_col] = 1
    }
  }

  # Multiply the original ms by the prepared filter
  out = ms * filter

  if (plot) {
    if(is.complex(out[1, 1])) {
      out_plot = abs(out)
    } else (
      out_plot = out
    )
    image(x = as.numeric(colnames(out_plot)),
          y = as.numeric(rownames(out_plot)),
          z = t(log(out_plot)))
  }
  invisible(out)
}


#' Spectrogram to modulation spectrum
#'
#' Takes a spectrogram (either complex or magnitude) and returns a MS with
#' proper row and column labels.
#' @return Returns a MS - matrix of complex values of the same dimension as
#'   spec, with AM in rows and FM in columns.
#' @param spec target spectrogram (numeric matrix, frequency in rows, time in
#'   columns)
#' @inheritParams spectrogram
#' @export
#' @examples
#' s = soundgen(sylLen = 500, amFreq = 25, amDep = 50,
#'              pitch = 250, samplingRate = 16000)
#' spec = spectrogram(s, samplingRate = 16000, windowLength = 25, step = 5)
#' ms = specToMS(spec)
#' image(x = as.numeric(colnames(ms)), y = as.numeric(rownames(ms)),
#'       z = t(log(abs(ms))), xlab = 'Amplitude modulation, Hz',
#'       ylab = 'Frequency modulation, cycles/kHz')
specToMS = function(spec, windowLength = NULL, step = NULL) {
  if ((is.null(colnames(spec)) & is.null(step)) |
      (is.null(rownames(spec)) & is.null(windowLength))) {
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
    if (is.null(windowLength)) {
      samplingRate = (max(abs(as.numeric(rownames(spec)))) +  # middle of top bin
                        min(abs(as.numeric(rownames(spec))))) *  # bin/2
        1000 * 2
      windowLength = nr * 2 / (samplingRate / 1000)
    }
    max_fm = windowLength / 2
    rownames(ms) = seq(-max_fm, max_fm, length.out = nr)     # FM
  }
  return(ms)
}


#' Modulation spectrum to spectrogram
#'
#' Takes a complex MS and transforms it to a complex spectrogram with proper row
#' (frequency) and column (time) labels.
#' @return Returns a spectrogram - a numeric matrix of complex numbers of
#'   the same dimensions as ms.
#' @param ms target modulation spectrum (matrix of complex numbers)
#' @inheritParams spectrogram
#' @export
#' @examples
#' s = soundgen(sylLen = 500, amFreq = 25, amDep = 50,
#'              pitch = 250, samplingRate = 16000)
#' spec = spectrogram(s, samplingRate = 16000, windowLength = 25, step = 5)
#' ms = specToMS(spec)
#' image(x = as.numeric(colnames(ms)), y = as.numeric(rownames(ms)),
#'       z = t(log(abs(ms))), xlab = 'Amplitude modulation, Hz',
#'       ylab = 'Frequency modulation, cycles/kHz')
#' spec_new = msToSpec(ms)
#' image(x = as.numeric(colnames(spec_new)), y = as.numeric(rownames(spec_new)),
#'       z = t(log(abs(spec_new))), xlab = 'Time, ms',
#'       ylab = 'Frequency, kHz')
msToSpec = function(ms, windowLength = NULL, step = NULL) {
  addNames = TRUE
  if ((is.null(colnames(ms)) & is.null(step)) |
      (is.null(rownames(ms)) & is.null(windowLength))) {
    addNames = FALSE
    message(paste("If ms doesn't have rownames/colnames,",
                  "you have to specify windowLength and step,",
                  "otherwise frequency and time stamps can't be",
                  "added to the spectrogram"))
  }

  # Inverse FFT
  s1 = fft(ms, inverse = TRUE) / length(ms)

  # Undo centering
  s2 = s1 / (-1)^(row(s1) + col(s1))

  # Add rownames & colnames
  if (addNames) {
    if (is.null(step)) {
      max_am = abs(as.numeric(colnames(ms)[1]))
      step = 1000 / 2 / max_am
    }
    if (is.null(windowLength)) {
      max_fm = max(abs(as.numeric(rownames(ms))))
      windowLength = max_fm * 2
    }
    # From the def in spectrogram():
    # windowLength_points = windowLength * samplingRate / 1000
    # Y = seq(bin_width / 2,
    #   samplingRate / 2 - bin_width / 2,
    #   length.out = nrow(s2)) / 1000
    # So:
    # bin_width = samplingRate / 2 / windowLength_points =
    # = samplingRate / 2 / windowLength / samplingRate * 1000 =
    # = 1 / 2 / windowLength * 1000 = 1000 / windowLength / 2
    bin_width = 1000 / windowLength / 2
    windowLength_points = nrow(s2) * 2
    samplingRate = windowLength_points / windowLength * 1000

    # frequency stamps
    rownames(s2) = seq(bin_width / 2,
                       samplingRate / 2 - bin_width / 2,
                       length.out = nrow(s2)) / 1000
    # time stamps
    colnames(s2) = windowLength / 2 + (0:(ncol(s2) - 1)) * step
  }
  return(s2)
}

