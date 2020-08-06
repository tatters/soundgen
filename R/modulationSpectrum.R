#' Modulation spectrum
#'
#' Produces a modulation spectrum of waveform(s) or audio file(s), with temporal
#' modulation along the X axis (Hz) and spectral modulation (1/KHz) along the Y
#' axis. A good visual analogy is decomposing the spectrogram into a sum of
#' ripples of various frequencies and directions. Algorithm: prepare a
#' spectrogram, take its logarithm (if \code{logSpec = TRUE}), center, perform a
#' 2D Fourier transform (see also spectral::spec.fft()), take the upper half of
#' the resulting symmetric matrix, and raise it to \code{power}. The result is
#' returned as \code{$original}. Roughness is calculated as the proportion of
#' energy / amplitude of the modulation spectrum within \code{roughRange} of
#' temporal modulation frequencies. For plotting purposes, the modulation matrix
#' is smoothed with Gaussian blur (see \code{\link{gaussianSmooth2D}}) and
#' log-warped (if \code{logWarp} is a positive number). This processed
#' modulation spectrum is returned as \code{$processed}. If the audio is long
#' enough, multiple windows are analyzed, resulting in a vector of roughness
#' values. For multiple inputs, such as a list of waveforms or path to a folder
#' with audio files, the ensemble of modulation spectra is interpolated to the
#' same spectral and temporal resolution and averaged, resulting in a single
#' roughness value. This is different from the behavior of
#' \code{\link{modulationSpectrumFolder}}, which produces a separate modulation
#' spectrum per file, without averaging.
#'
#' @seealso \code{\link{modulationSpectrumFolder}} \code{\link{spectrogram}}
#'
#' @references \itemize{
#'   \item Singh, N. C., & Theunissen, F. E. (2003). Modulation spectra of
#'   natural sounds and ethological theories of auditory processing. The Journal
#'   of the Acoustical Society of America, 114(6), 3394-3411.
#' }
#' @param x folder, path to a wav/mp3 file, a numeric vector representing a
#'   waveform, or a list of numeric vectors
#' @param samplingRate sampling rate of x (only needed if x is a numeric vector,
#'   rather than an audio file). For a list of sounds, give either one
#'   samplingRate (the same for all) or as many values as there are input files
#' @param roughRange the range of temporal modulation frequencies that
#'   constitute the "roughness" zone, Hz
#' @inheritParams spectrogram
#' @param amRes target resolution of amplitude modulation, Hz. If \code{NULL},
#'   the entire sound is analyzed at once (or in chunks \code{maxDur} s long),
#'   resulting in a single roughness value. Otherwise, roughness is calculated
#'   per frame, each selected to contain enough STFT windows to calculate
#'   roughness with precision given by \code{amRes}
#' @param maxDur maximum allowed duration of a single sound, s (longer sounds
#'   are split, and their modulation spectra averaged)
#' @param logSpec if TRUE, the spectrogram is log-transformed prior to taking 2D
#'   FFT
#' @param power raise modulation spectrum to this power (eg power = 2 for ^2, or
#'   "power spectrum")
#' @param returnComplex if TRUE, returns a complex modulation spectrum (without
#'   normalization and warping)
#' @param aggregComplex if TRUE, aggregates complex MS from multiple inputs,
#'   otherwise returns the complex MS of the first input (recommended when
#'   filtering and inverting the MS of a single sound, e.g. with
#'   \code{\link{filterSoundByMS}})
#' @param plot if TRUE, plots the (averaged) modulation spectrum
#' @param savePath if a valid path is specified, a plot is saved in this folder
#'   (defaults to NA)
#' @param logWarp the base of log for warping the modulation spectrum (ie log2
#'   if logWarp = 2); set to NULL or NA if you don't want to log-warp
#' @param quantiles labeled contour values, \% (e.g., "50" marks regions that
#'   contain 50\% of the sum total of the entire modulation spectrum)
#' @param kernelSize the size of Gaussian kernel used for smoothing (1 = no
#'   smoothing)
#' @param kernelSD the SD of Gaussian kernel used for smoothing, relative to its
#' size
#' @param xlab,ylab,main,xlim,ylim graphical parameters
#' @param width,height,units,res parameters passed to
#'   \code{\link[grDevices]{png}} if the plot is saved
#' @param ... other graphical parameters passed on to \code{filled.contour.mod}
#'   and \code{\link[graphics]{contour}} (see \code{\link{spectrogram}})
#' @return Returns a list with four components:
#' \itemize{
#' \item \code{$original} modulation spectrum prior to blurring and log-warping,
#' but after squaring if \code{power = TRUE}, a matrix of nonnegative values.
#' Rownames are spectral modulation frequencies (cycles/KHz), and colnames are
#' temporal modulation frequencies (Hz).
#' \item \code{$processed} modulation spectrum after blurring and log-warping
#' \item \code{$roughness} proportion of energy / amplitude of the modulation
#' spectrum within \code{roughRange} of temporal modulation frequencies, \% - a
#' vector if amRes is numeric and the sound is long enough, a single number
#' otherwise
#' \item \code{$complex} untransformed complex modulation spectrum (returned
#' only if returnComplex = TRUE)
#' }
#' @export
#' @examples
#' # White noise
#' ms = modulationSpectrum(runif(16000), samplingRate = 16000,
#'   logSpec = FALSE, power = TRUE)
#'
#' # Harmonic sound
#' s = soundgen()
#' ms = modulationSpectrum(s, samplingRate = 16000)
#' ms$roughness  # roughness over time
#'
#' # A single roughness measure per sound
#' modulationSpectrum(s, samplingRate = 16000, amRes = NULL)$roughness
#'
#' # Embellish
#' ms = modulationSpectrum(s, samplingRate = 16000,
#'   xlab = 'Temporal modulation, Hz', ylab = 'Spectral modulation, 1/KHz',
#'   colorTheme = 'heat.colors', main = 'Modulation spectrum', lty = 3)
#'
#' \dontrun{
#' # Roughness contour of a long sound (starts tonal, then gets rough)
#' s_long = soundgen(sylLen = 4000, pitch = 400, plot = TRUE,
#'   amDep = c(0, 0, 50), jitterDep = c(0, 0, 2))
#' modulationSpectrum(s_long, 16000, plot = FALSE)$roughness
#'
#' # Input can also be a list of waveforms (numeric vectors)
#' ss = vector('list', 10)
#' for (i in 1:length(ss)) {
#'   ss[[i]] = soundgen(sylLen = runif(1, 100, 1000), temperature = .4,
#'     pitch = runif(3, 400, 600))
#' }
#' # lapply(ss, playme)
#' ms1 = modulationSpectrum(ss[[1]], samplingRate = 16000)  # the first sound
#' dim(ms1$original)
#' ms2 = modulationSpectrum(ss, samplingRate = 16000)  # all 10 sounds
#' dim(ms2$original)
#'
#' # Careful with complex MS of multiple inputs:
#' ms3 = modulationSpectrum(ss, samplingRate = 16000,
#'   returnComplex = TRUE, aggregComplex = FALSE)
#' dim(ms3$complex)  # complex MS of the first input only
#' ms4 = modulationSpectrum(ss, samplingRate = 16000,
#'   returnComplex = TRUE, aggregComplex = TRUE)
#' dim(ms4$complex)  # aggregated over inputs
#'
#' # As with spectrograms, there is a tradeoff in time-frequency resolution
#' s = soundgen(pitch = 500, amFreq = 50, amDep = 100, samplingRate = 44100)
#' # playme(s, samplingRate = 44100)
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 50, overlap = 0, amRes = NULL)  # poor temporal resolution
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 5, overlap = 80, amRes = NULL)  # poor frequency resolution
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 15, overlap = 80, amRes = NULL)  # a reasonable compromise
#'
#' # customize the plot
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 15, overlap = 80, amRes = NULL,
#'   kernelSize = 17,  # more smoothing
#'   xlim = c(-70, 70), ylim = c(0, 4),  # zoom in on the central region
#'   quantiles = c(.25, .5, .8),  # customize contour lines
#'   colorTheme = 'heat.colors',  # alternative palette
#'   power = 2)                   # ^2
#' # Note the peaks at FM = 2/KHz (from "pitch = 500") and AM = 50 Hz (from
#' # "amFreq = 50")
#'
#' # Input can be a wav/mp3 file
#' ms = modulationSpectrum('~/Downloads/temp/200_ut_fear-bungee_11.wav')
#'
#' # Input can be path to folder with audio files (average modulation spectrum)
#' ms = modulationSpectrum('~/Downloads/temp', kernelSize = 11)
#' # NB: longer files will be split into fragments <maxDur in length, and no
#' # roughness contours are extracted
#'
#' # A sound with ~3 syllables per second and only downsweeps in F0 contour
#' s = soundgen(nSyl = 8, sylLen = 200, pauseLen = 100, pitch = c(300, 200))
#' # playme(s)
#' ms = modulationSpectrum(s, samplingRate = 16000, maxDur = .5,
#'   xlim = c(-25, 25), colorTheme = 'seewave',
#'   power = 2)
#' # note the asymmetry b/c of downsweeps
#'
#' # "power = 2" returns squared modulation spectrum - note that this affects
#' # the roughness measure!
#' ms$roughness
#' # compare:
#' modulationSpectrum(s, samplingRate = 16000, maxDur = .5,
#'   xlim = c(-25, 25), colorTheme = 'seewave', logWarp = NULL,
#'   power = 1)$roughness  # much higher roughness
#'
#' # Plotting with or without log-warping the modulation spectrum:
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000,
#'   logWarp = NA, plot = TRUE)
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000,
#'   logWarp = 2, plot = TRUE)
#'
#' # logWarp and kernelSize have no effect on roughness
#' # because it is calculated before these transforms:
#' modulationSpectrum(s, samplingRate = 16000, logWarp = 5)$roughness
#' modulationSpectrum(s, samplingRate = 16000, logWarp = NA)$roughness
#' modulationSpectrum(s, samplingRate = 16000, kernelSize = 17)$roughness
#'
#' # Log-transform the spectrogram prior to 2D FFT (affects roughness):
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000, logSpec = FALSE)
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000, logSpec = TRUE)
#'
#' # Complex modulation spectrum with phase preserved
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000,
#'                         returnComplex = TRUE)
#' image(t(log(abs(ms$complex))))
#' }
modulationSpectrum = function(
  x,
  samplingRate = NULL,
  amRes = 5,
  maxDur = 5,
  logSpec = FALSE,
  windowLength = 25,
  step = NULL,
  overlap = 80,
  wn = 'hanning',
  zp = 0,
  power = 1,
  roughRange = c(30, 150),
  returnComplex = FALSE,
  aggregComplex = TRUE,
  plot = TRUE,
  savePath = NA,
  logWarp = NA,
  quantiles = c(.5, .8, .9),
  kernelSize = 5,
  kernelSD = .5,
  colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
  xlab = 'Hz',
  ylab = '1/KHz',
  main = NULL,
  xlim = NULL,
  ylim = NULL,
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  ...
) {
  # determine the type of input
  if (is.character(x)) {
    # assume that this is a directory containing sound files
    myInput = as.list(list.files(x, pattern = "*.wav|.mp3", full.names = TRUE))
    if (!length(myInput) > 0) {
      # assume that it is a single sound file
      myInput = as.list(x)
      plotname = tail(unlist(strsplit(x, '/')), n = 1)
      plotname = ifelse(
        !missing(main) & !is.null(main),
        main,
        substring(plotname, first = 1,
                  last = (nchar(plotname) - 4))
      )
    } else {
      plotname = ''
    }
    samplingRate = rep(NA, length(myInput))
  } else if (is.numeric(x)) {
    # assume that it is an actual waveform vector
    myInput = list(x)
    if (!is.numeric(samplingRate)) {
      stop('Please specify samplingRate')
    }
    plotname = ifelse(!missing(main) & !is.null(main), main, '')
  } else if (is.list(x)) {
    # assume that it is a list of waveforms
    myInput = x
    if (!is.numeric(samplingRate)) {
      stop('Please specify samplingRate')
    } else {
      if (length(samplingRate) > 1) {
        if (length(samplingRate) != length(myInput)) {
          stop('Please specify samplingRate of length 1 or the same length as input')
        }
      } else {
        samplingRate = rep(samplingRate, length(myInput))
      }
    }
    plotname = ifelse(!missing(main) & !is.null(main), main, '')
  } else {
    stop('Input not recognized')
  }

  # load input
  duration = rep(NA, length(myInput))
  for (i in 1:length(myInput)) {
    if (is.character(myInput[[i]])) {
      sound1 = as.character(myInput[[i]])
      ext = substr(sound1, (nchar(sound1) - 2), nchar(sound1))
      if (ext %in% c('wav', 'WAV')) {
        temp = tuneR::readWave(myInput[[i]])
      } else if (ext %in% c('mp3', 'MP3')) {
        temp = tuneR::readMP3(myInput[[i]])
      } else {
        stop('Input not recognized')
      }
      myInput[[i]] = as.numeric(temp@left)
      samplingRate[i] = temp@samp.rate
      duration[i] = length(temp@left) / temp@samp.rate
    } else if (is.numeric(myInput[[i]])) {
      duration[i] = length(myInput[[i]]) / samplingRate[[i]]
    } else {
      stop('Input not recognized')
    }
  }

  # Re-set windowLength, step, and overlap so as to ensure that
  # windowLength_points and step_points are not fractions
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  step_points = round(step / 1000 * samplingRate)
  step = step_points[1] / samplingRate[1] * 1000
  windowLength_points = round(windowLength / 1000 * samplingRate)
  windowLength = windowLength_points[1] / samplingRate[1] * 1000
  overlap = 100 * (1 - step_points[1] / windowLength_points[1])
  sr = max(samplingRate)  # again, in case not the same

  max_am = 1000 / step / 2
  if (max_am < roughRange[1]) {
    warning(paste(
      'roughRange outside the analyzed range of temporal modulation frequencies;',
      'increase overlap / decrease step to improve temporal resolution,',
      'or else look for roughness in a lower range'))
  }
  if (is.numeric(amRes)) {
    nFrames = ceiling(max_am / amRes * 2)
  } else {
    nFrames = NULL
  }

  if (length(myInput) == 1 & is.numeric(nFrames)) {
    # one input, we want roughness contour
    roughContour = TRUE

    # split the input sound into chunks nFrames long
    chunk_ms = windowLength + step * (nFrames - 1)
    l = chunk_ms * samplingRate / 1000
    fullSound = myInput[[1]]
    l_full = length(fullSound)
    if (l > l_full) {
      message(paste('The sound is too short to be analyzed with amRes =', amRes,
                    'Hz. Roughness is probably not measured correctly'))
      l = l_full
    }
    myseq = 1 + l * (0:(floor(l_full / l) - 1))
    myInput = vector('list', length(myseq))
    n = length(myInput)
    for (i in 1:n) {
      idx = myseq[i]:(myseq[i] + l - 1)
      myInput[[i]] = fullSound[idx]
    }
    windowLength_points = rep(windowLength_points, n)
    step_points = rep(step_points, n)
    samplingRate = rep(samplingRate, n)
  } else {
    # multiple inputs to aggregate and/or we want a single roughness value
    roughContour = FALSE

    # split sounds that exceed maxDur
    toSplit = which(duration > maxDur)
    if (length(toSplit) > 0) {
      splitInto = ceiling(duration / maxDur)
      # so, for ex., if 2.1 times longer than maxDur, split into three
      for (i in toSplit) {
        idx = floor(seq(1, length(myInput[[i]]), length.out = splitInto[i] + 1))
        for (j in 2:splitInto[i]) {
          # append fragments 2-end to myInput
          start = idx[j] + 1
          end = idx[j + 1]
          myInput[[length(myInput) + 1]] = myInput[[i]][start:end]
          samplingRate = c(samplingRate, samplingRate[i])
          windowLength_points = c(windowLength_points, windowLength_points[length(windowLength_points)])
          step_points = c(step_points, step_points[length(step_points)])
        }
        # the first fragment replaces the old long sound in myInput
        myInput[[i]] = myInput[[i]][1:idx[2]]
      }
    }
  }

  # extract modulation spectrum per sound
  out = vector('list', length(myInput))
  if (returnComplex) {
    out_complex = out
  } else {
    out_aggreg_complex = NULL
  }
  for (i in 1:length(myInput)) {
    # Calling stdft is ~80 times faster than going through spectrogram (!)
    step_seq = seq(1, length(myInput[[i]]) + 1 - windowLength_points[i], step_points[i])
    # print(length(step_seq))
    if (length(step_seq) < 3) next
    s1 = seewave::stdft(
      wave = as.matrix(myInput[[i]]),
      wn = wn,
      wl = windowLength_points[i],  # for multiple inputs, samplingRate, wl etc can vary
      f = samplingRate[i],
      zp = zp,
      step = step_seq,
      scale = FALSE,
      norm = FALSE,
      complex = FALSE
    )
    # image(t(log(s1))); s1[1:3, 1:3]; dim(s1); range(s1)

    # log-transform amplitudes
    if (logSpec) {
      positives = which(s1 > 0)
      nonpositives = which(s1 <= 0)
      s1[positives] = log(s1[positives])
      if (length(positives) > 0 & length(nonpositives) > 0) {
        s1[nonpositives] = min(s1[positives])
      }
      s1 = s1 - min(s1) + 1e-16  # positive
      # image(t(log(s1)))
    }

    # 2D fft
    ms_complex = specToMS(s1, windowLength = windowLength, step = step)
    ms = abs(ms_complex)
    # image(as.numeric(colnames(ms)), as.numeric(rownames(ms)), t(log(ms)))
    symAxis = floor(nrow(ms) / 2) + 1
    # ms[(symAxis - 2) : (symAxis + 2), 1:2]
    ms_half = ms[symAxis:nrow(ms),, drop = FALSE]  # take only the upper half (always even)

    # power
    if (is.numeric(power) && power != 1) ms_half = ms_half ^ power

    # normalize
    if (any(s1 != 0)) {
      ms_half = ms_half - min(ms_half)
      ms_half = ms_half / max(ms_half)
    }
    # image(x = as.numeric(colnames(ms_half)), z = t(log(ms_half)))
    out[[i]] = ms_half

    if (returnComplex) {
      out_complex[[i]] = ms_complex
    }
  }

  keep = which(unlist(lapply(out, function(x) !is.null(x))))
  out = out[keep]
  if (length(out) < 1) {
    warning('The sound is too short or windowLength too long. Need at least 3 frames')
    return(list('original' = NA,
                'processed' = NA,
                'complex' = NA,
                'roughness' = NA))
  }

  # average modulation spectra across all sounds
  max_rows = max(unlist(lapply(out, nrow)))
  # normally same samplingRate, but in case not, upsample frequency resolution
  # typical ncol (depends on sound dur)
  typicalCols = round(median(unlist(lapply(out, ncol))))
  out1 = lapply(out, function(x) interpolMatrix(x, nr = max_rows, nc = typicalCols))
  out_aggreg = Reduce('+', out1) / length(myInput)
  X = as.numeric(colnames(out_aggreg))
  Y = as.numeric(rownames(out_aggreg))
  # image(X, Y, t(log(out_aggreg)))

  # prepare a separate summary of the complex ms
  if (returnComplex) {
    if (aggregComplex) {
      out1_complex = lapply(out_complex, function(x) {
        interpolMatrix(x, nr = max_rows * 2, nc = typicalCols)
      })
      out_aggreg_complex = Reduce('+', out1_complex) / length(myInput)
      colnames(out_aggreg_complex) = X
      rownames(out_aggreg_complex) = c(-rev(Y), Y)
      # image(t(log(abs(out_aggreg_complex))))
    } else {
      out_aggreg_complex = out_complex[[1]]
    }
  }

  # extract a measure of roughness
  if (roughContour) {
    roughness = unlist(lapply(out, function(x)
      getRough(x, roughRange, colNames = as.numeric(colnames(out[[1]])))))
  } else {
    roughness = getRough(out_aggreg, roughRange, colNames = X)
  }

  # log-transform the axes (or, actually, warps the matrix itself)
  if (is.numeric(logWarp)) {
    neg_col = which(X < 0)
    zero_col = which(X == 0)
    pos_col = which(X > 0)
    m_left = logMatrix(out_aggreg[, rev(neg_col)], base = logWarp)
    # NB: flip the left half!
    m_right = logMatrix(out_aggreg[, pos_col], base = logWarp)
    out_transf = cbind(m_left[, ncol(m_left):1], out_aggreg[, zero_col, drop = FALSE], m_right)
    X1 = as.numeric(colnames(out_transf))  # warped by logMatrix
    Y1 = as.numeric(rownames(out_transf))  # warped by logMatrix
  } else {
    out_transf = out_aggreg
  }
  # image(out_transf)

  # smoothing / blurring
  out_transf = gaussianSmooth2D(out_transf,
                                kernelSize = kernelSize,
                                kernelSD = kernelSD)

  # plot
  if (is.character(savePath)) {
    # make sure the last character of savePath is "/"
    last_char = substr(savePath, nchar(savePath), nchar(savePath))
    if(last_char != '/') savePath = paste0(savePath, '/')
    plot = TRUE
    f = ifelse(plotname == '',
               'sound',
               plotname)
    png(filename = paste0(savePath, f, ".png"),
        width = width, height = height, units = units, res = res)
  }

  if (plot) {
    color.palette = switchColorTheme(colorTheme)
    if (is.null(xlim)) xlim = c(X[1], -X[1])
    if (is.null(ylim)) ylim = range(Y)
    if (is.numeric(logWarp)) {
      filled.contour.mod(
        x = X, y = Y, z = t(out_transf),
        levels = seq(0, 1, length = 30),
        color.palette = color.palette,
        xlab = xlab, ylab = ylab,
        bty = 'n', axisX = FALSE, axisY = FALSE,
        main = plotname,
        xlim = xlim, ylim = ylim,
        ...
      )
      # add manually labeled x-axis
      max_tm = log(-X[1], logWarp)
      xl = unique(round(logWarp ^ pretty(c(1, max_tm), n = 4)))
      xl = c(-rev(xl), 0, xl)
      xl = xl[abs(xl) < -X1[1]]
      pos = apply(matrix(xl), 1, function(x) which.min(abs(x - X1)))
      axis(side = 1,
           at  = X[pos],
           labels = xl)

      # add manually labeled y-axis
      yseq = seq(1, length(Y), length.out = 7)
      digits = 0
      ry = round(Y1[yseq], digits = digits)
      while (length(ry) > length(unique(ry))) {
        digits = digits + 1
        ry = round(Y1[yseq], digits = digits)
      }
      axis(side = 2,
           at  = round(Y[yseq]),
           labels = ry)
      # max_fm = log(Y[length(Y)], logWarp)
      # yl = logWarp ^ pretty(c(0, max_fm))
      # yl = unique(round(c(0, yl[yl < max(Y)])))
      # yl1 = yl
      # for (i in 1:length(yl)) {
      #   yl1[i] = Y[which.min(abs(Y1 - yl[i]))]
      # }
      # axis(side = 2,
      #      at  = yl1,
      #      labels = yl)
    } else {
      filled.contour.mod(
        x = X, y = Y, z = t(out_transf),
        levels = seq(0, 1, length = 30),
        color.palette = color.palette,
        xlab = xlab, ylab = ylab,
        bty = 'n', xlim = xlim, ylim = ylim, ...
      )
    }
    abline(v = 0, lty = 3)
    # qntls = quantile(out_aggreg, probs = quantiles)  # could try HDI instead
    qntls = pDistr(as.numeric(out_transf), quantiles = quantiles)
    par(new = TRUE)
    contour(x = X, y = Y, z = t(out_transf),
            levels = qntls, labels = quantiles * 100,
            xaxs = 'i', yaxs = 'i',
            axes = FALSE, frame.plot = FALSE,
            main = plotname, xlim = xlim, ylim = ylim, ...)
    par(new = FALSE)
    if (is.character(savePath)) dev.off()
  }

  invisible(list('original' = out_aggreg,
                 'processed' = out_transf,
                 'complex' = out_aggreg_complex,
                 'roughness' = roughness))
}


#' Modulation spectrum per folder
#'
#' Extracts modulation spectra of all wav/mp3 files in a folder - separately for
#' each file, without averaging. Good for saving plots of the modulation spectra
#' and/or measuring the roughness of multiple files. See
#' \code{\link{modulationSpectrum}} for further details.
#'
#' @seealso \code{\link{modulationSpectrum}}
#'
#' @inheritParams analyzeFolder
#' @inheritParams modulationSpectrum
#' @inheritParams spectrogram
#' @return If \code{summaryFun} is defined, returns a dataframe with just
#'   roughness per audio file, otherwise returns a list with the actual
#'   modulation spectra.
#' @export
#' @examples
#' \dontrun{
#' ms = modulationSpectrumFolder('~/Downloads/temp', savePlots = TRUE,
#'                               kernelSize = 15)
#' ms
#' }
modulationSpectrumFolder = function(
  myfolder,
  summaryFun = c('mean', 'sd'),
  htmlPlots = TRUE,
  verbose = TRUE,
  amRes = 5,
  maxDur = 5,
  logSpec = FALSE,
  windowLength = 25,
  step = NULL,
  overlap = 80,
  wn = 'hamming',
  zp = 0,
  power = 1,
  roughRange = c(30, 150),
  plot = FALSE,
  savePlots = FALSE,
  logWarp = NA,
  quantiles = c(.5, .8, .9),
  kernelSize = 5,
  kernelSD = .5,
  colorTheme = c('bw', 'seewave', '...')[1],
  xlab = 'Hz',
  ylab = '1/KHz',
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  ...
) {
  time_start = proc.time()  # timing
  filenames = list.files(myfolder, pattern = "*.wav|.mp3|.WAV|.MP3", full.names = TRUE)
  filenames_base = basename(filenames)
  if (length(filenames) < 1) {
    stop(paste('No wav/mp3 files found in', myfolder))
  }
  # in order to provide more accurate estimates of time to completion,
  # check the size of all files in the target folder
  filesizes = file.info(filenames)$size

  # match par-s
  myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'myfolder' , 'htmlPlots', 'verbose', 'savePlots', 'summaryFun')]
  # exclude ...
  myPars = myPars[1:(length(myPars)-1)]
  if (savePlots) myPars$savePath = myfolder
  if (is.null(amRes)) summaryFun = 'identity'
  # nothing to summarize - 1 value per file

  result = list()
  for (i in 1:length(filenames)) {
    result[[i]] = do.call(modulationSpectrum, c(filenames[i], myPars, ...))
    if (verbose) {
      reportTime(i = i, nIter = length(filenames),
                 time_start = time_start, jobs = filesizes)
    }
  }

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    result_sum = lapply(result, function(x) {
      summarizeAnalyze(data.frame(roughness = x$roughness),
                       summaryFun = summaryFun,
                       var_noSummary = NULL)
    })
    output = cbind(data.frame(file = filenames_base),
                   do.call(rbind, result_sum))
    if (colnames(output)[2] == 'roughness_identity')
      colnames(output)[2] = 'roughness'
  } else {
    output = result
    names(output) = filenames_base
  }

  if (htmlPlots & savePlots) {
    htmlPlots(myfolder, myfiles = filenames, width = paste0(width, units))
  }

  invisible(output)
}


#' Get roughness
#'
#' Internal soundgen function
#'
#' Helper function for calculating roughness - the proportion of energy /
#' amplitude in the roughness range
#' @param m numeric matrix of non-negative values with colnames giving temporal
#'   modulation frequency
#' @param roughRange range of temporal modulation frequencies corresponding to
#'   roughness
#' @return Returns roughness in percent.
#' @keywords internal
#' @examples
#' m = modulationSpectrum(soundgen(), samplingRate = 16000)$original
#' soundgen:::getRough(m, roughRange = c(30, Inf))
getRough = function(m, roughRange, colNames = NULL) {
  if (is.null(colNames)) colNames = abs(as.numeric(colnames(m)))
  rough_cols = which(colNames > roughRange[1] &
                       colNames < roughRange[2])
  if (length(rough_cols) > 0) {
    roughness = sum(m[, rough_cols]) / sum(m) * 100
  } else {
    roughness = 0
  }
  return(roughness)
}
