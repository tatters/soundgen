#' Modulation spectrum
#'
#' Produces a modulation spectrum. Bla-bla-bla...
#'
#' @param x folder, path to a wav/mp3 file, a numeric vector representing a
#'   waveform, or a list of numeric vectors
#' @inheritParams spectrogram
#' @param maxDur maximum allowed duration of a single sound, s (longer sounds
#'   are split)
#' @param power if TRUE, returns power modulation spectrum (^2)
#' @param plot if TRUE, plots the modulation spectrum
#' @param quantiles quantiles labeled as separate contours on the plot
#' @param kernelSize the size of Gaussian kernel used for smoothing
#' @param kernelSD the SD of Gaussian kernel used for smoothing, relative to its
#'   size
#' @param ... other graphical parameters passed on to
#'   \code{seewave::filled.contour.modif2} and \code{contour}
#' @references \itemize{
#'   \item Singh, N. C., & Theunissen, F. E. (2003). Modulation spectra of
#'   natural sounds and ethological theories of auditory processing. The Journal
#'   of the Acoustical Society of America, 114(6), 3394-3411.
#'   }
#'
#' @examples
#' ss = vector('list', 10)
#' for (i in 1:length(ss)) {
#'   ss[[i]] = soundgen(sylLen = runif(1, 100, 1000), temperature = .4, pitch = runif(3, 400, 600))
#' }
#' # lapply(ss, playme)
#' ms = modulationSpectrum(ss[[1]], samplingRate = 16000)
#' ms = modulationSpectrum(ss, samplingRate = 16000)
#'
#' ms = modulationSpectrum('~/Downloads/temp/200_ut_fear-bungee_11.wav', kernelSize = 17)
#' ms = modulationSpectrum('~/Downloads/temp/200_ut_fear-bungee_11.wav', kernelSize = 17, xlim = c(-20, 20), ylim = c(0, 4), quantiles = c(.25, .5, .8), power = TRUE)
#'
#' ms = modulationSpectrum('~/Downloads/temp/', kernelSize = 17, power = TRUE)
modulationSpectrum = function(x,
                              samplingRate = NULL,
                              maxDur = Inf,
                              dynamicRange = 80,
                              windowLength = 25,
                              step = NULL,
                              overlap = 75,
                              wn = 'gaussian',
                              zp = 0,
                              power = FALSE,
                              plot = TRUE,
                              quantiles = c(.5, .8, .9),
                              kernelSize = 5,
                              kernelSD = .5,
                              ...) {
  # determine the type of input
  if (is.character(x)) {
    # assume that this is a directory containing sound files
    myInput = as.list(list.files(x, pattern = "*.wav|.mp3", full.names = TRUE))
    if (!length(myInput) > 0) {
      # assume that it is a single sound file
      myInput = as.list(x)
    }
    samplingRate = rep(NA, length(myInput))
  } else if (is.numeric(x)) {
    # assume that it is an actual waveform vector
    myInput = list(x)
    if (!is.numeric(samplingRate)) {
      stop('Please specify samplingRate')
    }
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
  } else {
    stop('Input not recognized')
  }
  if (is.null(step)) step = windowLength * (1 - overlap / 100)

  # load input
  duration = rep(NA, length(myInput))
  for (i in 1:length(myInput)) {
    if (is.character(myInput[[i]])) {
      sound1 = as.character(myInput[[i]])
      ext = substr(sound1, (nchar(sound1) - 2), nchar(sound1))
      if (ext %in% c('wav', 'WAV')) {
        temp = tuneR::readWave(myInput[[i]])
      } else if (ext %in% c('mp3', 'MP3')) {
        temp = tuneR::readMP3(myInput[[i]])@samp.rate
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
      }
      # the first fragment replaces the old long sound in myInput
      myInput[[i]] = myInput[[i]][1:idx[2]]
    }
  }

  # extract modulation spectrum per sound
  out = vector('list', length(myInput))
  for (i in 1:length(myInput)) {
    s1 = spectrogram(myInput[[i]],
                     samplingRate = samplingRate[i],
                     dynamicRange = dynamicRange,
                     windowLength = windowLength,
                     step = step,
                     wn = wn,
                     zp = zp,
                     plot = FALSE,
                     output = 'original')
    # center - see spectral::spec.fft
    s2 = s1 * (-1)^(row(s1) + col(s1))
    # 2D fft
    s3 = abs(fft(s2, inverse = FALSE))
    # image(t(s3))
    # image(t(log(s3)))
    s4 = s3[(nrow(s3) / 2) : nrow(s3), ]  # take only the upper half
    # power
    if (power) s4 = s4 ^ 2
    # normalize
    s5 = s4 - min(s4)
    s5 = s5 / max(s5)
    out[[i]] = t(s5)
  }

  # average modulation spectra across all sounds
  max_rows = max(unlist(lapply(out, nrow)))
  # normally same samplingRate, but in case not, upsample frequency resolution
  max_cols = max(unlist(lapply(out, ncol)))
  sr = max(samplingRate)  # again, in case not the same
  out1 = lapply(out, function(x) interpolMatrix(x, nr = max_rows, nc = max_cols))
  out_aggreg = Reduce('+', out1) / length(myInput)

  # smoothing
  out_aggreg = gaussianSmooth2D(out_aggreg,
                                kernelSize = kernelSize,
                                kernelSD = kernelSD)

  # get time and frequency labels
  max_am = 1000 / step / 2
  X = seq(-max_am, max_am, length.out = nrow(out_aggreg))  # time modulation
  max_fm = ncol(out_aggreg) / (sr / 2 / 1000)
  Y = seq(0, max_fm, length.out = ncol(out_aggreg))  # frequency modulation
  rownames(out_aggreg) = X
  colnames(out_aggreg) = Y

  # plot
  if (plot) {
    seewave::filled.contour.modif2(
      x = X, y = Y, z = out_aggreg,
      levels = seq(0, 1, length = 30),
      color.palette = function(x) gray(seq(from = 1, to = 0, length = x)),
      xlab = 'Hz', ylab = '1/KHz',
      bty = 'n', ...
    )
    # qntls = quantile(out_aggreg, probs = quantiles)  # could try HDI instead
    qntls = pDistr(as.numeric(out_aggreg), quantiles = quantiles)
    par(new = TRUE)
    contour(x = X, y = Y, z = out_aggreg,
            levels = qntls, labels = quantiles * 100,
            xaxs = 'i', yaxs = 'i',
            axes = FALSE, frame.plot = FALSE, ...)
    par(new = FALSE)
  }

  return(out_aggreg)
}
