#' Modulation spectrum
#'
#' Produces a modulation spectrum. Bla-bla-bla...
#'
#' @param x folder, path to a wav/mp3 file, a numeric vector representing a
#'   waveform, or a list of numeric vectors
#' @inheritParams spectrogram
#' @param power if TRUE, returns power modulation spectrum (^2)
#' @param plot if TRUE, plots the modulation spectrum
#' @param quantiles quantiles labeled as separate contours on the plot
#' @param kernelSize the size of Gaussian kernel used for smoothing
#' @param kernelSD the SD of Gaussian kernel used for smoothing, relative to its
#'   size
#'
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
#' ms = modulationSpectrum('~/Downloads/temp/200_ut_fear-bungee_11.wav')
#' ms = modulationSpectrum('/home/allgoodguys/Downloads/temp', digits = 5)
#'
#' ms = modulationSpectrum(ss, samplingRate = 16000, quantiles = c(.8, .9, .99), kernelSize = 6, power = FALSE)
modulationSpectrum = function(x,
                              samplingRate = NULL,
                              dynamicRange = 80,
                              windowLength = 25,
                              step = NULL,
                              overlap = 75,
                              wn = 'gaussian',
                              zp = 0,
                              power = FALSE,
                              plot = TRUE,
                              quantiles = c(.8, .9, .99),
                              kernelSize = 5,
                              kernelSD = .5) {
  # load input
  if (is.character(x)) {
    # assume that this is a directory containing sound files
    myInput = as.list(list.files(x, pattern = "*.wav|.mp3", full.names = TRUE))
    if (!length(myInput) > 0) {
      # assume that it is a single sound file
      myInput = as.list(x)
    }
    sound1 = as.character(myInput[[1]])
    ext = substr(sound1, (nchar(sound1) - 2), nchar(sound1))
    if (ext %in% c('wav', 'WAV')) {
      samplingRate = tuneR::readWave(myInput[[1]])@samp.rate
    } else if (ext %in% c('mp3', 'MP3')) {
      samplingRate = tuneR::readMP3(myInput[[1]])@samp.rate
    } else {
      stop('Input not recognized')
    }
  } else if (is.numeric(x)) {
    # assume that it is an actual sound
    myInput = list(x)
    if (!is.numeric(samplingRate)) {
      stop('Please specify samplingRate')
    }
  } else if (is.list(x)) {
    myInput = x
  } else {
    stop('Input not recognized')
  }
  if (is.null(step)) step = windowLength * (1 - overlap / 100)

  out = vector('list', length(myInput))
  for (i in 1:length(myInput)) {
    s0 = myInput[[i]]
    s1 = spectrogram(s0,
                     samplingRate = samplingRate,
                     dynamicRange = dynamicRange,
                     windowLength = windowLength,
                     step = step,
                     wn = wn,
                     zp = zp,
                     plot = FALSE,
                     output = 'original')
    # center the spectrum - see the "spectral" package, spec.fft
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
  nrows = unlist(lapply(out, nrow))
  ds = max(nrows)
  out1 = lapply(out, function(x) interpolMatrix(x, nr = ds))
  out_aggreg = Reduce('+', out1) / length(myInput)

  # smoothing
  out_aggreg = gaussianSmooth2D(out_aggreg,
                                kernelSize = kernelSize,
                                kernelSD = kernelSD)

  # get time and frequency labels
  longest_sound = which.max(nrows)
  max_am = 1000 / step / 2
  X = seq(-max_am, max_am, length.out = nrow(out_aggreg))  # time modulation
  max_fm = ncol(out_aggreg) / (samplingRate / 2 / 1000)
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
      bty = 'n'
    )
    qntls = quantile(out_aggreg, probs = quantiles)
    par(new = TRUE)
    contour(x = X, y = Y, z = out_aggreg,
            levels = qntls, labels = quantiles * 100,
            axes = FALSE, frame.plot = FALSE)
    par(new = FALSE)
  }

  return(out_aggreg)
}
