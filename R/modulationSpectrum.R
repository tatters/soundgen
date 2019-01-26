#' Modulation spectrum
#'
#' Produces a modulation spectrum of waveform(s) or audio file(s). Algorithm:
#' prepares a \code{\link{spectrogram}} and takes its 2D Fourier transform (see
#' also \code{\link[spectral]{spec.fft}}). For multiple inputs, the ensemble of
#' modulation spectra is interpolated to the same spectral and temporal
#' resolution and averaged. By default the plot is smoothed with Gaussian blur
#' (see \code{\link{gaussianSmooth2D}}).
#'
#' @return Returns a matrix of nonnegative values. Rownames are frequencies of
#'   amplitude modulation (Hz), and colnames are frequencies of frequency
#'   modulation (1/KHz).
#'
#' @param x folder, path to a wav/mp3 file, a numeric vector representing a
#'   waveform, or a list of numeric vectors
#' @param samplingRate sampling rate of x (only needed if x is a numeric vector,
#'   rather than an audio file). For a list of sounds, give either one
#'   samplingRate (the same for all) or as many values as there are input files
#' @inheritParams spectrogram
#' @param maxDur maximum allowed duration of a single sound, s (longer sounds
#'   are split)
#' @param power if TRUE, returns power modulation spectrum (^2)
#' @param plot if TRUE, plots the modulation spectrum
#' @param quantiles labeled contour values, % (e.g., "50" marks regions that
#'   contain 50% of the sum total of the entire modulation spectrum)
#' @param kernelSize the size of Gaussian kernel used for smoothing
#' @param kernelSD the SD of Gaussian kernel used for smoothing, relative to its
#' size
#' @param ... other graphical parameters passed on to
#'   \code{\link[seewave]{filled.contour.modif2}} and
#'   \code{\link[graphics]{contour}}
#' @references \itemize{
#'   \item Singh, N. C., & Theunissen, F. E. (2003). Modulation spectra of
#'   natural sounds and ethological theories of auditory processing. The Journal
#'   of the Acoustical Society of America, 114(6), 3394-3411.
#'   }
#' @export
#' @examples
#' ms = modulationSpectrum(soundgen(), samplingRate = 16000)
#'
#' \dontrun{
#' # Input can also be a list of waveforms (numeric vectors)
#' ss = vector('list', 10)
#' for (i in 1:length(ss)) {
#'   ss[[i]] = soundgen(sylLen = runif(1, 100, 1000), temperature = .4,
#'     pitch = runif(3, 400, 600))
#' }
#' # lapply(ss, playme)
#' ms = modulationSpectrum(ss[[1]], samplingRate = 16000)  # the first sound
#' ms = modulationSpectrum(ss, samplingRate = 16000)  # all 10 sounds
#'
#' # As with spectrograms, there is a tradeoff in time-frequency resolution
#' s = soundgen(pitch = 500, amFreq = 50, amDep = 100, samplingRate = 44100)
#' # playme(s, samplingRate = 44100)
#' spectrogram(s, samplingRate = 44100, osc = TRUE, ylim = c(0, 8))
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 50, overlap = 0)  # poor temporal resolution
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 5, overlap = 80)  # poor frequency resolution
#' ms = modulationSpectrum(s, samplingRate = 44100,
#'   windowLength = 30, overlap = 80)  # a reasonable compromise
#'
#' # Input can be a wav/mp3 file
#' ms = modulationSpectrum('~/Downloads/temp/200_ut_fear-bungee_11.wav')
#' ms = modulationSpectrum('~/Downloads/temp/200_ut_fear-bungee_11.wav',
#'   kernelSize = 17,  # more smoothing
#'   xlim = c(-20, 20), ylim = c(0, 4),  # zoom in on the central region
#'   quantiles = c(.25, .5, .8),  # customize contour lines
#'   colorTheme = 'heat.colors',  # alternative palette
#'   power = TRUE)  # ^2
#'
#' # Input can be path to folder with audio files (average modulation spectrum)
#' ms = modulationSpectrum('~/Downloads/temp/', kernelSize = 17, power = TRUE)
#' # NB: longer files will be split into fragments <maxDur in length
#'
#' # A sound with ~3 syllables per second and only downsweeps in F0 contour
#' s = soundgen(nSyl = 8, sylLen = 200, pauseLen = 100, pitch = c(300, 200))
#' # playme(s)
#' ms = modulationSpectrum(s, samplingRate = 16000, maxDur = .5,
#'   xlim = c(-25, 25), power = TRUE, colorTheme = 'seewave')
#' # note the asymmetry b/c of downsweeps
#' }
modulationSpectrum = function(x,
                              samplingRate = NULL,
                              maxDur = 5,
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
                              colorTheme = c('bw', 'seewave', '...')[1],
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
    if (colorTheme == 'bw') {
      color.palette = function(x) gray(seq(from = 1, to = 0, length = x))
    } else if (colorTheme == 'seewave') {
      color.palette = seewave::spectro.colors
    } else {
      colFun = match.fun(colorTheme)
      color.palette = function(x) rev(colFun(x))
    }

    seewave::filled.contour.modif2(
      x = X, y = Y, z = out_aggreg,
      levels = seq(0, 1, length = 30),
      color.palette = color.palette,
      xlab = 'Hz', ylab = '1/KHz',
      bty = 'n', ...
    )
    abline(v = 0, lty = 3)
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
