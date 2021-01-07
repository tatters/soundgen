### MODULATION SPECTRUM

#' Modulation spectrum folder
#'
#' Deprecated; use \code{\link{modulationSpectrum}} instead
#' @param ... any input parameters
modulationSpectrumFolder = function(...) {
  message('modulationSpectrumFolder() is deprecated;',
          'please use modulationSpectrum(averageMS = FALSE) instead')
}


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
#' @seealso \code{\link{spectrogram}} \code{\link{analyze}}
#'
#' @references \itemize{
#'   \item Singh, N. C., & Theunissen, F. E. (2003). Modulation spectra of
#'   natural sounds and ethological theories of auditory processing. The Journal
#'   of the Acoustical Society of America, 114(6), 3394-3411.
#' }
#' @inheritParams spectrogram
#' @inheritParams analyze
#' @param roughRange the range of temporal modulation frequencies that
#'   constitute the "roughness" zone, Hz
#' @param amRes target resolution of amplitude modulation, Hz. If \code{NULL},
#'   the entire sound is analyzed at once (or in chunks \code{maxDur} s long),
#'   resulting in a single roughness value. Otherwise, roughness is calculated
#'   per frame, each selected to contain enough STFT windows to calculate
#'   roughness with precision given by \code{amRes}
#' @param maxDur sounds longer than \code{maxDur} s are split into fragments,
#'   and the modulation spectra of all fragments are averaged
#' @param logSpec if TRUE, the spectrogram is log-transformed prior to taking 2D
#'   FFT
#' @param power raise modulation spectrum to this power (eg power = 2 for ^2, or
#'   "power spectrum")
#' @param returnMS if FALSE, only roughness is returned (much faster)
#' @param returnComplex if TRUE, returns a complex modulation spectrum (without
#'   normalization and warping)
#' @param averageMS if TRUE, the modulation spectra of all inputs are averaged
#'   into a single output; if FALSE, a separate MS is returned for each input
#' @param plot if TRUE, plots the modulation spectrum of each sound
#' @param savePlots if a valid path is specified, a plot is saved in this folder
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
#' s_long = soundgen(sylLen = 4000, pitch = 400,
#'   amDep = c(0, 0, 50), jitterDep = c(0, 0, 2))
#' rough = modulationSpectrum(s_long, 16000, plot = FALSE)$roughness
#' spectrogram(s_long, 16000, ylim = c(0, 4),
#'             extraContour = list(rough / max(rough) * 4000, col = 'blue'))
#'
#' # Input can also be a list of waveforms (numeric vectors)
#' ss = vector('list', 10)
#' for (i in 1:length(ss)) {
#'   ss[[i]] = soundgen(sylLen = runif(1, 100, 1000), temperature = .4,
#'     pitch = runif(3, 400, 600))
#' }
#' # lapply(ss, playme)
#' # MS of the first sound
#' ms1 = modulationSpectrum(ss[[1]], samplingRate = 16000, scale = 1)
#' # average MS of all 10 sounds
#' ms2 = modulationSpectrum(ss, samplingRate = 16000, scale = 1, averageMS = TRUE)
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
#' # Input can be path to folder with audio files. Each file is processed
#' # separately, and the output can contain an MS per file...
#' ms1 = modulationSpectrum('~/Downloads/temp', kernelSize = 11,
#'                          plot = FALSE, averageMS = FALSE)
#' str(ms1$original)
#' ms1$summary
#' # ...or a single MS can be calculated:
#' ms2 = modulationSpectrum('~/Downloads/temp', kernelSize = 11,
#'                          plot = FALSE, averageMS = TRUE)
#' str(ms2$original)
#' ms2$summary
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
  scale = NULL,
  from = NULL,
  to = NULL,
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
  returnMS = TRUE,
  returnComplex = FALSE,
  summaryFun = c('mean', 'median', 'sd'),
  averageMS = FALSE,
  reportEvery = NULL,
  plot = TRUE,
  savePlots = NULL,
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
  ## Prepare a list of arguments to pass to segmentSound()
  myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'savePlots', 'reportEvery', 'summaryFun', 'averageMS')]
  # exclude ...
  myPars = myPars[1:(length(myPars)-1)]

  # analyze
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = 'modulationSpectrumSound',
    myPars = myPars,
    reportEvery = reportEvery,
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots)) {
    htmlPlots(
      htmlFile = paste0(pa$input$savePlots, '00_clickablePlots_MS.html'),
      plotFiles = paste0(pa$input$savePlots, pa$input$filenames_base, "_MS.png"),
      audioFiles = pa$input$filenames,
      width = paste0(width, units))
  }

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in 1:pa$input$n) {
      if (!pa$input$failed[i]) {
        temp[[i]] = summarizeAnalyze(
          data.frame(roughness = pa$result[[i]]$roughness),
          summaryFun = summaryFun,
          var_noSummary = NULL)
      }
    }
    idx_failed = which(pa$input$failed)
    idx_ok = which(!pa$input$failed)
    if (length(idx_failed) > 0) {
      idx_ok = which(!pa$input$failed)
      if (length(idx_ok) > 0) {
        filler = temp[[idx_ok[1]]] [1, ]
        filler[1, ] = NA
      } else {
        stop('Failed to analyze any input')
      }
      for (i in idx_failed) temp[[i]] = filler
    }
    mysum_all = cbind(data.frame(file = pa$input$filenames_base),
                      do.call('rbind', temp))
  } else {
    mysum_all = NULL
  }
  for (i in idx_failed) pa$result[[i]] = list(
    original = NULL, processed = NULL, complex = NULL, roughness = NULL
  )
  435
  original = processed = complex = roughness = NULL
  # (otherwise note about no visible binding)
  out_prep = c('original', 'processed', 'complex', 'roughness')
  if (pa$input$n == 1) {
    # unlist
    for (op in out_prep) assign(noquote(op), pa$result[[1]] [[op]])
  } else {
    for (op in out_prep) assign(noquote(op), lapply(pa$result, function(x) x[[op]]))
  }

  # average MS across sounds
  op_prep2 = c('original', 'processed')  # roughness is not a matrix
  if (returnComplex) {
    op_prep2 = c(op_prep2, 'complex')
  } else {
    complex = NULL
  }
  if (averageMS & pa$input$n > 1) {
    for (op in op_prep2) {
      assign(
        noquote(op),
        averageMatrices(
          get(op) [idx_ok],
          rFun = 'max',   # normally same samplingRate, but if not, upsample frequency resolution
          cFun = 'median',   # typical ncol (depends on sound dur)
          reduceFun = '+')  # internally divides by length, so actually becomes mean
      )
    }
  }

  invisible(list(original = original,
                 processed = processed,
                 complex = complex,
                 roughness = roughness,
                 summary = mysum_all))
}


#' Modulation spectrum sound
#'
#' Internal soundgen function.
#' @inheritParams modulationSpectrum
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
modulationSpectrumSound = function(
  audio,
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
  returnMS = TRUE,
  returnComplex = FALSE,
  plot = TRUE,
  savePlots = NULL,
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
  # Re-set windowLength, step, and overlap so as to ensure that
  # windowLength_points and step_points are not fractions
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  step_points = round(step / 1000 * audio$samplingRate)
  step = step_points / audio$samplingRate * 1000
  windowLength_points = round(windowLength / 1000 * audio$samplingRate)
  windowLength = windowLength_points / audio$samplingRate * 1000
  overlap = 100 * (1 - step_points / windowLength_points)

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

  if (is.numeric(nFrames)) {
    # split the input sound into chunks nFrames long
    chunk_ms = windowLength + step * (nFrames - 1)
    splitInto = max(1, ceiling(audio$duration * 1000 / chunk_ms))
    if (chunk_ms > (audio$duration * 1000)) {
      message(paste('The sound is too short to be analyzed with amRes =', amRes,
                    'Hz. Roughness is probably not measured correctly'))
      chunk_ms = audio$duration
    }
  } else {
    # split only those sounds that exceed maxDur
    splitInto = max(1, ceiling(audio$duration / maxDur))
    # so, for ex., if 2.1 times longer than maxDur, split into three
  }
  if (splitInto > 1) {
    myseq = floor(seq(1, length(audio$sound), length.out = splitInto + 1))
    myInput = vector('list', splitInto)
    for (i in 1:splitInto) {
      idx = myseq[i]:(myseq[i + 1])
      myInput[[i]] = audio$sound[idx]
    }
  } else {
    myInput = list(audio$sound)
  }

  # extract modulation spectrum per fragment
  out = vector('list', splitInto)
  if (returnComplex) {
    out_complex = out
  } else {
    out_aggreg_complex = NULL
  }
  for (i in 1:splitInto) {
    ms_i = modulationSpectrumFragment(
      myInput[[i]],
      samplingRate = audio$samplingRate,
      windowLength = windowLength,
      windowLength_points = windowLength_points,
      step = step,
      step_points = step_points,
      wn = wn,
      zp = zp,
      logSpec = logSpec,
      power = power)
    out[[i]] = ms_i$ms_half

    if (returnComplex) {
      out_complex[[i]] = ms_i$ms_complex
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

  # extract a measure of roughness
  roughness = unlist(lapply(out, function(x)
    getRough(x, roughRange, colNames = as.numeric(colnames(out[[1]])))))

  # average modulation spectra across all sounds
  if (!returnMS) {
    result = list('original' = NULL,
                  'processed' = NULL,
                  'complex' = NULL,
                  'roughness' = roughness)
  } else {
    out_aggreg = averageMatrices(
      out,
      rFun = 'max',   # normally same samplingRate, but if not, upsample frequency resolution
      cFun = 'median',  # typical ncol (depends on sound dur)
      reduceFun = '+')  # internally divides by length, so actually becomes mean
    X = as.numeric(colnames(out_aggreg))
    Y = as.numeric(rownames(out_aggreg))
    # image(X, Y, t(log(out_aggreg)))

    # prepare a separate summary of the complex ms
    if (returnComplex) {
      out_aggreg_complex = averageMatrices(
        out_complex,
        rFun = 'max',
        cFun = 'median',
        reduceFun = '+')
      # image(t(log(abs(out_aggreg_complex))))
    }

    # log-transform the axes (or, actually, warps the matrix itself)
    if (is.numeric(logWarp)) {
      neg_col = which(X < 0)
      zero_col = which(X == 0)
      pos_col = which(X > 0)
      m_left = logMatrix(out_aggreg[, rev(neg_col)], base = logWarp)
      # NB: flip the left half!
      m_right = logMatrix(out_aggreg[, pos_col], base = logWarp)
      out_transf = cbind(m_left[, ncol(m_left):1],
                         out_aggreg[, zero_col, drop = FALSE], m_right)
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
    result = list('original' = out_aggreg,
                  'processed' = out_transf,
                  'complex' = out_aggreg_complex,
                  'roughness' = roughness)
  }  # end of if (returnMS)


  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_base, "_MS.png"),
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
        main = audio$filename_base,
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
            main = audio$filename_base, xlim = xlim, ylim = ylim, ...)
    par(new = FALSE)
    if (is.character(audio$savePlots)) dev.off()
  }

  invisible(result)
}


#' Modulation spectrum fragment
#'
#' Internal soundgen function.
#' @inheritParams modulationSpectrum
#' @param sound numeric vector
#' @keywords internal
#' @examples
#' s = soundgen(amFreq = 25, amDep = 100)
#' ms = soundgen:::modulationSpectrumFragment(s, 16000,
#'   windowLength = 50, windowLength_points = .05 * 16000,
#'   step = 5, step_points = .005 * 16000)
#' image(as.numeric(colnames(ms$ms_half)), as.numeric(rownames(ms$ms_half)),
#'       t(log(ms$ms_half)))
modulationSpectrumFragment = function(sound,
                                      samplingRate,
                                      windowLength,
                                      windowLength_points,
                                      step,
                                      step_points,
                                      wn = 'hanning',
                                      zp = 0,
                                      logSpec = FALSE,
                                      power = 1) {
  # Calling stdft is ~80 times faster than going through spectrogram (!)
  step_seq = seq(1, length(sound) + 1 - windowLength_points, step_points)
  # print(length(step_seq))
  if (length(step_seq) < 3) return(NULL)
  s1 = seewave::stdft(
    wave = as.matrix(sound),
    wn = wn,
    wl = windowLength_points,
    f = samplingRate,
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
  return(list(
    ms_half = ms_half,
    ms_complex = ms_complex
  ))
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


#' Average matrices
#'
#' Internal soundgen function.
#'
#' Takes a list of matrices (normally modulation spectra), interpolates them to
#' have the same size, and then reduces them (eg takes the average).
#' @param mat_list a list of matrices to aggregate (eg spectrograms or
#'   modulation spectra)
#' @param rFun, cFun functions used to determine the number of rows and columns
#'   in the result
#' @param aggFun function used to aggregate
#' @keywords internal
#' @examples
#' mat_list = list(
#'   matrix(1:30, nrow = 5),
#'   matrix(80:17, nrow = 8)
#' )
#' soundgen:::averageMatrices(mat_list)
#' soundgen:::averageMatrices(mat_list, cFun = 'max', reduceFun = '*')
averageMatrices = function(mat_list,
                           rFun = 'max',
                           cFun = 'median',
                           reduceFun = '+') {
  # normally same samplingRate, but in case not, upsample frequency resolution
  nr = round(do.call(rFun, list(unlist(lapply(mat_list, nrow)))))
  # take typical ncol (depends on sound dur)
  nc = round(do.call(cFun, list(unlist(lapply(mat_list, ncol)))))
  mat_list_sameDim = lapply(mat_list, function(x) interpolMatrix(x, nr = nr, nc = nc))
  agg = Reduce(reduceFun, mat_list_sameDim) / length(mat_list)
  return(agg)
}
