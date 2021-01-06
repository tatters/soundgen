#' Self-similarity matrix
#'
#' Calculates the self-similarity matrix and novelty vector of a sound.
#'
#' @seealso \code{\link{spectrogram}} \code{\link{modulationSpectrum}}
#'   \code{\link{segment}}
#'
#' @references \itemize{
#'   \item El Badawy, D., Marmaroli, P., & Lissek, H. (2013). Audio
#'   Novelty-Based Segmentation of Music Concerts. In Acoustics 2013 (No.
#'   EPFL-CONF-190844)
#'   \item Foote, J. (1999, October). Visualizing music and
#'   audio using self-similarity. In Proceedings of the seventh ACM
#'   international conference on Multimedia (Part 1) (pp. 77-80). ACM.
#'   \item
#'   Foote, J. (2000). Automatic audio segmentation using a measure of audio
#'   novelty. In Multimedia and Expo, 2000. ICME 2000. 2000 IEEE International
#'   Conference on (Vol. 1, pp. 452-455). IEEE.
#'   }
#' @inheritParams spectrogram
#' @inheritParams analyze
#' @param ssmWin window for averaging SSM, ms (has a smoothing effect and speeds
#'   up the processing)
#' @param sparse if TRUE, the entire SSM is not calculated, but only the central
#'   region needed to extract the novelty contour (speeds up the processing)
#' @param maxFreq highest band edge of mel filters, Hz. Defaults to
#'   \code{samplingRate / 2}. See \code{\link[tuneR]{melfcc}}
#' @param nBands number of warped spectral bands to use. Defaults to \code{100 *
#'   windowLength / 20}. See \code{\link[tuneR]{melfcc}}
#' @param input the spectral representation used to calculate the SSM
#' @param MFCC which mel-frequency cepstral coefficients to use; defaults to
#'   \code{2:13}
#' @param norm if TRUE, the spectrum of each STFT frame is normalized
#' @param simil method for comparing frames: "cosine" = cosine similarity, "cor"
#'   = Pearson's correlation
#' @param kernelLen length of checkerboard kernel for calculating novelty, ms
#'   (larger values favor global, slow vs. local, fast novelty)
#' @param kernelSD SD of checkerboard kernel for calculating novelty
#' @param padWith how to treat edges when calculating novelty: NA = treat sound
#'   before and after the recording as unknown, 0 = treat it as silence
#' @param plot if TRUE, plots the SSM
#' @param heights relative sizes of the SSM and spectrogram/novelty plot
#' @param specPars graphical parameters passed to \code{filled.contour.mod} and
#'   affecting the \code{\link{spectrogram}}
#' @param ssmPars graphical parameters passed to \code{filled.contour.mod} and
#'   affecting the plot of SSM
#' @param noveltyPars graphical parameters passed to
#'   \code{\link[graphics]{lines}} and affecting the novelty contour
#' @return Returns a list of two components: $ssm contains the self-similarity
#'   matrix, and $novelty contains the novelty vector.
#' @export
#' @seealso \code{\link{spectrogram}} \code{\link{modulationSpectrum}}
#' @examples
#' sound = c(soundgen(),
#'           soundgen(nSyl = 4, sylLen = 50, pauseLen = 70,
#'           formants = NA, pitch = c(500, 330)))
#' # playme(sound)
#' # detailed, local features (captures each syllable)
#' s1 = ssm(sound, samplingRate = 16000, kernelLen = 100,
#'          sparse = TRUE)  # much faster
#' # more global features (captures the transition b/w the two sounds)
#' s2 = ssm(sound, samplingRate = 16000, kernelLen = 400, sparse = TRUE)
#'
#' s2$summary
#' s2$novelty  # novelty contour
#' \dontrun{
#' ssm(sound, samplingRate = 16000,
#'     input = 'mfcc', simil = 'cor', norm = TRUE,
#'     ssmWin = 25,  # speed up the processing
#'     kernelLen = 300,  # global features
#'     specPars = list(colorTheme = 'heat.colors'),
#'     ssmPars = list(colorTheme = 'bw'),
#'     noveltyPars = list(type = 'l', lty = 3, lwd = 2))
#' }
ssm = function(
  x,
  samplingRate = NULL,
  from = NULL,
  to = NULL,
  windowLength = 25,
  overlap = NULL,
  step = 5,
  ssmWin = NULL,
  sparse = FALSE,
  maxFreq = NULL,
  nBands = NULL,
  MFCC = 2:13,
  input = c('mfcc', 'melspec', 'spectrum')[2],
  norm = FALSE,
  simil = c('cosine', 'cor')[1],
  kernelLen = 100,
  kernelSD = .5,
  padWith = 0,
  summaryFun = c('mean', 'sd'),
  reportEvery = NULL,
  plot = TRUE,
  savePlots = NULL,
  main = NULL,
  heights = c(2, 1),
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  specPars = list(
    levels = seq(0, 1, length = 30),
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[2],
    xlab = 'Time, s',
    ylab = 'kHz'
  ),
  ssmPars = list(
    levels = seq(0, 1, length = 30),
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[2],
    xlab = 'Time, s',
    ylab = 'Time, s'
  ),
  noveltyPars = list(
    type = 'b',
    pch = 16,
    col = 'black',
    lwd = 3
  )) {
  ## Prepare a list of arguments to pass to segmentSound()
  myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'from', 'to', 'savePlots', 'reportEvery',
    'summaryFun', 'specPars', 'ssmPars', 'noveltyPars', 'ssmWin')]
  # exclude ...
  myPars = myPars[1:(length(myPars)-1)]
  myPars$specPars = specPars
  myPars$ssmPars = ssmPars
  myPars$noveltyPars = noveltyPars
  if (is.null(ssmWin)) {
    myPars$win = 1
  } else {
    myPars$win = max(1, round(ssmWin / step / 2) * 2 - 1)
  }

  # analyze
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    from = from,
    to = to,
    funToCall = 'ssmSound',
    myPars = myPars,
    reportEvery = reportEvery,
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots)) {
    htmlPlots(
      htmlFile = paste0(pa$input$savePlots, '00_clickablePlots_ssm.html'),
      plotFiles = paste0(pa$input$savePlots, pa$input$filenames_base, "_ssm.png"),
      audioFiles = pa$input$filenames,
      width = paste0(width, units))
  }

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in 1:pa$input$n) {
      if (!pa$input$failed[i]) {
        temp[[i]] = summarizeAnalyze(
          data.frame(novelty = pa$result[[i]]$novelty),
          summaryFun = summaryFun,
          var_noSummary = NULL)
      }
    }
    idx_failed = which(pa$input$failed)
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

  if (pa$input$n == 1) {
    # unlist
    ssm = pa$result[[1]]$ssm
    novelty = pa$result[[1]]$novelty
  } else {
    ssm = lapply(pa$result, function(x) x[['ssm']])
    novelty = lapply(pa$result, function(x) x[['novelty']])
  }

  invisible(list(
    ssm = ssm,
    novelty = novelty,
    summary = mysum_all
  ))
}


#' SSM sound
#'
#' Internal soundgen function.
#' @inheritParams ssm
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
ssmSound = function(
  audio,
  windowLength = 40,
  overlap = 75,
  step = NULL,
  win = 1,
  sparse = FALSE,
  maxFreq = NULL,
  nBands = NULL,
  MFCC = 2:13,
  input = c('mfcc', 'melspec', 'spectrum')[2],
  norm = FALSE,
  simil = c('cosine', 'cor')[1],
  kernelLen = 100,
  kernelSD = .5,
  padWith = 0,
  plot = TRUE,
  main = NULL,
  heights = c(2, 1),
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  specPars = list(
    levels = seq(0, 1, length = 30),
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[2],
    xlab = 'Time, s',
    ylab = 'kHz'
  ),
  ssmPars = list(
    levels = seq(0, 1, length = 30),
    colorTheme = c('bw', 'seewave', 'heat.colors', '...')[2],
    xlab = 'Time, s',
    ylab = 'Time, s'
  ),
  noveltyPars = list(
    type = 'b',
    pch = 16,
    col = 'black',
    lwd = 3
  )) {
  ## set pars
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  if (is.null(nBands)) {
    nBands = 100 * windowLength / 20
  }
  if (is.null(step)) {
    step = windowLength / 4
  }
  windowLength_points = floor(windowLength / 1000 * audio$samplingRate / 2) * 2
  frame_points = round(audio$samplingRate * step / 1000)
  kernelSize = max(4, round(kernelLen * audio$samplingRate / 1000 / frame_points /
                              2) * 2)  # kernel size in frames, guaranteed to be even
  if (is.null(maxFreq)) {
    maxFreq = floor(audio$samplingRate / 2)  # Nyquist
  }

  ## compute mel-filtered spectrum and MFCCs
  sound = tuneR::Wave(left = audio$sound, samp.rate = audio$samplingRate, bit = 16)
  mel = tuneR::melfcc(
    sound,
    wintime = windowLength / 1000,
    hoptime = step / 1000,
    maxfreq = maxFreq,
    nbands = nBands,
    spec_out = TRUE,
    numcep = max(MFCC)
  )
  if (input == 'mfcc') {
    # the first cepstrum presumably makes no sense with amplitude normalization
    # (?), and it overestimates the similarity of different frames
    target_spec = t(mel$cepstra)[MFCC, ]
    target_spec[is.na(target_spec)] = 0  # MFCC are NaN for silent frames
  } else if (input == 'melspec') {
    target_spec = t(mel$aspectrum)
  } else if (input == 'spectrum') {
    target_spec = t(mel$pspectrum)
  }
  # image(t(log(target_spec + 1e-10)))

  ## compute self-similarity matrix
  s = selfsim(
    m = target_spec,
    norm = norm,
    simil = simil,
    win = win,
    sparse = sparse,
    kernelSize = kernelSize
  )
  # s = zeroOne(s^2)  # hist(s)
  # image(s)

  ## compute novelty
  novelty = getNovelty(ssm = s, kernelSize = kernelSize,
                       kernelSD = kernelSD, padWith = padWith)

  ## PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_base, "_ssm.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    # log-transform and normalize spectrogram
    if (input == 'melspec') {
      spec = log(zeroOne(mel$aspectrum) + 1e-4)  # dynamic range ~ 80 dB or 1e-4
    } else {
      spec = log(zeroOne(mel$pspectrum) + 1e-4)
    }
    spec = zeroOne(spec)

    if (is.null(main)) {
      if (audio$filename_base == 'sound') {
        main = ''
      } else {
        main = audio$filename_base
      }
    }

    op = par(c('mar', 'xaxt', 'yaxt', 'mfrow')) # save user's original pars
    layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = heights)
    par(mar = c(5.1, 4.1, 0, 2.1),
        xaxt = 's',
        yaxt = 's')

    # spectrogram
    specPars1 = list(
      levels = seq(0, 1, length = 30),
      colorTheme = 'seewave',
      xlab = 'Time, s',
      ylab = 'kHz',
      ylim = c(0, maxFreq / 1000)
    )
    for (i in 1:length(specPars)) {
      n = names(specPars)[i]
      specPars1[[n]] = specPars[[n]]
    }
    specPars1$color.palette = switchColorTheme(specPars1$colorTheme)
    specPars1[['colorTheme']] = NULL

    do.call(filled.contour.mod, c(list(
      x = seq(0, audio$duration, length.out = nrow(spec)),
      y = seq(
        0,
        (audio$samplingRate / 2) - (audio$samplingRate / windowLength_points),
        length.out = ncol(spec)
      ) / 1000,
      z = spec
    ), specPars1
    ))

    # novelty
    noveltyPars1 = list(
      type = 'b',
      pch = 16,
      col = 'black',
      lwd = 3
    )
    for (i in 1:length(noveltyPars)) {
      n = names(noveltyPars)[i]
      noveltyPars1[[n]] = noveltyPars[[n]]
    }
    do.call(lines, c(list(
      x = seq(0, audio$duration, length.out = length(novelty)),
      y = novelty / max(novelty, na.rm = TRUE) * maxFreq / 1000 * .95
    ), noveltyPars1
    ))
    axis(side = 1, labels = TRUE)
    par(mar = c(0, 4.1, 2.1, 2.1),
        xaxt = 'n',
        yaxt = 's')
    xlab = ''

    # SSM
    ssmPars1 = list(
      levels = seq(0, 1, length = 30),
      colorTheme = 'seewave',
      xlab = 'Time, s',
      ylab = 'Time, s',
      main = main
    )
    for (i in 1:length(ssmPars)) {
      n = names(ssmPars)[i]
      ssmPars1[[n]] = ssmPars[[n]]
    }
    ssmPars1$color.palette = switchColorTheme(ssmPars1$colorTheme)
    ssmPars1[['colorTheme']] = NULL
    timestamps_ssm = seq(0, audio$duration, length.out = nrow(s))
    do.call(filled.contour.mod, c(list(
      x = timestamps_ssm,
      y = timestamps_ssm,
      z = s
    ), ssmPars1
    ))
    # restore original pars
    par('mar' = op$mar, 'xaxt' = op$xaxt, 'yaxt' = op$yaxt, 'mfrow' = op$mfrow)
    if (is.character(audio$savePlots)) dev.off()
  }

  invisible(list(ssm = s, novelty = novelty))
}



#' Compute self-similarity
#'
#' Internal soundgen function.
#'
#' Called by \code{\link{ssm}}.
#' @param m input matrix such as a spectrogram
#' @inheritParams ssm
#' @param win the length of window for averaging self-similarity, frames
#' @return Returns a square self-similarity matrix.
#' @keywords internal
selfsim = function(m,
                   norm = FALSE,
                   simil = c('cosine', 'cor')[1],
                   win = 1,
                   sparse = FALSE,
                   kernelSize = NULL) {
  if (win > floor(ncol(m) / 2)) {
    win = floor(ncol(m) / 2)
    warning(paste('"win" must be smaller than half the number of frames',
                  'resetting to', floor(ncol(m) / 2)))
  }
  if (win %% 2 == 0) {
    win = max(ceiling(win / 2) * 2 - 1, 1)
  } # win must be odd

  # normalize input by column, if needed
  if (norm) {
    m = apply(m, 2, zeroOne, na.rm = TRUE)
    m[apply(m, c(1, 2), is.nan)] = 0
  }

  # calculate windows for averaging self-similarity
  numWins = ceiling(ncol(m) / win)
  winIdx = round(seq(1, ncol(m) - win, length.out = numWins))

  # calculate self-similarity
  out = matrix(NA, nrow = numWins, ncol = numWins)
  rownames(out) = colnames(out) = winIdx
  if (!sparse) j_idx = 1:numWins
  for (i in 1:length(winIdx)) {
    if (sparse) j_idx = max(1, i - kernelSize) : min(numWins, i + kernelSize)
    for (j in j_idx) {
      mi = as.vector(m[, winIdx[i]:(winIdx[i] + win - 1)])
      mj = as.vector(m[, winIdx[j]:(winIdx[j] + win - 1)])
      if (any(mi != 0) & any(mj != 0)) {
        if (simil == 'cosine') {
          # http://stackoverflow.com/questions/6597005/cosine-similarity-between-two-vectors-in-language-r
          out[i, j] = crossprod(mi, mj) / sqrt(crossprod(mi) * crossprod(mj))
        } else if (simil == 'cor') {
          out[i, j] = cor(mi, mj)
        }
      } else {
        # if at least one is a vector of zeros, set result to 0 (otherwise NA)
        out[i, j] = 0
      }
    }
  }
  # image(t(out))
  out = zeroOne(out, na.rm = TRUE)
  return(out)
}


#' Checkerboard kernel
#'
#' Internal soundgen function.
#'
#' Prepares a square matrix \code{size x size} specifying a gaussian kernel for
#' measuring novelty of self-similarity matrices. Called by
#' \code{\link{getNovelty}}
#' @param size kernel size (points), preferably an even number
#' @param kernel_mean,kernelSD mean and SD of the gaussian kernel
#' @param plot if TRUE, shows a perspective plot of the kernel
#' @param checker if TRUE, inverts two quadrants
#' @return Returns a square matrix with \code{size} rows and columns.
#' @keywords internal
#' @examples
#' kernel = soundgen:::getCheckerboardKernel(size = 64, kernelSD = 0.1, plot = TRUE)
#' dim(kernel)
#' kernel = soundgen:::getCheckerboardKernel(size = 19, kernelSD = .5,
#'   checker = FALSE, plot = TRUE)
getCheckerboardKernel = function(size,
                                 kernel_mean = 0,
                                 kernelSD = 0.5,
                                 plot = FALSE,
                                 checker = TRUE) {
  x = seq(-1, 1, length.out = size)
  kernelSD = kernelSD  # just to get rid of the "unused arg" warning in CMD check :-)
  if (size < 50) {
    # faster than mvtnorm::dmvnorm for small kernels
    kernel = matrix(NA, ncol = size, nrow = size)
    for (i in 1:nrow(kernel)) {
      for (j in 1:ncol(kernel)) {
        kernel[i, j] = dnorm(x[i], mean = kernel_mean, sd = kernelSD) *
          dnorm(x[j], mean = kernel_mean, sd = kernelSD)
      }
    }
  } else {
    # this is faster for large kernels
    sigma = diag(2) * kernelSD
    kernel_long = expand.grid(x1 = x, x2 = x)
    kernel_long$dd = mvtnorm::dmvnorm(x = kernel_long,
                                      mean = c(kernel_mean, kernel_mean),
                                      sigma = sigma)
    kernel = reshape2::acast(data = kernel_long, formula = x2 ~ x1, value.var = 'dd')
  }

  if (checker) {
    # quadrant 0 to 3 o'clock
    kernel[1:(floor(size / 2)), (ceiling(size / 2) + 1):size] =
      -kernel[1:(floor(size / 2)), (ceiling(size / 2) + 1):size]
    # quadrant 6 to 9 o'clock
    kernel[(ceiling(size / 2) + 1):size, 1:(ceiling(size / 2))] =
      -kernel[(ceiling(size / 2) + 1):size, 1:(ceiling(size / 2))]
  }

  kernel = kernel / max(kernel)
  if (plot) {
    persp (
      kernel,
      theta = -20,
      phi = 25,
      # zlim = c(-1, 4),
      ticktype = 'detailed'
    )
  }
  return (kernel)
}


#' SSM novelty
#'
#' Internal soundgen function.
#'
#' Calculates novelty in a self-similarity matrix. Called by \code{\link{ssm}}.
#' @param ssm self-similarity matrix, as produced by \code{\link{selfsim}}
#' @param kernelSize the size of gausisan kernel (points)
#' @param kernelSD the SD of gaussian kernel
#' @param normalize if TRUE, normalizes so that max = 1
#' @return Returns a numeric vector of length \code{nrow(ssm)}
#' @keywords internal
getNovelty = function(ssm,
                      kernelSize,
                      kernelSD,
                      padWith = 0,
                      normalize = TRUE) {
  kernel = getCheckerboardKernel(size = kernelSize, kernelSD = kernelSD)
  ## pad matrix with size / 2 zeros, so that we can correlate it with the
  #  kernel starting from the very edge
  ssm_padded = matrix(padWith,
                      nrow = nrow(ssm) + kernelSize,
                      ncol = nrow(ssm) + kernelSize)
  halfK = kernelSize / 2
  # indices in the padded matrix where we'll paste the original ssm
  idx = c(halfK + 1, nrow(ssm_padded) - halfK)
  # paste original. Now we have a padded ssm
  ssm_padded[idx[1]:idx[2], idx[1]:idx[2]] = ssm

  ## get novelty
  novelty = rep(NA, nrow(ssm))
  # for each point on the main diagonal, novelty = correlation between the checkerboard kernel and the ssm. See Badawy, "Audio novelty-based segmentation of music concerts"
  for (i in idx[1]:idx[2]) {
    n = (i - halfK):(i + halfK - 1)
    # suppress warnings, b/c otherwise cor complains of sd = 0 for silent segments
    novelty[i - halfK] =  suppressWarnings(
      cor(as.vector(ssm_padded[n, n]),
          as.vector(kernel))) #'pairwise.complete.obs'))
  }
  # novelty[is.na(novelty)] = 0
  return(novelty)
}
