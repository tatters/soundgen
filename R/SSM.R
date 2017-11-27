#' Self-similarity matrix
#'
#' Calculates the self-similarity matrix and novelty vector of a sound.
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
#' @param x path to a .wav file or a vector of amplitudes with specified
#'   samplingRate
#' @param samplingRate sampling rate of \code{x} (only needed if \code{x} is a
#'   numeric vector, rather than a .wav file)
#' @param windowLength length of FFT window, ms
#' @param overlap overlap between successive FFT frames, \%
#' @param step you can override \code{overlap} by specifying FFT step, ms
#' @param ssmWin window for averaging SSM, ms
#' @param maxFreq highest band edge of mel filters, Hz. Defaults to
#'   \code{samplingRate / 2}. See \code{\link[tuneR]{melfcc}}
#' @param nBands number of warped spectral bands to use. Defaults to \code{100 *
#'   windowLength / 20}. See \code{\link[tuneR]{melfcc}}
#' @param input either MFCCs ("cepstrum") or mel-filtered spectrum ("audiogram")
#' @param MFCC which mel-frequency cepstral coefficients to use; defaults to
#'   \code{2:13})
#' @param norm if TRUE, each FFT frame is normalized by max power
#' @param simil method for comparing frames: "cosine" = cosine similarity, "cor"
#'   = Pearson's correlation
#' @param kernelLen length of checkerboard kernel for calculating novelty, ms (the
#'   larger, the more global vs. local the novelty)
#' @param kernelSD SD of checkerboard kernel for calculating novelty
#' @param returnSSM if TRUE, returns the SSM
#' @param plot if TRUE, plots the SSM
#' @param specPars graphical parameters passed to
#'   \code{seewave::filled.contour.modif2} and affecting the spectrogram
#' @param ssmPars graphical parameters passed to
#'   \code{seewave::filled.contour.modif2} and affecting the plot of SSM
#' @param noveltyPars graphical parameters passed to
#'   \code{\link[graphics]{lines}} and affecting the novelty contour
#' @return If \code{returnSSM} is TRUE, returns a list of two components: $ssm
#'   contains the self-similarity matrix, and $novelty contains the novelty
#'   vector. If \code{returnSSM} is FALSE, only produces a plot.
#' @export
#' @examples
#' sound = c(soundgen(), soundgen(nSyl = 4, sylLen = 50, pauseLen = 70,
#'           formants = NA, pitchAnchors = c(500, 330)))
#' # playme(sound)
#' m1 = ssm(sound, samplingRate = 16000,
#'          input = 'audiogram', simil = 'cor', norm = FALSE,
#'          ssmWin = 10, kernelLen = 150)  # detailed, local features
#' \dontrun{
#' m2 = ssm(sound, samplingRate = 16000,
#'          input = 'mfcc', simil = 'cosine', norm = TRUE,
#'          ssmWin = 50, kernelLen = 600)  # more global
#' # plot(m2$novelty, type='b')  # use for peak detection, etc
#' }
ssm = function(x,
               samplingRate = NULL,
               windowLength = 40,
               overlap = 75,
               step = NULL,
               ssmWin = 40,
               maxFreq = NULL,
               nBands = NULL,
               MFCC = 2:13,
               input = c('mfcc', 'audiogram', 'spectrum')[1],
               norm = FALSE,
               simil = c('cosine', 'cor')[1],
               returnSSM = TRUE,
               kernelLen = 200,
               kernelSD = .2,
               plot = TRUE,
               specPars = list(
                 levels = seq(0, 1, length = 30),
                 color.palette = seewave::spectro.colors,
                 xlab = 'Time, s',
                 ylab = 'kHz',
                 ylim = c(0, maxFreq / 1000)
               ),
               ssmPars = list(
                 levels = seq(0, 1, length = 30),
                 color.palette = seewave::spectro.colors,
                 xlab = 'Time, s',
                 ylab = 'Time, s',
                 main = 'Self-similarity matrix'
               ),
               noveltyPars = list(
                 type = 'b',
                 pch = 16,
                 col = 'black',
                 lwd = 3
               )) {
  ## import a sound
  if (class(x) == 'character') {
    sound = tuneR::readWave(x)
    samplingRate = sound@samp.rate
  }  else if (class(x) == 'numeric' & length(x) > 1) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    } else {
      sound = tuneR::Wave(left = x, samp.rate = samplingRate, bit = 16)
      sound = tuneR::normalize(sound, unit = '32')
    }
  }
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
  win = max(1, round(ssmWin / step / 2) * 2 - 1)

  ## set pars
  duration = length(sound) / samplingRate
  frame_points = samplingRate * windowLength / 1000
  kernelSize = round(kernelLen * samplingRate / 1000 / frame_points /
                        2) * 2  # kernel size in frames, guaranteed to be even
  if (is.null(nBands)) {
    nBands = 100 * windowLength / 20
  }
  if (is.null(step)) {
    step = windowLength / 4
  }
  if (is.null(maxFreq)) {
    maxFreq = floor(samplingRate / 2)  # Nyquist
  }

  ## compute mel-filtered spectrum and MFCCs
  mel = tuneR::melfcc(
    sound,
    wintime = windowLength / 1000,
    hoptime = step / 1000,
    maxfreq = maxFreq,
    nbands = nBands,
    spec_out = T,
    numcep = max(MFCC)
  )
  if (input == 'mfcc') {
    # the first cepstrum presumably makes no sense with amplitude normalization
    # (?), and it overestimates the similarity of different frames
    target_spec = t(mel$cepstra)[MFCC, ]
  } else if (input == 'audiogram') {
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
    win = win
  )
  # s = zeroOne(s^2)  # hist(s)

  ## compute novelty
  novelty = getNovelty(ssm = s, kernelSize = kernelSize, kernelSD = kernelSD)

  ## plot
  if (plot) {
    spec = zeroOne(log(mel$pspectrum) ^ 2)
    op = par(c('mar', 'xaxt', 'yaxt', 'mfrow')) # save user's original pars
    layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = c(2, 1))
    par(mar = c(5.1, 4.1, 0, 2.1),
        xaxt = 's',
        yaxt = 's')
    # spectrogram
    do.call(seewave::filled.contour.modif2, c(list(
      x = seq(0, duration, length.out = nrow(spec)),
      y = seq(
        0,
        (samplingRate / 2) - (samplingRate / windowLength_points),
        length.out = ncol(spec)
      ) / 1000,
      z = spec
    ), specPars
    ))

    # novelty
    do.call(lines, c(list(
      x = seq(0, duration, length.out = length(novelty)),
      y = novelty / max(novelty) * maxFreq / 1000
      ), noveltyPars
    ))
    axis(side = 1, labels = TRUE)
    par(mar = c(0, 4.1, 2.1, 2.1),
        xaxt = 'n',
        yaxt = 's')
    xlab = ''

    # SSM
    timestamps_ssm = seq(0, duration, length.out = nrow(s))
    do.call(seewave::filled.contour.modif2, c(list(
      x = timestamps_ssm,
      y = timestamps_ssm,
      z = s
    ), ssmPars
    ))
    # restore original pars
    par('mar' = op$mar, 'xaxt' = op$xaxt, 'yaxt' = op$yaxt, 'mfrow' = op$mfrow)
  }

  if (returnSSM) {
    return(list(ssm = s, novelty = novelty))
  }
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
                   win = 1) {
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
    m = apply(m, 2, zeroOne)
  }

  # calculate windows for averaging self-similarity
  numWins = ceiling(ncol(m) / win)
  winIdx = round(seq(1, ncol(m) - win, length.out = numWins))

  # calculate self-similarity
  out = matrix(NA, nrow = length(winIdx), ncol = length(winIdx))
  rownames(out) = colnames(out) = winIdx
  for (i in 1:length(winIdx)) {
    for (j in 1:length(winIdx)) {
      mi = as.vector(m[, winIdx[i]:(winIdx[i] + win - 1)])
      mj = as.vector(m[, winIdx[j]:(winIdx[j] + win - 1)])
      if (simil == 'cosine') {
        # http://stackoverflow.com/questions/6597005/cosine-similarity-between-two-vectors-in-language-r
        out[i, j] = mean(crossprod(mi, mj) /
                           sqrt(crossprod(mi) * crossprod(mj)),
                         na.rm = TRUE)
      } else if (simil == 'cor') {
        out[i, j] = mean(cor(mi, mj), na.rm = TRUE)
      }
    }
  }
  out = zeroOne(out)
  return (out)
}


#' Checkerboard kernel
#'
#' Internal soundgen function.
#'
#' Prepares a square matrix \code{size x size} specifying a gaussian kernel for measuring novelty of self-similarity matrices. Called by \code{\link{getNovelty}}
#' @param size kernel size (points), preferably an even number
#' @param kernel_mean,kernelSD mean and SD of the gaussian kernel
#' @param plot if TRUE, shows a perspective plot of the kernel
#' @return Returns a square matrix with \code{size} rows and columns.
#' @keywords internal
#' @examples
#' kernel = soundgen:::getCheckerboardKernel(size = 64, kernelSD = 0.2, plot = TRUE)
#' dim(kernel)
getCheckerboardKernel = function(size,
                                 kernel_mean = 0,
                                 kernelSD = 0.5,
                                 plot = FALSE) {
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

  # quadrant 0 to 3 o'clock
  kernel[1:(floor(size / 2)), (ceiling(size / 2) + 1):size] =
    -kernel[1:(floor(size / 2)), (ceiling(size / 2) + 1):size]
  # quadrant 6 to 9 o'clock
  kernel[(ceiling(size / 2) + 1):size, 1:(ceiling(size / 2))] =
    -kernel[(ceiling(size / 2) + 1):size, 1:(ceiling(size / 2))]

  kernel = kernel / max(kernel)
  if (plot) {
    persp (
      kernel,
      theta = -20,
      phi = 25,
      zlim = c(-1, 4),
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
#' @param kernelSize the size of gaussian kernel (points)
#' @param kernelSD the SD of gaussian kernel
#' @return Returns a numeric vector of length \code{nrow(ssm)}
#' @keywords internal
getNovelty = function(ssm, kernelSize, kernelSD) {
  kernel = getCheckerboardKernel(size = kernelSize, kernelSD = kernelSD)
  ## pad matrix with size / 2 zeros, so that we can correlate it with the
  #  kernel starting from the very edge
  ssm_padded = matrix(0,
                      nrow = nrow(ssm) + kernelSize,
                      ncol = nrow(ssm) + kernelSize)
  # indices in the padded matrix where we'll paste the original ssm
  idx = c(kernelSize / 2 + 1, nrow(ssm_padded) - kernelSize / 2)
  # paste original. Now we have a padded ssm
  ssm_padded [idx[1]:idx[2], idx[1]:idx[2]] = ssm

  ## get novelty
  novelty = rep(NA, nrow(ssm))
  # for each point on the main diagonal, novelty = correlation between the checkerboard kernel and the ssm. See Badawy, "Audio novelty-based segmentation of music concerts"
  for (i in idx[1]:idx[2]) {
    n = (i - kernelSize / 2):(i + kernelSize / 2 - 1)
    # suppress warnings, b/c otherwise cor complains of sd = 0 for silent segments
    novelty[i - kernelSize / 2] =  suppressWarnings(cor(as.vector(ssm_padded[n, n]),
                                        as.vector(kernel)))
  }
  novelty[is.na(novelty)] = 0
  return (novelty)
}
