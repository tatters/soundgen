#' Invert spectrogram
#'
#' Transforms a spectrogram into a time series with inverse STFT. The problem is
#' that an ordinary spectrogram preserves only the magnitude (modulus) of the
#' complex STFT, while the phase is lost, and without phase it is impossible to
#' reconstruct the original audio accurately. So there are a number of
#' algorithms for "guessing" the phase that would produce an audio whose
#' magnitude spectrogram is very similar to the target spectrogram. Useful for
#' certain filtering operations that modify the magnitude spectrogram followed
#' by inverse STFT, such as filtering in the spectrotemporal modulation domain.
#'
#' Algorithm: takes the spectrogram, makes an initial guess at the phase (zero,
#' noise, or a more intelligent estimate by the SPSI algorithm), fine-tunes over
#' `nIter` iterations with the GL algorithm, reconstructs the complex
#' spectrogram using the best phase estimate, and performs inverse STFT. The
#' single-pass spectrogram inversion (SPSI) algorithm is implemented as
#' described in Beauregard et al. (2015) following the python code at
#' https://github.com/lonce/SPSI_Python. The Griffin-Lim (GL) algorithm is based
#' on Griffin & Lim (1984).
#'
#' @seealso \code{\link{spectrogram}} \code{\link{invertModulationSpectrum}}
#'
#' @references \itemize{
#'   \item Griffin, D., & Lim, J. (1984). Signal estimation from modified
#'   short-time Fourier transform. IEEE Transactions on Acoustics, Speech, and
#'   Signal Processing, 32(2), 236-243.
#'   \item Beauregard, G. T., Harish, M., & Wyse, L. (2015, July). Single pass
#'   spectrogram inversion. In 2015 IEEE International Conference on Digital
#'   Signal Processing (DSP) (pp. 427-431). IEEE.
#' }
#' @return Returns the reconstructed audio as a numeric vector.
#' @param spec the spectrogram that is to be transform to a time series: numeric
#'   matrix with frequency bins in rows and time frames in columns
#' @inheritParams spectrogram
#' @param specType the scale of target spectroram: 'abs' = absolute, 'log' =
#'   log-transformed, 'dB' = in decibels
#' @param initialPhase initial phase estimate: "zero" = set all phases to zero;
#'   "random" = Gaussian noise; "spsi" (default) = single-pass spectrogram
#'   inversion (Beauregard et al., 2015)
#' @param nIter the number of iterations of the GL algorithm (Griffin & Lim,
#'   1984), 0 = don't run
#' @param normalize if TRUE, normalizes the output to range from -1 to +1
#' @param play if TRUE, plays back the reconstructed audio
#' @param verbose if TRUE, prints estimated time left every 10\% of GL
#'   iterations
#' @param plotError if TRUE, produces a scree plot of squared error over GL
#'   iterations (useful for choosing `nIter`)
#' @export
#' @examples
#' # Create a spectrogram
#' samplingRate = 16000
#' windowLength = 40
#' overlap = 75
#' wn = 'hanning'
#'
#' s = soundgen(samplingRate = samplingRate, addSilence = 100)
#' spec = spectrogram(s, samplingRate = samplingRate,
#'   wn = wn, windowLength = windowLength, overlap = overlap,
#'   padWithSilence = FALSE, output = 'original')
#'
#' # Invert the spectrogram, attempting to guess the phase
#' # Note that samplingRate, wn, windowLength, and overlap must be the same as
#' # in the original (ie you have to know how the spectrogram was created)
#' s_new = invertSpectrogram(spec, samplingRate = samplingRate,
#'   windowLength = windowLength, overlap = overlap, wn = wn,
#'   initialPhase = 'spsi', nIter = 25, specType = 'abs')
#'
#' # Verify the quality of audio reconstruction
#' playme(s, samplingRate); playme(s_new, samplingRate)
#' spectrogram(s, samplingRate, osc = TRUE)
#' spectrogram(s_new, samplingRate, osc = TRUE)
invertSpectrogram = function(spec,
                             samplingRate,
                             windowLength,
                             overlap,
                             step = NULL,
                             wn = 'hanning',
                             specType = c('abs', 'log', 'dB')[1],
                             initialPhase = c('zero', 'random', 'spsi')[3],
                             nIter = 10,
                             normalize = TRUE,
                             play = TRUE,
                             verbose = FALSE,
                             plotError = TRUE) {
  nr = nrow(spec)  # frequency bins
  nc = ncol(spec)  # time frames
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  step_points = step / 1000 * samplingRate
  windowLength_points = windowLength / 1000 * samplingRate

  # Rescale the spectrogram
  if (specType == 'log') {
    spec = exp(spec)
  } else if (specType == 'dB') {
    spec = 10 ^ (spec / 20)
  }

  # Set the phase to zero / Gaussian noise / an intelligent guess
  if (initialPhase == 'zero') {
    phase_init = matrix(0, nrow = nr, ncol = nc)
  } else if (initialPhase == 'random') {
    phase_init = matrix(rnorm(nr * nc), nrow = nr, ncol = nc)
  } else if (initialPhase == 'spsi') {
    phase_init = guessPhase_spsi(spec = spec,
                                 windowLength_points = windowLength_points,
                                 step_points = step_points)
  } else {
    stop("initialPhase can be 'zero', 'random', or 'spsi'")
  }

  # Fine-tune the phase with an iterative algorithm
  if (nIter > 0) {
    gl = guessPhase_GL(spec = spec,
                       phase = phase_init,
                       nIter = nIter,
                       samplingRate = samplingRate,
                       windowLength_points = windowLength_points,
                       step_points = step_points,
                       overlap = overlap,
                       wn = wn,
                       verbose = verbose,
                       plotError = plotError)
    phase = gl$phase
  } else {
    phase = phase_init
  }

  # Inverse STFT
  spec_complex = spec * exp(1i * phase)
  out = as.numeric(seewave::istft(spec_complex,
                                   f = samplingRate,
                                   ovlp = overlap,
                                   wn = wn,
                                   wl = windowLength_points,
                                   output = 'matrix'))
  if (normalize) out = out / max(abs(out))

  # Play the reconstructed audio
  if (play == TRUE) {
    playme(out, samplingRate = samplingRate)
  }
  if (is.character(play)) {
    playme(out, samplingRate = samplingRate, player = play)
  }

  invisible(out)
}


#' Guess phase SPSI
#'
#' Internal soundgen function.
#'
#' Single-pass spectrogram inversion as described in Beauregard, G. T., Harish,
#' M., & Wyse, L. (2015, July). Single pass spectrogram inversion. In 2015 IEEE
#' International Conference on Digital Signal Processing (DSP) (pp. 427-431).
#' IEEE. See \code{\link{invertSpectrogram}} for details.
#' @inheritParams invertSpectrogram
#' @return Returns a matrix of the same dimensions as `spec` containing the
#'   guessed phase.
#' @param windowLength_points STFT window length in points
#' @param step_points STFT step in points
#' @keywords internal
guessPhase_spsi = function(spec,
                           windowLength_points,
                           step_points) {
  nr = nrow(spec)  # frequency bins
  nc = ncol(spec)  # time frames
  phaseAccumulator = rep(0, nr)
  phase = matrix(0, nrow = nr, ncol = nc)

  for (i in 1:nc) {
    spec_i = spec[, i]  # magnitude spectrum of frame i
    # plot(spec_i, type = 'b')
    for (j in 2:(nr - 1)) {  # for all freq bins except firts and last
      is_peak = spec_i[j] > spec_i[j - 1] & spec_i[j] > spec_i[j + 1]
      if (is_peak) {
        # find the true freq of the peak (may be b/w bins)
        alpha = spec_i[j - 1]
        beta = spec_i[j]
        gamma = spec_i[j + 1]
        denom = alpha - 2 * beta + gamma
        if(denom != 0) {
          p = 0.5 * (alpha - gamma) / denom
        } else {
          p = 0
        }
        # p is the estimated true peak frequency
        adjustedPhaseRate = 2 * pi * (j + p) / windowLength_points
        phaseAccumulator[j] = phaseAccumulator[j] + step_points * adjustedPhaseRate
        peakPhase = phaseAccumulator[j]

        if (p > 0) {  # If actual peak is to the right of the bin freq
          # First bin to the right has pi shift
          phaseAccumulator[j + 1] = peakPhase + pi

          # Bins to the left have shift of pi
          bin = j - 1
          while ((bin > 2) && (spec_i[bin] < spec_i[bin + 1])) {
            # until the first trough
            phaseAccumulator[bin] = peakPhase + pi
            bin = bin - 1
          }

          # Bins to the right (beyond the first) have 0 shift
          bin = j + 2
          while ((bin < nr) && (spec_i[bin] < spec_i[bin - 1])) {
            phaseAccumulator[bin] = peakPhase
            bin = bin + 1
          }
        } else if (p < 0) {
          # If actual peak is to the left of the bin frequency
          # First bin to left has pi shift
          phaseAccumulator[j - 1] = peakPhase + pi

          # same for bins to the right
          bin = j + 1
          while ((bin < nr) && (spec_i[bin] < spec_i[bin - 1])) {
            phaseAccumulator[bin] = peakPhase + pi
            bin = bin + 1
          }

          # Bins further to the left have zero shift
          bin = j - 2
          while ((bin > 2) && (spec_i[bin] < spec_i[bin + 1])) {  # until trough
            phaseAccumulator[bin] = peakPhase
            bin = bin - 1
          }
        }
      }
    }

    spec_complex = spec_i * exp(1i * phaseAccumulator)  # Euler's formula
    spec_complex[c(1, nr)] = 0  #  remove dc and nyquist
    phase[, i] = Arg(spec_complex)
  }
  return(phase)
}


#' Guess phase GL
#'
#' Internal soundgen function
#'
#' Uses the iterative method proposed by Griffin & Lim (1984) to guess the phase
#' of a magnitude spectrogram.
#' @inheritParams invertSpectrogram
#' @inheritParams guessPhase_spsi
#' @param phase an initial guess at the phase: numeric matrix of the same dimensions as spec
#' @return Returns a matrix of the same dimensions as `spec` containing the
#'   guessed phase.
#' @keywords internal
guessPhase_GL = function(spec,
                         phase,
                         nIter,
                         samplingRate,
                         overlap,
                         wn,
                         windowLength_points,
                         step_points,
                         verbose = TRUE,
                         plotError = TRUE) {
  step_seq = NULL
  squareError = rep(NA, nIter)
  time_start = proc.time()

  for (i in 1:nIter) {
    # Euler's formula - get complex spectrum from magnitude and phase
    spec_complex = spec * exp(1i * phase)
    # Inverse STFT to a candidate time series
    x = seewave::istft(spec_complex,
                        f = samplingRate,
                        ovlp = overlap,
                        wn = wn,
                        wl = windowLength_points,
                        output = 'matrix')

    # STFT of the candidate time series
    if (is.null(step_seq)) {
      step_seq = seq(1, length(x) + 1 - windowLength_points, step_points)
    }
    spec_new = seewave::stdft(wave = as.matrix(x),
                               wn = wn,
                               wl = windowLength_points,
                               f = samplingRate,
                               zp = 0,
                               step = step_seq,
                               scale = TRUE,
                               norm = FALSE,
                               complex = TRUE)

    # Set phase to that of the new spectrogram
    phase = Arg(spec_new)

    # Calculate error
    if (plotError) {
      squareError[i] = log(sum((abs(spec_new) - spec) ^ 2))
    }

    # Report progress
    if (verbose) {
      reportTime(i = i, nIter = nIter,
                 time_start = time_start, reportEvery = round(i / 10))
    }
  }
  # Plot square errors per iteration
  if (plotError) {
    plot(1:nIter, squareError, type = 'b',
         xlab = 'GL iteration', ylab = 'Square error',
         main = 'Improvement in fit')
  }
  return(list(phase = phase, squareError = squareError))
}
