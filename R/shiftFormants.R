#' Shift formants
#'
#' Raises or lowers formants (resonance frequencies), changing the voice quality
#' or timbre of the sound without changing its pitch. Note that this is only
#' possible when the fundamental frequency f0 is lower than the formant
#' frequencies. For best results, \code{freqWindow} should be no lower than f0
#' and no higher than formant bandwidths. Obviously, this is impossible for many
#' signals, so just try a few reasonable values, like ~200 Hz for speech. If
#' \code{freqWindow} is not specified, soundgen sets it to the average detected
#' f0, which is slow.
#'
#' Algorithm: phase vocoder. In the frequency domain, we separate the complex
#' spectrum of each STFT frame into two parts. The "receiver" is the flattened
#' or smoothed complex spectrum, where smoothing is achieved by obtaining a
#' smoothed magnitude envelope (the amount of smoothing is controlled by
#' \code{freqWindow}) and then dividing the complex spectrum by this envelope.
#' This basically removes the formants from the signal. The second component,
#' "donor", is a scaled and interpolated version of the same smoothed magnitude
#' envelope as above - these are the formants shifted up or down. Warping can be
#' easily implemented instead of simple scaling if nonlinear spectral
#' transformations are required. We then multiply the "receiver" and "donor"
#' spectrograms and reconstruct the audio with iSTFT.
#'
#' @seealso \code{\link{shiftPitch}} \code{\link{transplantFormants}}
#'
#' @inheritParams spectrogram
#' @inheritParams segment
#' @inheritParams soundgen
#' @param multFormants 1 = no change, >1 = raise formants (eg 1.1 = 10\% up, 2 =
#'   one octave up), <1 = lower formants
#' @param freqWindow the width of spectral smoothing window, Hz. Defaults to
#'   detected f0
#' @param interpol the method for interpolating scaled spectra
#' @export
#' @examples
#' s = soundgen(sylLen = 200, ampl = c(0,-10),
#'              pitch = c(250, 350), rolloff = c(-9, -15),
#'              noise = -40,
#'              formants = 'aii', addSilence = 50)
#' # playme(s)
#' s1 = shiftFormants(s, samplingRate = 16000, multFormants = 1.25,
#'                    freqWindow = 200)
#' # playme(s1)
#'
#' \dontrun{
#' data(sheep, package = 'seewave')  # import a recording from seewave
#' playme(sheep)
#' spectrogram(sheep)
#'
#' # Lower formants by 4 semitones or ~20% = 2 ^ (-4 / 12)
#' sheep1 = shiftFormants(sheep, multFormants = 2 ^ (-4 / 12), freqWindow = 150)
#' playme(sheep1, sheep@samp.rate)
#' spectrogram(sheep1, sheep@samp.rate)
#'
#' orig = seewave::meanspec(sheep, wl = 128, plot = FALSE)
#' shifted = seewave::meanspec(sheep1, wl = 128, f = sheep@samp.rate, plot = FALSE)
#' plot(orig[, 1], log(orig[, 2]), type = 'l')
#' points(shifted[, 1], log(shifted[, 2]), type = 'l', col = 'blue')
#'
#' # dynamic change: raise formants at the beginning, lower at the end
#' sheep2 = shiftFormants(sheep, multFormants = c(1.3, .7), freqWindow = 150)
#' playme(sheep2, sheep@samp.rate)
#' spectrogram(sheep2, sheep@samp.rate)
#' }
shiftFormants = function(
  x,
  multFormants,
  samplingRate = NULL,
  freqWindow = NULL,
  dynamicRange = 80,
  windowLength = 50,
  step = NULL,
  overlap = 75,
  wn = 'gaussian',
  interpol = c('approx', 'spline')[1],
  normalize = TRUE,
  play = FALSE,
  saveAudio = NULL,
  reportEvery = NULL,
  ...) {
  # match args
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'saveAudio')]

  pa = processAudio(x,
                    samplingRate = samplingRate,
                    saveAudio = saveAudio,
                    funToCall = '.shiftFormants',
                    myPars = myPars,
                    reportEvery = reportEvery
  )
  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Shift formants per sound
#'
#' Internal soundgen function called by \code{\link{shiftFormants}}
#' @inheritParams shiftFormants
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.shiftFormants = function(
  audio,
  multFormants,
  samplingRate = NULL,
  freqWindow = NULL,
  dynamicRange = 80,
  windowLength = 50,
  step = NULL,
  overlap = 75,
  wn = 'gaussian',
  interpol = c('approx', 'spline')[1],
  normalize = TRUE,
  play = FALSE) {
  if (!is.null(step)) {
    overlap = (1 - step / windowLength) * 100  # for istft
  }
  windowLength_points = floor(windowLength / 1000 * audio$samplingRate / 2) * 2

  # Get a spectrogram (use seewave to avoid inconsistent dims when running iSTFT)
  step_seq = seq(1,
                 max(1, (audio$ls - windowLength_points)),
                 windowLength_points - (overlap * windowLength_points / 100))
  spec = seewave::stdft(
    wave = as.matrix(audio$sound),
    f = audio$samplingRate,
    wl = windowLength_points,
    zp = 0,
    step = step_seq,
    wn = wn,
    fftw = FALSE,
    scale = TRUE,
    complex = TRUE
  )
  # image(t(log(abs(spec))))

  # Choose the width of smoothing window for the "recipient"
  if (!is.numeric(freqWindow)) {
    anal = analyze(audio$sound, audio$samplingRate, plot = FALSE)
    freqWindow = median(anal$detailed$pitch, na.rm = TRUE)
  }
  freqRange_kHz = audio$samplingRate / 2 / 1000 # diff(range(as.numeric(rownames(spec))))
  freqBin_Hz = freqRange_kHz * 1000 / nrow(spec)
  freqWindow_bins = round(freqWindow / freqBin_Hz, 0)
  if (freqWindow_bins < 3) {
    message(paste('freqWindow has to be at least 3 bins wide;
                  resetting to', ceiling(freqBin_Hz * 3)))
    freqWindow_rec_bins = 3
  }

  # Smooth the "donor" and "recipient" spectrograms
  spec_recipient = spec
  spec_donor = abs(spec)
  throwaway_lin = 10 ^ (-dynamicRange / 20) * audio$scale
  for (i in 1:ncol(spec)) {
    abs_s = abs(spec[, i])
    # plot(log(abs_s), type = 'l')

    # Smooth the "donor" spectrogram of formants (magnitude only)
    env_s = getEnv(
      sound = abs_s,
      windowLength_points = freqWindow_bins,
      method = 'peak'
    )
    spec_donor[, i] = env_s
    # plot(log(abs(spec[, i])), type = 'l')
    # points(log(spec_donor[, i]), type = 'l', col = 'blue')

    # Smooth the "recipient" spectrogram (complex numbers)
    idx = which(env_s > throwaway_lin)  # don't amplify very quiet sections
    spec_recipient[idx, i] = spec_recipient[idx, i] / env_s[idx]
    # plot(abs(spec_recipient[, i]), type = 'l')
  }
  # image(t(log(spec_donor)))
  # image(t(abs(spec_recipient)))

  # Warp the "donor" spectrogram
  spec_donor = warpMatrix(spec_donor,
                          scaleFactor = multFormants,
                          interpol = interpol)
  # image(t(log(abs(spec_donor))))

  # Multiply the spectrograms
  spec_new = spec_recipient * spec_donor
  # image(t(log(abs(spec_new))))

  # Reconstruct the audio
  soundFiltered = as.numeric(
    seewave::istft(
      spec_new,
      f = samplingRate,
      ovlp = overlap,
      wl = windowLength_points,
      wn = wn,
      output = "matrix"
    )
  )
  # spectrogram(audio$sound, audio$samplingRate)
  # spectrogram(soundFiltered, audio$samplingRate)

  # postprocessing
  if (normalize) {
    soundFiltered = soundFiltered - mean(soundFiltered)
    soundFiltered = soundFiltered / max(abs(soundFiltered))
  }
  if (play == TRUE) {
    playme(soundFiltered, samplingRate = audio$samplingRate)
  }
  if (is.character(play)) {
    playme(soundFiltered, samplingRate = audio$samplingRate, player = play)
  }
  if (is.character(audio$saveAudio)) {
    seewave::savewav(
      soundFiltered, f = audio$samplingRate,
      filename = paste0(audio$saveAudio, audio$filename_noExt, '.wav'))
  }

  return(soundFiltered)
}
