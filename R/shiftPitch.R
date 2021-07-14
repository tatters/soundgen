#' Shift pitch
#'
#' Raises or lowers pitch with or without also shifting the formants (resonance
#' frequencies) and performing a time-stretch. The three operations (pitch
#' shift, formant shift, and time stretch) are independent and can be performed
#' in any combination. Accordingly, this function can also be used to shift
#' formants without changing pitch or duration, but the dedicated
#' \code{\link{shiftFormants}} is faster for that task.
#'
#' Algorithm: phase vocoder. Pitch shifting is accomplished by performing a time
#' stretch (at present, with horizontal phase propagation) followed by
#' resampling. This shifts both pitch and formants; to preserve the original
#' formant frequencies or modify them independently of pitch, a variant of
#' \code{link{transplantFormants}} is performed to "transplant" the original or
#' scaled formants onto the time-stretched new sound.
#'
#' @seealso \code{\link{shiftFormants}} \code{\link{transplantFormants}}
#'
#' @inheritParams spectrogram
#' @inheritParams segment
#' @inheritParams soundgen
#' @param multPitch 1 = no change, >1 = raise pitch (eg 1.1 = 10\% up, 2 =
#'   one octave up), <1 = lower pitch
#' @param multFormants 1 = no change, >1 = raise formants (eg 1.1 = 10\% up, 2 =
#'   one octave up), <1 = lower formants
#' @param timeStretch 1 = no change, >1 = longer, <1 = shorter
#' @param freqWindow the width of spectral smoothing window, Hz. Defaults to
#'   detected f0 prior to pitch shifting - see \code{\link{shiftFormants}} for
#'   discussion and examples
#' @param interpol the method for interpolating scaled spectra
#' @export
#' @examples
#' s = soundgen(sylLen = 200, ampl = c(0,-10),
#'              pitch = c(250, 350), rolloff = c(-9, -15),
#'              noise = -40,
#'              formants = 'aii', addSilence = 50)
#' # playme(s)
#' s1 = shiftPitch(s, samplingRate = 16000, freqWindow = 400,
#'                 multPitch = 1.25, multFormants = .8)
#' # playme(s1)
#'
#' \dontrun{
#' data(sheep, package = 'seewave')  # import a recording from seewave
#' playme(sheep)
#' spectrogram(sheep)
#'
#' # Raise pitch and formants by 3 semitones, shorten by half
#' sheep1 = shiftPitch(sheep, multPitch = 2 ^ (3 / 12), timeStretch = 0.5)
#' playme(sheep1, sheep@samp.rate)
#' spectrogram(sheep1, sheep@samp.rate)
#'
#' # Just shorten
#' shiftPitch(sheep, multPitch = 1, timeStretch = 0.25, play = TRUE)
#'
#' # Raise pitch preserving formants
#' sheep2 = shiftPitch(sheep, multPitch = 1.2, multFormants = 1, freqWindow = 150)
#' playme(sheep2, sheep@samp.rate)
#' spectrogram(sheep2, sheep@samp.rate)
#' }
shiftPitch = function(
  x,
  multPitch = 1,
  multFormants = multPitch,
  timeStretch = 1,
  samplingRate = NULL,
  freqWindow = NULL,
  dynamicRange = 80,
  windowLength = 50,
  step = NULL,
  overlap = 75,
  wn = 'gaussian',
  interpol = c('approx', 'spline')[1],
  propagation = c('time', 'adaptive')[1],
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
                    funToCall = '.shiftPitch',
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
#' Internal soundgen function called by \code{\link{shiftPitch}}
#' @inheritParams shiftPitch
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.shiftPitch = function(
  audio,
  multPitch = 1,
  multFormants = multPitch,
  timeStretch = 1,
  samplingRate = NULL,
  freqWindow = NULL,
  dynamicRange = 80,
  windowLength = 50,
  step = NULL,
  overlap = 75,
  wn = 'gaussian',
  interpol = c('approx', 'spline')[1],
  propagation = c('time', 'adaptive')[1],
  normalize = TRUE,
  play = FALSE) {
  if (!any((multPitch != 1) | (multFormants != 1) | (timeStretch != 1))) {
    message('Nothing to do')
    return(audio$sound)
  }

  if (!is.null(step)) {
    overlap = (1 - step / windowLength) * 100  # for istft
  }
  windowLength_points = floor(windowLength / 1000 * audio$samplingRate / 2) * 2
  if (is.null(step)) step = (1 - overlap / 100) * windowLength

  # Get a spectrogram
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
  nr = nrow(spec)
  nc = ncol(spec)
  # image(t(log(abs(spec))))
  magn = abs(spec)

  ## Time-stretching / pitch-shifting
  if (!any((multPitch != 1) | (timeStretch != 1))) {
    soundFiltered = audio$sound
    recalculateSpec = FALSE
  } else {
    recalculateSpec = TRUE  # will need a new spec of stretched/pitch-shifted sound
    bin_width = audio$samplingRate / windowLength_points
    freqs = (0:(nr - 1)) * bin_width
    step_s = step / 1000

    # unwrap the phase  - see Royer 2019, Prusa 2017
    # could do simply: magn = warpMatrix(abs(z), scaleFactor = 1 / alpha)
    # ...but then formant bandwidths change, so it's better to transplant the
    # original formants back in after pitch-shifting (or do a formant shift)
    phase_orig = phase_new = Arg(spec)
    spec1 = spec
    if (FALSE) {
      if (nc > 1) {
        for (i in 2:n) {
          dPhase =  step_s * 2 * pi * freqs +
            princarg(phase_orig[, i] - phase_orig[, i - 1] - step_s * 2 * pi * freqs)
          # plot(dPhase, type = 'l')
          phase_new[, i] = phase_new[, i - 1] + dPhase * multPitch * timeStretch
          spec1[, i] = complex(modulus = magn[, i], argument = phase_new[, i])
        }
        # spec1 = matrix(complex(modulus = magn, argument = phase_new), nrow = nrow(spec))
      }
    } else {
      multPitch = getSmoothContour(multPitch, len = nc - 1)
      timeStretch = getSmoothContour(timeStretch, len = nc - 1)
      phase_new = dPhase(phase = phase_orig,
                         magn = magn,
                         step_s = step_s,
                         freqs = freqs,
                         alpha = multPitch * timeStretch,
                         propagation = propagation)
      spec1 = matrix(complex(modulus = magn, argument = phase_new), nrow = nrow(spec))
    }

    # Reconstruct the audio
    if (TRUE) {
      step_s_new = step_s * multPitch * timeStretch
      overlap_new = 100 * (1 - step_s_new * 1000 / windowLength)
      soundFiltered =  istft_mod(   # seewave::istft(
        spec1,
        f = audio$samplingRate,
        ovlp = overlap_new,
        wl = windowLength_points,
        wn = wn,
        mult_short = multPitch,
        mult_long = getSmoothContour(multPitch, len = nc)
      )
    } else {
      step_s_new = step_s * multPitch[1] * timeStretch[1]
      overlap_new = 100 * (1 - step_s_new * 1000 / windowLength)
      soundFiltered = as.numeric(
        seewave::istft(
          spec1,
          f = audio$samplingRate,
          ovlp = overlap_new,
          wl = windowLength_points,
          wn = wn,
          output = 'matrix'
        )
      )
      # Resample
      if (any(multPitch != 1))
        soundFiltered = resample(soundFiltered, mult = 1 / multPitch)
    }

    # normalize, otherwise glitches with shifting formats
    soundFiltered = soundFiltered / max(soundFiltered) * audio$scale
    # playme(soundFiltered, audio$samplingRate)
    # spectrogram(soundFiltered, audio$samplingRate)
  }  # end of time-streching / pitch-shifting


  ## Shift formants, unless they are supposed to shift with pitch
  multFormants = getSmoothContour(multFormants, len = nc - 1)
  if (any(multFormants != multPitch)) {
    if (recalculateSpec) {
      # Get a new spectrogram of the pitch-shifted sound
      step_seq_ps = seq(1,
                        max(1, (length(soundFiltered) - windowLength_points)),
                        windowLength_points - (overlap * windowLength_points / 100))
      spec_ps = seewave::stdft(
        wave = as.matrix(soundFiltered),
        f = audio$samplingRate,
        wl = windowLength_points,
        zp = 0,
        step = step_seq_ps,
        wn = wn,
        fftw = FALSE,
        scale = TRUE,
        complex = TRUE
      )
    } else {
      # reuse the original spec
      step_seq_ps = step_seq
      spec_ps = spec
    }
    # image(t(log(abs(spec_ps))))

    # Choose the width of smoothing window for the "recipient"
    if (!is.numeric(freqWindow)) {
      anal = analyze(audio$sound, audio$samplingRate, plot = FALSE)
      freqWindow = median(anal$detailed$pitch, na.rm = TRUE)
      if (!is.numeric(freqWindow)) freqWindow = 200
    }
    freqBin_Hz = audio$samplingRate / 2 / nrow(spec_ps)
    freqWindow_bins = round(freqWindow / freqBin_Hz, 0)
    # adjust freqWindow if shifting pitch
    freqWindow_bins_adj = max(3, round(freqWindow_bins * multPitch))
    if (freqWindow_bins < 3) {
      message(paste('freqWindow has to be at least 3 bins wide;
                  resetting to', ceiling(freqBin_Hz * 3)))
      freqWindow_rec_bins = 3
    }

    # Smooth the "donor" and "recipient" spectrograms
    spec_recipient = spec_ps
    throwaway_lin = 10 ^ (-dynamicRange / 20) * audio$scale

    # Flatten the recipient spectrogram
    for (i in 1:ncol(spec_recipient)) {
      abs_s = abs(spec_recipient[, i])
      # plot(log(abs_s), type = 'l')
      env_s = flatEnv(
        abs_s, samplingRate = 1, method = 'peak',
        dynamicRange = dynamicRange,
        windowLength_points = freqWindow_bins_adj)
      idx = which(env_s > throwaway_lin)  # don't amplify very quiet sections
      if (length(idx) > 0)
        spec_recipient[idx, i] = spec_recipient[idx, i] * env_s[idx] / abs_s[idx]
      # plot(abs(spec_recipient[, i]), type = 'l')
    }

    # Make sure the donor spec has the same dimensions as the recipient spec
    spec_donor = interpolMatrix(m = magn,
                                nr = nrow(spec_recipient),
                                nc = ncol(spec_recipient))

    # Smooth the donor spectrogram
    for (i in 1:ncol(spec_donor)) {
      spec_donor[, i] = getEnv(
        sound = spec_donor[, i],
        windowLength_points = freqWindow_bins,
        method = 'peak'
      )
    }

    # Warp the donor spectrogram
    spec_donor = warpMatrix(spec_donor,
                            scaleFactor = multFormants,
                            interpol = interpol)

    # Multiply the spectrograms and reconstruct the audio
    spec_recipient_new = spec_recipient * spec_donor
    # image(log(abs(t(spec_recipient))))
    # image(log(t(spec_donor)))
    # image(log(abs(t(spec_recipient_new))))

    soundFiltered = as.numeric(
      seewave::istft(
        spec_recipient_new,
        f = samplingRate,
        ovlp = overlap,
        wl = windowLength_points,
        wn = wn,
        output = "matrix"
      )
    )
    # spectrogram(audio$sound, audio$samplingRate)
    # spectrogram(soundFiltered, audio$samplingRate)
  }

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


dPhase = function(phase,
                  magn,
                  step_s,
                  freqs,
                  alpha,
                  propagation = c('time', 'adaptive')[1],
                  tol = 10^(-6),
                  nr = nrow(phase),
                  nc = ncol(phase)) {
  ## Calculate partial derivatives of phase with respect to time and frequency
  # horizontal (dTime)
  dp_hor = dp_ver = matrix(0, nrow = nr, ncol = nc)
  for (i in 2:(nc - 1)) {
    dp_backward = step_s * 2 * pi * freqs +
      princarg(phase[, i] - phase[, i - 1] - step_s * 2 * pi * freqs)
    dp_forward =step_s * 2 * pi * freqs +
      princarg(phase[, i + 1] - phase[, i] - step_s * 2 * pi * freqs)
    dp_hor[, i] = (dp_backward + dp_forward) / 2
    # plot(dp_hor[, i], type = 'l')
  }
  # only backward for the last frame
  dp_hor[, nc] = step_s * 2 * pi * freqs +
    princarg(phase[, nc] - phase[, nc - 1] - step_s * 2 * pi * freqs)

  # vertical (dFreq)
  bin_width = freqs[2] - freqs[1]
  for (i in 2:(nr - 1)) {
    dp_down = princarg(phase[i, ] - phase[i - 1, ]) / bin_width
    dp_up = princarg(phase[i + 1, ] - phase[i, ]) / bin_width
    dp_ver[i, ] = (dp_down + dp_up) / 2
    # plot(dp_ver[, i], type = 'l')
  }
  # only down for the last bin
  dp_ver[nr, ] = princarg(phase[nr, ] - phase[nr - 1, ]) / bin_width

  ## Combine partial derivatives by propagating either horizontally or
  ## vertically, depending on magnitudes
  phase_new = phase
  if (propagation == 'time') {
    for (i in 2:nc) {
      phase_new[, i] = phase_new[, i - 1] + (dp_hor[, i - 1] + dp_hor[, i]) / 2 * alpha[i - 1]
    }
  } else if (propagation == 'adaptive') {
    for (i in 2:nc) {
      phase_new[, i] = phasePropagate(i, dp_hor = dp_hor,
                                      dp_ver = dp_ver, magn = magn,
                                      phase_new = phase_new, tol = tol,
                                      bin_width = bin_width, alpha = alpha[i - 1])
      # print(i)
    }

  }
  # image(phase_new)
  return(phase_new)
}


phasePropagate = function(i,
                          dp_hor,
                          dp_ver,
                          magn,
                          phase_new,
                          tol,
                          bin_width,
                          alpha) {
  bin_width_new = alpha * bin_width
  out = runif(nrow(dp_hor), -pi, pi)
  abstol = tol * max(magn[, c(i, i - 1)])
  bins_cur = which(magn[, i] > abstol)
  if (!length(bins_cur) > 0) return(out)
  maxHeap = data.frame(frame = i - 1, bin = bins_cur, magn = magn[bins_cur, i - 1])
  while(length(bins_cur) > 0) {
    row_max = which.max(maxHeap$magn)
    top = maxHeap[row_max, ]
    maxHeap = maxHeap[-row_max, ]
    h = top$bin  # bin at top of the heap
    if (top$frame == i - 1) {
      if (h %in% bins_cur) {
        out[h] = phase_new[h, i - 1] + alpha * (dp_hor[h, i - 1] + dp_hor[h, i]) / 2
        bins_cur = bins_cur[bins_cur != h]
        maxHeap = rbind(maxHeap, data.frame(
          frame = i, bin = h, magn = magn[h, i]
        ))
      }
    } else if (top$frame == i) {
      if ((h + 1) %in% bins_cur) {
        out[h + 1] = out[h] + bin_width_new * (dp_ver[h] + dp_ver[h + 1]) / 2
        bins_cur = bins_cur[bins_cur != (h + 1)]
        maxHeap = rbind(maxHeap, data.frame(
          frame = i, bin = h + 1, magn = magn[h + 1, i]
        ))
      }
      if ((h - 1) %in% bins_cur) {
        out[h - 1] = out[h] - bin_width_new * (dp_ver[h - 1] + dp_ver[h]) / 2
        bins_cur = bins_cur[bins_cur != (h - 1)]
        maxHeap = rbind(maxHeap, data.frame(
          frame = i, bin = h - 1, magn = magn[h - 1, i]
        ))
      }
    }
    # print(length(bins_cur))
  }
  return(out)
}


istft_mod = function (stft, f, wl, ovlp = 75, wn = "hanning", mult_short = 1, mult_long = 1) {
  if (!is.complex(stft))
    stop("The object stft should be of complex mode (ie Re + Im.")
  h <- wl * (100 - ovlp)/100 / mult_short
  coln <- ncol(stft)
  # xlen <- wl + (coln - 1) * h
  # x <- numeric(xlen)
  xlen <- ceiling(wl / min(mult_long)) + sum(h)
  x <- rep(0, xlen)
  start_seq = c(0, cumsum(h))
  for (i in 1:length(start_seq)) {
    b = start_seq[i]
    X <- stft[, i]
    mirror <- rev(X[-1])
    mirror <- complex(real = Re(mirror), imaginary = -Im(mirror))
    X <- c(X, complex(real = Re(X[length(X)]), imaginary = 0),
           mirror)
    xprim <- Re(fft(X, inverse = TRUE)/length(X))
    if (any(mult_long != 1))
      xprim = resample(xprim, mult = 1 / mult_long[i])
    len_xprim = length(xprim)
    win <- seewave:::ftwindow(wl = len_xprim, wn = wn)
    idx = (b + 1) : (b + len_xprim )
    x[idx] = x[idx] + xprim * win
    # x <- addVectors(x, xprim * win, insertionPoint = b + 1, normalize = FALSE)
  }
  W0 <- sum(win^2)
  x <- x * mean(h)/W0
  return(x)
}

