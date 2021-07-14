#' Shift pitch
#'
#' Raises or lowers pitch with or without also shifting the formants (resonance
#' frequencies) and performing a time-stretch. The three operations (pitch
#' shift, formant shift, and time stretch) are independent and can be performed
#' in any combination, statically or dynamically. \code{shiftPitch} can also be
#' used to shift formants without changing pitch or duration, but the dedicated
#' \code{\link{shiftFormants}} is faster for that task.
#'
#' Algorithm: phase vocoder. Pitch shifting is accomplished by performing a time
#' stretch (at present, with horizontal or adaptive phase propagation) followed
#' by resampling. This shifts both pitch and formants; to preserve the original
#' formant frequencies or modify them independently of pitch, a variant of
#' \code{link{transplantFormants}} is performed to "transplant" the original or
#' scaled formants onto the time-stretched new sound.
#'
#' @seealso \code{\link{shiftFormants}} \code{\link{transplantFormants}}
#'
#' @inheritParams spectrogram
#' @inheritParams segment
#' @inheritParams soundgen
#' @param multPitch 1 = no change, >1 = raise pitch (eg 1.1 = 10\% up, 2 = one
#'   octave up), <1 = lower pitch. Anchor format accepted for multPitch /
#'   multFormant / timeStretch (see \code{\link{soundgen}})
#' @param multFormants 1 = no change, >1 = raise formants (eg 1.1 = 10\% up, 2 =
#'   one octave up), <1 = lower formants
#' @param timeStretch 1 = no change, >1 = longer, <1 = shorter
#' @param freqWindow the width of spectral smoothing window, Hz. Defaults to
#'   detected f0 prior to pitch shifting - see \code{\link{shiftFormants}} for
#'   discussion and examples
#' @param propagation the method for propagating phase: "time" = horizontal
#'   propagation (default), "adaptive" = an experimental implementation of
#'   "vocoder done right" (Prusa & Holighaus 2017)
#' @param interpol the method for interpolating scaled spectra and anchors
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
#' ## Dynamic manipulations
#' # Add a chevron-shaped pitch contour
#' s2 = shiftPitch(s, samplingRate = 16000, multPitch = c(1.1, 1.3, .8))
#' playme(s2)
#'
#' # Time-stretch only the middle
#' s3 = shiftPitch(s, samplingRate = 16000, timeStretch = list(
#'   time = c(0, .25, .31, .5, .55, 1),
#'   value = c(1, 1, 3, 3, 1, 1))
#' )
#' playme(s3)
#'
#'
#' ## Various combinations of 3 manipulations
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
  windowLength = 40,
  step = 2,
  overlap = NULL,
  wn = 'gaussian',
  interpol = c('approx', 'spline')[1],
  propagation = c('time', 'adaptive')[1],
  preserveEnv = NULL,
  transplantEnv_pars = list(windowLength = 10),
  normalize = TRUE,
  play = FALSE,
  saveAudio = NULL,
  reportEvery = NULL,
  ...) {
  multPitch = reformatAnchors(multPitch)
  multFormants = reformatAnchors(multFormants)
  timeStretch = reformatAnchors(timeStretch)

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
  multPitch,
  multFormants,
  timeStretch,
  samplingRate = NULL,
  freqWindow = NULL,
  dynamicRange = 80,
  windowLength = 50,
  step = NULL,
  overlap = 75,
  wn = 'gaussian',
  interpol = c('approx', 'spline')[1],
  propagation = c('time', 'adaptive')[1],
  preserveEnv = NULL,
  transplantEnv_pars = list(),
  normalize = TRUE,
  play = FALSE) {
  if (!(any(multPitch$value != 1) |
        any(multFormants$value != 1) |
        any(timeStretch$value != 1))) {
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
  if (!(any(multPitch$value != 1) | any(timeStretch$value != 1))) {
    soundFiltered = audio$sound
    recalculateSpec = FALSE
  } else {
    recalculateSpec = TRUE  # will need a new spec of stretched/pitch-shifted sound
    bin_width = audio$samplingRate / windowLength_points
    freqs = (0:(nr - 1)) * bin_width
    step_s = step / 1000

    # unwrap the phase, calculate instantaneous frequency - see Prusa 2017, Royer 2019
    # could do simply: magn = warpMatrix(abs(z), scaleFactor = 1 / alpha)
    # ...but then formant bandwidths change, so it's better to transplant the
    # original formants back in after pitch-shifting (or do a formant shift)
    phase_orig = phase_new = Arg(spec)
    spec1 = spec
    multPitch_vector = getSmoothContour(multPitch,
                                        len = nc - 1,
                                        interpol = interpol)
    timeStretch_vector = getSmoothContour(timeStretch,
                                          len = nc - 1,
                                          interpol = interpol)
    phase_new = dPhase(phase = phase_orig,
                       magn = magn,
                       step_s = step_s,
                       freqs = freqs,
                       alpha = multPitch_vector * timeStretch_vector,
                       propagation = propagation)
    spec1 = matrix(complex(modulus = magn, argument = phase_new),
                   nrow = nrow(spec))

    # Reconstruct the audio
    if (any(diff(multPitch$value) != 0) |
        any(diff(timeStretch$value) != 0)) {
      # dynamic
      step_s_new = step_s * multPitch_vector * timeStretch_vector
      overlap_new = 100 * (1 - step_s_new * 1000 / windowLength)
      soundFiltered =  istft_mod(   # instead of seewave::istft(
        spec1,
        f = audio$samplingRate,
        ovlp = overlap_new,
        wl = windowLength_points,
        wn = wn,
        mult_short = multPitch_vector,
        mult_long = getSmoothContour(multPitch, len = nc)
      )
    } else {
      # static - shortcut
      step_s_new = step_s * multPitch_vector[1] * timeStretch_vector[1]
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
      if (multPitch_vector[1] != 1)
        soundFiltered = resample(soundFiltered, mult = 1 / multPitch_vector[1])
    }

    # normalize, otherwise glitches with shifting formats
    soundFiltered = soundFiltered / max(soundFiltered) * audio$scale
    # playme(soundFiltered, audio$samplingRate)
    # spectrogram(soundFiltered, audio$samplingRate)
  }  # end of time-streching / pitch-shifting


  ## Shift formants, unless they are supposed to shift with pitch
  if ((nrow(multFormants) != nrow(multPitch)) ||
      any(multFormants$value != multPitch$value)) {
    multFormants_vector = getSmoothContour(multFormants, len = nc - 1)
    # for some weird reason, .shiftFormants() emphasizes low freqs (???), so
    # calling top function shiftFormants()
    soundFiltered = shiftFormants(
      soundFiltered,
      samplingRate = audio$samplingRate,
      multFormants = multFormants_vector / multPitch_vector,
      freqWindow = freqWindow,
      windowLength = windowLength,
      step = step,
      wn = wn,
      interpol = interpol,
      dynamicRange = dynamicRange,
      play = FALSE,
      normalize = normalize
    )
    # spectrogram(audio$sound, audio$samplingRate)
    # spectrogram(soundFiltered, audio$samplingRate)
  }

  # Transplant amplitude envelope from the original
  if (is.null(preserveEnv)) {
    if (any(diff(timeStretch$value, na.rm = TRUE) != 0)) {
      preserveEnv = FALSE
    } else {
      preserveEnv = TRUE
    }
  }
  if (preserveEnv) {
    soundFiltered = do.call(transplantEnv, c(
      transplantEnv_pars,
      list(
        donor = audio$sound,
        samplingRateD = audio$samplingRate,
        recipient = soundFiltered
      )
    ))
  }

  # Play, save, return
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


#' Phase derivatives
#'
#' Internal soundgen function called by pitchShift().
#' @inheritParams shiftPitch
#' @param phase,magn phase and magnitude of a spectrogram
#' @param step_s step in s
#' @param freqs a vector of central frequencies per bin
#' @param alpha stretch factor
#' @param tol tolerance of "vocoder done right" algorithm
#' @param nr,nc dimensions of input spectrogram
#' @keywords internal
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


#' Propagate phase
#'
#' Internal soundgen function called by dPhase(). Propagates phase using the
#' "vocoder done right" algorithm, as in Prusa & Holighaus 2017 "Phase vocoder
#' done right".
#' @inheritParams dPhase
#' @param i analyzed frame
#' @param dp_hor,dp_ver time and frequency partical derivatives of phase
#' @param phase_new matrix for storing the new phase
#' @param bin_width width of frequency bin, Hz
#' @keywords internal
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


#' Modified istft
#'
#' Internal soundgen function. Similar to seewave:::istft(), but adapted to work
#' with time-variable step sizes in the context of dynamic pitch shifting. Only
#' call it for dynamic istft because the original seewave function should be a
#' bit faster for static.
#' @param stft,f,wl,ovlp,wn see seewave:::istft()
#' @param mult_short stretch factor of length \code{ncol(stft) - 1}
#' @param mult_long stretch factor of length \code{ncol(stft)}
istft_mod = function (stft, f, wl, ovlp = 75, wn = "hanning",
                      mult_short = 1,
                      mult_long = 1) {
  if (!is.complex(stft))
    stop("The object stft should be of complex mode (ie Re + Im.")
  h = wl * (100 - ovlp)/100 / mult_short
  coln = ncol(stft)
  xlen = ceiling(wl / min(mult_long)) + sum(h)
  x = rep(0, xlen)
  start_seq = c(0, cumsum(h))
  for (i in 1:length(start_seq)) {
    b = start_seq[i]
    X = stft[, i]
    mirror = rev(X[-1])
    mirror = complex(real = Re(mirror), imaginary = -Im(mirror))
    X = c(X, complex(real = Re(X[length(X)]), imaginary = 0), mirror)
    xprim = Re(fft(X, inverse = TRUE)/length(X))
    if (any(mult_long != 1))
      xprim = resample(xprim, mult = 1 / mult_long[i])
    len_xprim = length(xprim)
    win = seewave::ftwindow(wl = len_xprim, wn = wn)
    idx = (b + 1) : (b + len_xprim )
    x[idx] = x[idx] + xprim * win
    # x <- addVectors(x, xprim * win, insertionPoint = b + 1, normalize = FALSE)
  }
  W0 = sum(win^2)
  x = x * mean(h)/W0
  return(x)
}

