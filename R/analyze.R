### MAIN FUNCTIONS FOR ACOUSTIC ANALYSIS ###

#' Analyze sound
#'
#' Acoustic analysis of a single sound file: pitch tracking, basic spectral
#' characteristics, and estimated loudness (see \code{\link{getLoudness}}). The
#' default values of arguments are optimized for human non-linguistic
#' vocalizations. See vignette('acoustic_analysis', package = 'soundgen') for
#' details.
#'
#' @inheritParams spectrogram
#' @inheritParams getLoudness
#' @param silence (0 to 1) frames with RMS amplitude below silence threshold are
#'   not analyzed at all. NB: this number is dynamically updated: the actual
#'   silence threshold may be higher depending on the quietest frame, but it
#'   will never be lower than this specified number.
#' @param scale maximum possible amplitude of input used for normalization of
#'   input vector (not needed if input is an audio file)
#' @param SPL_measured sound pressure level at which the sound is presented, dB
#'   (set to 0 to skip analyzing subjective loudness)
#' @param cutFreq (2 * pitchCeiling to Nyquist, Hz) repeat the calculation of
#'   spectral descriptives after discarding all info above \code{cutFreq}.
#'   Recommended if the original sampling rate varies across different analyzed
#'   audio files. Note that "entropyThres" applies only to this frequency range,
#'   which also affects which frames will not be analyzed with pitchAutocor.
#' @param nFormants the number of formants to extract per STFT frame (0 = no
#'   formant analysis). Calls \code{\link[phonTools]{findformants}} with default
#'   settings
#' @param pitchMethods methods of pitch estimation to consider for determining
#'   pitch contour: 'autocor' = autocorrelation (~PRAAT), 'cep' = cepstral,
#'   'spec' = spectral (~BaNa), 'dom' = lowest dominant frequency band ('' or
#'   NULL = no pitch analysis)
#' @param entropyThres pitch tracking is not performed for frames with Weiner
#'   entropy above \code{entropyThres}, but other spectral descriptives are
#'   still calculated
#' @param pitchFloor,pitchCeiling absolute bounds for pitch candidates (Hz)
#' @param priorMean,priorSD specifies the mean (Hz) and standard deviation
#'   (semitones) of gamma distribution describing our prior knowledge about the
#'   most likely pitch values for this file. For ex., \code{priorMean = 300,
#'   priorSD = 6} gives a prior with mean = 300 Hz and SD = 6 semitones (half
#'   an octave)
#' @param priorPlot deprecated; use \code{\link{getPrior}} to visualize the
#'   prior
#' @param nCands maximum number of pitch candidates per method (except for
#'   \code{dom}, which returns at most one candidate per frame), normally 1...4
#' @param minVoicedCands minimum number of pitch candidates that have to be
#'   defined to consider a frame voiced (if NULL, defaults to 2 if \code{dom} is
#'   among other candidates and 1 otherwise)
#' @param domThres (0 to 1) to find the lowest dominant frequency band, we
#'   do short-term FFT and take the lowest frequency with amplitude at least
#'   domThres
#' @param domSmooth the width of smoothing interval (Hz) for finding
#'   \code{dom}
#' @param autocorThres,cepThres,specThres (0 to 1) separate
#'   voicing thresholds for detecting pitch candidates with three different
#'   methods: autocorrelation, cepstrum, and BaNa algorithm (see Details). Note
#'   that HNR is calculated even for unvoiced frames.
#' @param autocorSmooth the width of smoothing interval (in bins) for
#'   finding peaks in the autocorrelation function. Defaults to 7 for sampling
#'   rate 44100 and smaller odd numbers for lower values of sampling rate
#' @param cepSmooth the width of smoothing interval (Hz) for finding peaks in
#'   the cepstrum
#' @param cepZp zero-padding of the spectrum used for cepstral pitch detection
#'   (final length of spectrum after zero-padding in points, e.g. 2 ^ 13)
#' @param specPeak,specHNRslope when looking for putative harmonics in
#'   the spectrum, the threshold for peak detection is calculated as
#'   \code{specPeak * (1 - HNR * specHNRslope)}
#' @param specSmooth the width of window for detecting peaks in the spectrum, Hz
#' @param specMerge pitch candidates within \code{specMerge} semitones are
#'   merged with boosted certainty
#' @param specSinglePeakCert (0 to 1) if F0 is calculated based on a single
#'   harmonic ratio (as opposed to several ratios converging on the same
#'   candidate), its certainty is taken to be \code{specSinglePeakCert}
#' @param shortestSyl the smallest length of a voiced segment (ms) that
#'   constitutes a voiced syllable (shorter segments will be replaced by NA, as
#'   if unvoiced)
#' @param shortestPause the smallest gap between voiced syllables (ms) that
#'   means they shouldn't be merged into one voiced syllable
#' @param interpolWin,interpolTol,interpolCert control the behavior of
#'   interpolation algorithm when postprocessing pitch candidates. To turn off
#'   interpolation, set \code{interpolWin = 0}. See \code{soundgen:::pathfinder}
#'   for details.
#' @param pathfinding method of finding the optimal path through pitch
#'   candidates: 'none' = best candidate per frame, 'fast' = simple heuristic,
#'   'slow' = annealing. See \code{soundgen:::pathfinder}
#' @param annealPars a list of control parameters for postprocessing of
#'   pitch contour with SANN algorithm of \code{\link[stats]{optim}}. This is
#'   only relevant if \code{pathfinding = 'slow'}
#' @param certWeight (0 to 1) in pitch postprocessing, specifies how much we
#'   prioritize the certainty of pitch candidates vs. pitch jumps / the internal
#'   tension of the resulting pitch curve
#' @param snakeStep optimized path through pitch candidates is further
#'   processed to minimize the elastic force acting on pitch contour. To
#'   disable, set \code{snakeStep = 0}
#' @param snakePlot if TRUE, plots the snake
#' @param smooth,smoothVars if \code{smooth} is a positive number, outliers of
#'   the variables in \code{smoothVars} are adjusted with median smoothing.
#'   \code{smooth} of 1 corresponds to a window of ~100 ms and tolerated
#'   deviation of ~4 semitones. To disable, set \code{smooth = 0}
#' @param summary if TRUE, returns only a summary of the measured acoustic
#'   variables (mean, median and SD). If FALSE, returns a list containing
#'   frame-by-frame values
#' @param summaryFun a vector of names of functions used to summarize each
#'   acoustic characteristic
#' @param plot if TRUE, produces a spectrogram with pitch contour overlaid
#' @param showLegend if TRUE, adds a legend with pitch tracking methods
#' @param savePath if a valid path is specified, a plot is saved in this folder
#'   (defaults to NA)
#' @param plotSpec if \code{FALSE}, the spectrogram will not be plotted
#' @param candPlot a list of graphical parameters for displaying
#' individual pitch candidates. Set to \code{NULL} or \code{NA} to suppress
#' @param pitchPlot a list of graphical parameters for displaying the final
#'   pitch contour. Set to \code{NULL} or \code{NA} to suppress
#' @param xlab,ylab,main plotting parameters
#' @param width,height,units,res parameters passed to
#'   \code{\link[grDevices]{png}} if the plot is saved
#' @param ... other graphical parameters passed to \code{\link{spectrogram}}
#' @return If \code{summary = TRUE}, returns a dataframe with one row and three
#'   columns per acoustic variable (mean / median / SD). If \code{summary =
#'   FALSE}, returns a dataframe with one row per STFT frame and one column per
#'   acoustic variable. The best guess at the pitch contour considering all
#'   available information is stored in the variable called "pitch". In
#'   addition, the output contains pitch estimates by separate algorithms
#'   included in \code{pitchMethods} and a number of other acoustic descriptors:
#'   \describe{
#'   \item{duration}{total duration, s}
#'   \item{duration_noSilence}{duration from the beginning of the first
#'   non-silent STFT frame to the end of the last non-silent STFT frame, s (NB:
#'   depends strongly on \code{windowLength} and \code{silence} settings)}
#'   \item{time}{time of the middle of each frame (ms)} \item{ampl}{root mean
#'   square of amplitude per frame, calculated as sqrt(mean(frame ^ 2))}
#'   \item{amplVoiced}{the same as ampl for voiced frames and NA for unvoiced
#'   frames} \item{dom}{lowest dominant frequency band (Hz) (see "Pitch tracking
#'   methods / Dominant frequency" in the vignette)} \item{entropy}{Weiner
#'   entropy of the spectrum of the current frame. Close to 0: pure tone or
#'   tonal sound with nearly all energy in harmonics; close to 1: white noise}
#'   \item{f1_freq, f1_width, ...}{the frequency and bandwidth of the first
#'   nFormants formants per STFT frame, as calculated by phonTools::findformants
#'   with default settings} \item{harmonics}{the amount of energy in upper
#'   harmonics, namely the ratio of total spectral mass above 1.25 x F0 to the
#'   total spectral mass below 1.25 x F0 (dB)} \item{HNR}{harmonics-to-noise
#'   ratio (dB), a measure of harmonicity returned by soundgen:::getPitchAutocor
#'   (see "Pitch tracking methods / Autocorrelation"). If HNR = 0 dB, there is
#'   as much energy in harmonics as in noise} \item{loudness}{subjective
#'   loudness, in sone, corresponding to the chosen SPL_measured - see
#'   \code{\link{getLoudness}}} \item{medianFreq}{50th quantile of the frame's
#'   spectrum} \item{peakFreq}{the frequency with maximum spectral power (Hz)}
#'   \item{peakFreqCut}{the frequency with maximum spectral power below cutFreq
#'   (Hz)} \item{pitch}{post-processed pitch contour based on all F0 estimates}
#'   \item{pitchAutocor}{autocorrelation estimate of F0}
#'   \item{pitchCep}{cepstral estimate of F0} \item{pitchSpec}{BaNa estimate of
#'   F0} \item{quartile25, quartile50, quartile75}{the 25th, 50th, and 75th
#'   quantiles of the spectrum below cutFreq (Hz)} \item{specCentroid}{the
#'   center of gravity of the frame’s spectrum, first spectral moment (Hz)}
#'   \item{specCentroidCut}{the center of gravity of the frame’s spectrum below
#'   cutFreq} \item{specSlope}{the slope of linear regression fit to the
#'   spectrum below cutFreq} \item{voiced}{is the current STFT frame voiced? TRUE
#'   / FALSE}
#' }
#' @export
#' @examples
#' sound = soundgen(sylLen = 300, pitch = c(900, 400, 2300),
#'   noise = list(time = c(0, 300), value = c(-40, 0)),
#'   temperature = 0.001, addSilence = 0)
#' # playme(sound, 16000)
#' a = analyze(sound, samplingRate = 16000, plot = TRUE)
#'
#' \dontrun{
#' # For maximum processing speed (just basic spectral descriptives):
#' a = analyze(sound, samplingRate = 16000,
#'   plot = FALSE,         # no plotting
#'   pitchMethods = NULL,  # no pitch tracking
#'   SPL_measured = NULL,  # no loudness analysis
#'   nFormants = 0         # no formant analysis
#' )
#'
#' sound1 = soundgen(sylLen = 900, pitch = list(
#'   time = c(0, .3, .9, 1), value = c(300, 900, 400, 2300)),
#'   noise = list(time = c(0, 300), value = c(-40, 0)),
#'   temperature = 0.001, addSilence = 0)
#' # improve the quality of postprocessing:
#' a1 = analyze(sound1, samplingRate = 16000, plot = TRUE, pathfinding = 'slow')
#' median(a1$pitch, na.rm = TRUE)
#' # (can vary, since postprocessing is stochastic)
#' # compare to the true value:
#' median(getSmoothContour(anchors = list(time = c(0, .3, .8, 1),
#'   value = c(300, 900, 400, 2300)), len = 1000))
#'
#' # the same pitch contour, but harder b/c of subharmonics and jitter
#' sound2 = soundgen(sylLen = 900, pitch = list(
#'   time = c(0, .3, .8, 1), value = c(300, 900, 400, 2300)),
#'   noise = list(time = c(0, 900), value = c(-40, 0)),
#'   subDep = 100, jitterDep = 0.5, nonlinBalance = 100, temperature = 0.001)
#' # playme(sound2, 16000)
#' a2 = analyze(sound2, samplingRate = 16000, plot = TRUE, pathfinding = 'slow')
#' # many candidates are off, but the overall contour should be mostly accurate
#'
#' # Fancy plotting options:
#' a = analyze(sound2, samplingRate = 16000, plot = TRUE,
#'   xlab = 'Time, ms', colorTheme = 'seewave',
#'   contrast = .5, ylim = c(0, 4),
#'   pitchMethods = c('dom', 'autocor', 'spec'),
#'   candPlot = list(
#'     col = c('gray70', 'yellow', 'purple'),  # same order as pitchMethods
#'     pch = c(1, 3, 5),
#'     cex = 3),
#'   pitchPlot = list(col = 'black', lty = 3, lwd = 3))
#'
#'# Plot pitch candidates w/o a spectrogram
#' a = analyze(sound2, samplingRate = 16000, plot = TRUE, plotSpec = FALSE)
#'
#' # Different formatting options for output
#' a = analyze(sound2, samplingRate = 16000, summary = FALSE)  # frame-by-frame
#' a = analyze(sound2, samplingRate = 16000, summary = TRUE,
#'             summaryFun = c('mean', 'range'))  # one row per sound
#' # ...with custom summaryFun
#' difRan = function(x) diff(range(x))
#' a = analyze(sound2, samplingRate = 16000, summary = TRUE,
#'             summaryFun = c('mean', 'difRan'))
#'
#' # Save the plot
#' a = analyze(sound, samplingRate = 16000,
#'             savePath = '~/Downloads/',
#'             width = 20, height = 15, units = 'cm', res = 300)
#'
#' ## Amplitude and loudness: analyze() should give the same results as
#' dedicated functions getRMS() / getLoudness()
#' # Create 1 kHz tone
#' samplingRate = 16000; dur_ms = 50
#' sound1 = sin(2*pi*1000/samplingRate*(1:(dur_ms/1000*samplingRate)))
#' a1 = analyze(sound1, samplingRate = samplingRate, windowLength = 25,
#'         overlap = 50, SPL_measured = 40, scale = 1,
#'         pitchMethods = NULL, plot = FALSE)
#' a1$loudness  # loudness per STFT frame (1 sone by definition)
#' getLoudness(sound1, samplingRate = samplingRate, windowLength = 25,
#'             overlap = 50, SPL_measured = 40, scale = 1)$loudness
#' a1$ampl  # RMS amplitude per STFT frame
#' getRMS(sound1, samplingRate = samplingRate, windowLength = 25,
#'        overlap = 50, scale = 1)
#' # or even simply: sqrt(mean(sound1 ^ 2))
#'
#' # The same sound as above, but with half the amplitude
#' a_half = analyze(sound1/2, samplingRate = samplingRate, windowLength = 25,
#'         overlap = 50, SPL_measured = 40, scale = 1,
#'         pitchMethods = NULL, plot = FALSE)
#' a1$ampl / a_half$ampl  # rms amplitude halved
#' a1$loudness/ a_half$loudness  # loudness is not a linear function of amplitude
#'
#' # Amplitude & loudness of an existing audio file
#' sound2 = '~/Downloads/temp/032_ut_anger_30-m-roar-curse.wav'
#' a2 = analyze(sound2, windowLength = 25, overlap = 50, SPL_measured = 40,
#'         pitchMethods = NULL, plot = FALSE)
#' apply(a2[, c('loudness', 'ampl')], 2, median, na.rm = TRUE)
#' median(getLoudness(sound2, windowLength = 25, overlap = 50,
#'                    SPL_measured = 40)$loudness)
#' median(getRMS(sound2, windowLength = 25, overlap = 50, scale = 1))
#' }
analyze = function(
  x,
  samplingRate = NULL,
  dynamicRange = 80,
  silence = 0.04,
  scale = NULL,
  SPL_measured = 70,
  Pref = 2e-5,
  windowLength = 50,
  step = NULL,
  overlap = 50,
  wn = 'gaussian',
  zp = 0,
  cutFreq = min(samplingRate / 2, max(7000, pitchCeiling * 2)),
  nFormants = 3,
  pitchMethods = c('autocor', 'spec', 'dom'),
  entropyThres = 0.6,
  pitchFloor = 75,
  pitchCeiling = 3500,
  priorMean = 300,
  priorSD = 6,
  priorPlot = 'deprecated',
  nCands = 1,
  minVoicedCands = NULL,
  domThres = 0.1,
  domSmooth = 220,
  autocorThres = 0.7,
  autocorSmooth = NULL,
  cepThres = 0.3,
  cepSmooth = 400,
  cepZp = 0,
  specThres = 0.3,
  specPeak = 0.35,
  specSinglePeakCert = 0.4,
  specHNRslope = 0.8,
  specSmooth = 150,
  specMerge = 1,
  shortestSyl = 20,
  shortestPause = 60,
  interpolWin = 75,
  interpolTol = 0.3,
  interpolCert = 0.3,
  pathfinding = c('none', 'fast', 'slow')[2],
  annealPars = list(maxit = 5000, temp = 1000),
  certWeight = .5,
  snakeStep = 0.05,
  snakePlot = FALSE,
  smooth = 1,
  smoothVars = c('pitch', 'dom'),
  summary = FALSE,
  summaryFun = c('mean', 'median', 'sd'),
  plot = TRUE,
  showLegend = TRUE,
  savePath = NA,
  plotSpec = TRUE,
  pitchPlot = list(
    col = rgb(0, 0, 1, .75),
    lwd = 3
  ),
  candPlot = list(),
  ylim = NULL,
  xlab = 'Time, ms',
  ylab = 'kHz',
  main = NULL,
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  ...
) {
  ## preliminaries
  if (!missing(priorPlot)) {
    message('priorPlot is deprecated. Use getPrior(..., plot = TRUE) to preview the pitch prior')
  }
  if ('osc' %in% names(match.call()) |
      'osc_dB' %in% names(match.call())) {
    # we are working with frameBank, not raw waveform
    osc = FALSE
    message('Plotting a spectrogram with oscillogram from analyze() is currently not implemented')
  }

  # import a sound
  if (class(x) == 'character') {
    extension = substr(x, nchar(x) - 2, nchar(x))
    if (extension == 'wav' | extension == 'WAV') {
      sound_wav = tuneR::readWave(x)
    } else if (extension == 'mp3' | extension == 'MP3') {
      sound_wav = tuneR::readMP3(x)
    } else {
      stop('Input not recognized: must be a numeric vector or wav/mp3 file')
    }
    samplingRate = sound_wav@samp.rate
    sound = sound_wav@left
    scale = 2 ^ (sound_wav@bit - 1)
    m = max(abs(sound))
    plotname = tail(unlist(strsplit(x, '/')), n = 1)
    plotname = ifelse(
      !missing(main) & !is.null(main),
      main,
      substring(plotname, first = 1,
                last = (nchar(plotname) - 4))
    )
  } else if (is.numeric(x)) {
    if (is.null(samplingRate)) {
      stop('Please specify "samplingRate", eg 44100')
    } else {
      sound = x
      plotname = ifelse(!missing(main) & !is.null(main), main, '')
    }
    m = max(abs(sound))
    if (is.null(scale)) {
      scale = max(m, 1)
      message(paste('Scale not specified. Assuming that max amplitude is', scale))
    } else if (is.numeric(scale)) {
      if (scale < m) {
        scale = m
        warning(paste('Scale cannot be smaller than observed max; resetting to', m))
      }
    }
  } else {
    stop('Input not recognized: must be a numeric vector or wav/mp3 file')
  }

  # calculate scaling coefficient for loudness calculation, but don't convert
  # yet, since most routines in analyze() require scale [-1, 1]
  scaleCorrection = NA
  if (is.numeric(SPL_measured) && SPL_measured > 0) {
    scaleCorrection = max(abs(scaleSPL(sound * m / scale,
                                       # NB: m / scale = 1 if the sound is normalized  to 0 dB (max amplitude)
                                       scale = 1,
                                       SPL_measured = SPL_measured,
                                       Pref = Pref))) /  # peak ampl of rescaled
      m  # peak ampl of original
  }

  # normalize to range from no less than -1 to no more than +1
  if (min(sound) > 0) {
    sound = sound - mean(sound)  # center
  }
  sound = sound / max(abs(sound))

  # Check simple numeric default pars
  simplePars = c('silence', 'entropyThres', 'domThres',
                 'autocorThres', 'autocorSmooth',
                 'cepThres', 'cepSmooth',
                 'specThres', 'specPeak',
                 'specSinglePeakCert', 'certWeight',
                 'interpolWin', 'interpolCert')
  for (p in simplePars) {
    gp = try(get(p), silent = TRUE)
    if (class(gp) != "try-error") {
      if (is.numeric(gp)) {
        if (any(gp < defaults_analyze[p, 'low']) |
            any(gp > defaults_analyze[p, 'high'])) {
          # reset p to default, with a warning
          assign(noquote(p), defaults_analyze[p, 'default'])
          warning(paste0(
            "\n", p, " should be between ", defaults_analyze[p, 'low'],
            " and ", defaults_analyze[p, 'high'],
            "; resetting to ", defaults_analyze[p, 'default']
          ))
        }
      }
    }
  }

  # Check defaults that depend on other pars or require customized warnings
  if (SPL_measured != 0) {  # if analyzing loudness
    if (samplingRate < 2000) {
      warning(paste('Sampling rate must be >2 KHz to resolve frequencies of at least 8 barks',
                    'and estimate loudness in sone'))
    } else if (samplingRate > 44100) {
      message(paste('Sampling rate above 44100, but discarding frequencies above 27 barks',
                    '(27 KHz) as inaudible to humans when estimating loudness'))
    }
  }
  duration = length(sound) / samplingRate
  if (!is.numeric(windowLength) | windowLength <= 0 |
      windowLength > (duration * 1000)) {
    windowLength = min(50, duration / 2 * 1000)
    warning(paste0('"windowLength" must be between 0 and sound_duration ms;
            defaulting to ', windowLength, ' ms'))
  }
  windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2
  # to ensure that the window length in points is a power of 2, say 2048 or 1024:
  # windowLength_points = 2^round (log(windowLength * samplingRate /1000)/log(2), 0)

  if (!is.numeric(step)) {
    if (!is.numeric(overlap) | overlap < 0 | overlap > 99) {
      overlap = 50
      warning('If "step" is not specified, overlap must be between 0 and 99%',
              '; overlap reset to 50%')
    } else {
      step = windowLength * (1 - overlap / 100)
    }
  } else {
    if (is.numeric(overlap) & overlap != 50) {  # step specified, overlap != default
      warning('"overlap" is ignored if "step" is not NULL')
    }
  }
  if (step <= 0 | step > (duration * 1000)) {
    step = windowLength / 2
    warning('"step" must be between 0 and sound_duration ms;
            defaulting to windowLength / 2')
  }
  if (step > windowLength) {
    warning(paste('"step" should normally not be larger than "windowLength" ms:',
                  'you are skipping parts of the sound!'))
  }

  if (!is.numeric(zp)) {
    zp = 0
  } else if (zp < 0) {
    zp = 0
    warning('"zp" must be non-negative; defaulting to 0')
  }
  if (!is.numeric(cutFreq) | cutFreq <= 0 | cutFreq > (samplingRate / 2)) {
    cutFreq = samplingRate / 2
    warning(paste('"cutFreq" must be between 0 and samplingRate / 2;',
                  'defaulting to samplingRate / 2'))
  }
  if (!is.numeric(pitchFloor) | pitchFloor <= 0 |
      pitchFloor > samplingRate / 2) {
    pitchFloor = 1
    warning(paste('"pitchFloor" must be between 0 and pitchCeiling;',
                  'defaulting to 1 Hz'))
  } # 1 Hz ~ 4 octaves below C0
  if (!is.numeric(pitchCeiling) | pitchCeiling > samplingRate / 2) {
    pitchCeiling = samplingRate / 2  # Nyquist
    warning(paste('"pitchCeiling" must be between 0 and Nyquist;',
                  'defaulting to samplingRate / 2'))
  }
  if (pitchFloor > pitchCeiling) {
    pitchFloor = 1
    pitchCeiling = samplingRate / 2
    warning(paste('"pitchFloor" cannot be above "pitchCeiling";',
                  'defaulting to 1 Hz and samplingRate / 2, respectively'))
  }
  if (is.numeric(priorMean)) {
    if (priorMean > samplingRate / 2 | priorMean <= 0) {
      priorMean = 300
      warning(paste('"priorMean" must be between 0 and Nyquist;',
                    'defaulting to 300; set to NULL to disable prior'))
    }
  }
  if (is.numeric(priorSD)) {
    if (priorSD <= 0) {
      priorSD = 6
      warning('"priorSD" must be positive; defaulting to 6 semitones')
    }
  }
  if (!is.numeric(nCands) | nCands < 1) {
    nCands = 1
    warning('"nCands" must be a positive integer; defaulting to 1')
  } else if (!is.integer(nCands)) {
    nCands = round(nCands)
  }
  if (!is.numeric(specMerge) | specMerge < 0) {
    specMerge = 1
    warning('"specMerge" must be non-negative; defaulting to 1 semitone')
  }
  if (!is.numeric(shortestSyl) | shortestSyl < 0) {
    shortestSyl = 0
    warning('shortestSyl must be non-negative; defaulting to 0 ms')
  }
  if (!is.numeric(shortestPause) | shortestPause < 0) {
    shortestPause = 0
    warning('shortestPause must be a non-negative number; defaulting to 0 ms')
  }
  if (shortestPause > 0 & interpolWin > 0) {
    if (interpolWin * step < shortestPause / 2) {
      interpolWin = ceiling(shortestPause / 2 / step)
      warning(paste('"interpolWin" reset to', interpolWin,
                    ': interpolation must be able to bridge merged voiced fragments'))
    }
  }
  if (interpolTol <= 0) {
    interpolTol = 0.3
    warning('"interpolTol" must be positive; defaulting to 0.3')
  }
  if (!is.numeric(autocorSmooth)) {
    autocorSmooth = 2 * ceiling(7 * samplingRate / 44100 / 2) - 1
    # width of smoothing interval, chosen to be proportionate to samplingRate (7
    # for samplingRate 44100), but always an odd number.
    # for(i in seq(16000, 60000, length.out = 10)) {
    #   print(paste(round(i), ':', 2 * ceiling(7 * i / 44100 / 2) - 1))
    # }
  }

  # Check non-numeric defaults
  supported_wn = c('bartlett', 'blackman', 'flattop', 'gaussian',
                   'hamming', 'hanning', 'rectangle')
  if (!wn %in% supported_wn) {
    wn = 'gaussian'
    warning(paste('Implemented "wn":',
                  paste(supported_wn, collapse = ', '),
                  '. Defaulting to "gaussian"'))
  }
  if (!pathfinding %in% c('none', 'fast', 'slow')) {
    pathfinding = 'fast'
    warning(paste('Implemented "pathfinding": "none", "fast", "slow";',
                  'defaulting to "fast"'))
  }

  # Set up filter for calculating pitchAutocor
  filter = ftwindow_modif(2 * windowLength_points, wn = wn) # plot(filter, type='l')
  powerSpectrum_filter = abs(fft(filter)) ^ 2
  autoCorrelation_filter = abs(fft(powerSpectrum_filter, inverse = TRUE)) ^ 2
  autoCorrelation_filter = autoCorrelation_filter[1:windowLength_points]
  autoCorrelation_filter = autoCorrelation_filter / max(autoCorrelation_filter)
  # plot(autoCorrelation_filter, type = 'l')

  ## fft and acf per frame
  if (is.character(savePath)) {
    # make sure the last character of savePath is "/"
    last_char = substr(savePath, nchar(savePath), nchar(savePath))
    if(last_char != '/') savePath = paste0(savePath, '/')
    plot = TRUE
    f = ifelse(plotname == '',
               'sound',
               plotname)
    png(filename = paste0(savePath, f, ".png"),
        width = width, height = height, units = units, res = res)
  }
  frameBank = getFrameBank(
    sound = sound,
    samplingRate = samplingRate,
    windowLength_points = windowLength_points,
    wn = wn,
    step = step,
    zp = zp,
    normalize = TRUE,
    filter = NULL
  )

  if (plot == TRUE & plotSpec) {
    plot_spec = TRUE
  } else {
    plot_spec = FALSE
  }

  extraSpecPars = list(...)
  extraSpecPars$osc = NULL
  s = do.call(spectrogram, c(list(
    x = NULL,
    frameBank = frameBank,
    dynamicRange = dynamicRange,
    duration = duration,
    samplingRate = samplingRate,
    windowLength = windowLength,
    zp = zp,
    wn = wn,
    step = step,
    main = plotname,
    plot = plot_spec,
    normalize = FALSE,
    output = 'original',
    ylim = ylim,
    xlab = xlab,
    ylab = ylab
  ), extraSpecPars))

  # calculate rms amplitude of each frame
  myseq = seq(1, (length(sound) - windowLength_points), length.out = ncol(s))
  ampl = apply(as.matrix(1:ncol(s)), 1, function(x) {
    # perceived intensity - root mean square of amplitude
    # (NB: m / scale corrects the scale back to original, otherwise sound is [-1, 1])
    sqrt(mean((sound[myseq[x]:(myseq[x] + windowLength_points - 1)] * m / scale) ^ 2))
  })
  # dynamically adjust silence threshold
  silence = max(silence, min(ampl))

  # calculate entropy of each frame within the most relevant
  # vocal range only (up to to cutFreq Hz)
  rowLow = 1 # which(as.numeric(rownames(s)) > 0.05)[1] # 50 Hz
  rowHigh = tail(which(as.numeric(rownames(s)) * 1000 <= cutFreq), 1) # 6000 Hz etc
  if (length(rowHigh) < 1 | !is.finite(rowHigh)) rowHigh = nrow(s)
  entropy = apply(as.matrix(1:ncol(s)), 1, function(x) {
    getEntropy(s[rowLow:rowHigh, x], type = 'weiner')
  })
  # if the frame is too quiet or too noisy, we will not analyze it
  cond_silence = ampl >= silence &
    as.logical(apply(s, 2, sum) > 0)  # b/c s frames are not 100% synchronized with ampl frames
  framesToAnalyze = which(cond_silence)
  cond_entropy = ampl > silence & entropy < entropyThres
  cond_entropy[is.na(cond_entropy)] = FALSE

  # save duration of non-silent part of audio
  if (length(framesToAnalyze) > 0) {
    time_start = step * (min(framesToAnalyze) - 1)  # the beginning of the first non-silent frame
    time_end = step * (max(framesToAnalyze))        # the end of the last non-silent frame
    duration_noSilence = (time_end - time_start) / 1000
  } else {
    duration_noSilence = 0
  }

  # autocorrelation for each frame
  autocorBank = matrix(NA, nrow = length(autoCorrelation_filter),
                       ncol = ncol(frameBank))
  for (i in which(cond_entropy)) {
    autocorBank[, i] = acf(frameBank[, i],
                           windowLength_points,
                           plot = FALSE)$acf / autoCorrelation_filter
  }
  autocorBank = autocorBank[-1, ]  # b/c it starts with zero lag (identity)
  # plot(autocorBank[, 78], type = 'l')
  rownames(autocorBank) = samplingRate / (1:nrow(autocorBank))

  ## FORMANTS
  formants = NULL
  if (nFormants > 0) {
    formants = matrix(NA, nrow = ncol(frameBank), ncol = nFormants * 2)
    colnames(formants) = paste0('f', rep(1:nFormants, each = 2),
                                rep(c('_freq', '_width'), nFormants))
    for (i in framesToAnalyze) {
      ff = try(phonTools::findformants(frameBank[, i],
                                       fs = samplingRate,
                                       verify = FALSE),
               silent = TRUE)
      if (class(ff) != 'try-error' & is.list(ff)) {
        temp = matrix(NA, nrow = nFormants, ncol = 2)
        availableRows = 1:min(nFormants, nrow(ff))
        temp[availableRows, ] = as.matrix(ff[availableRows, ])
        formants[i, ] = matrix(t(temp), nrow = 1)
      }
    }
  }



  ## PITCH and other spectral analysis of each frame from fft
  # set up an empty nested list to save values in - this enables us to analyze
  # only the non-silent and not-too-noisy frames but still have a consistently
  # formatted output
  frameInfo = rep(list(list(
    'pitchCands_frame' = data.frame(
      'pitchCand' = NA,
      'pitchCert' = NA,
      'pitchSource' = NA,
      stringsAsFactors = FALSE,
      row.names = NULL
    ),
    'summaries' = data.frame(
      'loudness' = NA,
      'HNR' = NA,
      'dom' = NA,
      'specCentroid' = NA,
      'specCentroidCut' = NA,
      'peakFreq' = NA,
      'peakFreqCut' = NA,
      'medianFreq' = NA,
      'quartile25' = NA,
      'quartile50' = NA,
      'quartile75' = NA,
      'specSlope' = NA
    )
  )), ncol(s))

  for (i in framesToAnalyze) {
    # for each frame that satisfies our condition, do spectral analysis (NB: we
    # do NOT analyze frames that are too quiet, so we only get NA's for those
    # frames, no meanFreq, dom etc!)
    frameInfo[[i]] = analyzeFrame(
      frame = s[, i],
      autoCorrelation = autocorBank[, i],
      samplingRate = samplingRate,
      scaleCorrection = scaleCorrection,
      trackPitch = cond_entropy[i],
      pitchMethods = pitchMethods,
      cutFreq = cutFreq,
      autocorThres = autocorThres,
      autocorSmooth = autocorSmooth,
      cepThres = cepThres,
      cepSmooth = cepSmooth,
      cepZp = cepZp,
      specThres = specThres,
      specPeak = specPeak,
      specHNRslope = specHNRslope,
      specSmooth = specSmooth,
      specMerge = specMerge,
      pitchFloor = pitchFloor,
      pitchCeiling = pitchCeiling,
      domThres = domThres,
      domSmooth = domSmooth,
      specSinglePeakCert = specSinglePeakCert,
      nCands = nCands
    )
  }

  # Store the descriptives provided by function analyzeFrame in a dataframe
  result = lapply(frameInfo, function(y) y[['summaries']])
  result = data.frame(matrix(unlist(result), nrow=length(frameInfo), byrow=TRUE))
  colnames(result) = names(frameInfo[[1]]$summaries)
  if (!is.null(formants)) result = cbind(result, formants)
  result$entropy = entropy
  result$ampl = ampl
  result$time = round(seq(0,
                          duration * 1000 - windowLength,
                          length.out = nrow(result)) + windowLength / 2, 0)
  result$duration_noSilence = duration_noSilence
  result$duration = duration
  nc = ncol(result)
  result = result[, c(rev((nc-4):nc), 1:(nc-5))]  # change the order of columns

  ## postprocessing
  # extract and prepare pitch candidates for the pathfinder algorithm
  max_cands = max(unlist(lapply(frameInfo, function(y)
    nrow(y[['pitchCands_frame']]))))
  if (max_cands == 0) {  # no pitch candidates at all, purely unvoiced
    result[, c('pitch', 'pitchAutocor', 'pitchCep', 'pitchSpec')] = NA
  } else {
    pitchCands_list = rep(list(matrix(
      NA,
      nrow = max_cands,
      ncol = length(frameInfo),
      dimnames = list(1:max_cands, result$time)
    )), 3)
    names(pitchCands_list) = c('freq', 'cert', 'source')
    for (i in 1:length(frameInfo)) {
      temp = frameInfo[[i]]$pitchCands_frame
      n = nrow(temp)
      if (n > 0) {
        pitchCands_list[[1]][1:n, i] = temp[, 1]
        pitchCands_list[[2]][1:n, i] = temp[, 2]
        pitchCands_list[[3]][1:n, i] = temp[, 3]
      }
    }

    # add prior
    if (is.numeric(priorMean) & is.numeric(priorSD)) {
      pitchCert_multiplier = getPrior(priorMean = priorMean,
                                      priorSD = priorSD,
                                      pitchFloor = pitchFloor,
                                      pitchCeiling = pitchCeiling,
                                      pitchCands = pitchCands_list$freq,
                                      plot = FALSE)
      pitchCands_list$cert = pitchCands_list$cert * pitchCert_multiplier
    }

    # divide the file into continuous voiced syllables
    voicedSegments = findVoicedSegments(
      pitchCands_list$freq,
      shortestSyl = shortestSyl,
      shortestPause = shortestPause,
      minVoicedCands = minVoicedCands,
      pitchMethods = pitchMethods,
      step = step,
      samplingRate = samplingRate
    )

    # for each syllable, impute NA's and find a nice path through pitch candidates
    pitchFinal = rep(NA, ncol(pitchCands_list$freq))
    if (nrow(voicedSegments) > 0) {
      # if we have found at least one putatively voiced syllable
      for (syl in 1:nrow(voicedSegments)) {
        myseq = voicedSegments$segmentStart[syl]:voicedSegments$segmentEnd[syl]
        # compute the optimal path through pitch candidates
        pitchFinal[myseq] = pathfinder(
          pitchCands = pitchCands_list$freq[, myseq, drop = FALSE],
          pitchCert = pitchCands_list$cert[, myseq, drop = FALSE],
          pitchSource = pitchCands_list$source[, myseq, drop = FALSE],
          certWeight = certWeight,
          pathfinding = pathfinding,
          annealPars = annealPars,
          interpolWin_bin = ceiling(interpolWin / step),
          interpolTol = interpolTol,
          interpolCert = interpolCert,
          snakeStep = snakeStep,
          snakePlot = snakePlot
        )
      }
    }

    # save optimal pitch track and the best candidates separately for
    # autocor, cepstrum and spectral
    result$pitch = pitchFinal # optimal pitch track
    result$pitchAutocor = as.numeric(lapply(frameInfo, function(x) {
      x$pitchCands_frame$pitchCand[x$pitchCands_frame$pitchSource == 'autocor'] [which.max(x$pitchCands_frame$pitchCert[x$pitchCands_frame$pitchSource == 'autocor'])]
    }))
    result$pitchCep = as.numeric(lapply(frameInfo, function(x) {
      x$pitchCands_frame$pitchCand[x$pitchCands_frame$pitchSource == 'cep'] [which.max(x$pitchCands_frame$pitchCert[x$pitchCands_frame$pitchSource == 'autocor'])]
    }))
    result$pitchSpec = as.numeric(lapply(frameInfo, function(x) {
      x$pitchCands_frame$pitchCand[x$pitchCands_frame$pitchSource == 'spec'] [which.max(x$pitchCands_frame$pitchCert[x$pitchCands_frame$pitchSource == 'autocor'])]
    }))
  }

  ## Median smoothing of specified contours (by default pitch & dom)
  if (smooth > 0) {
    points_per_sec = nrow(result) / duration
    # smooth of 1 means that smoothing window is ~100 ms
    smoothing_ww = round(smooth * points_per_sec / 10, 0)
    # the larger smooth, the heavier the smoothing (lower tolerance
    # threshold before values are replaced by median over smoothing window).
    # smooth of 1 gives smoothingThres of 4 semitones
    smoothingThres = 4 / smooth
    result[smoothVars] = medianSmoother(result[smoothVars],
                                        smoothing_ww = smoothing_ww,
                                        smoothingThres = smoothingThres)
  }

  ## Having decided upon the pitch for each frame, we save certain measurements
  # only for voiced frames (with non-NA pitch)
  voiced_idx = which(!is.na(result$pitch))
  unvoiced_idx = which(is.na(result$pitch))
  result$amplVoiced = NA
  result$amplVoiced[voiced_idx] = result$ampl[voiced_idx]
  result[unvoiced_idx, c('quartile25', 'quartile50', 'quartile75')] = NA
  result$voiced = FALSE
  result$voiced[voiced_idx] = TRUE

  # Calculate the % of energy in harmonics based on the final pitch estimates
  threshold = 1.25 * result$pitch / 1000
  result$harmonics = apply(matrix(1:ncol(s)), 1, function(x) {
    ifelse(is.na(threshold[x]),
           NA,
           sum(s[as.numeric(rownames(s)) > threshold[x], x]) / sum(s[, x]))
  })

  # Convert HNR and harmonics to dB
  result$HNR = to_dB(result$HNR)
  result$harmonics = to_dB(result$harmonics)

  # Arrange columns in alphabetical order (except the first three)
  result = result[, c(colnames(result)[1:3],
                      sort(colnames(result)[4:ncol(result)]))]

  ## Add pitch contours to the spectrogram
  if (plot) {
    addPitchCands(pitchCands = pitchCands_list$freq,
                  pitchCert = pitchCands_list$cert,
                  pitchSource = pitchCands_list$source,
                  pitch = result$pitch,
                  candPlot = candPlot,
                  pitchPlot = pitchPlot,
                  addToExistingPlot = plot_spec,
                  showLegend = showLegend,
                  ylim = ylim,
                  xlab = xlab,
                  ylab = ylab,
                  main = plotname,
                  ...)
  }
  if (is.character(savePath)) {
    dev.off()
  }

  # prepare the output
  if (summary == TRUE | summary == 'extended') {
    out = summarizeAnalyze(result, summaryFun)
  } else {
    out = result
  }
  if (summary == 'extended') {
    return(list(summary = out,
                result = result,
                pitchCands = pitchCands_list,
                spectrogram = s))
  } else {
    return(out)
  }
}


#' Analyze folder
#'
#' Acoustic analysis of all wav/mp3 files in a folder. See \code{\link{analyze}}
#' and vignette('acoustic_analysis', package = 'soundgen') for further details.
#' @param myfolder full path to target folder
#' @param verbose if TRUE, reports progress and estimated time left
#' @inheritParams analyze
#' @inheritParams spectrogram
#' @inheritParams getLoudness
#' @param savePlots if TRUE, saves plots as .png files
#' @param htmlPlots if TRUE, saves an html file with clickable plots
#' @return If \code{summary} is TRUE, returns a dataframe with one row per audio
#'   file. If \code{summary} is FALSE, returns a list of detailed descriptives.
#' @export
#' @examples
#' \dontrun{
#' # download 260 sounds from Anikin & Persson (2017)
#' # http://cogsci.se/publications/anikin-persson_2017_nonlinguistic-vocs/260sounds_wav.zip
#' # unzip them into a folder, say '~/Downloads/temp'
#' myfolder = '~/Downloads/temp'  # 260 .wav files live here
#' s = analyzeFolder(myfolder, verbose = TRUE)  # ~ 15-30 minutes!
#'
#' # Save spectrograms with pitch contours plus an html file for easy access
#' a = analyzeFolder('~/Downloads/temp', savePlots = TRUE,
#'   showLegend = TRUE,
#'   width = 20, height = 12,
#'   units = 'cm', res = 300)
#'
#' # Check accuracy: import manually verified pitch values (our "key")
#' key = pitchManual  # a vector of 260 floats
#' trial = s$pitch_median
#' cor(key, trial, use = 'pairwise.complete.obs')
#' plot(log(key), log(trial))
#' abline(a=0, b=1, col='red')
#' }
analyzeFolder = function(myfolder,
                         htmlPlots = TRUE,
                         verbose = TRUE,
                         samplingRate = NULL,
                         dynamicRange = 80,
                         silence = 0.04,
                         SPL_measured = 70,
                         Pref = 2e-5,
                         windowLength = 50,
                         step = NULL,
                         overlap = 50,
                         wn = 'gaussian',
                         zp = 0,
                         cutFreq = 6000,
                         nFormants = 3,
                         pitchMethods = c('autocor', 'spec', 'dom'),
                         entropyThres = 0.6,
                         pitchFloor = 75,
                         pitchCeiling = 3500,
                         priorMean = 300,
                         priorSD = 6,
                         priorPlot = FALSE,
                         nCands = 1,
                         minVoicedCands = NULL,
                         domThres = 0.1,
                         domSmooth = 220,
                         autocorThres = 0.7,
                         autocorSmooth = NULL,
                         cepThres = 0.3,
                         cepSmooth = NULL,
                         cepZp = 0,
                         specThres = 0.3,
                         specPeak = 0.35,
                         specSinglePeakCert = 0.4,
                         specHNRslope = 0.8,
                         specSmooth = 150,
                         specMerge = 1,
                         shortestSyl = 20,
                         shortestPause = 60,
                         interpolWin = 75,
                         interpolTol = 0.3,
                         interpolCert = 0.3,
                         pathfinding = c('none', 'fast', 'slow')[2],
                         annealPars = list(maxit = 5000, temp = 1000),
                         certWeight = .5,
                         snakeStep = 0.05,
                         snakePlot = FALSE,
                         smooth = 1,
                         smoothVars = c('pitch', 'dom'),
                         summary = TRUE,
                         summaryFun = c('mean', 'median', 'sd'),
                         plot = FALSE,
                         showLegend = TRUE,
                         savePlots = FALSE,
                         plotSpec = TRUE,
                         pitchPlot = list(
                           col = rgb(0, 0, 1, .75),
                           lwd = 3
                         ),
                         candPlot = list(
                           levels = c('autocor', 'spec', 'dom', 'cep'),
                           col = c('green', 'red', 'orange', 'violet'),
                           pch = c(16, 2, 3, 7),
                           cex = 2
                         ),
                         ylim = NULL,
                         xlab = 'Time, ms',
                         ylab = 'kHz',
                         main = NULL,
                         width = 900,
                         height = 500,
                         units = 'px',
                         res = NA,
                         ...) {
  time_start = proc.time()  # timing
  filenames = list.files(myfolder, pattern = "*.wav|.mp3", full.names = TRUE)
  # in order to provide more accurate estimates of time to completion,
  # check the size of all files in the target folder
  filesizes = file.info(filenames)$size

  # as.list(match.call()) also works, but we want to get default args as well,
  # since plot should default to TRUE for analyze() and FALSE for analyzeFolder(),
  # and summary vice versa.
  # See https://stackoverflow.com/questions/14397364/match-call-with-default-arguments
  myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'myfolder' , 'htmlPlots', 'verbose', 'savePlots',
    'pitchPlot', 'candPlot')]
  # exclude ...
  myPars = myPars[1:(length(myPars)-1)]
  # add plot pars correctly, without flattening the lists
  myPars$pitchPlot = pitchPlot
  myPars$candPlot = candPlot
  if (savePlots) myPars$savePath = myfolder

  result = list()
  for (i in 1:length(filenames)) {
    result[[i]] = do.call(analyze, c(filenames[i], myPars, ...))
    if (verbose) {
      reportTime(i = i, nIter = length(filenames),
                 time_start = time_start, jobs = filesizes)
    }
  }

  # prepare output
  if (summary == TRUE) {
    output = as.data.frame(t(sapply(result, function(x) unlist(rbind(x)))))
    output$sound = apply(matrix(1:length(filenames)), 1, function(x) {
      tail(unlist(strsplit(filenames[x], '/')), 1)
    })
    output = output[, c('sound', colnames(output)[1:(ncol(output) - 1)])]
  } else {
    output = result
    names(output) = filenames
  }

  if (htmlPlots & savePlots) {
    htmlPlots(myfolder, myfiles = filenames)
  }

  return (output)
}
