### MAIN FUNCTIONS FOR ACOUSTIC ANALYSIS ###

#' Analyze sound
#'
#' Acoustic analysis of a single sound file: pitch tracking, basic spectral
#' characteristics, and estimated loudness (see \code{\link{getLoudness}}). The
#' default values of arguments are optimized for human non-linguistic
#' vocalizations. See vignette('acoustic_analysis', package = 'soundgen') for
#' details. The defaults and reasonable ranges of all arguments can be found in
#' \link{defaults_analyze}.
#'
#' Each pitch tracker is controlled by its own list of settings, as follows:
#' \describe{\item{\code{pitchDom} (lowest dominant frequency band)}{\itemize{
#' \item \code{domThres} (0 to 1) to find the lowest dominant frequency band, we
#' do short-term FFT and take the lowest frequency with amplitude at least
#' domThres \item \code{domSmooth} the width of smoothing interval (Hz) for
#' finding \code{dom}}} \item{\code{pitchAutocor} (autocorrelation)}{\itemize{
#' \item \code{autocorThres} voicing threshold (unitless, ~0 to 1) \item
#' \code{autocorSmooth} the width of smoothing interval (in bins) for finding
#' peaks in the autocorrelation function. Defaults to 7 for sampling rate 44100
#' and smaller odd numbers for lower values of sampling rate \item
#' \code{autocorUpsample} upsamples acf to this resolution (Hz) to improve
#' accuracy in high frequencies \item \code{autocorBestPeak} amplitude of the
#' lowest best candidate relative to the absolute max of the acf }}
#' \item{\code{pitchCep} (cepstrum)}{\itemize{\item \code{cepThres} voicing
#' threshold (unitless, ~0 to 1) \item \code{cepSmooth} the width of smoothing
#' interval (Hz) for finding peaks in the cepstrum \item \code{cepZp}
#' zero-padding of the spectrum used for cepstral pitch detection (final length
#' of spectrum after zero-padding in points, e.g. 2 ^ 13)}} \item{
#' \code{pitchSpec} (ratio of harmonics - BaNa algorithm)}{\itemize{ \item
#' \code{specThres} voicing threshold (unitless, ~0 to 1) \item
#' \code{specPeak,specHNRslope} when looking for putative harmonics in the
#' spectrum, the threshold for peak detection is calculated as \code{specPeak *
#' (1 - HNR * specHNRslope)} \item specSmooth the width of window for detecting
#' peaks in the spectrum, Hz \item \code{specMerge} pitch candidates within
#' \code{specMerge} semitones are merged with boosted certainty \item
#' \code{specSinglePeakCert} (0 to 1) if F0 is calculated based on a single
#' harmonic ratio (as opposed to several ratios converging on the same
#' candidate), its certainty is taken to be \code{specSinglePeakCert}}} \item{
#' pitchHps (harmonic product spectrum)}{\itemize{\item \code{hpsNum} the
#' number of times to downsample the spectrum \item \code{hpsThres} voicing
#' threshold (unitless, ~0 to 1) \item \code{hpsNorm} the amount of inflation of
#' hps pitch certainty (0 = none) \item \code{hpsPenalty} the amount of
#' penalizing hps candidates in low frequencies (0 = none) }} }  Each of these
#' lists also accepts graphical parameters that affect how pitch candidates are
#' plotted, eg \code{pitchDom = list(domThres = .5, col = 'yellow')}. Other
#' arguments that are lists of subroutine-specific settings include: \describe{
#' \item{\code{harmonicHeight} (finding how high harmonics reach in the
#' spectrum)}{\itemize{\item \code{harmThres} minimum height of spectral peak,
#' dB \item \code{harmPerSel} the number of harmonics per sliding selection
#' \item \code{harmTol} maximum tolerated deviation of peak frequency from
#' multiples of f0, proportion of f0 \item + plotting pars, notably set
#' \code{type = 'l'} to plot the \code{harmHeight} contour }} }
#'
#' @seealso \code{\link{analyzeFolder}} \code{\link{pitch_app}}
#'   \code{\link{getLoudness}} \code{\link{segment}} \code{\link{getRMS}}
#'   \code{\link{modulationSpectrum}} \code{\link{ssm}}
#'
#' @inheritParams spectrogram
#' @inheritParams getLoudness
#' @param from,to if NULL (default), analyzes the whole sound, otherwise
#'   from...to (s)
#' @param silence (0 to 1) frames with RMS amplitude below silence threshold are
#'   not analyzed at all. NB: this number is dynamically updated: the actual
#'   silence threshold may be higher depending on the quietest frame, but it
#'   will never be lower than this specified number.
#' @param SPL_measured sound pressure level at which the sound is presented, dB
#'   (set to 0 to skip analyzing subjective loudness)
#' @param cutFreq if specified, spectral descriptives (peakFreq, specCentroid,
#'   specSlope, and quartiles) are calculated under \code{cutFreq}. Recommended
#'   when analyzing recordings with varying sampling rates: set to half the
#'   lowest sampling rate to make the spectra more comparable. Note that
#'   "entropyThres" applies only to this frequency range, which also affects
#'   which frames will not be analyzed with pitchAutocor.
#' @param formants a list of arguments passed to
#'   \code{\link[phonTools]{findformants}} - an external function called to
#'   perform LPC analysis
#' @param nFormants the number of formants to extract per STFT frame (0 = no
#'   formant analysis, NULL = as many as possible)
#' @param pitchMethods methods of pitch estimation to consider for determining
#'   pitch contour: 'autocor' = autocorrelation (~PRAAT), 'cep' = cepstral,
#'   'spec' = spectral (~BaNa), 'dom' = lowest dominant frequency band ('' or
#'   NULL = no pitch analysis)
#' @param pitchManual manually corrected pitch contour - a numeric vector of any
#'   length, but ideally as returned by \code{\link{pitch_app}} with the same
#'   windowLength and step as in current call to analyze
#' @param entropyThres pitch tracking is not performed for frames with Weiner
#'   entropy above \code{entropyThres}, but other spectral descriptives are
#'   still calculated
#' @param pitchFloor,pitchCeiling absolute bounds for pitch candidates (Hz)
#' @param priorMean,priorSD specifies the mean (Hz) and standard deviation
#'   (semitones) of gamma distribution describing our prior knowledge about the
#'   most likely pitch values for this file. For ex., \code{priorMean = 300,
#'   priorSD = 6} gives a prior with mean = 300 Hz and SD = 6 semitones (half
#'   an octave)
#' @param nCands maximum number of pitch candidates per method (except for
#'   \code{dom}, which returns at most one candidate per frame), normally 1...4
#' @param minVoicedCands minimum number of pitch candidates that have to be
#'   defined to consider a frame voiced (if NULL, defaults to 2 if \code{dom} is
#'   among other candidates and 1 otherwise)
#' @param pitchDom a list of control parameters for pitch tracking using the
#' lowest dominant frequency band or "dom" method; see details and
#' \code{?soundgen:::getDom}
#' @param pitchAutocor a list of control parameters for pitch tracking using the
#'   autocorrelation or "autocor" method; see details and
#'   \code{?soundgen:::getPitchAutocor}
#' @param pitchCep a list of control parameters for pitch tracking using the
#'   cepstrum or "cep" method; see details and \code{?soundgen:::getPitchCep}
#' @param pitchSpec a list of control parameters for pitch tracking using the
#'   BaNa or "spec" method; see details and \code{?soundgen:::getPitchSpec}
#' @param pitchHps a list of control parameters for pitch tracking using the
#'   harmonic product spectrum ("hps") method; see details and
#'   \code{?soundgen:::getPitchHps}
#' @param harmHeight a list of control parameters for estimating how high
#'   harmonics reach in the spectrum; see details and \code{?soundgen:::harmHeight}
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
#' @param summary deprecated
#' @param summaryFun functions used to summarize each acoustic characteristic,
#'   eg "c('mean', 'sd')"; user-defined functions are fine (see examples); NAs
#'   are omitted automatically for mean/median/sd/min/max/range/sum, otherwise
#'   take care of NAs yourself; if \code{summaryFun = NULL}, analyze() returns a
#'   list containing frame-by-frame values
#' @param invalidArgAction what to do if an argument is invalid or outside the
#'   range in \code{defaults_analyze}: 'adjust' = reset to default value,
#'   'abort' = stop execution, 'ignore' = throw a warning and continue (may
#'   crash)
#' @param plot if TRUE, produces a spectrogram with pitch contour overlaid
#' @param showLegend if TRUE, adds a legend with pitch tracking methods
#' @param savePath if a valid path is specified, a plot is saved in this folder
#'   (defaults to NA)
#' @param pitchPlot a list of graphical parameters for displaying the final
#'   pitch contour. Set to \code{list(type = 'n')} to suppress
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
#'   \describe{\item{duration}{total duration, s}
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
#'   nFormants formants per STFT frame, as calculated by
#'   phonTools::findformants} \item{harmEnergy}{the amount of energy in upper
#'   harmonics, namely the ratio of total spectral mass above 1.25 x F0 to the
#'   total spectral mass below 1.25 x F0 (dB)}\item{harmHeight}{how high
#'   harmonics reach in the spectrum, based on the best guess at pitch (or the
#'   manually provided pitch values)} \item{HNR}{harmonics-to-noise ratio (dB),
#'   a measure of harmonicity returned by soundgen:::getPitchAutocor (see "Pitch
#'   tracking methods / Autocorrelation"). If HNR = 0 dB, there is as much
#'   energy in harmonics as in noise} \item{loudness}{subjective loudness, in
#'   sone, corresponding to the chosen SPL_measured - see
#'   \code{\link{getLoudness}}} \item{peakFreq}{the frequency with maximum
#'   spectral power (Hz)} \item{pitch}{post-processed pitch contour based on all
#'   F0 estimates} \item{pitchAutocor}{autocorrelation estimate of F0}
#'   \item{pitchCep}{cepstral estimate of F0} \item{pitchSpec}{BaNa estimate of
#'   F0} \item{quartile25, quartile50, quartile75}{the 25th, 50th, and 75th
#'   quantiles of the spectrum of voiced frames (Hz)} \item{specCentroid}{the
#'   center of gravity of the frame’s spectrum, first spectral moment (Hz)}
#'   \item{specSlope}{the slope of linear regression fit to the spectrum below
#'   cutFreq} \item{voiced}{is the current STFT frame voiced? TRUE / FALSE}
#' }
#' @export
#' @examples
#' sound = soundgen(sylLen = 300, pitch = c(500, 400, 600),
#'   noise = list(time = c(0, 300), value = c(-40, 0)),
#'   temperature = 0.001,
#'   addSilence = 50)  # NB: always have some silence before and after!!!
#' # playme(sound, 16000)
#' a = analyze(sound, samplingRate = 16000)
#'
#' \dontrun{
#' # For maximum processing speed (just basic spectral descriptives):
#' a = analyze(sound, samplingRate = 16000,
#'   plot = FALSE,         # no plotting
#'   pitchMethods = NULL,  # no pitch tracking
#'   SPL_measured = 0,     # no loudness analysis
#'   nFormants = 0         # no formant analysis
#' )
#'
#' sound1 = soundgen(sylLen = 900, pitch = list(
#'   time = c(0, .3, .9, 1), value = c(300, 900, 400, 2300)),
#'   noise = list(time = c(0, 300), value = c(-40, 0)),
#'   temperature = 0.001, samplingRate = 44100, pitchSamplingRate = 44100)
#' # improve the quality of postprocessing:
#' a1 = analyze(sound1, samplingRate = 44100, priorSD = 24,
#'              plot = TRUE, pathfinding = 'slow', ylim = c(0, 5))
#' median(a1$pitch, na.rm = TRUE)
#' # (can vary, since postprocessing is stochastic)
#' # compare to the true value:
#' median(getSmoothContour(anchors = list(time = c(0, .3, .8, 1),
#'   value = c(300, 900, 400, 2300)), len = 1000))
#'
#' # the same pitch contour, but harder to analyze b/c of
#' # subharmonics and jitter
#' sound2 = soundgen(sylLen = 900, pitch = list(
#'   time = c(0, .3, .8, 1), value = c(300, 900, 400, 2300)),
#'   noise = list(time = c(0, 900), value = c(-40, -20)),
#'   subDep = 10, jitterDep = 0.5,
#'   temperature = 0.001, samplingRate = 44100, pitchSamplingRate = 44100)
#' # playme(sound2, 44100)
#' a2 = analyze(sound2, samplingRate = 44100, priorSD = 24,
#'              pathfinding = 'slow', ylim = c(0, 5))
#'
#' # Fancy plotting options:
#' a = analyze(sound1, samplingRate = 44100,
#'   xlab = 'Time, ms', colorTheme = 'seewave',
#'   contrast = .5, ylim = c(0, 4), main = 'My plot',
#'   pitchMethods = c('dom', 'autocor', 'spec', 'hps', 'cep'),
#'   priorMean = NA,  # no prior info at all
#'   pitchDom = list(col = 'red', domThres = .25),
#'   pitchPlot = list(col = 'black', lty = 3, lwd = 3),
#'   osc_dB = TRUE, heights = c(2, 1))
#'
#' # Different options for summarizing the output
#' a = analyze(sound1, 44100,
#'             summaryFun = NULL)  # frame-by-frame
#' a = analyze(sound1, 44100,
#'             summaryFun = c('mean', 'range'))  # one row per sound
#' # ...with custom summaryFun, eg time of peak relative to duration (0 to 1)
#' timePeak = function(x) which.max(x) / length(x)  # without omitting NAs
#' timeTrough = function(x) which.min(x) / length(x)
#' a = analyze(sound2, samplingRate = 16000,
#'             summaryFun = c('mean', 'timePeak', 'timeTrough'))
#'
#' # Analyze a selection rather than the whole sound
#' a = analyze(sound1, samplingRate = 16000, from = .4, to = .8)
#'
#' # Save the plot
#' a = analyze(sound1, 44100, ylim = c(0, 5),
#'             savePath = '~/Downloads/',
#'             width = 20, height = 15, units = 'cm', res = 300)
#'
#' ## Amplitude and loudness: analyze() should give the same results as
#' dedicated functions getRMS() / getLoudness()
#' # Create 1 kHz tone
#' samplingRate = 16000; dur_ms = 50
#' sound3 = sin(2*pi*1000/samplingRate*(1:(dur_ms/1000*samplingRate)))
#' a1 = analyze(sound3, samplingRate = samplingRate, windowLength = 25,
#'         overlap = 50, SPL_measured = 40, scale = 1,
#'         pitchMethods = NULL, plot = FALSE)
#' a1$loudness  # loudness per STFT frame (1 sone by definition)
#' getLoudness(sound3, samplingRate = samplingRate, windowLength = 25,
#'             overlap = 50, SPL_measured = 40, scale = 1)$loudness
#' a1$ampl  # RMS amplitude per STFT frame
#' getRMS(sound3, samplingRate = samplingRate, windowLength = 25,
#'        overlap = 50, scale = 1)
#' # or even simply: sqrt(mean(sound1 ^ 2))
#'
#' # The same sound as above, but with half the amplitude
#' a_half = analyze(sound3 / 2, samplingRate = samplingRate, windowLength = 25,
#'         overlap = 50, SPL_measured = 40, scale = 1,
#'         pitchMethods = NULL, plot = FALSE)
#' a1$ampl / a_half$ampl  # rms amplitude halved
#' a1$loudness/ a_half$loudness  # loudness is not a linear function of amplitude
#'
#' # Amplitude & loudness of an existing audio file
#' sound4 = '~/Downloads/temp/cry_451_soundgen.wav'
#' a2 = analyze(sound4, windowLength = 25, overlap = 50, SPL_measured = 40)
#' apply(a2[, c('loudness', 'ampl')], 2, median, na.rm = TRUE)
#' median(getLoudness(sound4, windowLength = 25, overlap = 50,
#'                    SPL_measured = 40)$loudness)
#' # NB: not identical b/c analyze() doesn't consider very quiet frames
#' median(getRMS(sound4, windowLength = 25, overlap = 50, scale = 1))
#'
#' # Analyzing ultrasounds (slow but possible, just adjust pitchCeiling)
#' s = soundgen(sylLen = 200, addSilence = 10,
#'   pitch = c(25000, 35000, 30000),
#'   formants = NA, rolloff = -12, rolloffKHz = 0,
#'   pitchSamplingRate = 350000, samplingRate = 350000, windowLength = 5,
#'   pitchCeiling = 45000, invalidArgAction = 'ignore')
#' # s is a bat-like ultrasound inaudible to humans
#' spectrogram(s, 350000, windowLength = 5)
#' a = analyze(s, 350000, pitchCeiling = 45000, priorMean = NA,
#'             windowLength = 5, overlap = 0,
#'             nFormants = 0, SPL_measured = 0)
#' # NB: ignore formants and loudness estimates for such non-human sounds
#' }
analyze = function(
  x,
  samplingRate = NULL,
  from = NULL,
  to = NULL,
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
  cutFreq = NULL,
  formants = list(verify = FALSE),
  nFormants = 3,
  pitchMethods = c('dom', 'autocor'),
  pitchManual = NULL,
  entropyThres = 0.6,
  pitchFloor = 75,
  pitchCeiling = 3500,
  priorMean = 300,
  priorSD = 6,
  nCands = 1,
  minVoicedCands = NULL,
  pitchDom = list(),
  pitchAutocor = list(),
  pitchCep = list(),
  pitchSpec = list(),
  pitchHps = list(),
  harmHeight = list(type = 'n'),
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
  summary = NULL,
  summaryFun = NULL,
  invalidArgAction = c('adjust', 'abort', 'ignore')[1],
  plot = TRUE,
  showLegend = TRUE,
  savePath = NA,
  osc = TRUE,
  osc_dB = FALSE,
  pitchPlot = list(col = rgb(0, 0, 1, .75), lwd = 3, showPrior = TRUE),
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
  ## preliminaries - deprecated pars
  if (!is.null(summary)) {
    message(paste0('summary', ' is deprecated, set "summaryFun = NULL" instead'))
  }


  # import a sound
  if (class(x)[1] == 'character') {
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
    # NB: m / scale = 1 if the sound is normalized to 0 dB (max amplitude)
    # scaleCorrection = max(abs(scaleSPL(sound * m / scale,
    #                                    scale = 1,
    #                                    SPL_measured = SPL_measured,
    #                                    Pref = Pref))) /  # peak ampl of rescaled
    #   m  # peak ampl of original
    scaleCorrection = max(abs(scaleSPL(sound,
                                       scale = scale,
                                       SPL_measured = SPL_measured,
                                       Pref = Pref)))
  }

  # Check simple numeric default pars
  simplePars = c('silence', 'entropyThres', 'domThres',
                 'autocorThres', 'autocorSmooth',
                 'cepThres', 'cepSmooth',
                 'specThres', 'specPeak',
                 'specSinglePeakCert', 'certWeight',
                 'interpolWin', 'interpolCert')
  for (p in simplePars) {
    gp = try(get(p), silent = TRUE)
    if (class(gp)[1] != "try-error") {
      if (is.numeric(gp)) {
        assign(noquote(p),
               validatePars(p, gp, defaults_analyze, invalidArgAction))
      }
    }
  }

  # Check parameters supplied as lists
  pitchDom_plotPars = pitchAutocor_plotPars =
    pitchCep_plotPars = pitchSpec_plotPars =
    pitchHps_plotPars = harmHeight_plotPars =
    NULL  # otherwise CMD check complains
  if (!is.null(harmHeight$plotPars)) {
    harm_plotPars = harmHeight$plotPars
  } else {
    harm_plotPars = list()
  }
  # Here we specify just the names of pars as c('', '').
  # (values are in defaults_analyze)
  parsToValidate = list(
    harmHeight = c('harmThres', 'harmTol', 'harmPerSel'),
    pitchDom = c('domThres', 'domSmooth'),
    pitchAutocor = c('autocorThres', 'autocorSmooth',
                     'autocorUpsample', 'autocorBestPeak'),
    pitchCep = c('cepThres', 'cepSmooth', 'cepZp'),
    pitchSpec = c('specSmooth', 'specHNRslope', 'specThres',
                  'specPeak', 'specSinglePeakCert', 'specMerge'),
    pitchHps = c('hpsNum', 'hpsThres', 'hpsNorm', 'hpsPenalty')
  )
  for (i in 1:length(parsToValidate)) {
    parGroup_user = get(names(parsToValidate)[i])
    # whatever is not in parsToValidate is interpreted as plotting options
    assign(paste0(names(parsToValidate)[i], '_plotPars'),
           parGroup_user[!names(parGroup_user) %in% parsToValidate[[i]]])
    # if there's nothing to add, it becomes an empty list()
    # now we check the value of those pars that ARE in parsToValidate
    parGroup_user = parGroup_user[names(parGroup_user) %in% parsToValidate[[i]]]
    parGroup_def = parsToValidate[[i]]
    for (p in parGroup_def) {
      if (is.null(parGroup_user[[p]])) {
        # fall back to the default value
        parGroup_user[p] = defaults_analyze[p, 'default']
      } else {
        # validate user-defined value
        if (is.numeric(parGroup_user[[p]])) {
          parGroup_user[[p]] = validatePars(
            p, parGroup_user[[p]], defaults_analyze, invalidArgAction)
        }
      }
    }
    assign(noquote(names(parsToValidate)[i]), parGroup_user)
  }

  # Check defaults that depend on other pars or require customized warnings
  if (is.character(pitchMethods) && pitchMethods[1] != '') {
    valid_names = c('dom', 'autocor', 'cep', 'spec', 'hps')
    invalid_names = pitchMethods[!pitchMethods %in% valid_names]
    if (length(invalid_names) > 0) {
      message(paste('Ignoring unknown pitch tracking methods:',
                    paste(invalid_names, collapse = ', '),
                    '; valid pitchMethods:',
                    paste(valid_names, collapse = ', ')))
    }
  }
  if (SPL_measured != 0) {  # if analyzing loudness
    if (samplingRate < 2000) {
      warning(paste('Sampling rate must be >2 KHz to resolve frequencies of at least 8 barks',
                    'and estimate loudness in sone'))
    } else if (samplingRate > 44100) {
      message(paste('Sampling rate above 44100, but discarding frequencies above 27 barks',
                    '(27 KHz) as inaudible to humans when estimating loudness'))
    }
  }

  # from...to selection
  ls = length(sound)
  if (any(is.numeric(c(from, to)))) {
    if (!is.numeric(from)) {
      from_points = 1
    } else {
      from_points = max(1, round(from * samplingRate))
    }
    if (!is.numeric(to)) {
      to_points = ls
    }  else {
      to_points = min(ls, round(to * samplingRate))
    }
    sound = sound[from_points:to_points]
    timeShift = from_points / samplingRate
    ls = length(sound)
  } else {
    timeShift = 0
  }
  duration = ls / samplingRate

  # normalize to range from no less than -1 to no more than +1
  if (any(sound > 0)) {
    sound = sound - mean(sound)  # center
  }
  sound = sound / max(abs(sound))

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
  if (!is.null(cutFreq) &&
      (!is.numeric(cutFreq) | cutFreq <= 0 | cutFreq > (samplingRate / 2))) {
    cutFreq = NULL
    warning(paste('"cutFreq" must be between 0 and samplingRate / 2; ignoring'))
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
  if (!is.numeric(pitchAutocor$autocorSmooth)) {
    pitchAutocor$autocorSmooth = 2 * ceiling(7 * samplingRate / 44100 / 2) - 1
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
    filter = NULL,
    padWithSilence = FALSE,
    timeShift = timeShift
  )
  timestamps = as.numeric(colnames(frameBank))

  extraSpecPars = list(...)
  extraSpecPars$osc = NULL
  s = do.call(spectrogram, c(list(
    x = NULL,
    internal = list(duration = duration,
                    frameBank = frameBank),
    dynamicRange = dynamicRange,
    samplingRate = samplingRate,
    windowLength = windowLength,
    zp = zp,
    wn = wn,
    step = step,
    normalize = FALSE,
    output = 'original',
    plot = FALSE
  ), extraSpecPars))
  if (is.na(s)[1]) {
    message(paste('The sound is too short to analyze',
                  'with windowLength =', windowLength))
    return(list(duration = duration))
  }
  bin = samplingRate / 2 / nrow(s)  # width of spectral bin, Hz
  freqs = as.numeric(rownames(s)) * 1000  # central bin freqs, Hz

  # calculate rms amplitude of each frame
  myseq = (timestamps - timeShift * 1000 - windowLength / 2) * samplingRate / 1000 + 1
  myseq[1] = 1  # just in case of rounding errors
  ampl = apply(as.matrix(1:length(myseq)), 1, function(x) {
    # perceived intensity - root mean square of amplitude
    # (NB: m / scale corrects the scale back to original, otherwise sound is [-1, 1])
    sqrt(mean((sound[myseq[x]:(myseq[x] + windowLength_points - 1)] * m / scale) ^ 2))
  })
  # dynamically adjust silence threshold
  silence = max(silence, min(ampl, na.rm = TRUE))

  # calculate entropy of each frame within the most relevant
  # vocal range only (up to to cutFreq Hz)
  rowLow = 1 # which(as.numeric(rownames(s)) > 0.05)[1] # 50 Hz
  if (!is.null(cutFreq)) {
    rowHigh = tail(which(freqs <= cutFreq), 1) # 6000 Hz etc
  } else {
    rowHigh = nrow(s)
  }
  if (length(rowHigh) < 1 | !is.finite(rowHigh)) rowHigh = nrow(s)
  entropy = apply(as.matrix(1:ncol(s)), 1, function(x) {
    getEntropy(s[rowLow:rowHigh, x], type = 'weiner')
  })
  # if the frame is too quiet or too noisy, we will not analyze it
  cond_silence = ampl >= silence &
    as.logical(apply(s, 2, sum) > 0)  # b/c s frames are not 100% synchronized with ampl frames
  framesToAnalyze = which(cond_silence)
  cond_entropy = cond_silence & entropy < entropyThres
  cond_entropy[is.na(cond_entropy)] = FALSE

  # save duration of non-silent part of audio
  nf = length(framesToAnalyze)
  if (nf > 0) {
    # the beginning of the first non-silent frame
    time_start = timestamps[framesToAnalyze[1]] - windowLength / 2
    # the end of the last non-silent frame
    time_end = timestamps[framesToAnalyze[nf]] + windowLength / 2
    duration_noSilence = (time_end - time_start) / 1000
  } else {
    duration_noSilence = 0
    message(paste('The audio is too quiet!',
                  'No frames above silence =', silence))
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
  # plot(autocorBank[, 15], type = 'l')
  rownames(autocorBank) = samplingRate / (1:nrow(autocorBank))

  ## FORMANTS
  fmts = NULL
  if (is.null(nFormants)) nFormants = 1000
  # try one frame to see how many formants are returned
  fmts_list = vector('list', length = nf)
  if (nFormants > 0 & nf > 0) {
    # we don't really know how many formants will be returned by phonTools, so
    # we save everything at first, and then trim to nFormants
    for (i in 1:nf) {
      fmts_list[[i]] = try(suppressWarnings(do.call(
        phonTools::findformants,
        c(formants,
          list(frameBank[, framesToAnalyze[i]],
               fs = samplingRate)))),
        silent = TRUE)
    }
    # check how many formants we will/can save
    nFormants = min(nFormants, max(unlist(lapply(fmts_list, nrow))))
    availableRows = 1:nFormants
    fmts = matrix(NA, nrow = ncol(frameBank), ncol = nFormants * 2)
    colnames(fmts) = paste0('f', rep(availableRows, each = 2),
                            rep(c('_freq', '_width'), nFormants))
    # iterate through the full formant list and save what's needed
    for (i in 1:nf) {
      ff = fmts_list[[i]]
      if (is.list(ff)) {
        nr = nrow(ff)
        if (nr < nFormants) {
          ff[(nr + 1):nFormants, ] = NA
        }
        temp = matrix(NA, nrow = nFormants, ncol = 2)
        temp[availableRows, ] = as.matrix(ff[availableRows, ])
        fmts[framesToAnalyze[i], ] = matrix(t(temp), nrow = 1)
      }
    }
  } else if (nFormants > 0 && nf == 0) {
    # no formant analysis
    availableRows = 1:nFormants
    fmts = matrix(NA, nrow = ncol(frameBank), ncol = nFormants * 2)
    colnames(fmts) = paste0('f', rep(availableRows, each = 2),
                            rep(c('_freq', '_width'), nFormants))
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
      'peakFreq' = NA,
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
      bin = bin, freqs = freqs,  # prepared in analyze() to save time
      autoCorrelation = autocorBank[, i],
      samplingRate = samplingRate,
      scaleCorrection = scaleCorrection,
      cutFreq = cutFreq,
      trackPitch = cond_entropy[i],
      pitchMethods = pitchMethods,
      nCands = nCands,
      pitchDom = pitchDom,
      pitchAutocor = pitchAutocor,
      pitchCep = pitchCep,
      pitchSpec = pitchSpec,
      pitchHps = pitchHps,
      pitchFloor = pitchFloor,
      pitchCeiling = pitchCeiling
    )
  }

  # Store the descriptives provided by function analyzeFrame in a dataframe
  result = lapply(frameInfo, function(y) y[['summaries']])
  result = data.frame(matrix(unlist(result), nrow=length(frameInfo), byrow=TRUE))
  colnames(result) = names(frameInfo[[1]]$summaries)
  if (!is.null(fmts)) result = cbind(result, fmts)
  result$entropy = entropy
  result$ampl = ampl
  result$time = as.numeric(colnames(frameBank))
  result$duration_noSilence = duration_noSilence
  result$duration = duration
  nc = ncol(result)
  result = result[, c(rev((nc-4):nc), 1:(nc-5))]  # change the order of columns

  ## Postprocessing
  # extract and prepare pitch candidates for the pathfinder algorithm
  pm_woDom = pitchMethods[pitchMethods != 'dom']
  if (length(pm_woDom) > 0) {
    pitchNames = data.frame(pitchMethod = pm_woDom,
                            stringsAsFactors = FALSE)
    pitchNames$pitchName = paste0(
      'pitch',
      toupper(substr(pitchNames$pitchMethod, 1, 1)),
      substr(pitchNames$pitchMethod, 2, nchar(pitchNames$pitchMethod))
    )
  } else {
    pitchNames = list('pitchMethod' = NULL, 'pitchName' = NULL)
  }

  max_cands = max(unlist(lapply(frameInfo, function(y)
    nrow(y[['pitchCands_frame']]))))
  if (max_cands == 0) {
    # no pitch candidates at all, purely unvoiced
    result[, c('pitch', pitchNames$pitchName)] = NA
    pitchCands_list = list()
  } else {
    # some pitch candidates found
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
    # each pitch tracking method
    result$pitch = pitchFinal # optimal pitch track
    if (!is.null(pitchNames$pitchMethod)) {
      for (p in 1:nrow(pitchNames)) {
        result[pitchNames$pitchName[p]] =  as.numeric(lapply(frameInfo, function(x) {
          idx_method = which(x$pitchCands_frame$pitchSource == pitchNames$pitchMethod[p])
          idx_maxCert = which.max(x$pitchCands_frame$pitchCert[idx_method])
          return(x$pitchCands_frame$pitchCand[idx_method] [idx_maxCert])
        }))
      }
    }
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
    result[, smoothVars] = medianSmoother(result[, smoothVars],
                                          smoothing_ww = smoothing_ww,
                                          smoothingThres = smoothingThres)
  }

  # Convert HNR to dB
  result$HNR = to_dB(result$HNR)

  ## Finalize / update results using the final pitch contour
  # (or the manual pitch contour, if provided)
  if (!is.null(pitchManual) &
      length(pitchManual) > 0) {  # numeric(0) if $pitch is missing in manual
    # up/downsample pitchManual to the right length
    pitch_true = upsamplePitchContour(
      pitch = pitchManual,
      len = nrow(result),
      plot = FALSE)
  } else {
    pitch_true = result$pitch
  }
  result = updateAnalyze(
    result = result,
    pitch_true = pitch_true,
    spectrogram = s,
    freqs = freqs,
    bin = bin,
    harmHeight_pars = harmHeight,
    smooth = smooth,
    smoothing_ww = smoothing_ww,
    smoothingThres = smoothing_ww
  )

  ## Add pitch contours to the spectrogram
  if (plot) {
    # we call spectrogram() a second time to get nice silence padding and to add
    # pitch contours internally in spectrogram() - a hassle, but it only take
    # a few ms, and otherwise it's hard to add pitch contours b/c the y-axis
    # is messed up if spectrogram() calls layout() to add an oscillogram
    do.call(spectrogram, c(list(
      x = sound,
      dynamicRange = dynamicRange,
      samplingRate = samplingRate,
      windowLength = windowLength,
      zp = zp,
      wn = wn,
      step = step,
      main = plotname,
      normalize = FALSE,
      scale = scale / m,
      output = 'original',
      ylim = ylim,
      xlab = xlab,
      ylab = ylab,
      plot = TRUE,
      osc = osc,
      osc_dB = osc_dB,
      internal = list(
        frameBank = frameBank,
        duration = duration,
        timeShift = timeShift,
        pitch = list(
          pitchCands = pitchCands_list$freq,
          pitchCert = pitchCands_list$cert,
          pitchSource = pitchCands_list$source,
          pitch = result$pitch,
          timestamps = result$time / 1000,  # spetcrogram always plots in s
          candPlot = list(
            dom = pitchDom_plotPars,
            autocor = pitchAutocor_plotPars,
            cep = pitchCep_plotPars,
            spec = pitchSpec_plotPars,
            hps = pitchHps_plotPars
          ),
          pitchPlot = pitchPlot,
          extraContour = result$harmHeight,
          extraContour_pars = harmHeight_plotPars,
          priorMean = priorMean,
          priorSD = priorSD,
          pitchFloor = pitchFloor,
          pitchCeiling = pitchCeiling,
          addToExistingPlot = TRUE,
          showLegend = showLegend,
          ylim = ylim,
          xlab = xlab,
          ylab = ylab,
          main = plotname,
          timeShift = timeShift
        ))), extraSpecPars))
  }
  if (is.character(savePath)) {
    dev.off()
  }

  # prepare the output
  sf = summaryFun[which(summaryFun != 'extended')]
  if (!is.null(sf) && any(!is.na(sf))) {
    out = summarizeAnalyze(result, sf)
  } else {
    out = result
  }
  if ('extended' %in% summaryFun) {
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
#' See \code{\link{pitch_app}} for a more realistic workflow: extract manually
#' corrected pitch contours with pitch_app(), then run analyzeFolder() with
#' these manual contours
#'
#' @seealso \code{\link{analyze}} \code{\link{pitch_app}}
#'   \code{\link{getLoudness}} \code{\link{segment}} \code{\link{getRMS}}
#'
#' @param myfolder full path to target folder
#' @param verbose if TRUE, reports progress and estimated time left
#' @param pitchManual normally the output of \code{\link{pitch_app}} containing
#'   a manually corrected pitch contour, ideally with the same windowLength and
#'   step as current call to analyzeFolder; a dataframe with at least two
#'   columns: "file" (w/o path) and "pitch" (character like "NA, 150, 175, NA")
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
#' s = analyzeFolder(myfolder, verbose = TRUE)  # ~ 10-20 minutes!
#' # s = write.csv(s, paste0(myfolder, '/temp.csv'))  # save a backup
#'
#' # Check accuracy: import manually verified pitch values (our "key")
#' # pitchManual   # "ground truth" of mean pitch per sound
#' # pitchContour  # "ground truth" of complete pitch contours per sound
#' files_manual = paste0(names(pitchManual), '.wav')
#' idx = match(s$file, files_manual)  # in case the order is wrong
#' s$key = pitchManual[idx]
#'
#' # Compare manually verified mean pitch with the output of analyzeFolder:
#' cor(s$key, s$pitch_median, use = 'pairwise.complete.obs')
#' plot(s$key, s$pitch_median, log = 'xy')
#' abline(a=0, b=1, col='red')
#'
#' # Re-running analyzeFolder with manually corrected contours gives correct
#' pitch-related descriptives like amplVoiced and harmonics (NB: you get it "for
#' free" when running pitch_app)
#' s1 = analyzeFolder(myfolder, verbose = TRUE, pitchManual = pitchContour)
#' plot(s$harmonics_median, s1$harmonics_median)
#' abline(a=0, b=1, col='red')
#'
#' # Save spectrograms with pitch contours plus an html file for easy access
#' s2 = analyzeFolder('~/Downloads/temp', savePlots = TRUE,
#'   showLegend = TRUE, pitchManual = pitchContour,
#'   width = 20, height = 12,
#'   units = 'cm', res = 300, ylim = c(0, 5))
#' }
analyzeFolder = function(
  myfolder,
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
  cutFreq = NULL,
  formants = list(verify = FALSE),
  nFormants = 3,
  pitchMethods = c('dom', 'autocor'),
  pitchManual = NULL,
  entropyThres = 0.6,
  pitchFloor = 75,
  pitchCeiling = 3500,
  priorMean = 300,
  priorSD = 6,
  nCands = 1,
  minVoicedCands = NULL,
  pitchDom = list(),
  pitchAutocor = list(),
  pitchCep = list(),
  pitchSpec = list(),
  pitchHps = list(),
  harmHeight = list(type = 'n'),
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
  summary = NULL,
  summaryFun = c('mean', 'median', 'sd'),
  plot = FALSE,
  showLegend = TRUE,
  savePlots = FALSE,
  pitchPlot = list(col = rgb(0, 0, 1, .75), lwd = 3, showPrior = TRUE),
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
  ## preliminaries - deprecated pars
  if (!missing('summary')) {
    message(paste0('summary', ' is deprecated, set "summaryFun = NULL" instead'))
  }

  warnAboutResetSummary = FALSE
  time_start = proc.time()  # timing
  filenames = list.files(myfolder, pattern = "*.wav|.mp3|.WAV|.MP3", full.names = TRUE)
  if (length(filenames) < 1) {
    stop(paste('No wav/mp3 files found in', myfolder))
  }
  filenames_base = basename(filenames)
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
    'pitchPlot', 'pitchManual')]
  # exclude ...
  myPars = myPars[1:(length(myPars)-1)]
  # add plot pars correctly, without flattening the lists
  myPars$pitchPlot = pitchPlot
  if (savePlots) myPars$savePath = myfolder

  # add pitchManual, if any
  if (!is.null(pitchManual)) {
    pitchManual_idx = match(filenames_base, pitchManual$file)
  }

  result = list()
  for (i in 1:length(filenames)) {
    pitch_file = NULL
    if (!is.null(pitchManual)) {
      if (is.finite(pitchManual_idx[i])) {
        pitch_file = suppressWarnings(as.numeric(unlist(strsplit(
          as.character(pitchManual$pitch[pitchManual_idx[i]]), ','))))
      } else {
        message(paste('File', filenames_base[i], 'not found in pitchManual$file'))
        summary = FALSE
        warnAboutResetSummary = TRUE
      }
    }
    result[[i]] = do.call(analyze, c(filenames[i],
                                     myPars,
                                     list(pitchManual = pitch_file),
                                     ...))
    if (verbose) {
      reportTime(i = i, nIter = length(filenames),
                 time_start = time_start, jobs = filesizes)
    }
  }

  # prepare output
  if (warnAboutResetSummary) {
    message('Cannot summarize the results when some files are missing in pitchManual')
  }
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    # if some sounds are too short, analyze() returns only duration, so we pad
    # those with NA to the right length in order to run rbind afterwards
    nc = max(unlist(lapply(result, ncol)), na.rm = TRUE) - 1
    for (i in 1:length(result)) {
      if (length(result[[i]]) == 1) {
        result[[i]] = c(as.numeric(result[[i]]), rep(NA, nc))
      }
    }
    output = cbind(data.frame(file = filenames_base),
                   do.call(rbind, result))
    # output = as.data.frame(t(sapply(result, function(x) unlist(rbind(x)))))
    # output$file = filenames_base
    # output = output[, c('file', colnames(output)[1:(ncol(output) - 1)])]
  } else {
    output = result
    names(output) = filenames_base
  }

  if (htmlPlots & savePlots) {
    htmlPlots(myfolder, myfiles = filenames, width = paste0(width, units))
  }

  invisible(output)
}
