# TODO: morph multiple sounds not just 2; maybe vectorize lipRad/noseRad; some smart rbind_fill in all ...Folder functions() in case of missing columns; soundgen- use psola when synthesizing 1 gc at a time; gaussian wn implemented in seewave (check updates!); soundgen - pitch2 for dual source (desynchronized vocal folds); AM aspiration noise (not really needed, except maybe for glottis > 0); soundgen() should accept smth like pitch = c(300, NA, 150, 250) and interpret this as two syllables with a pause - use eg as preview in manual pitch correction; morph() - tempEffects; streamline saving all plots a la ggsave: filename, path, different supported devices instead of only png(); automatic addition of pitch jumps at high temp in soundgen() (?)

# Debugging tip: run smth like options('browser' = '/usr/bin/chromium-browser') or options('browser' = '/usr/bin/google-chrome') to check a Shiny app in a non-default browser

#' @import stats graphics utils grDevices shinyBS
#' @encoding UTF-8
NULL

#' Generate a sound
#'
#' Generates a bout of one or more syllables with pauses between them. Two basic
#' components are synthesized: the harmonic component (the sum of sine waves
#' with frequencies that are multiples of the fundamental frequency) and the
#' noise component. Both components can be filtered with independently specified
#' formants. Intonation and amplitude contours can be applied both within each
#' syllable and across multiple syllables. Suggested application: synthesis of
#' animal or human non-linguistic vocalizations. For more information, see
#' \url{http://cogsci.se/soundgen.html} and vignette('sound_generation', package
#' = 'soundgen').
#'
#' @seealso \code{\link{generateNoise}} \code{\link{generateNoise}}
#'   \code{\link{fart}} \code{\link{beat}}
#'
#' @param repeatBout number of times the whole bout should be repeated
#' @param nSyl number of syllables in the bout. `pitchGlobal`, `amplGlobal`, and
#'   `formants` span multiple syllables, but not multiple bouts
#' @param sylLen average duration of each syllable, ms (vectorized)
#' @param pauseLen average duration of pauses between syllables, ms (can be
#'   negative between bouts: force with invalidArgAction = 'ignore')
#'   (vectorized)
#' @param pitch a numeric vector of f0 values in Hz or a dataframe
#'   specifying the time (ms or 0 to 1) and value (Hz) of each anchor, hereafter
#'   "anchor format". These anchors are used to create a smooth contour of
#'   fundamental frequency f0 (pitch) within one syllable
#' @param pitchGlobal unlike \code{pitch}, these anchors are
#'   used to create a smooth contour of average f0 across multiple syllables.
#'   The values are in semitones relative to the existing pitch, i.e. 0 = no
#'   change (anchor format)
#' @param glottis anchors for specifying the proportion of a
#'   glottal cycle with closed glottis, \% (0 = no modification, 100 = closed
#'   phase as long as open phase); numeric vector or dataframe specifying time
#'   and value (anchor format)
#' @param temperature hyperparameter for regulating the amount of stochasticity
#'   in sound generation
#' @param tempEffects a list of scaling coefficients regulating the effect of
#'   temperature on particular parameters. To change, specify just those pars
#'   that you want to modify (1 = default, 0 = no stochastic behavior).
#'   \code{amplDep, pitchDep, noiseDep}: random fluctuations of user-specified
#'   amplitude / pitch / noise anchors; \code{amplDriftDep}: drift of amplitude
#'   mirroring pitch drift; \code{formDisp}: dispersion of stochastic formants;
#'   \code{formDrift}: formant frequencies; \code{glottisDep}: proportion of
#'   glottal cycle with closed glottis; \code{pitchDriftDep}: amount of slow
#'   random drift of f0; \code{pitchDriftFreq}: frequency of slow random drift
#'   of f0; \code{rolloffDriftDep}: drift of rolloff mirroring pitch drift;
#'   \code{specDep}: rolloff, rolloffNoise, nonlinear effects, attack;
#'   \code{subDriftDep}: drift of subharmonic frequency and bandwidth mirroring
#'   pitch drift; \code{sylLenDep}: duration of syllables and pauses
#' @param maleFemale hyperparameter for shifting f0 contour, formants, and
#'   vocalTract to make the speaker appear more male (-1...0) or more female
#'   (0...+1); 0 = no change
#' @param creakyBreathy hyperparameter for a rough adjustment of voice quality
#'   from creaky (-1) to breathy (+1); 0 = no change
#' @param nonlinBalance hyperparameter for regulating the (approximate)
#'   proportion of sound with different regimes of pitch effects (none /
#'   subharmonics only / subharmonics and jitter). 0\% = no noise; 100\% = the
#'   entire sound has jitter + subharmonics. Ignored if temperature = 0
#' @param nonlinRandomWalk a numeric vector specifying the timing of nonliner
#'   regimes: 0 = none, 1 = subharmonics, 2 = subharmonics + jitter + shimmer
#' @param jitterLen duration of stable periods between pitch jumps, ms. Use a
#'   low value for harsh noise, a high value for irregular vibrato or shaky
#'   voice (anchor format)
#' @param jitterDep cycle-to-cycle random pitch variation, semitones (anchor
#'   format)
#' @param vibratoFreq the rate of regular pitch modulation, or vibrato, Hz
#'   (anchor format)
#' @param vibratoDep the depth of vibrato, semitones (anchor format)
#' @param shimmerDep random variation in amplitude between individual glottal
#'   cycles (0 to 100\% of original amplitude of each cycle) (anchor format)
#' @param shimmerLen duration of stable periods between amplitude jumps, ms. Use
#'   a low value for harsh noise, a high value for shaky voice (anchor format)
#' @param attackLen duration of fade-in / fade-out at each end of syllables and
#'   noise (ms): a vector of length 1 (symmetric) or 2 (separately for fade-in
#'   and fade-out)
#' @param rolloff basic rolloff from lower to upper harmonics, db/octave
#'   (exponential decay). All rolloff parameters are in anchor format. See
#'   \code{\link{getRolloff}} for more details
#' @param rolloffOct basic rolloff changes from lower to upper harmonics
#'   (regardless of f0) by \code{rolloffOct} dB/oct. For example, we can get
#'   steeper rolloff in the upper part of the spectrum
#' @param rolloffParab an optional quadratic term affecting only the first
#'   \code{rolloffParabHarm} harmonics. The middle harmonic of the first
#'   \code{rolloffParabHarm} harmonics is amplified or dampened by
#'   \code{rolloffParab} dB relative to the basic exponential decay
#' @param rolloffParabHarm the number of harmonics affected by
#'   \code{rolloffParab}
#' @param rolloffKHz rolloff changes linearly with f0 by \code{rolloffKHz}
#'   dB/kHz. For ex., -6 dB/kHz gives a 6 dB steeper basic rolloff as f0 goes up
#'   by 1000 Hz
#' @param rolloffExact user-specified exact strength of harmonics: a vector or
#'   matrix with one row per harmonic, scale 0 to 1 (overrides all other rolloff
#'   parameters)
#' @param lipRad the effect of lip radiation on source spectrum, dB/oct (the
#'   default of +6 dB/oct produces a high-frequency boost when the mouth is
#'   open)
#' @param noseRad the effect of radiation through the nose on source spectrum,
#'   dB/oct (the alternative to \code{lipRad} when the mouth is closed)
#' @param mouthOpenThres open the lips (switch from nose radiation to lip
#'   radiation) when the mouth is open \code{>mouthOpenThres}, 0 to 1
#' @param formants either a character string like "aaui" referring to default
#'   presets for speaker "M1" or a list of formant times, frequencies,
#'   amplitudes, and bandwidths (see ex. below). \code{formants = NA} defaults
#'   to schwa. Time stamps for formants and mouthOpening can be specified in ms
#'   or an any other arbitrary scale. See \code{\link{getSpectralEnvelope}} for
#'   more details
#' @param formantDep scale factor of formant amplitude (1 = no change relative
#'   to amplitudes in \code{formants})
#' @param formantDepStoch the amplitude of additional stochastic formants added
#'   above the highest specified formant, dB (only if temperature > 0)
#' @param formantWidth scale factor of formant bandwidth (1 = no change)
#' @param formantCeiling frequency to which stochastic formants are calculated,
#'   in multiples of the Nyquist frequency; increase up to ~10 for long vocal
#'   tracts to avoid losing energy in the upper part of the spectrum
#' @param formantLocking the approximate proportion of sound in which one of the
#'   harmonics is locked to the nearest formant, 0 = none, 1 = the entire sound
#'   (anchor format)
#' @param vocalTract the length of vocal tract, cm. Used for calculating formant
#'   dispersion (for adding extra formants) and formant transitions as the mouth
#'   opens and closes. If \code{NULL} or \code{NA}, the length is estimated
#'   based on specified formant frequencies, if any (anchor format)
#' @param subRatio a positive integer giving the ratio of f0 (the main
#'   fundamental) to g0 (a lower frequency): 1 = no subharmonics, 2 = period
#'   doubling regardless of pitch changes, 3 = period tripling, etc; subRatio
#'   overrides subFreq (anchor format)
#' @param subFreq instead of a specific number of subharmonics (subRatio), we
#'   can specify the approximate g0 frequency (Hz), which is used only if
#'   subRatio = 1 and is adjusted to f0 so f0/g0 is always an integer (anchor
#'   format)
#' @param subDep the depth of subharmonics relative to the main frequency
#'   component (f0), \%. 0: no subharmonics; 100: g0 harmonics are as strong as
#'   the nearest f0 harmonic (anchor format)
#' @param subWidth Width of subharmonic sidebands - regulates how rapidly
#'   g-harmonics weaken away from f-harmonics: large values like the default
#'   10000 means that all g0 harmonics are equally strong (anchor format)
#' @param shortestEpoch minimum duration of each epoch with unchanging
#'   subharmonics regime or formant locking, in ms
#' @param amDep amplitude modulation (AM) depth, \%. 0: no change; 100: AM with
#'   amplitude range equal to the dynamic range of the sound (anchor format)
#' @param amFreq AM frequency, Hz (anchor format)
#' @param amType "sine" = sinusoidal, "logistic" = logistic (default)
#' @param amShape ignore if amType = "sine", otherwise determines the shape of
#'   non-sinusoidal AM: 0 = ~sine, -1 = notches, +1 = clicks (anchor format)
#' @param noise loudness of turbulent noise (0 dB = as loud as
#'   voiced component, negative values = quieter) such as aspiration, hissing,
#'   etc (anchor format)
#' @param formantsNoise the same as \code{formants}, but for unvoiced instead of
#'   voiced component. If NA (default), the unvoiced component will be filtered
#'   through the same formants as the voiced component, approximating aspiration
#'   noise [h]
#' @param rolloffNoise,noiseFlatSpec linear rolloff of the excitation source for
#'   the unvoiced component, \code{rolloffNoise} dB/kHz (anchor format) applied
#'   above \code{noiseFlatSpec} Hz
#' @param rolloffNoiseExp exponential rolloff of the excitation source for the
#'   unvoiced component, dB/oct (anchor format) applied above 0 Hz
#' @param noiseAmpRef noise amplitude is defined relative to: "f0" = the
#'   amplitude of the first partial (fundamental frequency), "source" = the
#'   amplitude of the harmonic component prior to applying formants, "filtered"
#'   =  the amplitude of the harmonic component after applying formants
#' @param mouth mouth opening (0 to 1, 0.5 = neutral, i.e. no
#'   modification) (anchor format)
#' @param ampl amplitude envelope (dB, 0 = max amplitude) (anchor
#'   format)
#' @param amplGlobal global amplitude envelope spanning
#'   multiple syllables (dB, 0 = no change) (anchor format)
#' @param smoothing a list of parameters that control the smoothing of pitch and
#'   amplitude contours: \code{interpol} (loess, spline, or approx), loessSpan
#'   (1 = strong, .5 = weak smoothing), discontThres, and jumpThres (if two
#'   anchors are closer in time than \code{discontThres}, the contour is broken
#'   into segments with a linear transition between these anchors; if anchors
#'   are closer than \code{jumpThres}, a new section starts with no transition
#'   at all (e.g. for adding pitch jumps). See \code{\link{getSmoothContour}}
#' @param interpol,discontThres,jumpThres deprecated
#' @param samplingRate sampling frequency, Hz
#' @param windowLength length of FFT window, ms
#' @param overlap FFT window overlap, \%. For allowed values, see
#'   \code{\link[seewave]{istft}}
#' @param addSilence silence before and after the bout, ms
#' @param pitchFloor,pitchCeiling lower & upper bounds of f0
#' @param pitchSamplingRate sampling frequency of the pitch contour only, Hz.
#'   Low values reduce processing time. Set to \code{pitchCeiling} for optimal
#'   speed or to \code{samplingRate} for optimal quality
#' @param dynamicRange dynamic range, dB. Harmonics and noise more than
#'   dynamicRange under maximum amplitude are discarded to save computational
#'   resources
#' @param invalidArgAction what to do if an argument is invalid or outside the
#'   range in \code{permittedValues}: 'adjust' = reset to default value, 'abort'
#'   = stop execution, 'ignore' = throw a warning and continue (may crash)
#' @param plot if TRUE, plots a spectrogram
#' @param play if TRUE, plays the synthesized sound using the default player on
#'   your system. If character, passed to \code{\link[tuneR]{play}} as the name
#'   of player to use, eg "aplay", "play", "vlc", etc. In case of errors, try
#'   setting another default player for \code{\link[tuneR]{play}}
#' @param savePath full path for saving the output, e.g. '~/Downloads/temp.wav'.
#'   If NA (default), doesn't save anything
#' @param ... other plotting parameters passed to \code{\link{spectrogram}}
#' @export
#' @return Returns the synthesized waveform as a numeric vector.
#' @examples
#' # NB: GUI for soundgen is available as a Shiny app.
#' # Type "soundgen_app()" to open it in default browser
#'
#'# Set "playback" to TRUE for default system player or the name of preferred
#' # player (eg "aplay") to play back the audio from examples
#' playback = c(TRUE, FALSE, 'aplay', 'vlc')[2]
#'
#' sound = soundgen(play = playback)
#' # spectrogram(sound, 16000, osc = TRUE)
#' # playme(sound)
#'
#' # Control of intonation, amplitude envelope, formants
#' s0 = soundgen(
#'   pitch = c(300, 390, 250),
#'   ampl = data.frame(time = c(0, 50, 300), value = c(-5, -10, 0)),
#'   attack = c(10, 50),
#'   formants = c(600, 900, 2200),
#'   play = playback
#' )
#'
#' # Use the in-built collection of presets:
#' # names(presets)  # speakers
#' # names(presets$Chimpanzee)  # calls per speaker
#' s1 = eval(parse(text = presets$Chimpanzee$Scream_conflict))  # screaming chimp
#' # playme(s1)
#' s2 = eval(parse(text = presets$F1$Scream))  # screaming woman
#' # playme(s2, 18320)
#' \dontrun{
#' # unless temperature is 0, the sound is different every time
#' for (i in 1:3) sound = soundgen(play = playback, temperature = .2)
#'
#' # Bouts versus syllables. Compare:
#' sound = soundgen(formants = 'uai', repeatBout = 3, play = playback)
#' sound = soundgen(formants = 'uai', nSyl = 3, play = playback)
#'
#' # Intonation contours per syllable and globally:
#' sound = soundgen(nSyl = 5, sylLen = 200, pauseLen = 140,
#'   play = playback, pitch = data.frame(
#'     time = c(0, 0.65, 1), value = c(977, 1540, 826)),
#'   pitchGlobal = data.frame(time = c(0, .5, 1), value = c(-6, 7, 0)))
#'
#' # Subharmonics in sidebands (noisy scream)
#' sound = soundgen (subFreq = 75, subDep = runif(10, 0, 60), subWidth = 130,
#'   pitch = data.frame(
#'     time = c(0, .3, .9, 1), value = c(1200, 1547, 1487, 1154)),
#'   sylLen = 800,
#'   play = playback, plot = TRUE)
#'
#' # Jitter and mouth opening (bark, dog-like)
#' sound = soundgen(repeatBout = 2, sylLen = 160, pauseLen = 100,
#'   subFreq = 100, subDep = 100, subWidth = 60, jitterDep = 1,
#'   pitch = c(559, 785, 557),
#'   mouth = c(0, 0.5, 0),
#'   vocalTract = 5, formants = NULL,
#'   play = playback, plot = TRUE)
#'
#' # See the vignette on sound generation for more examples and in-depth
#' # explanation of the arguments to soundgen()
#' # Examples of code for creating human and animal vocalizations are available
#' # on project's homepage: http://cogsci.se/soundgen.html
#' }
soundgen = function(
  repeatBout = 1,
  nSyl = 1,
  sylLen = 300,
  pauseLen = 200,
  pitch = list(time = c(0, .1, .9, 1),
               value = c(100, 150, 135, 100)),
  pitchGlobal = NA,
  glottis = 0,
  temperature = 0.025,
  tempEffects = list(),
  maleFemale = 0,
  creakyBreathy = 0,
  nonlinBalance = 100,
  nonlinRandomWalk = NULL,
  subRatio = 2,
  subFreq = 0,
  subDep = 0,
  subWidth = 10000,
  shortestEpoch = 300,
  jitterLen = 1,
  jitterDep = 0,
  vibratoFreq = 5,
  vibratoDep = 0,
  shimmerDep = 0,
  shimmerLen = 1,
  attackLen = 50,
  rolloff = -9,
  rolloffOct = 0,
  rolloffKHz = -3,
  rolloffParab = 0,
  rolloffParabHarm = 3,
  rolloffExact = NULL,
  lipRad = 6,
  noseRad = 4,
  mouthOpenThres = 0,
  formants = c(860, 1430, 2900),
  formantDep = 1,
  formantDepStoch = 1,
  formantWidth = 1,
  formantCeiling = 2,
  formantLocking = 0,
  vocalTract = NA,
  amDep = 0,
  amFreq = 30,
  amType = c('logistic', 'sine')[1],
  amShape = 0,
  noise = NULL,
  formantsNoise = NA,
  rolloffNoise = -4,
  noiseFlatSpec = 1200,
  rolloffNoiseExp = 0,
  noiseAmpRef = c('f0', 'source', 'filtered')[3],
  mouth = list(time = c(0, 1),
               value = c(.5, .5)),
  ampl = NA,
  amplGlobal = NA,
  smoothing = list(interpol = c('approx', 'spline', 'loess')[3],
                   loessSpan = NULL,
                   discontThres = .05,
                   jumpThres = .01),
  interpol = 'deprecated',
  discontThres = 'deprecated',
  jumpThres = 'deprecated',
  samplingRate = 16000,
  windowLength = 50,
  overlap = 75,
  addSilence = 100,
  pitchFloor = 1,
  pitchCeiling = 3500,
  pitchSamplingRate = 16000,
  dynamicRange = 80,
  invalidArgAction = c('adjust', 'abort', 'ignore')[1],
  plot = FALSE,
  play = FALSE,
  savePath = NA,
  ...
) {
  # deprecated pars
  if (!missing('interpol')) {
    smoothing$interpol = interpol
    message('interpol is deprecated; use "smoothing" instead')
  }
  if (!missing('discontThres')) {
    smoothing$discontThres = discontThres
    message('discontThres is deprecated; use "smoothing" instead')
  }
  if (!missing('jumpThres')) {
    smoothing$discontThres = discontThres
    message('discontThres is deprecated; use "smoothing" instead')
  }
  if (FALSE) shinyjs::info('adja')  # to avoid a NOTE on CRAN

  # check that values of numeric arguments are valid and within range
  pars_to_check = rownames(permittedValues)[1:which(
    rownames(permittedValues) == 'noiseFlatSpec'
  )]

  for (p in pars_to_check) {
    gp = try(get(p), silent = TRUE)
    if (class(gp)[1] != "try-error") {
      if (is.numeric(gp)) {
        assign(noquote(p),
               validatePars(p, gp, permittedValues, invalidArgAction))
      }
    }
  }
  if (any(pauseLen < 0) & nSyl > 1) {
    stop(paste(
      'Negative pauseLen is allowed between bouts, but not between syllables.',
      'Use repeatBout instead of nSyl if you need syllables to overlap'
    ))
  }

  # check that the overlap setting is valid
  o = 25 / (100 - overlap)
  if (round(o) != o) {
    overlap = 75
    warning(paste(
      'overlap must satisfy 100 * (1 - 1 / (4 * positive_integer)).',
      'Resetting to 75%. OK values: 75, 87.5, 93.75, 95. See ?seewave::istft'
    ))
  }

  ## stochastic rounding of the number of syllables and repeatBouts
  #   (eg for nSyl = 2.5, we'll have 2 or 3 syllables with equal probs)
  #   NB: this is very useful for morphing
  idx_nonInt_nSyl = which(!is.integer(nSyl))
  if (any(idx_nonInt_nSyl)) {
    nSyl[idx_nonInt_nSyl] = floor(nSyl[idx_nonInt_nSyl]) +
      rbinom(1, 1, nSyl[idx_nonInt_nSyl] - floor(nSyl[idx_nonInt_nSyl]))
  }
  if (!is.integer(repeatBout)) {
    repeatBout = floor(repeatBout) +
      rbinom(1, 1, repeatBout - floor(repeatBout))
  }

  # check and, if necessary, reformat anchors to dataframes
  for (anchor in c('pitch', 'pitchGlobal', 'glottis',
                   'ampl', 'amplGlobal',
                   'mouth', 'vocalTract', 'formantLocking',
                   'vibratoFreq', 'vibratoDep',
                   'subRatio', 'subFreq', 'subDep', 'subWidth',
                   'jitterLen', 'jitterDep', 'shimmerLen', 'shimmerDep',
                   'rolloff', 'rolloffOct', 'rolloffKHz',
                   'rolloffParab', 'rolloffParabHarm',
                   'amDep', 'amFreq', 'amShape')) {
    assign(anchor, reformatAnchors(get(anchor)))
  }
  if (is.numeric(noise)) {
    # if noise is numeric, fix exactly same timing as voiced
    lockNoiseToVoiced = TRUE
    if (length(noise) > 0) {
      noise = data.frame(
        time = seq(0, sylLen[1], length.out = max(2, length(noise))),
        value = noise
      )
    }
  } else {
    lockNoiseToVoiced = FALSE
  }
  if (is.list(pitch) & invalidArgAction != 'ignore') {
    if (any(pitch$value < pitchFloor)) {
      pitchFloor = 0.1
      message('Some pitch values are lower than pitchFloor; lowering to 0.1 Hz')
    }
    if (any(pitch$value >= samplingRate / 2)) {
      samplingRate = max(pitch$value * 4)
      message(paste('Some pitch values exceed Nyquist frequency;',
                    'raising samplingRate to', samplingRate, 'Hz'))
    }
    if (any(pitch$value > pitchSamplingRate)) {
      pitchSamplingRate = samplingRate
      message(paste('Some pitch values exceed pitchSamplingRate.',
                    'Resetting pitchSamplingRate to samplingRate.'))
    }
    if (any(pitch$value > pitchCeiling)) {
      pitchCeiling = samplingRate / 2
      message(paste('Some pitch values exceed pitchCeiling.',
                    'Resetting pitchCeiling to Nyquist frequency (samplingRate / 2).'))
    }
    mp = max(pitch$value)
    if (mp * 10 > pitchSamplingRate) {
      pitchSamplingRate = mp * 10
      message(paste0('pitchSampingRate should be much higher than the ',
                     'highest pitch; resetting to ', mp * 10, ' Hz'))
    }
    if (pitchSamplingRate > samplingRate) {
      samplingRate = pitchSamplingRate
      message(paste0('Resetting samplingRate to ',
                     samplingRate, ' Hz because of high pitch'))
    }
  }

  # check amplitude anchors and make all values negative
  if (is.list(ampl)) {
    if (any(ampl$value > 0)) {
      ampl$value = ampl$value - max(ampl$value)
      message(paste('The recommended range for ampl is (-dynamicRange, 0).',
                    'If positive, values are transformed to be non-positive'))
    }
  }

  # for amplGlobal, make the first value 0
  if (is.list(amplGlobal)) {
    if (any(amplGlobal$value != 0)) {
      amplGlobal$value = amplGlobal$value - amplGlobal$value[1]
    }
  }

  # make sure sylLen and pauseLen are vectors of appropriate length
  sylLen = getSmoothContour(anchors = sylLen, len = nSyl)
  if (nSyl > 1) {
    pauseLen = getSmoothContour(anchors = pauseLen, len = nSyl - 1)
  }

  windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2

  # # preliminary glottis contour
  # glottisClosed = getSmoothContour(anchors = glottis, len = 100)
  # # adjust length based on proportion of closed glottis (pauses added)
  # mean_closed = mean(glottisClosed) / 100
  # sylLen = sylLen / (mean_closed + 1)
  # if (is.list(pitch)) {
  #   pitch$value = pitch$value * (mean_closed + 1)
  # }

  # tempEffects are either left at default levels or multiplied by user-supplied
  # scaling coefficients (1 = no change)
  es = c('amplDep', 'amplDriftDep', 'formDisp', 'formDrift', 'glottisDep',
         'noiseDep', 'pitchDep', 'pitchDriftDep', 'pitchDriftFreq',
         'rolloffDriftDep', 'specDep', 'subDriftDep', 'sylLenDep')
  for (e in es) {
    if (!is.numeric(tempEffects[[e]])) {
      tempEffects[[e]] = defaults[[e]]
    } else {
      tempEffects[[e]] = defaults[[e]] * tempEffects[[e]]
    }
  }
  for (s in 1:length(es)) {
    name_s = names(tempEffects)[s]
    if (!name_s %in% es) {
      message(paste0('"', name_s, '" is not among valid temEffects parameters (',
                     paste(es, collapse = ', '),
                     '). See ?soundgen'))
    }
  }

  # Validate smoothing par-s
  sm = c('discontThres', 'jumpThres', 'loessSpan', 'interpol')
  for (s in 1:length(smoothing)) {
    name_s = names(smoothing)[s]
    if (!name_s %in% sm) {
      message(paste0('"', name_s, '" is not among valid smoothing parameters (',
                     paste(sm, collapse = ', '),
                     '). See ?getSmoothContour'))
    }
  }
  for (s in c('discontThres', 'jumpThres')) {
    if (is.numeric(smoothing[[s]])) {
      smoothing[[s]] = validatePars(
        s, smoothing[[s]], permittedValues, invalidArgAction
      )
    } else {
      smoothing[[s]] = defaults[[s]]
    }
  }
  if (!is.null(smoothing$loessSpan)) {
    smoothing$loessSpan = validatePars(
      'loessSpan', smoothing$loessSpan, permittedValues, invalidArgAction
    )
  }
  if (is.null(smoothing$interpol)) {
    smoothing$interpol = 'loess'
  } else if (!smoothing$interpol %in% c('approx', 'spline', 'loess')) {
    warning(paste('Supported interpol: approx, spline, loess;',
                  'defaulting to loess'))
    smoothing$interpol = 'loess'
  }

  # expand formants to full format for adjusting bandwidth if creakyBreathy > 0
  formants = reformatFormants(formants)
  formantsNoise = reformatFormants(formantsNoise)
  if (is.list(formantsNoise) & !is.numeric(vocalTract) & !is.list(vocalTract)) {
    # the only cond in which we do not create extra stochastic formants for
    # noise is when we have user-specified formantsNoise (ie not just breathing)
    # and we don't know VTL
    formantDepStoch_noise = 0
  } else {
    formantDepStoch_noise = formantDepStoch
  }

  # Do we have source-filter interaction?
  sourceFilterInter = FALSE
  if (is.list(formantLocking)) {
    if (any(formantLocking$value > 0)) {
      sourceFilterInter = TRUE
    }
  }

  ## adjust parameters according to the specified hyperparameters
  # effects of creakyBreathy hyper
  if (creakyBreathy < 0) {
    # for creaky voice
    nonlinBalance = min(100, nonlinBalance - creakyBreathy * 100)
    jitterDep$value = jitterDep$value - creakyBreathy / 2
    jitterDep$value[jitterDep$value < 0] = 0
    shimmerDep$value = shimmerDep$value - creakyBreathy * 5
    shimmerDep$value[shimmerDep$value < 0] = 0
    subDep$value = subDep$value - 100 * creakyBreathy
  } else if (creakyBreathy > 0) {
    # for breathy voice, add breathing
    if (!is.list(noise)) {
      noise = data.frame(time = c(0, sylLen[1] + 100),
                         value = c(-dynamicRange, -dynamicRange))
    }
    noise$value = noise$value +
      creakyBreathy * (dynamicRange + permittedValues['noiseAmpl', 'high'])
    noise$value[noise$value >
                  permittedValues['noiseAmpl', 'high']] =
      permittedValues['noiseAmpl', 'high']
    # increase formant bandwidths by up to 100%
    if (is.list(formants)) {
      for (f in 1:length(formants)) {
        formants[[f]]$width = formants[[f]]$width * (creakyBreathy + 1)
      }
    }
  }
  # adjust rolloff for both creaky and breathy voices
  rolloff$value = rolloff$value - creakyBreathy * 10
  rolloffOct$value = rolloffOct$value - creakyBreathy * 1
  if (invalidArgAction != 'ignore') {
    rolloff$value[rolloff$value < permittedValues['rolloff', 'low']] =
      permittedValues['rolloff', 'low']
    rolloff$value[rolloff$value > permittedValues['rolloff', 'high']] =
      permittedValues['rolloff', 'high']
    rolloffOct$value[rolloffOct$value < permittedValues['rolloffOct', 'low']] =
      permittedValues['rolloffOct', 'low']
    rolloffOct$value[rolloffOct$value > permittedValues['rolloffOct', 'high']] =
      permittedValues['rolloffOct', 'high']
  }

  # reformat noise
  if (!is.list(noise)) {
    noise = data.frame(
      time = c(0, sylLen[1]),
      value = c(-dynamicRange, -dynamicRange)
    )
  }

  # effects of maleFemale hyper
  if (maleFemale != 0) {
    # adjust pitch and formants along the male-female dimension
    # pitch varies by 1 octave up or down
    if (is.list(pitch)) {
      pitch$value = pitch$value * 2 ^ maleFemale
    }
    if (is.list(formants)) {
      for (f in 1:length(formants)) {
        # formants vary by 25% up or down:
        #   see http://www.santiagobarreda.com/vignettes/v1/v1.html)
        if (!is.null(formants[[f]]$freq)) {
          formants[[f]]$freq = formants[[f]]$freq * 1.25 ^ maleFemale
        }
      }
    }
    # vocalTract varies by 25% from the average
    if (is.list(vocalTract)) {
      if (is.numeric(vocalTract$value)) {
        vocalTract$value = vocalTract$value * (1 - .25 * maleFemale)
      }
    }
  }

  # prepare a list of pars for calling generateHarmonics()
  pars_to_vary = c(
    'attackLen',
    'shortestEpoch'
  )  # don't add nonlinBalance, otherwise there is no simple way to remove noise at temp>0
  anchors_to_wiggle = c(
    'pitch', 'ampl', 'glottis',
    'vibratoFreq', 'vibratoDep',
    'subRatio', 'subDep', 'subWidth',  # no subFreq - overrides subRatio
    'jitterLen', 'jitterDep',
    'shimmerLen', 'shimmerDep',
    'rolloff', 'rolloffKHz', 'rolloffOct',
    'rolloffParab', 'rolloffParabHarm')
  pars_to_round = c('attackLen', 'subRatio', 'subFreq', 'subWidth')
  pars_list = list(
    'attackLen' = attackLen,
    'jitterDep' = jitterDep,
    'jitterLen' = jitterLen,
    'vibratoFreq' = vibratoFreq,
    'vibratoDep' = vibratoDep,
    'shimmerDep' = shimmerDep,
    'shimmerLen' = shimmerLen,
    'rolloff' = rolloff,
    'rolloffOct' = rolloffOct,
    'rolloffKHz' = rolloffKHz,
    'rolloffParab' = rolloffParab,
    'rolloffParabHarm' = rolloffParabHarm,
    'rolloffExact' = rolloffExact,
    'temperature' = temperature,
    'pitchDriftDep' = tempEffects$pitchDriftDep,
    'pitchDriftFreq' = tempEffects$pitchDriftFreq,
    'amplDriftDep' = tempEffects$amplDriftDep,
    'subDriftDep' = tempEffects$subDriftDep,
    'rolloffDriftDep' = tempEffects$rolloffDriftDep,
    'shortestEpoch' = shortestEpoch,
    'subRatio' = subRatio,
    'subFreq' = subFreq,
    'subDep' = subDep,
    'subWidth' = subWidth,
    'nonlinBalance' = nonlinBalance,
    'nonlinRandomWalk' = nonlinRandomWalk,
    'pitchFloor' = pitchFloor,
    'pitchCeiling' = pitchCeiling,
    'pitchSamplingRate' = pitchSamplingRate,
    'dynamicRange' = dynamicRange,
    'interpol' = smoothing$interpol,
    'samplingRate' = samplingRate,
    'overlap' = overlap
  )
  pars_syllable = pars_list
  pitchDeltas = rep(1, nSyl)
  if (is.list(pitchGlobal)) {
    if (any(!is.na(pitchGlobal))) {
      if (any(pitchGlobal$value != 0) & nSyl > 1) {
        pitchDeltas = 2 ^ (
          getDiscreteContour(
            len = nSyl,
            anchors = pitchGlobal,
            interpol = 'spline',
            plot = FALSE
          ) / 12
        )
      }
    }
  }

  # make sure pitch$time ranges from 0 to 1
  if (is.list(pitch)) {
    if (min(pitch$time) < 0) {
      pitch$time = pitch$time - min(pitch$time)
    }
    if (max(pitch$time) > 1) {
      pitch$time = pitch$time / max(pitch$time)
    }
  }

  wiggleNoise = FALSE
  if (is.list(noise)) {
    if (temperature > 0 &
        any(noise$value > -dynamicRange) &
        !lockNoiseToVoiced) {
      wiggleNoise = TRUE
    }
  }

  wiggleAmpl_per_syl = FALSE
  if (is.numeric(ampl) | is.list(ampl)) {
    if (temperature > 0 & any(ampl$value < dynamicRange)) {
      wiggleAmpl_per_syl = TRUE
    }
  }

  wiggleGlottis = FALSE
  if (is.numeric(ampl) | is.list(ampl)) {
    if (temperature > 0 & any(glottis$value > 0)) {
      wiggleGlottis = TRUE
    }
  }

  # For polysyllabic vocalizations, calculate amplitude envelope correction
  # per voiced syllable
  if (is.list(amplGlobal)) {
    if (any(amplGlobal$value != 0)) {
      amplEnvelope = do.call(getSmoothContour, c(
        smoothing, list(
          anchors = amplGlobal,
          len = nSyl,
          valueFloor = -dynamicRange,
          valueCeiling = dynamicRange,
          samplingRate = samplingRate
        )))
      # convert from dB to linear multiplier
      amplEnvelope = 10 ^ (amplEnvelope / 20)
    }
  } else {
    amplEnvelope = rep(1, nSyl)
  }

  # prepare a list of formantPars in case we add source-filter interaction
  formantPars = list(
    vocalTract = vocalTract,
    formantDep = formantDep,
    formantWidth = formantWidth,
    formantCeiling = formantCeiling,
    lipRad = lipRad,
    noseRad = noseRad,
    mouthOpenThres = mouthOpenThres,
    mouth = mouth,
    interpol = smoothing$interpol,
    temperature = temperature,
    formDrift = tempEffects$formDrift,
    formDisp = tempEffects$formDisp,
    samplingRate = samplingRate,
    windowLength_points = windowLength_points,
    overlap = overlap
  )

  # START OF BOUT GENERATION
  for (b in 1:repeatBout) {
    # syllable segmentation
    syllables = divideIntoSyllables(
      sylLen = sylLen,
      nSyl = nSyl,
      pauseLen = pauseLen,
      sylDur_min = permittedValues['sylLen', 'low'],
      sylDur_max = permittedValues['sylLen', 'high'],
      pauseDur_min = permittedValues['pauseLen', 'low'],
      pauseDur_max = permittedValues['pauseLen', 'high'],
      temperature = temperature * tempEffects$sylLenDep
    )
    # end of syllable segmentation

    # Prepare a spectral envelope. It's added after syllable generation,
    # but we set it up here to be able to add source-filter interactions
    # such as formant-locking
    if (sourceFilterInter) {
      step_points = windowLength_points - (overlap * windowLength_points / 100)
      approx_dur_points = syllables$end[nrow(syllables)] / 1000 * samplingRate

      specEnv_list = do.call(getSpectralEnvelope, c(
        formantPars,
        list(nc = round(approx_dur_points / step_points), # approximate n of windows for fft
             nr = windowLength_points / 2, # n of frequency bins for fft
             formants = formants,
             formantDepStoch = formantDepStoch,
             normalize = FALSE,
             output = 'detailed')
      ))
      # image(t(specEnv_list$specEnv))
    }


    # START OF SYLLABLE GENERATION
    voiced = vector()
    unvoiced = list()
    noise_syl = list()

    for (s in 1:nrow(syllables)) {
      # scale noise anchors for polysyllabic sounds with lengh(sylLen) > 1
      noise_syl[[s]] = noise
      noise_syl[[s]]$time = scaleNoiseAnchors(
        noiseTime = noise_syl[[s]]$time,
        sylLen_old = sylLen[1],
        sylLen_new = syllables$dur[s]
      )

      # wiggle par values for this particular syllable, making sure
      #   they are within the permitted range for each variable
      for (anchor in anchors_to_wiggle) {
        newName = paste0(anchor, '_per_syl')
        assign(newName, get(anchor))  # just create a copy with name _per_syl
      }

      if (temperature > 0) {
        # OR if (temperature>0 & nrow(syllables)>1)
        # if you don't want to mess with single-syllable vocalizations
        for (p in 1:length(pars_to_vary)) {
          par_value = as.numeric(unlist(pars_list[pars_to_vary[p]]))
          l = permittedValues[pars_to_vary[p], 'low']
          h = permittedValues[pars_to_vary[p], 'high']
          sd = (h - l) * temperature * tempEffects$specDep
          pars_syllable[[pars_to_vary[p]]] = rnorm_truncated(
            n = length(par_value),
            mean = par_value,
            low = l,
            high = h,
            sd = sd,
            roundToInteger = (pars_to_vary[p] %in% pars_to_round),
            invalidArgAction = invalidArgAction
          )
        }
        if (is.list(pitch_per_syl)) {
          pitch_per_syl = wiggleAnchors(
            df = pitch_per_syl,
            temperature = temperature,
            low = c(0, pitchFloor),
            high = c(1, pitchCeiling),
            temp_coef = tempEffects$pitchDep,
            invalidArgAction = invalidArgAction
          )
        }
        if (wiggleNoise) {
          noise_syl[[s]] = wiggleAnchors(
            df = noise_syl[[s]],
            temperature = temperature,
            low = c(-Inf, -dynamicRange),
            high = c(+Inf, permittedValues['noiseAmpl', 'high']),
            wiggleAllRows = TRUE,
            temp_coef = tempEffects$noiseDep,
            invalidArgAction = invalidArgAction
          )
        }
        if (wiggleAmpl_per_syl) {
          ampl_per_syl = wiggleAnchors(
            df = ampl_per_syl,
            temperature = temperature,
            low = c(0, -dynamicRange),
            high = c(1, 0),
            temp_coef = tempEffects$amplDep,
            invalidArgAction = invalidArgAction
          )
        }
        if (wiggleGlottis) {
          glottis_per_syl = wiggleAnchors(
            df = glottis_per_syl,
            temperature = temperature,
            low = c(0, 0),
            high = c(1, Inf),
            temp_coef = tempEffects$glottisDep,
            invalidArgAction = invalidArgAction
          )
        }
        # wiggle anchors except special cases (pitch, ampl, glottis)
        for (i in 4:length(anchors_to_wiggle)) {
          anchor = anchors_to_wiggle[i]
          # anchor_per_syl = paste0(anchor, '_per_syl')
          l = permittedValues[anchor, 'low']
          h = permittedValues[anchor, 'high']
          anchor_new = wiggleAnchors(
            df = get(anchor),
            temperature = temperature,
            low = c(0, l),
            high = c(1, h),
            temp_coef = tempEffects$specDep,
            sd_values = (h - l) * temperature * tempEffects$specDep,
            roundToInteger = (anchor %in% pars_to_round),
            invalidArgAction = invalidArgAction
          )
          # assign(anchor_per_syl, anchor_new)
          pars_syllable[[anchor]] = anchor_new
        }
      }

      # generate smooth pitch contour for this particular syllable
      dur_syl = as.numeric(syllables[s, 'end'] - syllables[s, 'start'])
      if (is.list(pitch_per_syl) | is.numeric(pitch_per_syl)) {
        pitchContour_syl = do.call(getSmoothContour, c(
          smoothing, list(
            anchors = pitch_per_syl,
            len = round(dur_syl * pitchSamplingRate / 1000),
            samplingRate = pitchSamplingRate,
            valueFloor = pitchFloor,
            valueCeiling = pitchCeiling,
            thisIsPitch = TRUE
          ))) * pitchDeltas[s]
        # plot(pitchContour_syl, type = 'l')
      }

      # generate the voiced part only if noise is weaker than 40 dB
      #   and the voiced part is long enough to bother synthesizing it
      generateVoiced = TRUE
      if (dur_syl < permittedValues['sylLen', 'low'] | !is.list(pitch_per_syl)) {
        generateVoiced = FALSE
      }
      if (is.list(noise)) {
        if (min(noise$value) >= 40) {
          generateVoiced = FALSE
        }
      }

      if (!generateVoiced) {
        syllable = rep(0, round(dur_syl * samplingRate / 1000))
      } else {
        # ***THE ACTUAL SYNTHESIS IS HERE***
        # print(pars_syllable)
        syllable = try(do.call(generateHarmonics, c(
          pars_syllable,
          list(pitch = pitchContour_syl,
               ampl = ampl_per_syl,
               glottis = glottis_per_syl,
               normalize = ifelse(noiseAmpRef == 'f0', FALSE, TRUE),
               formantLocking = switch(sourceFilterInter + 1, NULL, formantLocking),
               # ifelse doesn't return NULL properly, thus switch() instead
               specEnv = switch(sourceFilterInter + 1, NULL, specEnv_list$specEnv),
               formantSummary = switch(sourceFilterInter + 1, NULL, specEnv_list$formantSummary))
        )) * amplEnvelope[s]  # correction of amplitude per syllable
        )
      }
      # plot(syllable, type = 'l')
      # spectrogram(syllable, samplingRate = samplingRate)
      # playme(syllable, samplingRate = samplingRate)
      # ***THE ACTUAL SYNTHESIS IS HERE***

      if (class(syllable)[1] == 'try-error') {
        stop('Failed to generate the new syllable!')
      }
      # if (any(is.na(syllable))) {
      #   stop('The new syllable contains NA values!')
      # }

      # generate a pause for all but the last syllable
      if (s < nrow(syllables)) {
        pause = rep(0, floor((syllables[s + 1, 'start'] - syllables[s, 'end']) *
                               samplingRate / 1000))
      } else {
        pause = numeric()
      }

      # add syllable and pause to the growing bout
      actualSylLen = length(syllable) / samplingRate * 1000
      voiced = c(voiced, syllable, pause)

      # update syllable timing info, b/c with temperature > 0, jitter etc
      # there may be deviations from the target duration
      actualBoutLen = length(voiced) / samplingRate * 1000
      if (s < nrow(syllables)) {
        correction = actualBoutLen - syllables[s + 1, 'start']
        syllables[(s + 1):nrow(syllables), c('start', 'end')] =
          syllables[(s + 1):nrow(syllables), c('start', 'end')] + correction
      }

      # scale noise anchors again to take into account the actual sylLen
      if (lockNoiseToVoiced) {
        noise_syl[[s]]$time = scaleNoiseAnchors(
          noiseTime = noise_syl[[s]]$time,
          sylLen_old = max(noise_syl[[s]]$time),
          sylLen_new = actualSylLen # syllables$dur[s]
        )
      }

      # generate the unvoiced part, but don't add it to the sound just yet
      if (is.list(noise)) {
        if (any(noise$value > -dynamicRange)) {
          if (is.list(rolloffNoise)) {
            rolloffNoise_syl = wiggleAnchors(
              rolloffNoise,
              temperature = temperature,
              temp_coef = tempEffects$specDep,
              low = c(-Inf, permittedValues['rolloffNoise', 'low']),
              high = c(Inf, permittedValues['rolloffNoise', 'high']),
              wiggleAllRows = TRUE,
              invalidArgAction = invalidArgAction
            )
          } else {
            rolloffNoise_syl = rnorm_truncated(
              n = length(rolloffNoise),
              mean = rolloffNoise,
              sd = abs(rolloffNoise) * temperature * tempEffects$specDep,
              low = permittedValues['rolloffNoise', 'low'],
              high = permittedValues['rolloffNoise', 'high'],
              invalidArgAction = invalidArgAction
            )
          }

          if (is.list(rolloffNoiseExp)) {
            rolloffNoiseExp_syl = wiggleAnchors(
              rolloffNoiseExp,
              temperature = temperature,
              temp_coef = tempEffects$specDep,
              low = c(-Inf, permittedValues['rolloffNoiseExp', 'low']),
              high = c(Inf, permittedValues['rolloffNoiseExp', 'high']),
              wiggleAllRows = TRUE,
              invalidArgAction = invalidArgAction
            )
          } else {
            rolloffNoiseExp_syl = rnorm_truncated(
              n = length(rolloffNoiseExp),
              mean = rolloffNoiseExp,
              sd = abs(rolloffNoiseExp) * temperature * tempEffects$specDep,
              low = permittedValues['rolloffNoiseExp', 'low'],
              high = permittedValues['rolloffNoiseExp', 'high'],
              invalidArgAction = invalidArgAction
            )
          }
          # synthesize the unvoiced part
          unvoiced[[s]] = generateNoise(
            len = round(diff(range(noise_syl[[s]]$time)) * samplingRate / 1000),
            noise = noise_syl[[s]],
            rolloffNoise = rolloffNoise_syl,
            rolloffNoiseExp = rolloffNoiseExp_syl,
            noiseFlatSpec = noiseFlatSpec,
            temperature = 0,  # wiggled separately in soundgen
            attackLen = attackLen,
            samplingRate = samplingRate,
            windowLength_points = windowLength_points,
            overlap = overlap,
            dynamicRange = dynamicRange,
            invalidArgAction = invalidArgAction,
            spectralEnvelope = NULL # spectralEnvelopeNoise
          ) * amplEnvelope[s]  # correction of amplitude per syllable
          # plot(unvoiced[[s]], type = 'l')
        }
      }
    }
    # plot(voiced, type = 'l')
    # spectrogram(voiced, samplingRate = samplingRate, osc = TRUE)
    # playme(voiced, samplingRate = samplingRate)
    # END OF SYLLABLE GENERATION

    ## Add unvoiced fragments together
    sound_unvoiced = rep(0, length(voiced))
    if (length(unvoiced) > 0) {
      for (s in 1:length(unvoiced)) {
        # calculate where syllable s begins
        syllableStartIdx = round(syllables[s, 'start'] * samplingRate / 1000)
        if (s == 1) syllableStartIdx = 1  # instead of 0

        # calculate where unvoiced is to be inserted
        insertionIdx = syllableStartIdx +
          noise_syl[[s]]$time[1] * samplingRate / 1000
        sound_unvoiced = addVectors(sound_unvoiced,
                                    unvoiced[[s]],
                                    insertionPoint = insertionIdx,
                                    normalize = FALSE)

        # update syllable timing if inserting before the bout
        # (increasing its length)
        if (insertionIdx < 0) {
          syllables[, c('start', 'end')] =
            syllables[, c('start', 'end')] - insertionIdx / samplingRate * 1000
        }
      }
    }

    ## Merging voiced and unvoiced components and adding formants
    # for noiseAmpRef == "filtered", enforce adding formants separately
    # followed by independent normalization of voiced & unvoiced
    if (noiseAmpRef == 'filtered' & !is.list(formantsNoise)) {
      formantsNoise = formants
    }

    if (length(unvoiced) > 0) {
      if (!is.numeric(formantsNoise) & !is.list(formantsNoise)) {
        # OPTION 1: mix voiced + unvoiced, then apply the same formant filter
        sound = addVectors(
          voiced,
          sound_unvoiced,
          insertionPoint = -syllables$start[1] * samplingRate / 1000,
          normalize = FALSE
        )

        if (length(sound) / samplingRate * 1000 > permittedValues['sylLen', 'low']) {
          soundFiltered = do.call(addFormants, c(
            formantPars,
            list(sound = sound,
                 formants = formants,
                 formantDepStoch = formantDepStoch,
                 normalize = FALSE)
          ))
        } else {
          soundFiltered = sound
        }
      } else {
        # OPTION 2: apply different formant filters to voiced and unvoiced, then mix
        # add formants to voiced
        if (length(voiced) / samplingRate * 1000 > permittedValues['sylLen', 'low']) {
          voicedFiltered = do.call(addFormants, c(
            formantPars,
            list(sound = voiced,
                 formants = formants,
                 formantDepStoch = formantDepStoch,
                 normalize = ifelse(noiseAmpRef == 'filtered', TRUE, FALSE))
          ))
        } else {
          voicedFiltered = voiced
        }
        # add formants to unvoiced
        if (length(sound_unvoiced) / samplingRate * 1000 > permittedValues['sylLen', 'low']) {
          unvoicedFiltered = do.call(addFormants, c(
            formantPars,
            list(sound = sound_unvoiced,
                 formants = formantsNoise,
                 formantDepStoch = formantDepStoch_noise,
                 normalize = ifelse(noiseAmpRef == 'filtered', TRUE, FALSE))
          ))
        } else {
          unvoicedFiltered = sound_unvoiced
        }
        # mix filtered version of the voiced and unvoiced components
        if(noiseAmpRef == 'filtered') {
          unvoicedFiltered = unvoicedFiltered * 10 ^ (max(noise$value) / 20)
        }
        soundFiltered = addVectors(
          voicedFiltered,
          unvoicedFiltered,
          insertionPoint = -syllables$start[1] * samplingRate / 1000,
          normalize = FALSE
        )
      }
    } else {
      # no unvoiced component - just add formants to voiced
      # plot(voiced, type = 'l')
      if (length(voiced) / samplingRate * 1000 > permittedValues['sylLen', 'low']) {
        soundFiltered = do.call(addFormants, c(
          formantPars,
          list(sound = voiced,
               formants = formants,
               formantDepStoch = formantDepStoch,
               normalize = FALSE)
        ))
      } else {
        soundFiltered = voiced
      }
    }
    # plot(soundFiltered, type = 'l')

    # Add amplitude modulation (affects both voiced and unvoiced)
    if (is.list(amDep)) {
      if (any(amDep$value > 0)) {
        soundFiltered = addAM(
          x = soundFiltered,
          samplingRate = samplingRate,
          amDep = amDep,
          amFreq = amFreq,
          amType = amType,
          amShape = amShape,
          invalidArgAction = invalidArgAction,
          plot = FALSE,
          play = FALSE,
          checkFormat = FALSE
        )
        # plot(soundFiltered, type = 'l')
      }
    }

    # grow bout
    if (b == 1) {
      bout = soundFiltered
    } else {
      bout = addVectors(
        bout,
        soundFiltered,
        insertionPoint = length(bout) + round(pauseLen[1] * samplingRate / 1000),
        normalize = FALSE
      )
    }
  }

  # normalize
  m = max(abs(bout))
  if (m != 0) bout = bout / m

  # add some silence before and after the entire bout
  if (is.numeric(addSilence)) {
    n = round(samplingRate / 1000 * addSilence)
    bout = c(rep(0, n), bout, rep(0, n))
  }

  if (play == TRUE) {
    playme(bout, samplingRate = samplingRate)
  }
  if (is.character(play)) {
    playme(bout, samplingRate = samplingRate, player = play)
  }
  if (!is.na(savePath)) {
    seewave::savewav(bout, filename = savePath, f = samplingRate)
  }
  if (plot) {
    spectrogram(bout, samplingRate = samplingRate,
                windowLength = windowLength, overlap = overlap,
                dynamicRange = dynamicRange, ...)
  }
  invisible(bout)
}
