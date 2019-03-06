# TODO: write getLoudnessFolder (without plotting, like getRMSFolder); check the new functions in amplitude.R and add some info to vignette on analysis (+on modulation spectra); update ALL code (vignettes, online demos) in terms of noise$value; remove pitchAnchors etc; streamline saving all plots a la ggsave: filename, path, different supported devices instead of only png(); automatic addition of pitch jumps at high temp in soundgen() (?)

#' @import stats graphics utils grDevices
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
#' @param repeatBout number of times the whole bout should be repeated
#' @param nSyl number of syllables in the bout. `pitchGlobal`, `amplGlobal`, and
#'   `formants` span multiple syllables, but not multiple bouts
#' @param sylLen average duration of each syllable, ms (vectorized)
#' @param pauseLen average duration of pauses between syllables, ms (can be
#'   negative between bouts: force with invalidArgAction = 'ignore')
#'   (vectorized)
#' @param pitch,pitchAnchors a numeric vector of f0 values in Hz or a dataframe
#'   specifying the time (ms or 0 to 1) and value (Hz) of each anchor, hereafter
#'   "anchor format". These anchors are used to create a smooth contour of
#'   fundamental frequency f0 (pitch) within one syllable. NB: "pitchAnchors" is
#'   deprecated; use simply "pitch" instead (same for other arguments with
#'   "Anchors" below)
#' @param pitchGlobal,pitchAnchorsGlobal unlike \code{pitch}, these anchors are
#'   used to create a smooth contour of average f0 across multiple syllables.
#'   The values are in semitones relative to the existing pitch, i.e. 0 = no
#'   change (anchor format)
#' @param glottis,glottisAnchors anchors for specifying the proportion of a
#'   glottal cycle with closed glottis, \% (0 = no modification, 100 = closed
#'   phase as long as open phase); numeric vector or dataframe specifying time
#'   and value (anchor format)
#' @param temperature hyperparameter for regulating the amount of stochasticity
#'   in sound generation
#' @param tempEffects a list of scaling coefficients regulating the effect of
#'   temperature on particular parameters. To change, specify just those pars
#'   that you want to modify (default is 1 for all of them). \code{sylLenDep}:
#'   duration of syllables and pauses; \code{formDrift}: formant frequencies;
#'   \code{formDisp}: dispersion of stochastic formants; \code{pitchDriftDep}:
#'   amount of slow random drift of f0; \code{pitchDriftFreq}: frequency of slow
#'   random drift of f0; \code{amplDriftDep}: drift of amplitude mirroring pitch
#'   drift; \code{subDriftDep}: drift of subharmonic frequency and bandwidth
#'   mirroring pitch drift; \code{rolloffDriftDep}: drift of rolloff mirroring
#'   pitch drift; \code{pitchAnchorsDep, noiseAnchorsDep, amplAnchorsDep}:
#'   random fluctuations of user-specified pitch / noise / amplitude anchors;
#'   \code{glottisAnchorsDep}: proportion of glottal cycle with closed glottis;
#'   \code{specDep}: rolloff, rolloffNoise, nonlinear effects, attack
#' @param maleFemale hyperparameter for shifting f0 contour, formants, and
#'   vocalTract to make the speaker appear more male (-1...0) or more female
#'   (0...+1); 0 = no change
#' @param creakyBreathy hyperparameter for a rough adjustment of voice quality
#'   from creaky (-1) to breathy (+1); 0 = no change
#' @param nonlinBalance hyperparameter for regulating the (approximate)
#'   proportion of sound with different regimes of pitch effects (none /
#'   subharmonics only / subharmonics and jitter). 0\% = no noise; 100\% = the
#'   entire sound has jitter + subharmonics. Ignored if temperature = 0
#' @param nonlinDep hyperparameter for regulating the intensity of subharmonics
#'   and jitter, 0 to 100\% (50\% = jitter and subharmonics are as specified,
#'   <50\% weaker, >50\% stronger). Ignored if temperature = 0
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
#' @param formantWidth = scale factor of formant bandwidth (1 = no change)
#' @param vocalTract the length of vocal tract, cm. Used for calculating formant
#'   dispersion (for adding extra formants) and formant transitions as the mouth
#'   opens and closes. If \code{NULL} or \code{NA}, the length is estimated
#'   based on specified formant frequencies (if any)
#' @param subFreq target frequency of subharmonics, Hz (lower than f0, adjusted
#'   dynamically so f0 is always a multiple of subFreq) (anchor format)
#' @param subDep the width of subharmonic band, Hz. Regulates how quickly the
#'   strength of subharmonics fades as they move away from harmonics in f0 stack
#'   (anchor format)
#' @param shortestEpoch minimum duration of each epoch with unchanging
#'   subharmonics regime, in ms
#' @param amDep amplitude modulation depth, \%. 0: no change; 100: amplitude
#'   modulation with amplitude range equal to the dynamic range of the sound
#'   (anchor format)
#' @param amFreq amplitude modulation frequency, Hz (anchor format)
#' @param amShape amplitude modulation shape (-1 to +1, defaults to 0) (anchor
#'   format)
#' @param noise,noiseAnchors loudness of turbulent noise (0 dB = as loud as
#'   voiced component, negative values = quieter) such as aspiration, hissing,
#'   etc (anchor format)
#' @param formantsNoise the same as \code{formants}, but for unvoiced instead of
#'   voiced component. If NA (default), the unvoiced component will be filtered
#'   through the same formants as the voiced component, approximating aspiration
#'   noise [h]
#' @param rolloffNoise linear rolloff of the excitation source for the unvoiced
#'   component, dB/kHz (anchor format)
#' @param noiseFlatSpec keeps noise spectrum flat to this frequency, Hz
#' @param noiseAmpRef noise amplitude is defined relative to: "f0" = the
#'   amplitude of the first partial (fundamental frequency), "source" = the
#'   amplitude of the harmonic component prior to applying formants, "filtered"
#'   =  the amplitude of the harmonic component after applying formants
#' @param mouth,mouthAnchors mouth opening (0 to 1, 0.5 = neutral, i.e. no
#'   modification) (anchor format)
#' @param ampl,amplAnchors amplitude envelope (dB, 0 = max amplitude) (anchor
#'   format)
#' @param amplGlobal,amplAnchorsGlobal global amplitude envelope spanning
#'   multiple syllables (dB, 0 = no change) (anchor format)
#' @param interpol the method of smoothing envelopes based on provided anchors:
#'   'approx' = linear interpolation, 'spline' = cubic spline, 'loess' (default)
#'   = polynomial local smoothing function. NB: this does not affect contours for
#'   "noise", "glottal", and the smoothing of formants
#' @param discontThres,jumpThres if two anchors are closer in time than
#'   \code{discontThres}, the contour is broken into segments with a linear
#'   transition between these anchors; if anchors are closer than
#'   \code{jumpThres}, a new section starts with no transition at all (e.g. for
#'   adding pitch jumps)
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
#' @param throwaway (deprecated) same as dynamicRange, but negative
#' @param invalidArgAction what to do if an argument is invalid or outside the
#'   range in \code{permittedValues}: 'adjust' = reset to default value, 'abort'
#'   = stop execution, 'ignore' = throw a warning and continue (may crash)
#' @param plot if TRUE, plots a spectrogram
#' @param play if TRUE, plays the synthesized sound. In case of errors, try
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
#' playback = c(TRUE, FALSE)[2]  # set to TRUE to play back the audio from examples
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
#' # playme(s2)
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
#' sound = soundgen (nonlinBalance = 100, subFreq = 75, subDep = 130,
#'   pitch = data.frame(
#'     time = c(0, .3, .9, 1), value = c(1200, 1547, 1487, 1154)),
#'   sylLen = 800,
#'   play = playback, plot = TRUE)
#'
#' # Jitter and mouth opening (bark, dog-like)
#' sound = soundgen(repeatBout = 2, sylLen = 160, pauseLen = 100,
#'   nonlinBalance = 100, subFreq = 100, subDep = 60, jitterDep = 1,
#'   pitch = c(559, 785, 557),
#'   mouth = c(0, 0.5, 0),
#'   vocalTract = 5, play = playback)
#'
#' # Use nonlinRandomWalk to crease reproducible examples of sounds with
#' nonlinear effects. For ex., to make a sound with no effect in the first
#' third, subharmonics in the second third, and jitter in the final third of the
#' total duration:
#' a = c(rep(0, 100), rep(1, 100), rep(2, 100))
#' s = soundgen(sylLen = 800, pitch = 300, temperature = 0.001,
#'              subFreq = 100, subDep = 70, jitterDep = 1,
#'              nonlinRandomWalk = a, plot = TRUE, ylim = c(0, 4))
#' # playme(s)
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
  pitch = data.frame(time = c(0, .1, .9, 1),
                     value = c(100, 150, 135, 100)),
  pitchAnchors = data.frame(time = c(0, .1, .9, 1),
                            value = c(100, 150, 135, 100)),
  pitchGlobal = NA,
  pitchAnchorsGlobal = NA,
  glottis = 0,
  glottisAnchors = 0,
  temperature = 0.025,
  tempEffects = list(),
  maleFemale = 0,
  creakyBreathy = 0,
  nonlinBalance = 0,
  nonlinDep = 50,
  nonlinRandomWalk = NULL,
  jitterLen = 1,
  jitterDep = 1,
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
  formantDepStoch = 20,
  formantWidth = 1,
  vocalTract = NA,
  subFreq = 100,
  subDep = 100,
  shortestEpoch = 300,
  amDep = 0,
  amFreq = 30,
  amShape = 0,
  noise = NULL,
  noiseAnchors = NULL,
  formantsNoise = NA,
  rolloffNoise = -4,
  noiseFlatSpec = 1200,
  noiseAmpRef = c('f0', 'source', 'filtered')[3],
  mouth = data.frame(time = c(0, 1),
                     value = c(.5, .5)),
  mouthAnchors = data.frame(time = c(0, 1),
                            value = c(.5, .5)),
  ampl = NA,
  amplAnchors = NA,
  amplGlobal = NA,
  amplAnchorsGlobal = NA,
  interpol = c('approx', 'spline', 'loess')[3],
  discontThres = .05,
  jumpThres = .01,
  samplingRate = 16000,
  windowLength = 50,
  overlap = 75,
  addSilence = 100,
  pitchFloor = 1,
  pitchCeiling = 3500,
  pitchSamplingRate = 3500,
  dynamicRange = 80,
  throwaway = -80,
  invalidArgAction = c('adjust', 'abort', 'ignore')[1],
  plot = FALSE,
  play = FALSE,
  savePath = NA,
  ...
) {
  # deprecated pars
  if (!missing('throwaway')) {
    dynamicRange = -throwaway
    message('throwaway is deprecated; used dynamicRange instead')
  }
  formerAnchors = c(
    'pitchAnchors', 'pitchAnchorsGlobal', 'glottisAnchors',
    'amplAnchors', 'amplAnchorsGlobal', 'mouthAnchors', 'noiseAnchors'
  )
  newAnchors = c('pitch', 'pitchGlobal', 'glottis', 'ampl', 'amplGlobal', 'mouth', 'noise')
  for (i in 1:length(formerAnchors)) {
    p = formerAnchors[i]
    q = newAnchors[i]
    # simply missing(noquote(p)) doesn't work, so use do.call
    if (!do.call(missing, list(noquote(p)))) {
      # user wrote "pitchAnchors = ..." etc
      message(paste(p, 'is deprecated; use', q, 'instead'))
    } else {
      assign(p, get(q))
    }
  }


  # check that values of numeric arguments are valid and within range
  pars_to_check = rownames(permittedValues)[1:which(
    rownames(permittedValues) == 'noiseFlatSpec'
  )]

  for (p in pars_to_check) {
    gp = try(get(p), silent = TRUE)
    if (class(gp) != "try-error") {
      if (is.numeric(gp)) {
        if (any(gp < permittedValues[p, 'low']) |
            any(gp > permittedValues[p, 'high'])) {
          if (invalidArgAction == 'abort') {
            # exit with a warning
            stop(paste(p, 'must be between',
                       permittedValues[p, 'low'],
                       'and',
                       permittedValues[p, 'high']))
          } else if (invalidArgAction == 'ignore') {
            # throw a warning and continue
            warning(paste(p, "outside its range in 'permittedValues'"))
          } else {
            # reset p to default, with a warning
            assign(noquote(p), permittedValues[p, 'default'])
            warning(paste0("\n", p, " outside permitted range, reset to ", get(p),
                           ". Edit 'permittedValues' or use ",
                           "invalidArgAction = 'ignore' to force."))
          }
        }
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
  for (anchor in c('pitchAnchors', 'pitchAnchorsGlobal', 'glottisAnchors',
                   'amplAnchors', 'amplAnchorsGlobal', 'mouthAnchors',
                   'vibratoFreq', 'vibratoDep', 'subFreq', 'subDep',
                   'jitterLen', 'jitterDep', 'shimmerLen', 'shimmerDep',
                   'rolloff', 'rolloffOct', 'rolloffKHz',
                   'rolloffParab', 'rolloffParabHarm',
                   'amDep', 'amFreq', 'amShape')) {
    assign(anchor, reformatAnchors(get(anchor)))
  }
  if (is.numeric(noiseAnchors)) {
    if (length(noiseAnchors) > 0) {
      noiseAnchors = data.frame(
        time = seq(0, sylLen[1], length.out = max(2, length(noiseAnchors))),
        value = noiseAnchors
      )
    }
  }
  if (is.list(pitchAnchors)) {
    if (any(pitchAnchors$value < pitchFloor)) {
      pitchFloor = 0.1
      message('Some pitch values are lower than pitchFloor; lowering to 0.1 Hz')
    }
    if (any(pitchAnchors$value >= samplingRate / 2)) {
      samplingRate = max(pitchAnchors$value * 4)
      message(paste('Some pitch values exceed Nyquist frequency;',
                    'raising samplingRate to', samplingRate, 'Hz'))
    }
    if (any(pitchAnchors$value > pitchSamplingRate)) {
      pitchSamplingRate = samplingRate
      message(paste('Some pitch values exceed pitchSamplingRate.',
                    'Resetting pitchSamplingRate to samplingRate.'))
    }
    if (any(pitchAnchors$value > pitchCeiling)) {
      pitchCeiling = samplingRate / 2
      message(paste('Some pitch values exceed pitchCeiling.',
                    'Resetting pitchCeiling to Nyquist frequency (samplingRate / 2).'))
    }
  }

  # check amplitude anchors and make all values negative
  if (is.list(amplAnchors)) {
    if (any(amplAnchors$value > 0)) {
      amplAnchors$value = amplAnchors$value - dynamicRange
      message(paste('The recommended range for amplAnchors is (-dynamicRange, 0).',
                    'If positive, values are transformed by subtracting dynamicRange'))
    }
  }

  # for amplAnchorsGlobal, make the first value 0
  if (is.list(amplAnchorsGlobal)) {
    if (any(amplAnchorsGlobal$value != 0)) {
      amplAnchorsGlobal$value = amplAnchorsGlobal$value - amplAnchorsGlobal$value[1]
    }
  }

  # make sure sylLen and pauseLen are vectors of appropriate length
  sylLen = getSmoothContour(anchors = sylLen, len = nSyl)
  if (nSyl > 1) {
    pauseLen = getSmoothContour(anchors = pauseLen, len = nSyl - 1)
  }

  windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2

  # # preliminary glottis contour
  # glottisClosed = getSmoothContour(anchors = glottisAnchors, len = 100)
  # # adjust length based on proportion of closed glottis (pauses added)
  # mean_closed = mean(glottisClosed) / 100
  # sylLen = sylLen / (mean_closed + 1)
  # if (is.list(pitchAnchors)) {
  #   pitchAnchors$value = pitchAnchors$value * (mean_closed + 1)
  # }

  # tempEffects are either left at default levels or multiplied by user-supplied values
  es = c('sylLenDep', 'formDrift', 'formDisp', 'pitchDriftDep',
         'amplDriftDep', 'subDriftDep', 'rolloffDriftDep', 'pitchAnchorsDep',
         'noiseAnchorsDep', 'amplAnchorsDep', 'glottisAnchorsDep', 'specDep')
  for (e in es) {
    if (!is.numeric(tempEffects[[e]])) {
      tempEffects[[e]] = defaults[[e]]
    } else {
      tempEffects[[e]] = defaults[[e]] * tempEffects[[e]]
    }
  }

  # expand formants to full format for adjusting bandwidth if creakyBreathy > 0
  formants = reformatFormants(formants)
  formantsNoise = reformatFormants(formantsNoise)

  ## adjust parameters according to the specified hyperparameters
  # effects of creakyBreathy hyper
  if (creakyBreathy < 0) {
    # for creaky voice
    nonlinBalance = min(100, nonlinBalance - creakyBreathy * 100)
    jitterDep$value = jitterDep$value - creakyBreathy / 2
    jitterDep$value[jitterDep$value < 0] = 0
    shimmerDep$value = shimmerDep$value - creakyBreathy * 5
    shimmerDep$value[shimmerDep$value < 0] = 0
    subDep$value = subDep$value * 2 ^ (-creakyBreathy)
  } else if (creakyBreathy > 0) {
    # for breathy voice, add breathing
    if (!is.list(noiseAnchors)) {
      noiseAnchors = data.frame(time = c(0, sylLen[1] + 100),
                                value = c(-dynamicRange, -dynamicRange))
    }
    noiseAnchors$value = noiseAnchors$value +
      creakyBreathy * (dynamicRange + permittedValues['noiseAmpl', 'high'])
    noiseAnchors$value[noiseAnchors$value >
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

  # reformat noiseAnchors
  if (!is.list(noiseAnchors)) {
    noiseAnchors = data.frame(
      time = c(0, sylLen[1]),
      value = c(-dynamicRange, -dynamicRange)
    )
  }

  # effects of nonlinDep hyper
  subFreq$value = 2 * (subFreq$value - 50) / (1 + exp(-.1 * (50 - nonlinDep))) + 50
  # subFreq unchanged for nonlinDep=50%, raised for lower and
  # lowered for higher noise intensities. Max set at 2*subFreq-50, min at 50 Hz.
  # Jitter and shimmer go to 0 if nonlinDep = 0 and double if nonlinDep = 1
  # Illustration: subFreq=250; nonlinDep=0:100; plot(nonlinDep,
  #   2 * (subFreq - 50) / (1 + exp(-.1 * (50 - nonlinDep))) + 50, type = 'l')
  jitterDep$value = 2 * jitterDep$value / (1 + exp(.1 * (50 - nonlinDep)))
  # Illustration: jitterDep = 1; nonlinDep = 0:100;
  # plot(nonlinDep, 2 * jitterDep / (1 + exp(.1 * (50 - nonlinDep))), type = 'l')
  shimmerDep$value = 2 * shimmerDep$value / (1 + exp(.1 * (50 - nonlinDep)))
  # Illustration: shimmerDep = 1.5; nonlinDep = 0:100;
  # plot(nonlinDep, 2 * shimmerDep / (1 + exp(.1 * (50 - nonlinDep))), type = 'l')

  # effects of maleFemale hyper
  if (maleFemale != 0) {
    # adjust pitch and formants along the male-female dimension
    # pitch varies by 1 octave up or down
    if (is.list(pitchAnchors)) {
      pitchAnchors$value = pitchAnchors$value * 2 ^ maleFemale
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
    if (is.numeric(vocalTract)) {
      vocalTract = vocalTract * (1 - .25 * maleFemale)
    }
  }

  # prepare a list of pars for calling generateHarmonics()
  pars_to_vary = c(
    'nonlinDep',
    'attackLen',
    'shortestEpoch'
  )  # don't add nonlinBalance, otherwise there is no simple way to remove noise at temp>0
  anchors_to_wiggle = c(
    'pitchAnchors', 'amplAnchors', 'glottisAnchors',
    'vibratoFreq', 'vibratoDep',
    'jitterLen', 'jitterDep',
    'shimmerLen', 'shimmerDep',
    'rolloff', 'rolloffKHz', 'rolloffOct',
    'rolloffParab', 'rolloffParabHarm')
  pars_to_round = c('attackLen', 'subFreq', 'subDep')
  pars_list = list(
    'attackLen' = attackLen,
    'jitterDep' = jitterDep,
    'jitterLen' = jitterLen,
    'vibratoFreq' = vibratoFreq,
    'vibratoDep' = vibratoDep,
    'shimmerDep' = shimmerDep,
    'shimmerLen' = shimmerLen,
    'creakyBreathy' = creakyBreathy,
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
    'subFreq' = subFreq,
    'subDep' = subDep,
    'nonlinBalance' = nonlinBalance,
    'nonlinDep' = nonlinDep,
    'nonlinRandomWalk' = nonlinRandomWalk,
    'pitchFloor' = pitchFloor,
    'pitchCeiling' = pitchCeiling,
    'pitchSamplingRate' = pitchSamplingRate,
    'dynamicRange' = dynamicRange,
    'interpol' = interpol,
    'samplingRate' = samplingRate,
    'overlap' = overlap
  )
  pars_syllable = pars_list
  pitchDeltas = rep(1, nSyl)
  if (is.list(pitchAnchorsGlobal)) {
    if (any(!is.na(pitchAnchorsGlobal))) {
      if (any(pitchAnchorsGlobal$value != 0) & nSyl > 1) {
        pitchDeltas = 2 ^ (
          getDiscreteContour(
            len = nSyl,
            anchors = pitchAnchorsGlobal,
            interpol = 'spline',
            plot = FALSE
          ) / 12
        )
      }
    }
  }

  # make sure pitchAnchors$time range from 0 to 1
  if (is.list(pitchAnchors)) {
    if (min(pitchAnchors$time) < 0) {
      pitchAnchors$time = pitchAnchors$time - min(pitchAnchors$time)
    }
    if (max(pitchAnchors$time) > 1) {
      pitchAnchors$time = pitchAnchors$time / max(pitchAnchors$time)
    }
  }

  wiggleNoise = FALSE
  if (is.numeric(noiseAnchors) | is.list(noiseAnchors)) {
    if (temperature > 0 & any(noiseAnchors$value > -dynamicRange)) {
      wiggleNoise = TRUE
    }
  }

  wiggleAmpl_per_syl = FALSE
  if (is.numeric(amplAnchors) | is.list(amplAnchors)) {
    if (temperature > 0 & any(amplAnchors$value < dynamicRange)) {
      wiggleAmpl_per_syl = TRUE
    }
  }

  wiggleGlottis = FALSE
  if (is.numeric(amplAnchors) | is.list(amplAnchors)) {
    if (temperature > 0 & any(glottisAnchors$value > 0)) {
      wiggleGlottis = TRUE
    }
  }

  # For polysyllabic vocalizations, calculate amplitude envelope correction
  # per voiced syllable
  if (is.list(amplAnchorsGlobal)) {
    if (any(amplAnchorsGlobal$value != 0)) {
      amplEnvelope = getSmoothContour(
        anchors = amplAnchorsGlobal,
        len = nSyl,
        interpol = interpol,
        discontThres = discontThres,
        jumpThres = jumpThres,
        valueFloor = -dynamicRange,
        valueCeiling = dynamicRange,
        samplingRate = samplingRate
      )
      # convert from dB to linear multiplier
      amplEnvelope = 10 ^ (amplEnvelope / 20)
    }
  } else {
    amplEnvelope = rep(1, nSyl)
  }

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

    # START OF SYLLABLE GENERATION
    voiced = vector()
    unvoiced = list()
    noiseAnchors_syl = list()

    for (s in 1:nrow(syllables)) {
      # scale noise anchors for polysyllabic sounds with lengh(sylLen) > 1
      noiseAnchors_syl[[s]] = noiseAnchors
      noiseAnchors_syl[[s]]$time = scaleNoiseAnchors(
        noiseTime = noiseAnchors_syl[[s]]$time,
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
          pars_syllable[[pars_to_vary[p]]] = rnorm_bounded(
            n = length(par_value),
            mean = par_value,
            low = l,
            high = h,
            sd = sd,
            roundToInteger = (pars_to_vary[p] %in% pars_to_round),
            invalidArgAction = invalidArgAction
          )
        }
        if (is.list(pitchAnchors_per_syl)) {
          pitchAnchors_per_syl = wiggleAnchors(
            df = pitchAnchors_per_syl,
            temperature = temperature,
            low = c(0, pitchFloor),
            high = c(1, pitchCeiling),
            temp_coef = tempEffects$pitchAnchorsDep,
            invalidArgAction = invalidArgAction
          )
        }
        if (wiggleNoise) {
          noiseAnchors_syl[[s]] = wiggleAnchors(
            df = noiseAnchors_syl[[s]],
            temperature = temperature,
            low = c(-Inf, -dynamicRange),
            high = c(+Inf, permittedValues['noiseAmpl', 'high']),
            wiggleAllRows = TRUE,
            temp_coef = tempEffects$noiseAnchorsDep,
            invalidArgAction = invalidArgAction
          )
        }
        if (wiggleAmpl_per_syl) {
          amplAnchors_per_syl = wiggleAnchors(
            df = amplAnchors_per_syl,
            temperature = temperature,
            low = c(0, -dynamicRange),
            high = c(1, 0),
            temp_coef = tempEffects$amplAnchorsDep,
            invalidArgAction = invalidArgAction
          )
        }
        if (wiggleGlottis) {
          glottisAnchors_per_syl = wiggleAnchors(
            df = glottisAnchors_per_syl,
            temperature = temperature,
            low = c(0, 0),
            high = c(1, Inf),
            temp_coef = tempEffects$glottisAnchorsDep,
            invalidArgAction = invalidArgAction
          )
        }
        # wiggle anchors except special cases (pitch, ampl, glottis)
        for (i in 4:length(anchors_to_wiggle)) {
          anchor = anchors_to_wiggle[i]
          anchor_per_syl = paste0(anchor, '_per_syl')
          l = permittedValues[anchor, 'low']
          h = permittedValues[anchor, 'high']
          anchor_new = wiggleAnchors(
            df = get(anchor),
            temperature = temperature,
            low = c(0, l),
            high = c(1, h),
            temp_coef = tempEffects$specDep,
            sd_values = (h - l) * temperature * tempEffects$specDep,
            invalidArgAction = invalidArgAction
          )
          assign(anchor_per_syl, anchor_new)
        }
      }

      # generate smooth pitch contour for this particular syllable
      dur_syl = as.numeric(syllables[s, 'end'] - syllables[s, 'start'])
      if (is.list(pitchAnchors_per_syl) | is.numeric(pitchAnchors_per_syl)) {
        pitchContour_syl = getSmoothContour(
          anchors = pitchAnchors_per_syl,
          len = round(dur_syl * pitchSamplingRate / 1000),
          interpol = interpol,
          discontThres = discontThres,
          jumpThres = jumpThres,
          samplingRate = pitchSamplingRate,
          valueFloor = pitchFloor,
          valueCeiling = pitchCeiling,
          thisIsPitch = TRUE
        ) * pitchDeltas[s]
        # plot(pitchContour_syl, type = 'l')
      }

      # generate the voiced part only if noise is weaker than 40 dB
      #   and the voiced part is long enough to bother synthesizing it
      generateVoiced = TRUE
      if (dur_syl < permittedValues['sylLen', 'low'] | !is.list(pitchAnchors_per_syl)) {
        generateVoiced = FALSE
      }
      if (is.list(noiseAnchors)) {
        if (min(noiseAnchors$value) >= 40) {
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
               amplAnchors = amplAnchors_per_syl,
               glottisAnchors = glottisAnchors_per_syl,
               normalize = ifelse(noiseAmpRef == 'f0', FALSE, TRUE))
        )) * amplEnvelope[s]  # correction of amplitude per syllable
        )
      }
      # plot(syllable, type = 'l')
      # spectrogram(syllable, samplingRate = samplingRate)
      # playme(syllable, samplingRate = samplingRate)
      # ***THE ACTUAL SYNTHESIS IS HERE***

      if (class(syllable) == 'try-error') {
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
      voiced = c(voiced, syllable, pause)

      # update syllable timing info, b/c with temperature > 0, jitter etc
      # there may be deviations from the target duration
      if (s < nrow(syllables)) {
        correction = length(voiced) / samplingRate * 1000 - syllables[s + 1, 'start']
        syllables[(s + 1):nrow(syllables), c('start', 'end')] =
          syllables[(s + 1):nrow(syllables), c('start', 'end')] + correction
      }

      # generate the unvoiced part, but don't add it to the sound just yet
      if (is.list(noiseAnchors)) {
        if (any(noiseAnchors$value > -dynamicRange)) {
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
            rolloffNoise_syl = rnorm_bounded(
              n = length(rolloffNoise),
              mean = rolloffNoise,
              sd = abs(rolloffNoise) * temperature * tempEffects$specDep,
              low = permittedValues['rolloffNoise', 'low'],
              high = permittedValues['rolloffNoise', 'high']
            )
          }
          # synthesize the unvoiced part
          unvoiced[[s]] = generateNoise(
            len = round(diff(range(noiseAnchors_syl[[s]]$time)) * samplingRate / 1000),
            noiseAnchors = noiseAnchors_syl[[s]],
            rolloffNoise = rolloffNoise_syl,
            noiseFlatSpec = noiseFlatSpec,
            temperature = 0,  # wiggled separately in soundgen
            attackLen = attackLen,
            samplingRate = samplingRate,
            windowLength_points = windowLength_points,
            overlap = overlap,
            dynamicRange = dynamicRange,
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
          noiseAnchors_syl[[s]]$time[1] * samplingRate / 1000
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
    formantPars = list(
      vocalTract = vocalTract,
      formantDep = formantDep,
      formantWidth = formantWidth,
      lipRad = lipRad,
      noseRad = noseRad,
      mouthOpenThres = mouthOpenThres,
      mouthAnchors = mouthAnchors,
      interpol = interpol,
      temperature = temperature,
      formDrift = tempEffects$formDrift,
      formDisp = tempEffects$formDisp,
      samplingRate = samplingRate,
      windowLength_points = windowLength_points,
      overlap = overlap
    )
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
          # add extra stochastic formants to unvoiced only if vocalTract is user-specified
          fds = ifelse(
            is.numeric(vocalTract), formantDepStoch, 0
          )
          unvoicedFiltered = do.call(addFormants, c(
            formantPars,
            list(sound = sound_unvoiced,
                 formants = formantsNoise,
                 formantDepStoch = fds,
                 normalize = ifelse(noiseAmpRef == 'filtered', TRUE, FALSE))
          ))
        } else {
          unvoicedFiltered = sound_unvoiced
        }
        # mix filtered version of the voiced and unvoiced components
        if(noiseAmpRef == 'filtered') {
          unvoicedFiltered = unvoicedFiltered * 10 ^ (max(noiseAnchors$value) / 20)
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

    # trill - rapid regular amplitude modulation
    # (affects both voiced and unvoiced)
    if (is.list(amDep)) {
      if (any(amDep$value > 0)) {
        # vectorize
        amPar_vect = c('amDep', 'amFreq', 'amShape')
        # just to get rid of of NOTE on CRAN:
        amDep_vector = amFreq_vector = amShape_vector = vector()
        for (p in amPar_vect) {
          p_unique_value = unique(get(p)$value)
          if (length(p_unique_value) > 1) {
            p_vectorized = getSmoothContour(
              anchors = get(p),
              len = length(soundFiltered),
              interpol = 'approx',
              valueFloor = permittedValues[p, 'low'],
              valueCeiling = permittedValues[p, 'high']
            )
            # plot(p_vectorized, type = 'l')
            assign(paste0(p, '_vector'), p_vectorized)
          } else {
            assign(paste0(p, '_vector'), p_unique_value)
          }
        }

        # prepare am
        sig = getSigmoid(len = length(soundFiltered),
                         samplingRate = samplingRate,
                         freq = amFreq_vector,
                         shape = amShape_vector)
        trill = 1 - sig * amDep_vector / 100
        # plot(trill, type='l')

        # apply am
        soundFiltered = soundFiltered * trill
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

  if (play) {
    playme(bout, samplingRate = samplingRate)
    # spectrogram((soundFiltered, samplingRate = samplingRate, osc = TRUE)
  }
  if (!is.na(savePath)) {
    seewave::savewav(bout, filename = savePath, f = samplingRate)
  }
  if (plot) {
    spectrogram(bout, samplingRate = samplingRate,
                windowLength = windowLength, overlap = overlap,
                dynamicRange = dynamicRange, ...)
  }
  return(bout)
}
