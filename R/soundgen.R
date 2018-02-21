# TODO: vectorize sylLen, pauseLen, jitterDep and the rest of nonlin pars (?); check why subh are stronger with low vs high F0; rename seewave function stft() to stdft() when seewave is updated to 2.0.6 (stft is only used in formants.R);

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
#' \url{http://cogsci.se/soundgen.html} and the vignette on sound generation.
#' @param repeatBout the number of times the whole bout should be repeated
#' @param nSyl the number of syllables in the bout. Intonation, amplitude, and
#'   formants contours span multiple syllables, but not multiple bouts (see
#'   Details)
#' @param sylLen average duration of each syllable, ms
#' @param pauseLen average duration of pauses between syllables, ms (can be
#'   negative between bouts, but not between syllables, i.e. if repeatBout > 1
#'   but nSyl = 1)
#' @param pitchAnchors a numeric vector of f0 values in Hz (assuming equal time
#'   steps) or a dataframe specifying the time (ms or 0 to 1) and value (Hz) of
#'   each anchor. These anchors are used to create a smooth contour of
#'   fundamental frequency f0 (pitch) within one syllable (see Examples)
#' @param pitchAnchorsGlobal unlike \code{pitchAnchors}, these anchors are used
#'   to create a smooth contour of average f0 across multiple syllables. The
#'   values are in semitones relative to the existing pitch, i.e. 0 = no change
#' @param glottisAnchors anchors for specifying the proportion of a glottal
#'   cycle with closed glottis, % (0 = no modification, 100 = closed phase as
#'   long as open phase); numeric vector or dataframe specifying time and value
#' @param temperature hyperparameter for regulating the amount of stochasticity
#'   in sound generation
#' @param tempEffects a list of scale factors regulating the effect of
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
#'   \code{specDep}: rolloff, nonlinear effects, attack
#' @param maleFemale hyperparameter for shifting f0 contour, formants, and
#'   vocalTract to make the speaker appear more male (-1...0) or more female
#'   (0...+1)
#' @param creakyBreathy hyperparameter for a rough adjustment of voice quality
#'   from creaky (-1) to breathy (+1)
#' @param nonlinBalance hyperparameter for regulating the (approximate)
#'   proportion of sound with different regimes of pitch effects (none /
#'   subharmonics only / subharmonics and jitter). 0\% = no noise; 100\% = the
#'   entire sound has jitter + subharmonics. Ignored if temperature = 0
#' @param nonlinDep hyperparameter for regulating the intensity of
#'   subharmonics and jitter, 0 to 100\% (50\% = jitter and subharmonics are as
#'   specified, <50\% weaker, >50\% stronger). Ignored if temperature = 0
#' @param nonlinRandomWalk a numeric vector specifying the timing of nonliner
#'   regimes: 0 = none, 1 = subharmonics, 2 = subharmonics + jitter + shimmer
#' @param jitterLen duration of stable periods between pitch jumps, ms. Use a
#'   low value for harsh noise, a high value for irregular vibrato or shaky
#'   voice
#' @param jitterDep cycle-to-cycle random pitch variation, semitones
#' @param vibratoFreq the rate of regular pitch modulation, or vibrato, Hz (vectorized)
#' @param vibratoDep the depth of vibrato, semitones (vectorized)
#' @param shimmerDep random variation in amplitude between individual glottal
#'   cycles (0 to 100\% of original amplitude of each cycle)
#' @param attackLen duration of fade-in / fade-out at each end of syllables and
#'   noise (ms)
#' @param rolloff basic rolloff from lower to upper harmonics, db/octave
#'   (exponential decay). All rolloff parameters are vectorized. See
#'   \code{\link{getRolloff}} for more details
#' @param rolloffOct basic rolloff changes from lower to upper
#'   harmonics (regardless of f0) by \code{rolloffOct} dB/oct. For
#'   example, we can get steeper rolloff in the upper part of the spectrum
#' @param rolloffParab an optional quadratic term affecting only the
#'   first \code{rolloffParabHarm} harmonics. The middle harmonic
#'   of the first \code{rolloffParabHarm} harmonics is amplified or
#'   dampened by \code{rolloffParab} dB relative to the basic
#'   exponential decay
#' @param rolloffParabHarm the number of harmonics affected by
#'   \code{rolloffParab}
#' @param rolloffKHz rolloff changes linearly with f0 by
#'   \code{rolloffKHz} dB/kHz. For ex., -6 dB/kHz gives a 6 dB
#'   steeper basic rolloff as f0 goes up by 1000 Hz
#' @param lipRad the effect of lip radiation on source spectrum, dB/oct
#'   (the default of +6 dB/oct produces a high-frequency boost when the mouth is
#'   open)
#' @param rolloffLip same as lipRad (deprecated since soundgen 1.1.0)
#' @param noseRad the effect of radiation through the nose on source
#'   spectrum, dB/oct (the alternative to \code{lipRad} when the mouth is
#'   closed)
#' @param mouthOpenThres open the lips (switch from nose radiation to lip
#'   radiation) when the mouth is more than \code{mouthOpenThres} open, 0 to 1
#' @param formants either a character string like "aaui" referring to default
#'   presets for speaker "M1" or a list of formant times, frequencies,
#'   amplitudes, and bandwidths (see ex. below). \code{formants = NA} defaults
#'   to schwa. Time stamps for formants and mouthOpening can be specified in ms
#'   or an any other arbitrary scale. See \code{\link{getSpectralEnvelope}} for more details
#' @param formantDep scale factor of formant amplitude (1 = no change relative
#'   to amplitudes in \code{formants})
#' @param formantDepStoch the amplitude of additional stochastic formants added above
#'   the highest specified formant, dB (only if temperature > 0)
#' @param formantWidth = scale factor of formant bandwidth (1 = no change)
#' @param vocalTract the length of vocal tract, cm. Used for calculating formant
#'   dispersion (for adding extra formants) and formant transitions as the mouth
#'   opens and closes. If \code{NULL} or \code{NA}, the length is estimated
#'   based on specified formant frequencies (if any)
#' @param subFreq target frequency of subharmonics, Hz (lower than f0, adjusted
#'   dynamically so f0 is always a multiple of subFreq)
#' @param subDep the width of subharmonic band, Hz. Regulates how quickly the
#'   strength of subharmonics fades as they move away from harmonics in f0
#'   stack. Low values produce narrow sidebands, high values produce uniformly
#'   strong subharmonics
#' @param shortestEpoch minimum duration of each epoch with unchanging
#'   subharmonics regime, in ms
#' @param amDep amplitude modulation depth, %. 0: no change; 100: amplitude
#'   modulation with amplitude range equal to the dynamic range of the sound
#' @param amFreq amplitude modulation frequency, Hz (vectorized)
#' @param amShape amplitude modulation shape (-1 to +1, defaults to 0)
#'   (vectorized)
#' @param noiseAnchors a numeric vector of noise amplitudes (throwaway dB = none, 0
#'   dB = as loud as voiced component) or a dataframe specifying the time (ms)
#'   and amplitude (dB) of anchors for generating the noise component such as
#'   aspiration, hissing, etc
#' @param formantsNoise the same as \code{formants}, but for the
#'   noise component instead of the harmonic component. If NA (default), the
#'   noise component will be filtered through the same formants as the harmonic
#'   component, approximating aspiration noise [h]
#' @param rolloffNoise rolloff of noise, dB/octave. It is analogous to
#'   \code{rolloff}, but while \code{rolloff} applies to the harmonic component,
#'   \code{rolloffNoise} applies to the noise component (vectorized)
#' @param mouthAnchors a numeric vector of mouth opening (0 to 1, 0.5 = neutral,
#'   i.e. no modification) or a dataframe specifying the time (ms) and value of
#'   mouth opening
#' @param amplAnchors a numeric vector of amplitude envelope (0 to 1) or a
#'   dataframe specifying the time (ms) and value of amplitude anchors
#' @param amplAnchorsGlobal a numeric vector of global amplitude envelope
#'   spanning multiple syllables or a dataframe specifying the time (ms) and
#'   value (0 to 1) of each anchor
#' @param interpol the method of smoothing envelopes based on provided pitch,
#'   amplitude, and mouth anchors: 'approx' = linear interpolation, 'spline' =
#'   cubic spline, 'loess' (default) = polynomial local smoothing function. NB:
#'   this does not affect noiseAnchors, glottalAnchors, and the smoothing of
#'   formants
#' @param discontThres,jumpThres if two anchors are closer in time than
#'   \code{discontThres}, the contour is broken into segments with a linear
#'   transition between these anchors; if anchors are closer than
#'   \code{jumpThres}, a new section starts with no transition at all (e.g. for
#'   adding pitch jumps)
#' @param samplingRate sampling frequency, Hz
#' @param windowLength length of FFT window, ms
#' @param overlap FFT window overlap, \%
#' @param addSilence silence before and after the bout, ms
#' @param pitchFloor,pitchCeiling lower & upper bounds of f0
#' @param pitchSamplingRate sampling frequency of the pitch contour only, Hz. Low
#'   values reduce processing time. A rule of thumb is to set this to
#'   the same value as \code{pitchCeiling}
#' @param throwaway discard harmonics and noise that are quieter than this
#'   number (in dB, defaults to -80) to save computational resources
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
#' # Type "soundgen_app()" to start it
#'
#' playback = c(TRUE, FALSE)[2]  # set to TRUE to play back the audio from examples
#'
#' sound = soundgen(play = playback)
#' # spectrogram(sound, 16000, osc = TRUE)
#' # playme(sound)
#'
#' # Use the in-built collection of presets:
#' # names(presets)  # speakers
#' # names(presets$Chimpanzee)  # calls per speaker
#' s1 = eval(parse(text = presets$Chimpanzee$Scream_conflict))  # screaming chimp
#' # playme(s1)
#' s2 = eval(parse(text = presets$F1$Scream_conflict))
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
#'   play = playback, pitchAnchors = data.frame(
#'     time = c(0, 0.65, 1), value = c(977, 1540, 826)),
#'   pitchAnchorsGlobal = data.frame(time = c(0, .5, 1), value = c(-6, 7, 0)))
#'
#' # Subharmonics in sidebands (noisy scream)
#' sound = soundgen (nonlinBalance = 100, subFreq = 75, subDep = 130,
#'   pitchAnchors = data.frame(
#'     time = c(0, .3, .9, 1), value = c(1200, 1547, 1487, 1154)),
#'   sylLen = 800,
#'   play = playback, plot = TRUE)
#'
#' # Jitter and mouth opening (bark, dog-like)
#' sound = soundgen(repeatBout = 2, sylLen = 160, pauseLen = 100,
#'   nonlinBalance = 100, subFreq = 100, subDep = 60, jitterDep = 1,
#'   pitchAnchors = data.frame(time = c(0, 0.52, 1), value = c(559, 785, 557)),
#'   mouthAnchors = data.frame(time = c(0, 0.5, 1), value = c(0, 0.5, 0)),
#'   vocalTract = 5, play = playback)
#'
#' # Use nonlinRandomWalk to crease reproducible examples of sounds with nonlinear effects. For ex., to make a sound with no effect in the first third, subharmonics in the second third, and jitter in the final third of the total duration:
#' a = c(rep(0, 100), rep(1, 100), rep(2, 100))
#' s = soundgen(sylLen = 800, pitchAnchors = 300, temperature = 0.001,
#'              subFreq = 100, subDep = 70, jitterDep = 1,
#'              nonlinRandomWalk = a, plot = T, ylim = c(0, 4))
#' }
soundgen = function(repeatBout = 1,
                    nSyl = 1,
                    sylLen = 300,
                    pauseLen = 200,
                    pitchAnchors = data.frame(time = c(0, .1, .9, 1),
                                              value = c(100, 150, 135, 100)),
                    pitchAnchorsGlobal = NA,
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
                    attackLen = 50,
                    rolloff = -9,
                    rolloffOct = -3,
                    rolloffKHz = -3,
                    rolloffParab = 0,
                    rolloffParabHarm = 3,
                    lipRad = 6,
                    rolloffLip = NULL,
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
                    noiseAnchors = NULL,
                    formantsNoise = NA,
                    rolloffNoise = -4,
                    mouthAnchors = data.frame(time = c(0, 1),
                                              value = c(.5, .5)),
                    amplAnchors = NA,
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
                    throwaway = -80,
                    invalidArgAction = c('adjust', 'abort', 'ignore')[1],
                    plot = FALSE,
                    play = FALSE,
                    savePath = NA,
                    ...) {
  # deprecated pars
  if (!missing(rolloffLip)) {
    lipRad = rolloffLip
    message('rolloffLip is deprecated; use lipRad instead')
  }

  # check that values of numeric arguments are valid and within range
  for (p in rownames(permittedValues)[1:which(
    rownames(permittedValues) == 'rolloffNoise'
  )]) {
    if (!is.numeric(get(p)) ||
        any(get(p) < permittedValues[p, 'low']) ||
        any(get(p) > permittedValues[p, 'high'])) {
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
        warning(paste0(p, " outside permitted range, reset to ", get(p),
                       ". Edit 'permittedValues' or use ",
                       "invalidArgAction = 'ignore' to force."))
      }
    }
  }
  if (pauseLen < 0 && nSyl > 1) {
    stop(paste(
      'Negative pauseLen is allowed between bouts, but not between syllables.',
      'Use repeatBout instead of nSyl if you need syllables to overlap'
    ))
  }

  # check and, if necessary, reformat anchors to dataframes
  for (anchor in c('pitchAnchors', 'pitchAnchorsGlobal', 'glottisAnchors',
                   'amplAnchors', 'amplAnchorsGlobal', 'mouthAnchors')) {
    assign(anchor, reformatAnchors(get(anchor)))
  }
  if (is.numeric(noiseAnchors) && length(noiseAnchors) > 0) {
    noiseAnchors = data.frame(
      time = seq(0, sylLen, length.out = max(2, length(noiseAnchors))),
      value = noiseAnchors
    )
  }

  windowLength_points = floor(windowLength / 1000 * samplingRate / 2) * 2

  # preliminary glottis contour
  glottisClosed = getSmoothContour(anchors = glottisAnchors, len = 100)
  # adjust length based on proportion of closed glottis (pauses added)
  mean_closed = mean(glottisClosed) / 100
  sylLen = sylLen / (mean_closed + 1)
  if (is.list(pitchAnchors)) {
    pitchAnchors$value = pitchAnchors$value * (mean_closed + 1)
  }

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
  if (!is.list(noiseAnchors)) {
    noiseAnchors = data.frame(
      time = c(0, sylLen),
      value = c(throwaway, throwaway)
    )
  }

  # effects of creakyBreathy hyper
  if (creakyBreathy < 0) {
    # for creaky voice
    nonlinBalance = min(100, nonlinBalance - creakyBreathy * 100)
    jitterDep = max(0, jitterDep - creakyBreathy / 2)
    shimmerDep = max(0, shimmerDep - creakyBreathy * 5)
    subDep = subDep * 2 ^ (-creakyBreathy)
  } else if (creakyBreathy > 0) {
    # for breathy voice, add breathing
    if (!is.list(noiseAnchors)) {
      noiseAnchors = data.frame(time = c(0, sylLen + 100),
                                value = c(throwaway, throwaway))
    }
    noiseAnchors$value = noiseAnchors$value + creakyBreathy * (-throwaway)
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
  rolloff = rolloff - creakyBreathy * 10
  if (invalidArgAction != 'ignore') {
    rolloff[rolloff < permittedValues['rolloff', 'low']] =
      permittedValues['rolloff', 'low']
    rolloff[rolloff > permittedValues['rolloff', 'high']] =
      permittedValues['rolloff', 'high']
    rolloffOct = rolloffOct - creakyBreathy * 5
    rolloffOct[rolloffOct < permittedValues['rolloffOct', 'low']] =
      permittedValues['rolloffOct', 'low']
    rolloffOct[rolloffOct > permittedValues['rolloffOct', 'high']] =
      permittedValues['rolloffOct', 'high']
  }

  # effects of nonlinDep hyper
  subFreq = 2 * (subFreq - 50) / (1 + exp(-.1 * (50 - nonlinDep))) + 50
  # subFreq unchanged for nonlinDep=50%, raised for lower and
  # lowered for higher noise intensities. Max set at 2*subFreq-50, min at 50 Hz.
  # Jitter and shimmer go to 0 if nonlinDep = 0 and double if nonlinDep = 1
  # Illustration: subFreq=250; nonlinDep=0:100; plot(nonlinDep,
  #   2 * (subFreq - 50) / (1 + exp(-.1 * (50 - nonlinDep))) + 50, type = 'l')
  jitterDep = 2 * jitterDep / (1 + exp(.1 * (50 - nonlinDep)))
  # Illustration: jitterDep = 1; nonlinDep = 0:100;
  # plot(nonlinDep, 2 * jitterDep / (1 + exp(.1 * (50 - nonlinDep))), type = 'l')
  shimmerDep = 2 * shimmerDep / (1 + exp(.1 * (50 - nonlinDep)))
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
    vocalTract = vocalTract * (1 - .25 * maleFemale)
  }

  ## stochastic rounding of the number of syllables and repeatBouts
  #   (eg for nSyl = 2.5, we'll have 2 or 3 syllables with equal probs)
  #   NB: this is very useful for morphing
  if (!is.integer(nSyl)) {
    nSyl = floor(nSyl) + rbinom(1, 1, nSyl - floor(nSyl))
  }
  if (!is.integer(repeatBout)) {
    repeatBout = floor(repeatBout) +
      rbinom(1, 1, repeatBout - floor(repeatBout))
  }

  # prepare a list of pars for calling generateHarmonics()
  pars_to_vary = c(
    'nonlinDep',
    'attackLen',
    'jitterDep',
    'shimmerDep',
    'rolloff',
    'rolloffOct',
    'shortestEpoch',
    'subFreq',
    'subDep'
  )
  # don't add nonlinBalance, otherwise there is no simple way to remove noise at temp>0
  pars_to_round = c('attackLen', 'subFreq', 'subDep')
  pars_list = list(
    'attackLen' = attackLen,
    'jitterDep' = jitterDep,
    'jitterLen' = jitterLen,
    'vibratoFreq' = vibratoFreq,
    'vibratoDep' = vibratoDep,
    'shimmerDep' = shimmerDep,
    'creakyBreathy' = creakyBreathy,
    'rolloff' = rolloff,
    'rolloffOct' = rolloffOct,
    'rolloffKHz' = rolloffKHz,
    'rolloffParab' = rolloffParab,
    'rolloffParabHarm' = rolloffParabHarm,
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
    'throwaway' = throwaway,
    'samplingRate' = samplingRate,
    'overlap' = overlap
  )
  pars_syllable = pars_list
  if (is.list(pitchAnchorsGlobal) &&
      any(!is.na(pitchAnchorsGlobal)) &&
      any(pitchAnchorsGlobal$value != 0) &&
      nSyl > 1) {
    pitchDeltas = 2 ^ (
      getDiscreteContour(
        len = nSyl,
        anchors = pitchAnchorsGlobal,
        interpol = 'spline',
        plot = FALSE
      ) / 12
    )
  } else {
    pitchDeltas = rep(1, nSyl)
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

  wiggleNoise = temperature > 0 &&
    class(noiseAnchors) == 'data.frame' &&
    any(noiseAnchors$value > throwaway)
  wiggleAmpl_per_syl = temperature > 0 &&
    !is.na(amplAnchors) &&
    any(amplAnchors$value < -throwaway)
  wiggleGlottis = temperature > 0 &&
    class(glottisAnchors) == 'data.frame' &&
    any(glottisAnchors$value > 0)

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
      # wiggle par values for this particular syllable, making sure
      #   they are within the permitted range for each variable
      pitchAnchors_per_syl = pitchAnchors
      amplAnchors_per_syl = amplAnchors
      glottisAnchors_per_syl = glottisAnchors

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
            low = c(0, permittedValues['pitch', 'low']),
            high = c(1, permittedValues['pitch', 'high']),
            temp_coef = tempEffects$pitchAnchorsDep,
            invalidArgAction = invalidArgAction
          )
        }
        if (wiggleNoise) {
          noiseAnchors_syl[[s]] = wiggleAnchors(
            df = noiseAnchors,
            temperature = temperature,
            low = c(-Inf, permittedValues['noiseAmpl', 'low']),
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
            low = c(0, 0),
            high = c(1,-throwaway),
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

      # generate the voiced part
      if (dur_syl < permittedValues['sylLen', 'low'] |
          (!is.na(noiseAnchors) && min(noiseAnchors$value) >= 40) |
          !is.list(pitchAnchors_per_syl)) {
        # only synthesize voiced part if noise is weaker than 40 dB
        #   and the voiced part is long enough to bother synthesizing it
        syllable = rep(0, round(dur_syl * samplingRate / 1000))
      } else {
        # the actual synthesis is here
        # print(pars_syllable)
        syllable = try(do.call(generateHarmonics, c(
          pars_syllable,
          list(pitch = pitchContour_syl,
               amplAnchors = amplAnchors_per_syl,
               glottisAnchors = glottisAnchors_per_syl)
        ))
        )
        # update the actual syllable duration (varies if there is strong jitter etc)
        dur_syl = length(syllable) / samplingRate * 1000
      }
      # spectrogram(syllable, samplingRate = samplingRate)
      # playme(syllable, samplingRate = samplingRate)
      if (class(syllable) == 'try-error') {
        stop('Failed to generate the new syllable!')
      }
      # if (any(is.na(syllable))) {
      #   stop('The new syllable contains NA values!')
      # }

      # generate pause for all but the last syllable
      if (s < nrow(syllables)) {
        pause = rep(0, floor((syllables[s + 1, 1] - syllables[s, 2]) *
                               samplingRate / 1000))
      } else {
        pause = numeric()
      }

      # add syllable and pause to the growing sound
      voiced = c(voiced, syllable, pause)

      # update syllable timing info, b/c with temperature > 0 there will be deviations
      if (s < nrow(syllables)) {
        correction = length(voiced) / samplingRate * 1000 - syllables[s + 1, 'start']
        syllables[(s + 1):nrow(syllables), ] = syllables[(s + 1):nrow(syllables), ] + correction
      }

      # generate the unvoiced part, but don't add it to the sound just yet
      if (!is.na(noiseAnchors) &&
          sum(noiseAnchors$value > throwaway) > 0) {
        # adjust noiseAnchors$time to match the actual syllable duration
        noiseAnchors_syl[[s]] = noiseAnchors
        idx_mid = which(noiseAnchors_syl[[s]]$time > 0 &  # not before syl
                          noiseAnchors_syl[[s]]$time < sylLen)  # not after syl
        idx_after = which(noiseAnchors_syl[[s]]$time >= sylLen)  # after syl
        noiseAnchors_syl[[s]]$time[idx_mid] =
          noiseAnchors_syl[[s]]$time[idx_mid] * dur_syl / sylLen
        noiseAnchors_syl[[s]]$time[idx_after] =
          noiseAnchors_syl[[s]]$time[idx_after] - sylLen + dur_syl
        # negative time anchors are not changed: the pre-aspiration length
        # is constant, regardless of the actual syllable duration.
        # Time anchors from 0 to sylLen are proportional to the actual
        # syllable duration re the average expected duration (which the user
        # sees in the UI when choosing time anchors)
        # Time anchors beyond sylLen are scaled to preserve post-aspiration len
        unvoicedDur_syl = round(diff(range(noiseAnchors_syl[[s]]$time)) *
                                  samplingRate / 1000)

        # calculate noise spectrum
        if (is.null(formantsNoise) || is.na(formantsNoise[1])) {
          spectralEnvelopeNoise = NA
        } else {
          movingFormants = max(unlist(lapply(formantsNoise, length))) > 1 |
            sum(mouthAnchors$value != .5) > 0 # are noise formants moving, as opposed to constant?
          nInt = ifelse(movingFormants,
                        round(diff(range(noiseAnchors_syl[[s]]$time)) / 10),
                        1) # the number of different noise spectra,
          # allowing one column (noise spectrum) per 10 ms of audio
          spectralEnvelopeNoise = getSpectralEnvelope(
            nr = windowLength_points / 2,
            nc = nInt,
            formants = formantsNoise,
            formantDep = formantDep,
            formantWidth = formantWidth,
            formantDepStoch = 0,  # formantDepStoch,
            lipRad = lipRad,
            noseRad = noseRad,
            mouthOpenThres = mouthOpenThres,
            mouthAnchors = mouthAnchors,
            interpol = interpol,
            temperature = temperature,
            formDrift = tempEffects$formDrift,
            formDisp = tempEffects$formDisp,
            samplingRate = samplingRate,
            vocalTract = vocalTract
          )
          # image(t(spectralEnvelopeNoise))
        }

        # synthesize the unvoiced part
        unvoiced[[s]] = generateNoise(
          len = unvoicedDur_syl,
          noiseAnchors = noiseAnchors_syl[[s]],
          rolloffNoise = rnorm(n = length(rolloffNoise),
                               mean = rolloffNoise,
                               sd = abs(rolloffNoise) * temperature * tempEffects$specDep),
          attackLen = attackLen,
          samplingRate = samplingRate,
          windowLength_points = windowLength_points,
          overlap = overlap,
          throwaway = throwaway,
          filterNoise = spectralEnvelopeNoise
        )
        # plot(unvoiced[[s]], type = 'l')
      }
    }
    # plot(voiced, type = 'l')
    # spectrogram(voiced, samplingRate = samplingRate, osc = TRUE)
    # playme(voiced, samplingRate = samplingRate)
    # END OF SYLLABLE GENERATION

    ## calculate updated syllable timing for inserting unvoiced segments
    syllableStartIdx = round(syllables[, 'start'] * samplingRate / 1000)
    syllableStartIdx[1] = 1
    # if noise is added before the voiced part of each syllable
    #   (negative time anchors) or starts later than the voiced part,
    #   we need to shift noise insertion points
    if (!is.na(noiseAnchors) && noiseAnchors$time[1] != 0) {
      shift = -round(noiseAnchors$time[1] * samplingRate / 1000)
      if (noiseAnchors$time[1] < 0) {
        # only the first syllableStartIdx is shifted, because that changes
        # the sound length and the remaining syllableStartIdx, if any,
        # are already shifted appropriately
        syllableStartIdx[1] = syllableStartIdx[1] - shift
      } else {
        syllableStartIdx = syllableStartIdx - shift # shift for each syllable
      }
    }

    # if the unvoiced noise is of type "breathing" (the same formants as in
    #   the voiced part), we mix voiced+unvoiced BEFORE filtering the sound,
    #   otherwise we filter first and then mix voiced+unvoiced
    sound = voiced
    if (length(unvoiced) > 0 && (is.null(formantsNoise) || is.na(formantsNoise))) {
      for (s in 1:length(unvoiced)) {
        sound = addVectors(sound, unvoiced[[s]],
                           insertionPoint = syllableStartIdx[s])
      }
    }
    # plot(sound, type = 'l')
    # spectrogram(sound, samplingRate = samplingRate)
    # playme(sound, samplingRate = samplingRate)

    # for polysyllabic vocalizations, apply amplitude envelope (if specified)
    #   over the entire bout and normalize to -1...+1
    if (!is.na(amplAnchorsGlobal) &&
        length(which(amplAnchorsGlobal$value < -throwaway)) > 0) {
      amplEnvelope = getSmoothContour(
        anchors = amplAnchorsGlobal,
        len = length(sound),
        interpol = interpol,
        discontThres = discontThres,
        jumpThres = jumpThres,
        valueFloor = 0,
        valueCeiling = -throwaway,
        samplingRate = samplingRate
      )
      # convert from dB to linear multiplier
      amplEnvelope = 10 ^ (amplEnvelope / 20)
      # plot(amplEnvelope)
      sound = sound * amplEnvelope
    }

    # add formants
    if (length(sound) / samplingRate * 1000 > permittedValues['sylLen', 'low']) {
      soundFiltered = addFormants(
        sound = sound,
        formants = formants,
        vocalTract = vocalTract,
        formantDep = formantDep,
        formantWidth = formantWidth,
        formantDepStoch = formantDepStoch,
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
    } else {
      soundFiltered = sound
    }
    # spectrogram(soundFiltered, samplingRate = samplingRate)
    # playme(soundFiltered, samplingRate = samplingRate)

    # add the separately filtered noise back into the sound
    # at the appropriate time points AFTER filtering the sound
    if (length(unvoiced) > 0 && is.list(formantsNoise)) {
      for (s in 1:length(unvoiced)) {
        soundFiltered = addVectors(soundFiltered, unvoiced[[s]],
                                   insertionPoint = syllableStartIdx[s])
      }
    } # plot(soundFiltered, type = 'l')

    # trill - rapid regular amplitude modulation
    if (any(amDep > 0)) {
      # trill = 1 - sin(2 * pi * (1:length(soundFiltered)) /
      #                  samplingRate * amFreq) * amDep / 100
      if (length(amDep) > 1) {
        amDep = getSmoothContour(anchors = amDep,
                                 len = length(soundFiltered),
                                 interpol = 'spline',
                                 valueFloor = 0)
      }
      sig = getSigmoid(len = length(soundFiltered),
                       samplingRate = samplingRate,
                       freq = amFreq,
                       shape = amShape)
      trill = 1 - sig * amDep / 100
      # plot(trill, type='l')
      soundFiltered = soundFiltered * trill
    }

    # grow bout
    if (b == 1) {
      bout = soundFiltered
    } else {
      bout = addVectors(
        bout,
        soundFiltered,
        insertionPoint = length(bout) + round(pauseLen * samplingRate / 1000)
      )
    }
  }

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
  if(plot) {
    spectrogram(bout, samplingRate = samplingRate, ...)
  }
  return(bout)
}
