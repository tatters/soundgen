###
### D E F A U L T   P A R   V A L U E S   &   P R E S E T S
###

# devtools::use_data(permittedValues, defaults, presets, overwrite = TRUE)


#' Defaults and ranges
#'
#' A dataset containing defaults and ranges of key variables in the Shiny app.
#' Adjust as needed.
#'
#' @format A matrix with 58 rows and 4 variables:
#' \describe{
#'   \item{default}{default value}
#'   \item{low}{lowest permitted value}
#'   \item{high}{highest permitted value}
#'   \item{step}{increment for adjustment}
#'   ...
#' }
"permittedValues"
permittedValues = matrix(c(
  # sliderInput's to be reset for each callType
  'repeatBout', 1, 1, 20, 1,  # default, low, high, step
  'nSyl', 1, 1, 10, 1,
  'sylLen', 300, 20, 5000, 10,
  'pauseLen', 200, 20, 1000, 10,
  'temperature', .025, 0, 1, .025,
  'maleFemale', 0, -1, 1, 0.1,
  'creakyBreathy', 0, -1, 1, 0.1,
  'nonlinBalance', 0, 0, 100, 1,
  'nonlinDep', 50, 0, 100, 1,
  'jitterDep', 3, 0, 24, 0.1,
  'jitterLen', 1, 1, 100, 1,
  'vibratoFreq', 5, 3, 10, .5,
  'vibratoDep', 0, 0, 3, 0.125,
  'shimmerDep', 0, 0, 100, 1,
  'attackLen', 50, 0, 200, 10,
  'rolloff', -9, -30, 0, 1,
  'rolloffOct', -3, -30, 10, 1,
  'rolloffParab', 0, -30, 30, 1,
  'rolloffParabHarm', 3, 1, 20, 1,
  'rolloffKHz', -3, -20, 0, 1,
  'rolloffLip', 6, 0, 20, 1,
  'rolloffNose', 4, 0, 20, 1,
  'mouthOpenThres', 0, 0, 1, .05,
  'formantDep', 1, 0, 2, .1,
  'formantDepStoch', 20, 0, 40, 5,
  'subFreq', 100, 10, 1000, 10,
  'subDep', 100, 0, 500, 10,
  'shortestEpoch', 300, 50, 500, 25,
  'amDep', 0, 0, 100, 5,
  'amFreq', 30, 10, 100, 5,
  'amShape', 0, -1, 1, .025,
  'samplingRate', 16000, 8000, 44100, 100,
  'windowLength', 40, 5, 100, 2.5, # default, low, high, step
  'rolloffNoise', -4, -20, 20, 1,

  # other soundgen settings, which are NOT updateable sliders in soundgen_app()
  'vocalTract', 15.5, 2, 100, .5,
  'overlap', 50, 0, 99, 1,
  'addSilence', 100, 0, 1000, 50,
  'pitchFloor', 50, 1, 1000, 1,
  'pitchCeiling', 3500, 10, 100000, 10,
  'pitchSamplingRate', 3500, 10, 100000, 10,
  'throwaway', -80, -200, -10, 10,

  # soundgen_app() settings, which are not needed for soundgen()
  'specWindowLength', 40, 5, 100, 2.5,
  'specContrast', .2, -1, 1, .05,
  'specBrightness', 0, -1, 1, .05,
  'mouthOpening', .5, 0, 1, .05,
  'pitch', 100, 25, 3500, 1,  # set pitch range per species
  'pitchDeltas', 0, -24, 24, 1,  # amplPitchGlobal range
  'time', 0, 0, 5000, 1,
  'noiseAmpl', 0, -80, 40, 1  # for plotting - noise ylim
), ncol=5, byrow=TRUE)
temp = permittedValues[,1]
permittedValues = apply (permittedValues[,2:5], 2, as.numeric)
colnames(permittedValues) = c('default', 'low', 'high', 'step')
rownames(permittedValues) = temp
# devtools::use_data(permittedValues, overwrite = TRUE)

# a list of default values for Shiny app - mostly the same as for
# soundgen(). NB: if defaults change, this has to be updated!!!
defaults = list(
  repeatBout = 1,
  nSyl = 1,
  sylLen = 300,
  pauseLen = 200,
  temperature = 0.025,
  maleFemale = 0,
  creakyBreathy = 0,
  nonlinBalance = 0,
  nonlinDep = 50,
  jitterDep = 3,
  jitterLen = 1,
  vibratoFreq = 5,
  vibratoDep = 0,
  shimmerDep = 0,
  attackLen = 50,
  rolloff = -9,
  rolloffOct = -3,
  rolloffParab = 0,
  rolloffParabHarm = 3,
  rolloffKHz = -3,
  rolloffLip = 6,
  rolloffNose = 4,
  mouthOpenThres = 0,
  formantDep = 1,
  formantDepStoch = 20,
  subFreq = 100,
  subDep = 100,
  shortestEpoch = 300,
  amDep = 0,
  amFreq = 30,
  amShape = 0,
  samplingRate = 16000,
  windowLength = 40,
  rolloffNoise = -4,
  windowLength_points = 512,
  overlap = 75,
  addSilence = 100,
  pitchFloor = 25,
  pitchCeiling = 3500,
  pitchSamplingRate = 3500,
  throwaway = -80,
  pitchAnchors = list(
    time = c(0, .1, .9, 1),
    value = c(100, 150, 135, 100)
  ),
  pitchAnchorsGlobal = list(time = c(0, 1), value = c(0, 0)),
  noiseAnchors = list(time = c(0, 1), value = c(-80, -80)),
  mouthAnchors = list(time = c(0, 1), value = c(.5, .5)),
  amplAnchors = list(time = c(0, 1), value = c(80, 80)),
  amplAnchorsGlobal = list(time = c(0, 1), value = c(80, 80)),
  formants = list(f1 = 860, f2 = 1430, f3 = 2900),
  formantsNoise = NA,
  vowelString = NA,
  vocalTract = NA
)
# devtools::use_data(defaults, overwrite = TRUE)

# -------------------------------------------------------------
# A library of presets for easy generation of a few nice sounds
# -------------------------------------------------------------
#' Presets
#'
#' A library of presets for easy generation of a few nice sounds.
#'
#' @format A list of length 4.
"presets"
presets = list(
  M1 = list(
    Vowel1 = 'soundgen()', # these are just the global defaults

    Gasp = 'soundgen(sylLen = 240, pitchAnchors = c(160, 150), nonlinBalance = 100, jitterDep = 0.6, rolloff = -24, rolloffOct = -7, rolloffParab = 15, rolloffParabHarm = 2, formantDep = 1.5, subDep = 0, shortestEpoch = 100, noiseAnchors = list(time = c(-1, 170, 362), value = c(-44, -32, -56)), rolloffNoise = -7, mouthAnchors = list(time = c(0, 0.07, 1), value = c(0, 0.48, 0.32)))',

    Roar = 'soundgen(sylLen = 960, pitchAnchors = list(time = c(0, 0.13, 0.9, 1), value = c(151, 187, 139, 120)), temperature = 0.1, nonlinBalance = 60, jitterDep = 0.7, rolloff = -12, rolloffOct = 3, formants = c(500, 1000, 1500), vocalTract = 19, subFreq = 90, subDep = 60, shortestEpoch = 150, mouthAnchors = list(time = c(0, 0.1, 1), value = c(0.38, 0.71, 0.42)))',

    Moan = 'soundgen(sylLen = 800, pitchAnchors = list(time = c(0, 0.21, 1), value = c(230, 181, 143)), nonlinBalance = 100, jitterDep = 1, jitterLen = 60, attackLen = 100, rolloff = -12, rolloffOct = 0, formants = list(f1 = c(630, 860), f2 = c(900, 1430), f3 = c(3000, 2900), f4 = c(3960, 3960)), subDep = 0, noiseAnchors = list(time = c(-20, 801, 911), value = c(-39, -27, -80)), rolloffNoise = -8, mouthAnchors = c(0.54, 0.3), amplAnchors = c(80, 71))',

    Sigh = 'soundgen(sylLen = 50, pitchAnchors = NA, temperature = 0.1, formants = c(860, 1430, 2900), vocalTract = 17, noiseAnchors = list(time = c(-20, 104, 756, 1252), value = c(26, 40, 23, -22)))',

    Laugh = 'soundgen(nSyl = 5, sylLen = 120, pauseLen = 120, pitchAnchors = c(180, 166), pitchAnchorsGlobal = list(time = c(0, 0.3, 1), value = c(1, 2, 0)), temperature = 0.1, nonlinBalance = 50, jitterDep = 0.8, attackLen = 10, formants = list(f1 = c(860, 550, 550), f2 = c(1430, 2300, 2300), f3 = c(2900, 2800, 2800), f4 = 4000), subDep = 0, shortestEpoch = 50, noiseAnchors = list(time = c(-10, 57, 237), value = c(-80, -40, -80)), rolloffNoise = -6, amplAnchors = list(time = c(0, 0.32, 1), value = c(80, 60, 80)), amplAnchorsGlobal = list(time = c(0, 0.77, 1), value = c(80, 69, 0)))',

    Snore = 'soundgen(sylLen = 960, pitchAnchors = list(time = c(0, 0.15, 0.87, 1), value = c(175, 199, 188, 140)), temperature = 0.1, nonlinBalance = 100, jitterDep = 0.75, rolloffOct = -14, formants = c(560, 1000, 1450, 3800), subDep = 80, shortestEpoch = 200, noiseAnchors = list(time = c(-18, 991), value = c(-48, -48)), mouthAnchors = 0, amplAnchorsGlobal = c(80, 43))',

    # 'Formants' is a reserved name. The list of presets for every caller should end with
    # a list of 'Formants' presets for each vowel and consonant, otherwise you won't be
    # able to specify formants in a string like 'aui' for this speaker
    Formants = list(
      vowels = list(
        'a' = list(f1 = 860, f2 = 1430, f3 = 2900),
        'o' = list(f1 = 630, f2 = 900, f3 = 3000, f4 = 3960),
        'i' = list(f1 = 300, f2 = 2700, f3 = 3400),
        'e' = list(f1 = 550, f2 =2300, f3 = 2800,  f4 = 4000),
        'u' = list(f1 = 300, f2 = 610, f3 = 2100, f4 = 4200),
        '0' = list(f1 = 640, f2 = 1670, f3 = 2700, f4 = 3880)  # schwa
      ),
      consonants = list(
        'h' = list(
          label = 'h',
          rolloffNoise = 0,
          f1=list(freq=420, amp=10),
          f2=list(freq=1200, amp=30),
          f3=list(freq=3400, amp=25),
          f4=list(freq=5000, amp=25),
          f5=list(freq=8500, amp=20)
        ),
        's' = list(
          label = 's',
          rolloffNoise = 0,
          f1=list(freq=8000, amp = 25)
        ),
        'x' = list(
          label = 'sh',
          rolloffNoise = -9,
          f1=list(freq=1700, amp=15, width=80),
          f2=list(freq=2600, amp=30, width=300),
          f3=list(freq=3400, amp=25, width=200),
          f4=list(freq=4800, amp=10, width=300)
        ),
        'f' = list(
          label = 'f',
          rolloffNoise = 0,
          f1 = list(freq = 1400, amp = 20, width = 300)
        ),
        'n' = list(
          label = 'snuffle',  # sNuffle (breathing through the nose)
          rolloffNoise = 0,
          f1=list(freq=5400, amp=25, width=2000)
        )
      )
    )
  ),

  F1 = list(
    Vowel2 = 'soundgen(sylLen = 500, pitchAnchors = list(time = c(0, 0.6, 1), value = c(340, 370, 340)), formants = c(900, 1300, 3300, 4340))',

    Scream = 'soundgen(sylLen = 1110, pitchAnchors = list(time = c(0, 0.1, 0.85, 1), value = c(900, 1832, 1618, 1200)), temperature = 0.1, nonlinBalance = 70, jitterDep = 1, shimmerDep = 10, rolloff = -6, vocalTract = 13.5, subFreq = 400, rolloffNoise = 0)',

    Growl = 'soundgen(sylLen = 1100, pitchAnchors = list(time = c(0, 0.12, 0.34, 1), value = c(238, 254, 448, 203)), temperature = 0.1, nonlinBalance = 71, jitterDep = 1.8, rolloff = -6, rolloffParab = -20, rolloffParabHarm = 20, vocalTract = 13.5, subDep = 0, shortestEpoch = 150, amDep = 40, amFreq = 35, amShape = -0.4, noiseAnchors = list(time = c(-6, 550, 1122), value = c(-24, -13, -26)), rolloffNoise = 0, mouthAnchors = 0, amplAnchorsGlobal = list(time = c(0,  0.13, 1), value = c(71, 80, 26)))',

    Moan = 'soundgen(sylLen = 360, pitchAnchors = c(380, 260), attackLen = 100, rolloff = -24, formants = c(900, 1300, 3300, 4340), vocalTract = 13.5, noiseAnchors = list(time = c(0, 417, 508), value = c(-63, -29, -80)), mouthAnchors = list(time = c(0, 0.09, 1), value = c(0.12, 0.37, 0.5)), amplAnchorsGlobal = c(80, 18))',

    Laugh = 'soundgen(nSyl = 3, sylLen = 60, pauseLen = 90, pitchAnchors = c(368, 284), temperature = 0.075, attackLen = 10, formants = c(900, 1300, 3300, 4340), noiseAnchors = list(time = c(0, 67, 86, 186), value = c(-45, -47, -89, -80)), rolloffNoise = -8, amplAnchorsGlobal = c(80, 20))',

    Cry = 'soundgen(sylLen = 1600, pitchAnchors = c(610, 511), temperature = 0.2, nonlinBalance = 40, vocalTract = 13.5, subFreq = 125, subDep = 70, mouthAnchors = 0, amplAnchorsGlobal = c(80, 60))',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      vowels = list(
        'a' = list(f1 = 900, f2 = 1300, f3 = 3300, f4 = 4340),
        'o' = list(f1 = 800, f2 = 1100, f3 = 3560, f4 = 5830),
        'i' = list(f1 = 330, f2 = 2700, f3 = 3580, f4 = 4710, f5 = 5800),
        'e' = list(f1 = 930, f2 = 2470, f3 = 3300, f4 = 4200),
        'u' = list(f1 = 450, f2 = 850, f3 = 2900, f4 = 4100),
        '0' = list(f1 = 790, f2 = 1600, f3 = 3100, f4 = 3900)  # schwa
      ),
      consonants = list(
        'h' = list(
          label = 'h',
          rolloffNoise = -13,
          f1=list(time=0, freq=420, amp=25, width=150),
          f2=list(time=0, freq=1200, amp=50, width=250),
          f3=list(time=0, freq=5000, amp=10, width=200),
          f4=list(time=0, freq=8500, amp=10, width=300)
        ),
        's' = list(
          label = 's',
          rolloffNoise = 0,
          f1=list(time=0, freq=5500, amp=25, width=200), # NB: amplitude in dB for consonants
          f2=list(time=0, freq=7000, amp=30, width=1000),
          f3=list(time=0, freq=9000, amp=30, width=1000)
        ),
        'x' = list(
          label = 'sh',
          rolloffNoise = -9,
          f1=list(time=0, freq=1700, amp=15, width=80),
          f2=list(time=0, freq=2600, amp=30, width=300),
          f3=list(time=0, freq=3400, amp=25, width=200),
          f4=list(time=0, freq=4800, amp=10, width=300)
        ),
        'f' = list(
          label = 'f',
          rolloffNoise = -10,
          f1=list(time=0, freq=1400, amp=30, width=200),
          f2=list(time=0, freq=2000, amp=10, width=80),
          f3=list(time=0, freq=2900, amp=25, width=1000)
        ),
        'n' = list(
          label = 'snuffle',  # sNuffle (breathing through the nose)
          rolloffNoise=0,
          f1=list(time=0, freq=5400, amp=25, width=2000)
        )
      )
    )
  ),

  Chimpanzee = list(
    Bark_alarm = 'soundgen(sylLen = 160, pitchAnchors = c(232, 185), nonlinBalance = 100, jitterDep = 2.8, attackLen = 61, rolloff = -19, formants = c(400, 1000), vocalTract = 23, subFreq = 125, subDep = 60, noiseAnchors = list(time = c(0, 63, 344), value = c(-80, -10, -80)), rolloffNoise = -14)',

    Scream_conflict = 'soundgen(sylLen = 740, pitchAnchors = list(time = c(0, 0.3, 0.9, 1), value = c(1200, 1547, 1487, 1154)), temperature = 0.05, nonlinBalance = 100, jitterDep = 0.3, rolloff = -6, rolloffOct = 0, subFreq = 75, subDep = 130)',

    Grunt_excited = 'soundgen(nSyl = 6, sylLen = 100, pauseLen = 220, pitchAnchors = c(216, 164), pitchAnchorsGlobal = list(time = c(0, 0.4, 1), value = c(0, 1, -8.4)), temperature = 0.15, nonlinBalance = 100, jitterDep = 3.1, attackLen = 10, formants = list(f1 = c(410, 250), f2 = c(990, 650)), vocalTract = 20.5, subDep = 0, noiseAnchors = list(time = c(-8, 22, 298), value = c(-80, -41, -80)), rolloffNoise = -7, amplAnchors = c(119, 89), amplAnchorsGlobal = list(time = c(0, 0.35, 1), value = c(60, 80, 31)))',

    Hoot_excited = 'soundgen(sylLen = 730, pitchAnchors = list(time = c(0, 0.52, 1), value = c(440, 405, 440)), nonlinBalance = 100, jitterDep = 0.4,  attackLen = 0, rolloff = -9, rolloffOct = -3, rolloffParabHarm = 1, formants = list(f1 = c(250, 450), f2 = 800, f3 = 1600), vocalTract = 25.5, subFreq = 190, subDep = 0, noiseAnchors = list(time = c(-19, 26, 173, 738), value = c(-44, -4, -37, -39)))',

    Laugh_playing = 'soundgen(nSyl = 6, sylLen = 120, pauseLen = 120, pitchAnchors = c(127, 102), temperature = 0.05, rolloff = -12, rolloffParab = 15, rolloffParabHarm = 1, formants = c(400, 900, 1500), vocalTract = 23, noiseAnchors = list(time = c(-6, 30, 110, 168, 219), value = c(-42, -63, -62, -61, -80)), rolloffNoise = -20, amplAnchorsGlobal = list(time = c(0, 0.35, 1), value = c(59, 80, 31)))',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # ...
    )
  ),

  Cat = list(
    Chirp = 'soundgen(sylLen = 120, pitchAnchors = c(828, 768, 423), nonlinBalance = 100, jitterDep = 0.3, shimmerDep = 11, attackLen = 1, rolloff = -3, rolloffOct = -4, formants = c(875, 1944, 2409), subDep = 0, noiseAnchors = list(time = c(-1, 80, 161), value = c(-60, 14, -47)), amplAnchors = list(time = c(0, 0.33, 1), value = c(80, 80, 57)), windowLength = 10)',

    Growl = 'soundgen(sylLen = 2700, pitchAnchors = c(154, 121), nonlinBalance = 100, jitterDep = 1.3, jitterLen = 6, formants = c(200, 400, 700), vocalTract = 8, subFreq = 80, subDep = 50, shortestEpoch = 50)',

    Hiss = 'soundgen(pitchAnchors = NULL, formants = c(1112, 2610, 4587), formantDep = 0.8, noiseAnchors = list(time = c(-185, 74, 506), value = c(-5, 40, 0)), rolloffNoise = -8, mouthAnchors = list(time = c(0, 0.13, 1), value = c(0, 0.5, 0.5)))',

    Howl = 'soundgen(sylLen = 3150, pitchAnchors = list(time = c(0, 0.05, 0.35, 0.89, 1), value = c(218, 339, 352, 291, 229)), temperature = 0.125, rolloff = -5, rolloffOct = -2, rolloffParabHarm = 1, formants = list(f1 = 895, f2 = c(1500, 2000), f3 = 3287), vocalTract = 10, mouthAnchors = list(time = c(0, 0.18, 0.25, 0.41, 0.89, 1), value = c(0.32, 0.24, 0.66, 0.37, 0.39, 0.27)))',

    Heat = 'soundgen(sylLen = 800, pitchAnchors = c(480, 552, 501), temperature = 0.1, nonlinBalance = 100, jitterDep = 0.3, jitterLen = 60, rolloff = -6, rolloffParabHarm = 1, formants = list(f1 = c(1500, 500), f2 = 2100, f3 = 5500), subDep = 0, amDep = 20, amFreq = 40, mouthAnchors = list(time = c(0, 0.12, 0.86, 1), value = c(0, 0.52, 0.57, 0)))',

    Meow = 'soundgen(sylLen = 920, pitchAnchors = list(time = c(0, 0.2, 1), value = c(480, 550, 515)), attackLen = 150, rolloff = -2, rolloffOct = -4, formants = c(1594, 3600), mouthAnchors = list(time = c(0, 0.16, 0.91, 1), value = c(0, 1, 0.31, 0.06)))',

    Purr = 'soundgen(repeatBout = 2, nSyl = 2, sylLen = 800, pauseLen = 50, pitchAnchors = 25, temperature = 0.1, jitterDep = 0, jitterLen = 34, rolloff = -12, rolloffLip = 0, formants = c(1200, 2600, 5200, 7000), subDep = 0, amDep = 70, amFreq = 25, noiseAnchors = list(time = c(-113, 96, 839), value = c(-80, -39, -58)))',

    Scream = 'soundgen(repeatBout = 2, sylLen = 1610, pauseLen = 500, pitchAnchors = list(time = c(0, 0.28, 0.53, 0.88, 1), value = c(388, 385, 669, 663, 392)), temperature = 0.05, nonlinBalance = 35, jitterDep = 1.9, rolloff = -6, formants = list(f1 = c(1800, 1000), f2 = 2400), subFreq = 150, subDep = 80)',

    Snarl = 'soundgen(sylLen = 450, pitchAnchors = list(time = c(0, 0.08, 1), value = c(193, 454, 434)), temperature = 0.1, nonlinBalance = 75, jitterDep = 1.1, attackLen = 0, rolloff = -6, rolloffOct = -2, formants = list(f1 = list(freq = c(1500, 2000), width = 300), f2 = list(freq = 2500, width = 350)), subFreq = 230, subDep = 170, shortestEpoch = 75, noiseAnchors = -8, mouthAnchors = list(time = c(0, 0.3, 0.63, 1), value = c(0, 1, 1, 0.5)), windowLength = 10)',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # ...
    )
  ),

  Misc = list(
    Dog_bark = 'soundgen(repeatBout = 2, sylLen = 140, pauseLen = 100, pitchAnchors = list(time = c(0, 0.29, 1), value = c(559, 716, 647)), temperature = 0.05, nonlinBalance = 100, jitterDep = 2.1, formants = c(1500, 3300, 6000), formantDep = 1, vocalTract = 8.5, subDep = 0, noiseAnchors = list(time = c(0, 78, 160), value = c(-80, 12, -80)), rolloffNoise = -9, mouthAnchors = c(0, 0.5, 0))',

    Duck = 'soundgen(repeatBout = 5, sylLen = 110, pauseLen = 170, pitchAnchors = c(119, 110), temperature = 0.1, rolloff = -3, rolloffOct = -2, formants = c(1600, 2700, 5600, 6400), noiseAnchors = -13, mouthAnchors = c(0.34, 0.57, 0.35))',

    Elephant = 'soundgen(sylLen = 510, pitchAnchors = list(time = c(0, 0.36, 1), value = c(450, 485, 328)), nonlinBalance = 50, jitterDep = 0.3, rolloff = -3, rolloffOct = 0, rolloffKHz = 0, formantDep = 0.5, vocalTract = 37.5, subFreq = 75, subDep = 40, shortestEpoch = 50, noiseAnchors = -19, amplAnchors = c(80, 80, 61))',

    Seagull = 'soundgen(nSyl = 8, sylLen = 200, pauseLen = 140, pitchAnchors = list(time = c(0, 0.71, 1), value = c(977, 1530, 826)), nonlinBalance = 100, jitterDep = 0, rolloff = -6, rolloffParabHarm = 6, rolloffKHz = 0, formants = c(2500, 4500), subFreq = 525, subDep = 220, noiseAnchors = list(time = c(0, 44, 141, 201), value = c(-11, -12, -80, -12)), samplingRate = 24000)',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # ...
    )
  )

  # # empty container for adding new speakers
  # NewSpeaker = list(
  #   Formants = list()
  # )
) # END of presets / dictionaries

# devtools::use_data(presets, overwrite = TRUE)


## extras
# cow1 = soundgen(sylLen = 1400, pitchAnchors = list(time = c(0, 11/14, 1), value = c(75, 130, 200)), temperature = 0.1, rolloff = -6, rolloffOct = -3, rolloffParab = 12, mouthOpenThres = 0.6, formants = NULL, vocalTract = 36.5, mouthAnchors = list(time = c(0, 0.82, 1), value = c(0.6, 0, 1)), noiseAnchors = list(time = c(0, 1400), value = c(-25, -25)), rolloffNoise = -4, addSilence = 0)
# cow2 = soundgen(sylLen = 310, pitchAnchors = list(time = c(0, 1), value = c(359, 359)), temperature = 0.05, nonlinBalance = 100, jitterDep = 1.3, attackLen = 0, rolloff = -6, rolloffOct = -3, rolloffKHz = -0, formants = NULL, vocalTract = 36.5, subFreq = 150, subDep = 70, noiseAnchors = list(time = c(0, 26, 317, 562), value = c(-80, -23, -22, -80)), rolloffNoise = -6, addSilence = 0)
# s = crossFade(cow1, cow2, samplingRate = 16000, crossLen = 150)
# # playme(s, 16000)
# # spectrogram(s, 16000, osc=T)
# # seewave::savewav(s, f = 16000, '~/Downloads/cow_soundgen.wav')
