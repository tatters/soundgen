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
  'rolloff', -12, -60, 0, 1,
  'rolloffOct', -12, -30, 10, 1,
  'rolloffParab', 0, -50, 50, 5,
  'rolloffParabHarm', 3, 1, 20, 1,
  'rolloffKHz', -6, -20, 0, 1,
  'rolloffLip', 6, 0, 20, 1,
  'formantDep', 1, 0, 5, .1,
  'formantDepStoch', 30, 0, 60, 10,
  'vocalTract', 15.5, 2, 100, .5,
  'subFreq', 100, 10, 1000, 10,
  'subDep', 100, 0, 500, 10,
  'shortestEpoch', 300, 50, 500, 25,
  'amDep', 0, 0, 100, 5,
  'amFreq', 30, 10, 100, 5,
  'amShape', 0, -1, 1, .025,
  'samplingRate', 16000, 8000, 44100, 100,
  'windowLength', 40, 5, 100, 2.5, # default, low, high, step
  'rolloffNoise', -14, -20, 20, 1,

  # other soundgen settings, which are NOT updateable sliders in soundgen_app()
  'overlap', 50, 0, 99, 1,
  'addSilence', 100, 0, 1000, 50,
  'pitchFloor', 50, 1, 1000, 1,
  'pitchCeiling', 3500, 10, 100000, 10,
  'pitchSamplingRate', 3500, 10, 100000, 10,
  'throwaway', -120, -200, -10, 10,

  # soundgen_app() settings, which are not needed for soundgen()
  'specWindowLength', 40, 5, 100, 2.5,
  'specContrast', .2, -1, 1, .05,
  'specBrightness', 0, -1, 1, .05,
  'mouthOpening', .5, 0, 1, .05,
  'pitch', 100, 25, 3500, 1,  # set pitch range per species
  'pitchDeltas', 0, -24, 24, 1,  # amplPitchGlobal range
  'time', 0, 0, 5000, 1,
  'noiseAmpl', 0, -120, 40, 1  # for plotting - noise ylim
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
  rolloff = -12,
  rolloffOct = -12,
  rolloffParab = 0,
  rolloffParabHarm = 3,
  rolloffKHz = -6,
  rolloffLip = 6,
  formantDep = 1,
  formantDepStoch = 30,
  vocalTract = 15.5,
  subFreq = 100,
  subDep = 100,
  shortestEpoch = 300,
  amDep = 0,
  amFreq = 30,
  amShape = 0,
  rolloffNoise = -14,
  samplingRate = 16000,
  windowLength = 40,
  windowLength_points = 512,
  overlap = 75,
  addSilence = 100,
  pitchFloor = 25,
  pitchCeiling = 3500,
  pitchSamplingRate = 3500,
  throwaway = -120,
  pitchAnchors = list(
    time = c(0, .1, .9, 1),
    value = c(100, 150, 135, 100)
  ),
  pitchAnchorsGlobal = list(time = c(0, 1), value = c(0, 0)),
  noiseAnchors = list(time = c(0, 300), value = c(-120, -120)),
  mouthAnchors = list(time = c(0, 1), value = c(.5, .5)),
  amplAnchors = list(time = c(0, 1), value = c(120, 120)),
  amplAnchorsGlobal = list(time = c(0, 1), value = c(120, 120)),
  formants = list(f1 = list(time = 0, freq = 860,
                            amp = 30, width = 120),
                  f2 = list(time = 0, freq = 1280,
                            amp = 40, width = 120),
                  f3 = list(time = 0, freq = 2900,
                            amp = 25, width = 200)),
  formantsNoise = NA,
  vowelString = NA,
  samplingRate = 16000,
  windowLength = 50
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

    Gasp = 'soundgen(sylLen = 250, pitchAnchors = list(time = c(0, 0.38, 1), value = c(147, 163, 150)), temperature = 0.05, nonlinBalance = 45, jitterDep = 1, shimmerDep = 15, rolloff = -25, rolloffOct = -9, rolloffParab = 40, rolloffParabHarm = 5, rolloffKHz = -15, rolloffLip = 0, formants = list(f1 = list(time = 0, freq = 900, amp = 40, width = 100), f2 = list(time = 0, freq = 1400, amp = 30, width = 150), f3 = list(time = 0, freq = 2200, amp = 40, width = 150)), formantDepStoch = 20, vocalTract = 18, subFreq = 80, subDep = 35, shortestEpoch = 50, amDep = 20, amFreq = 10, noiseAnchors = list(time = c(-36, 8, 242, 333), value = c(-86, -24, -34, -118)), formantsNoise = list(f1 = list(time = 0, freq = 900, amp = 60, width = 100), f2 = list(time = 0, freq = 1400, amp = 60, width = 150), f3 = list(time = 0, freq = 2200, amp = 60, width = 150), f4 = list(time = c(0, 1), freq = c(3100, 3800), amp = 60, width = 200)), rolloffNoise = -5, amplAnchors = list(time = c(0, 1), value = c(120, 72)))',

    Roar = 'soundgen(sylLen = 960, pitchAnchors = list(time = c(0, 0.14, 0.88, 1), value = c(174, 200, 191, 131)), temperature = 0.05, nonlinBalance = 70, jitterDep = 0.7, rolloff = -20, rolloffOct = -3, rolloffParab = -10, rolloffParabHarm = 13, formants = list(f1 = list(time = 0, freq = 620, amp = 40, width = 80), f2 = list(time = 0, freq = 1000, amp = 50, width = 80), f3 = list(time = 0, freq = 1800, amp = 40, width = 200), f4 = list(time = 0, freq = 2560, amp = 50, width = 300), f5 = list(time = 0, freq = 3400, amp = 35, width = 300)), subDep = 40, noiseAnchors = list(time = c(0, 960), value = c(-120, -120)), formantsNoise = list(f1 = list(time = 0, freq = 620, amp = 40, width = 80), f2 = list(time = 0, freq = 1000, amp = 50, width = 80), f3 = list(time = 0, freq = 1800, amp = 40, width = 200), f4 = list(time = 0, freq = 2560, amp = 50, width = 300), f5 = list(time = 0, freq = 3400, amp = 35, width = 300)), mouthAnchors = list(time = c(0, 0.14, 0.88, 1), value = c(0.4, 0.59, 0.61, 0.45)))',

    Moan = 'soundgen(sylLen = 800, pitchAnchors = list(time = c(0, 1), value = c(202, 144)), attackLen = 100, rolloff = -23, rolloffOct = -6, formants = list(f1 = list(time = 0, freq = 980, amp = 35, width = 120), f2 = list(time = 0, freq = 1400, amp = 35, width = 150), f3 = list(time = 0, freq = 2680, amp = 30, width = 200), f4 = list(time = 0, freq = 3400, amp = 30, width = 200), f5 = list(time = 0, freq = 4150, amp = 30, width = 400)), noiseAnchors = list(time = c(-7, 268, 590, 869), value = c(-67, -45, -24, -46)), formantsNoise = list(f1 = list(time = 0, freq = 980, amp = 35, width = 120), f2 = list(time = 0, freq = 1400, amp = 35, width = 150), f3 = list(time = 0, freq = 2680, amp = 30, width = 200), f4 = list(time = 0, freq = 3400, amp = 30, width = 200), f5 = list(time = 0, freq = 4150, amp = 30, width = 400)), rolloffNoise = -10, mouthAnchors = list(time = c(0, 0.3, 1), value = c(0.24, 0.41, 0.19)))',

    Sigh = 'soundgen(sylLen = 50, pitchAnchors = list(time = c(0, 1), value = c(202, 144)), temperature = 0.1, formants = list(f1 = list(time = 0, freq = 800, amp = 30, width = 100), f1.5 = list(time = 0, freq = 1200, amp = 30, width = 100), f2 = list(time = 0, freq = 1500, amp = 30, width = 100), f3 = list(time = 0, freq = 2600, amp = 30, width = 100), f4 = list(time = 0, freq = 4000, amp = 30, width = 100)), noiseAnchors = list(time = c(-20, 104, 756, 1252), value = c(26, 40, 23, -22)))',

    Laugh = 'soundgen(nSyl = 4, sylLen = 120, pauseLen = 120, pitchAnchors = list(time = c(0, 1), value = c(180, 166)), pitchAnchorsGlobal = list(time = c(0, 0.3, 1), value = c(1, 2, 0)), temperature = 0.1, nonlinBalance = 50, jitterDep = 0.8, attackLen = 10, formants = list(f1 = list(time = c(0, 0.5, 1), freq = c(860, 530, 530), amp = c(30, 30, 30), width = c(120, 50, 50)), f1.4 = list(time = c(0, 0.5, 1), freq = c(1100, 1100, 1100), amp = c(0, -20, -20), width = c(100, 100, 100)), f1.6 = list(time = c(0, 0.5, 1), freq = c(1400, 1400, 1400), amp = c(0, 20, 20), width = c(100, 100, 100)), f2 = list(time = c(0, 0.5, 1), freq = c(1280, 2400, 2400), amp = c(40, 40, 40), width = c(120, 300, 300)), f3 = list(time = c(0, 0.5, 1), freq = c(2900, 4000, 4000), amp = c(25, 30, 30), width = c(200, 300, 300))), subDep = 0, shortestEpoch = 50, noiseAnchors = list(time = c(39, 56, 167), value = c(-120, -44, -120)), formantsNoise = list(f1 = list(time = c(0, 1), freq = c(860, 530), amp = c(30, 30), width = c(120, 50)), f1.4 = list(time = c(0, 1), freq = c(1100, 1100), amp = c(0, -20), width = c(100, 100)), f1.6 = list(time = c(0, 1), freq = c(1400, 1400), amp = c(0, 20), width = c(100, 100)), f2 = list(time = c(0, 1), freq = c(1280, 2400), amp = c(40, 40), width = c(120, 300)), f3 = list(time = c(0, 1), freq = c(2900, 4000), amp = c(25, 30), width = c(200, 300)), f4 = list(time = c(0, 1), freq = c(3800, 3800), amp = c(20, 0), width = c(100, 100))), rolloffNoise = -5, amplAnchors = list(time = c(0, 0.32, 1), value = c(120, 67, 120)), amplAnchorsGlobal = list(time = c(0, 0.55, 1), value = c(120, 110, 56)))',

    Snore = 'soundgen(sylLen = 960, pitchAnchors = list(time = c(0, 0.15, 0.87, 1), value = c(175, 199, 188, 140)), temperature = 0.05, nonlinBalance = 67, jitterDep = 0.75, formants = list(f1 = list(time = 0, freq = 560, amp = 30, width = 120), f2 = list(time = 0, freq = 1000, amp = 40, width = 120), f3 = list(time = 0, freq = 1450, amp = 25, width = 200), f4 = list(time = 0, freq = 3800, amp = 20, width = 100)), subDep = 80, shortestEpoch = 200, noiseAnchors = list(time = c(0, 960), value = c(-40, -40)), formantsNoise = list(f1 = list(time = 0, freq = 560, amp = 30, width = 120), f2 = list(time = 0, freq = 1000, amp = 40, width = 120), f3 = list(time = 0, freq = 1450, amp = 25, width = 200), f4 = list(time = 0, freq = 3800, amp = 20, width = 100)), rolloffNoise = -12, mouthAnchors = list(time = c(0, 1), value = c(0, 0)), amplAnchorsGlobal = list(time = c(0, 1), value = c(120, 43)))',

    # 'Formants' is a reserved name. The list of presets for every caller should end with
    # a list of 'Formants' presets for each vowel and consonant, otherwise you won't be
    # able to specify formants in a string like 'aui' for this speaker
    Formants = list(
      vowels = list(
        'a' = list(
          f1=list(time=0, freq=860, amp=30, width=120), # amplitude in dB, freq and width in Hz
          f2=list(time=0, freq=1280, amp=40, width=120),
          f3=list(time=0, freq=2900, amp=25, width=200) # any number of formants may be specified
        ),
        'o' = list(
          f1=list(time=0, freq=630, amp=35, width=100),
          f2=list(time=0, freq=900, amp=35, width=100),
          f3=list(time=0, freq=3000, amp=30, width=200),
          f4=list(time=0, freq=3960, amp=30, width=200)
        ),
        'i' = list(
          f1=list(time=0, freq=300, amp=25, width=80),
          f2=list(time=0, freq=2700, amp=30, width=100),
          f3=list(time=0, freq=3400, amp=40, width=350),
          f4=list(time=0, freq=4200, amp=40, width=350)
        ),
        'e' = list(
          f1=list(time=0, freq=530, amp=30, width=50),
          'f1.4'=list(time=0, freq=1100, amp=-20, width=100), # insert a zero-pole pair between F1 and F2
          'f1.6'=list(time=0, freq=1400, amp=20, width=100),  # insert a zero-pole pair between F1 and F2
          f2=list(time=0, freq=2400, amp=40, width=300),
          f3=list(time=0, freq=4000, amp=30, width=300)
        ),
        'u' = list(
          f1=list(time=0, freq=375, amp=25, width=80),
          f2=list(time=0, freq=550, amp=35, width=120),
          f3=list(time=0, freq=2100, amp=25, width=300),
          f4=list(time=0, freq=4200, amp=45, width=250)
        ),
        '0' = list(  # schwa
          f1=list(time=0, freq=640, amp=30, width=100),
          f2=list(time=0, freq=1670, amp=30, width=100),
          f3=list(time=0, freq=2700, amp=30, width=100),
          f4=list(time=0, freq=3880, amp=30, width=100)
        )
      ),
      consonants = list(
        'h' = list(
          label = 'h',
          rolloffNoise = -13,
          f1=list(time=0, freq=420, amp=20, width=150),
          f2=list(time=0, freq=1200, amp=50, width=250),
          f3=list(time=0, freq=5000, amp=10, width=200),
          f4=list(time=0, freq=8500, amp=10, width=300)
        ),
        's' = list(
          label = 's',
          rolloffNoise = 0,
          f1=list(time=0, freq=5500, amp=20, width=200),
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
          f3=list(time=0, freq=2900, amp=20, width=1000)
        ),
        'n' = list(
          label = 'snuffle',  # sNuffle (breathing through the nose)
          rolloffNoise = 0,
          f1=list(time=0, freq=5400, amp=25, width=2000)
        )
      )
    )
  ),

  F1 = list(
    Vowel2 = 'soundgen(sylLen = 500, pitchAnchors = list(time = c(0, 0.6, 1), value = c(340, 370, 340)), formants = list(f1 = list(time = 0, freq = 900, amp = 30, width = 80), f2 = list(time = 0, freq = 1300, amp = 30, width = 160), f3 = list(time = 0, freq = 3300, amp = 25, width = 130), f4 = list(time = 0, freq = 4340, amp = 20, width = 370)))',

    Scream = 'soundgen(sylLen = 1110, pitchAnchors = list(time = c(0, 0.1, 0.85, 1), value = c(900, 1832, 1618, 1200)), temperature = 0.1, nonlinBalance = 70, jitterDep = 1, shimmerDep = 10, formants = list(f1 = list(time = 0, freq = 900, amp = 30, width = 80), f2 = list(time = 0, freq = 1300, amp = 30, width = 160), f3 = list(time = 0, freq = 3300, amp = 25, width = 130), f4 = list(time = 0, freq = 4340, amp = 20, width = 370)), subFreq = 400, noiseAnchors = list(time = c(0, 1110), value = c(-120, -120)))',

    Growl = 'soundgen(sylLen = 1100, pitchAnchors = list(time = c(0, 0.1, 0.3, 1), value = c(238, 251, 449, 205)), temperature = 0.1, rolloff = -15, rolloffOct = -6, rolloffParab = -20, rolloffParabHarm = 20, formants = list(f1 = list(time = 0, freq = 800, amp = 20, width = 120), f2 = list(time = 0, freq = 2150, amp = 30, width = 300), f3 = list(time = 0, freq = 4250, amp = 30, width = 300), f4 = list(time = 0, freq = 6600, amp = 30, width = 300)), amDep = 40, amFreq = 35, amShape = -0.4, noiseAnchors = list(time = c(0, 1100), value = c(-120, -120)), formantsNoise = list(f1 = list(time = 0, freq = 800, amp = 20, width = 120), f2 = list(time = 0, freq = 2150, amp = 30, width = 300), f3 = list(time = 0, freq = 4250, amp = 30, width = 300), f4 = list(time = 0, freq = 6600, amp = 30, width = 300)), mouthAnchors = list(time = c(0, 0.13, 0.76, 1), value = c(0.4, 0.59, 0.61, 0.45)), amplAnchorsGlobal = list(time = c(0, 0.13, 1), value = c(101, 120, 26)))',

    Moan = 'soundgen(sylLen = 360, pitchAnchors = list(time = c(0, 1), value = c(380, 260)), attackLen = 100, rolloff = -20, rolloffOct = -6, formants = list(f1 = list(time = c(0, 0.5, 1), freq = c(900, 900, 790), amp = c(30, 30, 30), width = c(80, 80, 100)), f2 = list(time = c(0, 0.5, 1), freq = c(1300, 1300, 1600), amp = c(30, 30, 30), width = c(160, 160, 100)), f3 = list(time = c(0, 0.5, 1), freq = c(3300, 3300, 3100), amp = c(25, 25, 30), width = c(130, 130, 100)), f4 = list(time = c(0, 0.5, 1), freq = c(4340, 4340, 3900), amp = c(20, 20, 30), width = c(370, 370, 100))), noiseAnchors = list(time = c(0, 417, 508), value = c(-63, -29, -120)), formantsNoise = list(f1 = list(time = c(0, 0.5, 1), freq = c(900, 900, 790), amp = c(30, 30, 30), width = c(80, 80, 100)), f2 = list(time = c(0, 0.5, 1), freq = c(1300, 1300, 1600), amp = c(30, 30, 30), width = c(160, 160, 100)), f3 = list(time = c(0, 0.5, 1), freq = c(3300, 3300, 3100), amp = c(25, 25, 30), width = c(130, 4340, 3900), amp = c(20, 20, 30), width = c(370, 370, 100))), mouthAnchors = list(time = c(0, 0.14, 1), value = c(0.24, 0.41, 0.19)), amplAnchorsGlobal = list(time = c(0, 1), value = c(120, 18)))',

    Laugh = 'soundgen(nSyl = 3, sylLen = 60, pauseLen = 90, pitchAnchors = list(time = c(0, 1), value = c(368, 284)), temperature = 0.075, attackLen = 10, formants = list(f1 = list(time = 0, freq = 790, amp = 30, width = 100), f2 = list(time = 0, freq = 1600, amp = 30, width = 100), f3 = list(time = 0, freq = 3100, amp = 30, width = 100), f4 = list(time = 0, freq = 3900, amp = 30, width = 100)), noiseAnchors = list(time = c(0, 67, 86, 186), value = c(-45, -47, -89, -120)), formantsNoise = list(f1 = list(time = 0, freq = 790, amp = 30, width = 100), f2 = list(time = 0, freq = 1600, amp = 30, width = 100), f3 = list(time = 0, freq = 3100, amp = 30, width = 100), f4 = list(time = 0, freq = 3900, amp = 30, width = 100)), rolloffNoise = -8, amplAnchorsGlobal = list(time = c(0, 1), value = c(120, 20)))',

    Cry = 'soundgen(sylLen = 1600, pitchAnchors = list(time = c(0, 1), value = c(610, 511)), temperature = 0.2, nonlinBalance = 15, formants = list(f1 = list(time = 0, freq = 790, amp = 30, width = 100), f2 = list(time = 0, freq = 1600, amp = 30, width = 100), f3 = list(time = 0, freq = 3100, amp = 30, width = 100), f4 = list(time = 0, freq = 3900, amp = 30, width = 100)), subFreq = 125, subDep = 70, noiseAnchors = list(time = c(0, 1600), value = c(-120, -120)), rolloffNoise = 0, mouthAnchors = list(time = c(0, 1), value = c(0, 0)), amplAnchorsGlobal = list(time = c(0, 1), value = c(120, 60)))',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      vowels = list(
        'a' = list(
          f1=list(time=0, freq=900, amp=30, width=80), # amplitude in dB, freq and width in Hz
          f2=list(time=0, freq=1300, amp=30, width=160),
          f3=list(time=0, freq=3300, amp=25, width=130),
          f4=list(time=0, freq=4340, amp=20, width=370) # any number of formants may be specified
        ),
        'o' = list(
          f1=list(time=0, freq=800, amp=30, width=80),
          f2=list(time=0, freq=1100, amp=30, width=80),
          f3=list(time=0, freq=3560, amp=40, width=200),
          f4=list(time=0, freq=5830, amp=50, width=200)
        ),
        'i' = list(
          f1=list(time=0, freq=330, amp=30, width=120),
          f2=list(time=0, freq=2700, amp=40, width=120),
          f3=list(time=0, freq=3580, amp=30, width=200),
          f4=list(time=0, freq=4710, amp=30, width=200),
          'f5'=list(time=0, freq=5800, amp=30, width=200)
        ),
        'e' = list(
          f1=list(time=0, freq=930, amp=30, width=100),
          f2=list(time=0, freq=2470, amp=30, width=100),
          f3=list(time=0, freq=3300, amp=25, width=120),
          f4=list(time=0, freq=4200, amp=30, width=200)
        ),
        'u' = list(
          f1=list(time=0, freq=450, amp=30, width=80),
          f2=list(time=0, freq=850, amp=40, width=120),
          f3=list(time=0, freq=2900, amp=30, width=200),
          f4=list(time=0, freq=4100, amp=30, width=275)
        ),
        '0' = list(  # schwa
          f1=list(time=0, freq=790, amp=30, width=100),
          f2=list(time=0, freq=1600, amp=30, width=100),
          f3=list(time=0, freq=3100, amp=30, width=100),
          f4=list(time=0, freq=3900, amp=30, width=100)
        )
      ),
      consonants = list(
        'h' = list(
          label = 'h',
          rolloffNoise = -13,
          f1=list(time=0, freq=420, amp=20, width=150),
          f2=list(time=0, freq=1200, amp=50, width=250),
          f3=list(time=0, freq=5000, amp=10, width=200),
          f4=list(time=0, freq=8500, amp=10, width=300)
        ),
        's' = list(
          label = 's',
          rolloffNoise = 0,
          f1=list(time=0, freq=5500, amp=20, width=200), # NB: amplitude in dB for consonants
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
          f3=list(time=0, freq=2900, amp=20, width=1000)
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
    Bark_alarm = 'soundgen(sylLen = 160, pitchAnchors = list(time = c(0, 1), value = c(232, 185)), nonlinBalance = 100, jitterDep = 4.9, attackLen = 61, rolloff = -30, rolloffOct = 0, formants = list(f1 = list(time = 0, freq = 415, amp = 60, width = 120), f2 = list(time = 0, freq = 1000, amp = 60, width = 120), f3 = list(time = 0, freq = 3000, amp = 20, width = 200), f4 = list(time = 0, freq = 5000, amp = 20, width = 1000)), subFreq = 125, subDep = 90, noiseAnchors = list(time = c(0, 76, 158, 344), value = c(-120, 16, -21, -120)), formantsNoise = list(f1 = list(time = 0, freq = 415, amp = 60, width = 120), f2 = list(time = 0, freq = 1000, amp = 60, width = 120), f3 = list(time = 0, freq = 3000, amp = 20, width = 200), f4 = list(time = 0, freq = 5000, amp = 20, width = 1000)), rolloffNoise = -14)',

    Scream_conflict = 'soundgen(sylLen = 740, pitchAnchors = list(time = c(0, 0.3, 0.9, 1), value = c(1200, 1547, 1487, 1154)), temperature = 0.05, nonlinBalance = 100, jitterDep = 0.3, rolloff = -6, rolloffOct = 0, formants = list(f1 = list(time = 0, freq = 800, amp = 30, width = 80), f2 = list(time = 0, freq = 1600, amp = 30, width = 160), f3 = list(time = 0, freq = 3000, amp = 25, width = 130), f4 = list(time = 0, freq = 4340, amp = 20, width = 370)), subFreq = 75, subDep = 130, noiseAnchors = list(time = c(0, 740), value = c(-120, -120)), formantsNoise = list(f1 = list(time = 0, freq = 800, amp = 30, width = 80), f2 = list(time = 0, freq = 1600, amp = 30, width = 160), f3 = list(time = 0, freq = 3000, amp = 25, width = 130), f4 = list(time = 0, freq = 4340, amp = 20, width = 370)), rolloffNoise = -12)',

    Grunt_excited = 'soundgen(nSyl = 6, sylLen = 100, pauseLen = 220, pitchAnchors = list(time = c(0, 1), value = c(127, 102)), temperature = 0.05, rolloffOct = -6, formants = list(f1 = list(time = c(0, 1), freq = c(415, 300), amp = c(60, 60), width = c(120, 120)), f2 = list(time = c(0, 1), freq = c(1000, 500), amp = c(60, 60), width = c(120, 120)), f3 = list(time = c(0, 1), freq = c(2200, 2500), amp = c(20, 20), width = c(200, 200)), f4 = list(time = 0, freq = 3600, amp = 20, width = 200), f5 = list(time = c(0, 1), freq = c(4600, 4200), amp = c(20, 20), width = c(200, 200))), noiseAnchors = list(time = c(0, 83, 205), value = c(-120, -6, -120)), rolloffNoise = -12, amplAnchorsGlobal = list(time = c(0, 0.35, 1), value = c(89, 120, 31)))',  # TODO: CHECK BREATHING!!!

    Hoot_excited = 'soundgen(sylLen = 730, pitchAnchors = list(time = c(0, 0.52, 1), value = c(440, 405, 440)), rolloff = -20, rolloffOct = 0, formants = list(f1 = list(time = c(0, 0.4, 1), freq = c(300, 500, 400), amp = c(30, 30, 30), width = c(80, 80, 80)), f2 = list(time = c(0, 0.2, 1), freq = c(500, 1000, 700), amp = c(30, 30, 30), width = c(120, 120, 120)), f3 = list(time = 0, freq = 2500, amp = 30, width = 120), f4 = list(time = 0, freq = 4000, amp = 30, width = 200), f5 = list(time = 0, freq = 5580, amp = 30, width = 200)), noiseAnchors = list(time = c(0, 730), value = c(-8, -8)), rolloffNoise = -13)',

    Laugh_playing = 'soundgen(nSyl = 5, sylLen = 100, pauseLen = 150, pitchAnchors = list(time = c(0, 0.5, 1), value = c(134, 144, 117)), rolloff = -22, rolloffOct = 0, rolloffParab = -20, rolloffParabHarm = 8, formants = list(f1 = list(time = 0, freq = 300, amp = 30, width = 80), f2 = list(time = 0, freq = 950, amp = 30, width = 120), f3 = list(time = 0, freq = 1600, amp = 40, width = 120), f4 = list(time = 0, freq = 2240, amp = 25, width = 200), f5 = list(time = 0, freq = 2900, amp = 20, width = 100)), noiseAnchors = list(time = c(20, 60, 100), value = c(-120, -19, -120)), formantsNoise = list(f1 = list(time = 0, freq = 300, amp = 30, width = 80), f2 = list(time = 0, freq = 950, amp = 30, width = 120), f3 = list(time = 0, freq = 1600, amp = 40, width = 120), f4 = list(time = 0, freq = 2240, amp = 25, width = 200), f5 = list(time = 0, freq = 2900, amp = 20, width = 100)), rolloffNoise = -16)',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # ...
    )
  ),

  Cat = list(
    Chirp = 'soundgen(sylLen = 120, pitchAnchors = list(time = c(0, 0.5, 1), value = c(828, 768, 423)), nonlinBalance = 100, jitterDep = 0.3, shimmerDep = 11, attackLen = 1, rolloff = -3, rolloffOct = -4, formants = list(f1 = list(time = 0, freq = 875, amp = 30, width = 282), f2 = list(time = 0, freq = 1944, amp = 30, width = 372), f3 = list(time = 0, freq = 2409, amp = 30, width = 448)), vocalTract = 10, subDep = 0, noiseAnchors = list(time = c(-1, 120, 161), value = c(-60, 14, -47)), amplAnchors = list(time = c(0, 0.33, 1), value = c(120, 120, 87)), windowLength = 10)',

    Growl = 'soundgen(sylLen = 2700, pitchAnchors = list(time = c(0, 0.8, 1), value = c(65, 65, 48)), temperature = 0.05, nonlinBalance = 100, jitterDep = 1.7, jitterLen = 17, shimmerDep = 14, rolloff = -21, rolloffOct = 0, rolloffParab = 20, rolloffParabHarm = 20, formants = list(f1 = list(time = 0, freq = 400, amp = 40, width = 200), f2 = list(time = 0, freq = 3000, amp = 10, width = 200)), vocalTract = 10, subDep = 0, noiseAnchors = list(time = c(8, 2676), value = c(-120, -120)))',

    Hiss = 'soundgen(pitchAnchors = NULL, formants = list(f1 = list(time = 0, freq = 1112, amp = 30, width = 277), f2 = list(time = 0, freq = 2610, amp = 30, width = 310), f3 = list(time = 0, freq = 4587, amp = 30, width = 529)), formantDepStoch = 20, vocalTract = 10, noiseAnchors = list(time = c(-185, 74, 503), value = c(-5, 40, -24)), rolloffNoise = -12, mouthAnchors = list(time = c(0, 0.13, 1), value = c(0, 0.5, 0.5)))',

    Howl = 'soundgen(sylLen = 3150, pitchAnchors = list(time = c(0, 0.05, 0.18, 0.45, 0.91, 1), value = c(221, 322, 346, 304, 273, 253)), temperature = 0.075, rolloffOct = -13, formants = list(f1 = list(time = 0, freq = 895, amp = 30, width = 150), f2 = list(time = c(0, 1), freq = c(1500, 2000), amp = 40, width = 150), f3 = list(time = 0, freq = 3287, amp = 30, width = 165)), vocalTract = 10, mouthAnchors = list(time = c(0, 0.1, 0.19, 0.25, 0.41, 0.91,  1), value = c(0, 0.37, 0.29, 0.66, 0.37, 0.34, 0.27)))',

    Heat = 'soundgen(sylLen = 800, pitchAnchors = list(time = c(0, 0.5, 0.93, 1), value = c(480, 552, 549, 517)), temperature = 0.1, rolloffOct = -10, rolloffParab = -20, rolloffParabHarm = 1, formants = list(f1 = list(time = c(0, 1), freq = c(1500, 500), amp = 30, width = 150), f2 = list(time = 0, freq = 2100, amp = 30, width = 150), f3 = list(time = c(0, 1), freq = 5500, amp = c(10, 30), width = 200)), vocalTract = 10, amDep = 20, amFreq = 40, noiseAnchors = list(time = c(0, 800), value = c(-120, -120)), mouthAnchors = list(
  time = c(0, 0.12, 0.86, 1), value = c(0, 0.52, 0.57, 0)))',

    Meow = 'soundgen(sylLen = 920, pitchAnchors = list(time = c(0, 0.2, 1), value = c(480, 550, 515)), attackLen = 150, rolloff = -6, formants = list(f1 = list(time = 0, freq = 1594, amp = 30, width = 200), f3 = list(time = 0, freq = 3600, amp = 30, width = 200)), vocalTract = 10, noiseAnchors = list(time = c(0, 920), value = c(-120, -120)), mouthAnchors = list(time = c(0, 0.16, 0.91, 1), value = c(0, 1, 0.31, 0.06)))',

    Purr = 'soundgen(repeatBout = 2, nSyl = 2, sylLen = 800, pauseLen = 50, pitchAnchors = list(time = c(0, 1), value = c(25, 25)), temperature = 0.1, nonlinBalance = 100, jitterDep = 1.2, jitterLen = 34, rolloff = -18, rolloffOct = -30, rolloffParab = 25, rolloffParabHarm = 9, rolloffLip = 0, formants = list(f1 = list(time = 0, freq = 1200, amp = 30, width = 150)), vocalTract = 10, subDep = 0, amDep = 70, amFreq = 25, noiseAnchors = list(time = c(-191, -115, 839), value = c(-92, -26, -58)), rolloffNoise = -13)',

    Scream = 'soundgen(repeatBout = 2, sylLen = 1610, pauseLen = 500, pitchAnchors = list(time = c(0, 0.28, 0.53, 0.88, 1), value = c(388, 385, 669, 663, 392)), temperature = 0.05, nonlinBalance = 35, jitterDep = 1.9, rolloff = -8, formants = list(f1 = list(time = c(0, 1), freq = c(1800, 1000), amp = 40, width = 150), f2 = list(time = 0, freq = 2400, amp = 40, width = 150)), vocalTract = 10, subFreq = 150, subDep = 80, noiseAnchors = list(time = c(0, 603), value = c(-120, -120)))',

    Snarl = 'soundgen(sylLen = 450, pitchAnchors = list(time = c(0, 0.08, 1), value = c(193, 454, 434)), nonlinBalance = 72, jitterDep = 2.9, attackLen = 0, rolloff = 0, formants = list(f1 = list(time = 0, freq = 1500, amp = 40, width = 250), f2 = list(time = 0, freq = 2500, amp = 40, width = 300)), vocalTract = 10, subFreq = 150, subDep = 110, shortestEpoch = 75, noiseAnchors = list(time = c(0, 83), value = c(-120, -120)), mouthAnchors = list(time = c(0, 0.3, 0.63, 1), value = c(0, 1, 1, 0.5)), windowLength = 10)',

    Formants = list( # reserved name - the list of presets for every caller must end with a list of 'Formants' presets for each vowel and consonant
      # ...
    )
  ),

  Misc = list(
    Cat_miaw = 'soundgen(sylLen = 1300, pitchAnchors = list(time = c(0, 0.25, 1), value = c(264, 315, 274)), temperature = 0.05, rolloff = -6, rolloffOct = -6, rolloffParab = 20, rolloffParabHarm = 20, formants = list(f1 = list(time = c(0, 0.15, 1), freq = c(1540, 1900, 1020), amp = c(20, 20, 20), width = c(200, 200, 200)), f2 = list(time = c(0, 0.15, 1), freq = c(1800, 2900, 2800), amp = c(20, 20, 20), width = c(200, 200, 200)), f2.5 = list(time = c(0, 0.15, 1), freq = c(2500, 3700, 3400), amp = c(-20, -20, -20), width = c(400, 400, 400)), f3 = list(time = c(0, 0.15, 1), freq = c(3200, 5300, 5200), amp = c(20, 20, 20), width = c(400, 400, 400)), f4 = list(time = c(0, 0.15, 1), freq = c(6000, 6200, 6000), amp = c(20, 20, 20), width = c(400, 400, 400))), vocalTract = 8, noiseAnchors = list(time = c(-44, 1108, 1338), value = c(-55, -44, -76)), formantsNoise = list(f1 = list(time = c(0, 0.15, 1), freq = c(1540, 1900, 1020), amp = c(20, 20, 20), width = c(200, 200, 200)), f2 = list(time = c(0, 0.15, 1), freq = c(1800, 2900, 2800), amp = c(20, 20, 20), width = c(200, 200, 200)), f2.5 = list(time = c(0, 0.15, 1), freq = c(2500, 3700, 3400), amp = c(-20, -20, -20), width = c(400, 400, 400)), f3 = list(time = c(0, 0.15, 1), freq = c(3200, 5300, 5200), amp = c(20, 20, 20), width = c(400, 400, 400)), f4 = list(time = c(0, 0.15, 1), freq = c(6000, 6200, 6000), amp = c(20, 20, 20), width = c(400, 400, 400))), rolloffNoise = -17, mouthAnchors = list(time = c(0, 0.14, 0.84, 1), value = c(0, 0.51, 0.38, 0)), amplAnchors = list(time = c(0, 0.3, 1), value = c(35, 120, 36)))',

    Cow = 'soundgen(sylLen = 1610, pitchAnchors = list(time = c(0, 0.61, 0.85, 1), value = c(71, 104, 200, 197)), temperature = 0.05, nonlinBalance = 66, jitterDep = 2, rolloff = -24, rolloffOct = 0, rolloffKHz = -10, formants = list(f1 = list(time = c(0, 1), freq = c(300, 300), amp = c(30, 30), width = c(120, 120)), f2 = list(time = 0, freq = 800, amp = 40, width = 120), f3 = list(time = 0, freq = 1100, amp = 40, width = 150)), formantDep = 1.6, vocalTract = 34, subDep = 50, shortestEpoch = 125, noiseAnchors = list(time = c(-55, 1404, 1608, 1846), value = c(-120, -120, 3, -120)), formantsNoise = list(f1 = list(time = c(0, 1), freq = c(300, 300), amp = c(30, 30), width = c(120, 120)), f2 = list(time = 0, freq = 800, amp = 40, width = 120), f3 = list(time = 0, freq = 1100, amp = 40, width = 150)), rolloffNoise = -17, mouthAnchors = list(time = c(0, 0.6, 0.83, 1), value = c(0, 0, 0.89, 0.81)))',

    Dog_bark = 'soundgen(repeatBout = 2, sylLen = 140, pauseLen = 100, pitchAnchors = list(time = c(0, 0.52, 1), value = c(559, 785, 557)), temperature = 0.05, nonlinBalance = 100, jitterDep = 1, rolloffOct = -6, formants = list(f1 = list(time = c(0, 1), freq = c(1700, 1700), amp = c(40, 40), width = c(400, 400)), f2 = list(time = 0, freq = 3300, amp = 50, width = 400), f3 = list(time = 0, freq = 7300, amp = 40, width = 1000)), vocalTract = 8, subDep = 60, noiseAnchors = list(time = c(0, 80, 160), value = c(-120, 22, -120)), formantsNoise = list(f1 = list(time = c(0, 1), freq = c(1700, 1700), amp = c(40, 40), width = c(400, 400)), f2 = list(time = 0, freq = 3300, amp = 50, width = 400), f3 = list(time = 0, freq = 7300, amp = 40, width = 1000)), rolloffNoise = -13, mouthAnchors = list(time = c(0, 0.5, 1), value = c(0, 0.5, 0)))',

    Duck = 'soundgen(repeatBout = 5, sylLen = 110, pauseLen = 170, pitchAnchors = list(time = c(0, 1), value = c(119, 110)), temperature = 0.1, rolloffOct = 0, formants = list(f1 = list(time = 0, freq = 1600, amp = 40, width = 250), f2 = list(time = 0, freq = 2100, amp = 40, width = 250), f3 = list(time = 0, freq = 2870, amp = 40, width = 300), f4 = list(time = 0, freq = 5600, amp = 40, width = 300), f5 = list(time = 0, freq = 6400, amp = 40, width = 300), f6 = list(time = 0, freq = 7200, amp = 40, width = 300)), noiseAnchors = list(time = c(0, 110), value = c(0, 0)), formantsNoise = list(f1 = list(time = 0, freq = 1600, amp = 40, width = 250), f2 = list(time = 0, freq = 2100, amp = 40, width = 250), f3 = list(time = 0, freq = 2870, amp = 40, width = 300), f4 = list(time = 0, freq = 5600, amp = 40, width = 300), f5 = list(time = 0, freq = 6400, amp = 40, width = 300), f6 = list(time = 0, freq = 7200, amp = 40, width = 300)), mouthAnchors = list(time = c(0, 0.5, 1), value = c(0.3, 0.5, 0.3)))',

    Elephant = 'soundgen(sylLen = 510, pitchAnchors = list(time = c(0, 0.37, 1), value = c(436, 452, 328)), nonlinBalance = 50, jitterDep = 0.3, rolloffOct = -2, formants = NA, vocalTract = 100, subFreq = 75, subDep = 40, shortestEpoch = 50, noiseAnchors = list(time = c(0, 510), value = c(-120, -120)), amplAnchors = list(time = c(0, 0.53, 1), value = c(120, 120, 61)))',

    Seagull = 'soundgen(nSyl = 8, sylLen = 200, pauseLen = 140, pitchAnchors = list(time = c(0, 0.65, 1), value = c(977, 1540, 826)), temperature = 0.05, nonlinBalance = 100, jitterDep = 0, rolloff = 0, rolloffOct = -4, rolloffParab = 25, rolloffParabHarm = 6, formants = list(f1 = list(time = 0, freq = 1000, amp = 20, width = 250), f2 = list(time = 0, freq = 2200, amp = 30, width = 400), f3 = list(time = 0, freq = 4400, amp = 10, width = 200), f4 = list(time = 0, freq = 5900, amp = 10, width = 300), f5 = list(time = 0, freq = 7200, amp = 10, width = 300)), vocalTract = 15, subFreq = 525, subDep = 220, noiseAnchors = list(time = c(-3, 41, 166, 201), value = c(-37, -120, -120, -45)), formantsNoise = list(f1 = list(time = 0, freq = 1000, amp = 20, width = 250), f2 = list(time = 0, freq = 2200, amp = 30, width = 400), f3 = list(time = 0, freq = 4400, amp = 10, width = 200), f4 = list(time = 0, freq = 5900, amp = 10, width = 300), f5 = list(time = 0, freq = 7200, amp = 10, width = 300)), rolloffNoise = -16, samplingRate = 24000)',

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
