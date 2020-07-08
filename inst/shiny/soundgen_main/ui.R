ui = fluidPage(
  # headerPanel('soundgen 1.1.0'),

  fluidRow(
    column(
      width = 6,
      tabsetPanel(
        id= 'parGroup',
        navbarMenu(
          "Main",
          tabPanel(
            "Syllables",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                sliderInput(
                  'sylLen', 'Syllable length, ms ("sylLen")',
                  value = permittedValues['sylLen','default'],
                  min = permittedValues['sylLen', 'low'],
                  max = permittedValues['sylLen', 'high'],
                  step = permittedValues['sylLen','step']),
                sliderInput(
                  'nSyl', 'Number of syllables ("nSyl")',
                  value = permittedValues['nSyl','default'],
                  min = permittedValues['nSyl', 'low'],
                  max = permittedValues['nSyl', 'high'],
                  step = permittedValues['nSyl','step']),
                sliderInput(
                  'pauseLen', 'Pause, ms ("pauseLen")',
                  value = permittedValues['pauseLen','default'],
                  min = permittedValues['pauseLen', 'low'],
                  max = permittedValues['pauseLen', 'high'],
                  step = permittedValues['pauseLen','step']),
                sliderInput(
                  'repeatBout', 'Repeat bout # times ("repeatBout")',
                  value = permittedValues['repeatBout','default'],
                  min = permittedValues['repeatBout', 'low'],
                  max = permittedValues['repeatBout', 'high'],
                  step = permittedValues['repeatBout','step'])
              ),
              mainPanel(
                width = 6,
                plotOutput('plotSyllables')
              )
            )
          ),

          tabPanel(
            "Hypers",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                sliderInput(
                  'temperature', 'Temperature ("temperature")',
                  value = permittedValues['temperature','default'],
                  min = permittedValues['temperature', 'low'],
                  max = permittedValues['temperature', 'high'],
                  step = permittedValues['temperature','step']),
                sliderInput(
                  'maleFemale', 'Male-female *hyper* ("maleFemale")',
                  value = permittedValues['maleFemale','default'],
                  min = permittedValues['maleFemale', 'low'],
                  max = permittedValues['maleFemale', 'high'],
                  step = permittedValues['maleFemale','step']),
                sliderInput(
                  'creakyBreathy',
                  'Creaky-breathy *hyper* ("creakyBreathy")',
                  value = permittedValues['creakyBreathy','default'],
                  min = permittedValues['creakyBreathy', 'low'],
                  max = permittedValues['creakyBreathy', 'high'],
                  step = permittedValues['creakyBreathy','step'])
              ),
              mainPanel(
                width = 6,
                plotOutput('plotHypers')
              )
            )
          ),

          tabPanel(
            "Settings",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                numericInput(
                  'samplingRate',
                  'Sampling rate, Hz ("samplingRate")',
                  value = permittedValues['samplingRate','default'],
                  min = permittedValues['samplingRate', 'low'],
                  max = permittedValues['samplingRate', 'high'],
                  step = permittedValues['samplingRate','step']),
                numericInput(
                  'windowLength',
                  'FFT window length, ms ("windowLength")',
                  value = permittedValues['windowLength','default'],
                  min = permittedValues['windowLength', 'low'],
                  max = permittedValues['windowLength', 'high'],
                  step = permittedValues['windowLength','step']),
                numericInput(
                  'pitchSamplingRate',
                  'Pitch sampling rate, Hz ("pitchSamplingRate")',
                  value = permittedValues['pitchSamplingRate', 'default'],
                  min = permittedValues['pitchSamplingRate', 'low'],
                  max = permittedValues['pitchSamplingRate', 'high'],
                  step = permittedValues['pitchSamplingRate', 'step']),
                numericInput(
                  'dynamicRange', 'Dynamic range, dB ("dynamicRange")',
                  value = permittedValues['dynamicRange', 'default'],
                  min = permittedValues['dynamicRange', 'low'],
                  max = permittedValues['dynamicRange', 'high'],
                  step = permittedValues['dynamicRange', 'step']),
                sliderInput(
                  'pitchFloorCeiling',
                  'Synthesized pitch range, Hz ("pitchFloorCeiling")',
                  value=c(permittedValues['pitch', 'low'],
                          permittedValues['pitch', 'high']),
                  min = permittedValues['pitch', 'low'],
                  max = 8000,
                  step = permittedValues['pitch', 'step'])
              ),
              mainPanel(
                width = 6,
                plotOutput('plotSettings')
              )
            )
          )
        ),

        navbarMenu(
          "Intonation",
          tabPanel(
            "Intonation syllable",
            sidebarLayout(
              sidebarPanel(
                checkboxInput(
                  inputId = 'generateVoiced',
                  label = 'Generate voiced component?',
                  value = TRUE),
                actionButton(
                  inputId = "pitch_flatten",
                  label = "Flatten pitch contour"),
                tableOutput("pitch_anchors"),
                width = 6),
              mainPanel(
                width = 6,
                plotOutput(
                  'plotIntonation',
                  click = "plotIntonation_click",
                  dblclick = dblclickOpts(id = "plotIntonation_dblclick")),
                sliderInput(
                  'pitchRange', 'Plotted pitch range, Hz',
                  value = c(75, 150),
                  min = permittedValues['pitch', 'low'],
                  max = permittedValues['pitch', 'high'],
                  step = permittedValues['pitch', 'step'])
              )
            )
          ),

          tabPanel(
            "Intonation global",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                actionButton(
                  inputId = "pitch_flattenGlobal",
                  label = "Flatten pitch contour"),
                tableOutput("pitch_anchorsGlobal")
              ),
              mainPanel(
                width = 6,
                plotOutput(
                  'plotIntonationGlobal',
                  click = "plotIntonation_clickGlobal",
                  dblclick = dblclickOpts(id = "plotIntonation_dblclickGlobal"))
              )
            )
          ),

          tabPanel(
            "Vibrato",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                sliderInput(
                  'vibratoFreq', 'Vibrato rate, Hz ("vibratoFreq")',
                  value = permittedValues['vibratoFreq','default'],
                  min = permittedValues['vibratoFreq', 'low'],
                  max = permittedValues['vibratoFreq', 'high'],
                  step = permittedValues['vibratoFreq','step']),
                sliderInput(
                  'vibratoDep', 'Vibrato depth, semitones ("vibratoDep")',
                  value = permittedValues['vibratoDep','default'],
                  min = permittedValues['vibratoDep', 'low'],
                  max = permittedValues['vibratoDep', 'high'],
                  step = permittedValues['vibratoDep','step'])
              ),
              mainPanel(
                width = 6,
                plotOutput('plotVibrato')
              )
            )
          )
        ),

        navbarMenu(
          "Amplitude",
          tabPanel(
            "Amplitude syllable",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                sliderInput (
                  'attackLen', 'Attack length, ms ("attackLen")',
                  value = permittedValues['attackLen','default'],
                  min = permittedValues['attackLen', 'low'],
                  max = permittedValues['attackLen', 'high'],
                  step = permittedValues['attackLen','step']),
                actionButton(
                  inputId = "ampl_syl_flatten",
                  label = "Flatten amplitude envelope"),
                tableOutput("ampl_syl_anchors")
              ),
              mainPanel(
                width = 6,
                plotOutput(
                  'plotAmplSyl',
                  click = "plotAmplSyl_click",
                  dblclick = dblclickOpts(id = "plotAmplSyl_dblclick"))
              )
            )
          ),

          tabPanel(
            "Amplitude global",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                actionButton(
                  inputId = "amplGlobal_flatten",
                  label = "Flatten amplitude envelope"),
                tableOutput("amplGlobal_anchors")
              ),
              mainPanel(
                width = 6,
                plotOutput(
                  'plotAmplGlobal',
                  click = "plotAmplGlobal_click",
                  dblclick = dblclickOpts(id = "plotAmplGlobal_dblclick"))
              )
            )
          ),

          tabPanel(
            "Amplitude modulation",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                sliderInput(
                  'amDep', 'AM depth ("amDep")',
                  value = permittedValues['amDep','default'],
                  min = permittedValues['amDep', 'low'],
                  max = permittedValues['amDep', 'high'],
                  step = permittedValues['amDep','step']),
                sliderInput(
                  'amFreq', 'AM frequency, Hz ("amFreq")',
                  value = permittedValues['amFreq','default'],
                  min = permittedValues['amFreq', 'low'],
                  max = permittedValues['amFreq', 'high'],
                  step = permittedValues['amFreq','step']),
                sliderInput(
                  'amShape', 'AM shape ("amShape")',
                  value = permittedValues['amShape','default'],
                  min = permittedValues['amShape', 'low'],
                  max = permittedValues['amShape', 'high'],
                  step = permittedValues['amShape','step'])
              ),
              mainPanel(
                width = 6,
                plotOutput('plotAM'),
              )
            )
          )
        ),

        navbarMenu(
          "Source",
          tabPanel(
            "Glottal",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                sliderInput(
                  'rolloff', 'Source rolloff, dB/octave ("rolloff")',
                  value = permittedValues['rolloff','default'],
                  min = permittedValues['rolloff', 'low'],
                  max = permittedValues['rolloff', 'high'],
                  step = permittedValues['rolloff','step']),
                shinyBS::bsCollapsePanel(
                  "Advanced",
                  sliderInput(
                    'rolloffOct',
                    'Change of rolloff with frequency, dB/octave ("rolloffOct")',
                    value = permittedValues['rolloffOct','default'],
                    min = permittedValues['rolloffOct', 'low'],
                    max = permittedValues['rolloffOct', 'high'],
                    step = permittedValues['rolloffOct','step']),
                  sliderInput(
                    'rolloffKHz',
                    'Adjust rolloff per f0,  dB/kHz ("rolloffKHz")',
                    value = permittedValues['rolloffKHz','default'],
                    min = permittedValues['rolloffKHz', 'low'],
                    max = permittedValues['rolloffKHz', 'high'],
                    step = permittedValues['rolloffKHz','step']),
                  sliderInput(
                    'rolloffParab',
                    'Parabolic rolloff adjustment, dB/octave ("rolloffParab")',
                    value = permittedValues['rolloffParab','default'],
                    min = permittedValues['rolloffParab', 'low'],
                    max = permittedValues['rolloffParab', 'high'],
                    step = permittedValues['rolloffParab','step']),
                  sliderInput(
                    'rolloffParabHarm',
                    'Harmonics boosted ("rolloffParabHarm")',
                    value = permittedValues['rolloffParabHarm','default'],
                    min = permittedValues['rolloffParabHarm', 'low'],
                    max = permittedValues['rolloffParabHarm', 'high'],
                    step = permittedValues['rolloffParabHarm','step']),
                  sliderInput(
                    'glottis', 'Closed glottis, % ("glottis")',
                    value = permittedValues['glottis', 'default'],
                    min = permittedValues['glottis', 'low'],
                    max = permittedValues['glottis', 'high'],
                    step = permittedValues['glottis','step'])
                )
              ),
              mainPanel(
                width = 6,
                plotOutput('plotRolloff')
              )
            )
          ),

          tabPanel(
            "Nonlinear effects",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                sliderInput(
                  'nonlinBalance',
                  'Balance between nonlinear regimes, % ("nonlinBalance")',
                  value = permittedValues['nonlinBalance','default'],
                  min = permittedValues['nonlinBalance', 'low'],
                  max = permittedValues['nonlinBalance', 'high'],
                  step = permittedValues['nonlinBalance','step']),
                sliderInput(
                  'shortestEpoch',
                  'Shortest epoch length ("shortestEpoch")',
                  value = permittedValues['shortestEpoch','default'],
                  min = permittedValues['shortestEpoch', 'low'],
                  max = permittedValues['shortestEpoch', 'high'],
                  step = permittedValues['shortestEpoch','step']),
                shinyBS::bsCollapsePanel(
                  "Subharmonics",
                  sliderInput(
                    'subRatio',
                    'Subharmonic frequency ratio ("subRatio")',
                    value = permittedValues['subRatio','default'],
                    min = permittedValues['subRatio', 'low'],
                    max = permittedValues['subRatio', 'high'],
                    step = permittedValues['subRatio','step']),
                  sliderInput(
                    'subFreq',
                    'Target subharmonic frequency, Hz (overrides number of subharmonics) ("subFreq")',
                    value = permittedValues['subFreq','default'],
                    min = permittedValues['subFreq', 'low'],
                    max = permittedValues['subFreq', 'high'],
                    step = permittedValues['subFreq','step']),
                  sliderInput(
                    'subDep', 'Depth of subharmonics, % ("subDep")',
                    value = permittedValues['subDep','default'],
                    min = permittedValues['subDep', 'low'],
                    max = permittedValues['subDep', 'high'],
                    step = permittedValues['subDep','step']),                                                                                                                                            sliderInput(
                      'subWidth',
                      'Width of sidebands, Hz ("subWidth")',
                      value = permittedValues['subWidth','default'],
                      min = permittedValues['subWidth', 'low'],
                      max = permittedValues['subWidth', 'high'],
                      step = permittedValues['subWidth','step'])
                ),
                shinyBS::bsCollapsePanel(
                  "Chaos",
                  sliderInput(
                    'jitterDep',
                    'Jitter depth, semitones ("jitterDep")',
                    value = permittedValues['jitterDep','default'],
                    min = permittedValues['jitterDep', 'low'],
                    max = permittedValues['jitterDep', 'high'],
                    step = permittedValues['jitterDep','step']),
                  sliderInput(
                    'jitterLen', 'Jitter period, ms ("jitterLen")',
                    value = permittedValues['jitterLen','default'],
                    min = permittedValues['jitterLen', 'low'],
                    max = permittedValues['jitterLen', 'high'],
                    step = permittedValues['jitterLen','step']),
                  sliderInput(
                    'shimmerDep', 'Shimmer depth, % ("shimmerDep")',
                    value = permittedValues['shimmerDep','default'],
                    min = permittedValues['shimmerDep', 'low'],
                    max = permittedValues['shimmerDep', 'high'],
                    step = permittedValues['shimmerDep','step']),
                  sliderInput(
                    'shimmerLen', 'Shimmer period, ms ("shimmerLen")',
                    value = permittedValues['shimmerLen','default'],
                    min = permittedValues['shimmerLen', 'low'],
                    max = permittedValues['shimmerLen', 'high'],
                    step = permittedValues['shimmerLen','step'])
                )
              ),
              mainPanel(
                width = 6,
                plotOutput('plotNonlin')
              )
            )
          ),

          tabPanel(
            "Unvoiced timing",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                actionButton(
                  inputId = "noise_flatten",
                  label = "Flatten contour"),
                tableOutput("noise_anchors")
              ),
              mainPanel(
                width = 6,
                plotOutput(
                  'plotUnvoiced',
                  click = "plotUnvoiced_click",
                  dblclick = dblclickOpts(id = "plotUnvoiced_dblclick")),
                sliderInput(
                  'noiseTime',
                  'Breathing start / end, ms',
                  value = c(0, 300),
                  min = -permittedValues['sylLen', 'high'] / 2,
                  max = permittedValues['sylLen', 'high'],
                  step = permittedValues['sylLen','step'])
              )
            )
          )
        ),

        navbarMenu(
          "Tract",
          tabPanel(
            "Formants",
            sidebarLayout(
              sidebarPanel(
                sliderInput(
                  'formantDep',
                  'Formant prominence *hyper* ("formantDep")',
                  value = permittedValues['formantDep','default'],
                  min = permittedValues['formantDep', 'low'],
                  max = permittedValues['formantDep', 'high'],
                  step = permittedValues['formantDep','step']),
                sliderInput(
                  'formantWidth',
                  'Formant width *hyper* ("formantWidth")',
                  value = permittedValues['formantWidth','default'],
                  min = permittedValues['formantWidth', 'low'],
                  max = permittedValues['formantWidth', 'high'],
                  step = permittedValues['formantWidth','step']),
                textInput(
                  'vowelString',
                  label = 'String of vowel presets *hyper*',
                  value = "a", width = NULL, placeholder = 'uaaao'),
                shinyBS::bsCollapsePanel(
                  "Show & modify formants manually",
                  tags$style(type="text/css", "textarea {width:100%}"),
                  # NB: this little hack ties the width of the following
                  # textarea to the width of the panel in which it is embedded;
                  # http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                  tags$textarea(
                    id = "formants",
                    label = 'Exact formants',
                    rows = 10, cols = 20, value = NA,
                    placeholder = "list()")
                ),
                shinyBS::bsCollapsePanel(
                  "Advanced",
                  checkboxInput(
                    inputId = 'estimateVTL',
                    label = 'Estimate vocal tract length from formants?',
                    value = FALSE),
                  sliderInput(
                    'vocalTract',
                    'The length of vocal tract, cm ("vocalTract")',
                    value = permittedValues['vocalTract', 'default'],
                    min = permittedValues['vocalTract', 'low'],
                    max = permittedValues['vocalTract', 'high'],
                    step = permittedValues['vocalTract', 'step']),
                  sliderInput(
                    'formantDepStoch',
                    'Added formants, dB ("formantDepStoch")',
                    value = permittedValues['formantDepStoch','default'],
                    min = permittedValues['formantDepStoch', 'low'],
                    max = permittedValues['formantDepStoch', 'high'],
                    step = permittedValues['formantDepStoch','step']),
                  sliderInput(
                    'lipRad', 'Lip radiation, dB/oct ("lipRad")',
                    value = permittedValues['lipRad','default'],
                    min = permittedValues['lipRad', 'low'],
                    max = permittedValues['lipRad', 'high'],
                    step = permittedValues['lipRad','step']),
                  sliderInput(
                    'noseRad', 'Nose radiation, dB/oct ("noseRad")',
                    value = permittedValues['noseRad','default'],
                    min = permittedValues['noseRad', 'low'],
                    max = permittedValues['noseRad', 'high'],
                    step = permittedValues['noseRad','step'])
                ),
                width = 6
              ),
              mainPanel(
                width = 6,
                plotOutput(
                  'plotFormants',
                  click = "plotFormants_click",
                  dblclick = dblclickOpts(id = "plotFormants_dblclick")),
                fluidRow(
                  radioButtons(
                    inputId= 'formants_spectrogram_or_spectrum',
                    label = "Filter preview (voiced)",
                    choices = c("Spectrogram" = "spectrogram",
                                "Spectrum" = "spectrum",
                                "Formant picker" = "formantPicker"),
                    selected = 'spectrum', inline = TRUE, width = NULL)
                )
              )
            )
          ),

          tabPanel(
            "Mouth opening",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                actionButton(
                  inputId = "mouth_flatten",
                  label = "Flatten mouth opening contour"),
                tableOutput("mouth_anchors"),
                sliderInput(
                  'mouthOpenThres', 'Open lips at',
                  value = permittedValues['mouthOpenThres','default'],
                  min = permittedValues['mouthOpenThres', 'low'],
                  max = permittedValues['mouthOpenThres', 'high'],
                  step = permittedValues['mouthOpenThres','step'])
              ),
              mainPanel(
                width = 6,
                plotOutput(
                  'plotMouth',
                  click = "plotMouth_click",
                  dblclick = dblclickOpts(id = "plotMouth_dblclick"))
              )
            )
          ),

          tabPanel(
            "Unvoiced type",
            sidebarLayout(
              sidebarPanel(
                width = 6,
                selectInput(
                  inputId = 'noiseType',
                  label = "Presets",
                  choices = c('Breathing' = 'b', 'Snuffling'= 'n',
                              'h'= 'h', 'sh' = 'x',
                              'f' = 'f', 's' = 's'),
                  selected= 'b'),
                sliderInput(
                  'rolloffNoise',
                  'Linear noise rolloff, dB/oct ("rolloffNoise")',
                  value = permittedValues['rolloffNoise','default'],
                  min = permittedValues['rolloffNoise', 'low'],
                  max = permittedValues['rolloffNoise', 'high'],
                  step = permittedValues['rolloffNoise','step']),
                sliderInput(
                  'rolloffNoiseExp',
                  'Exponential noise rolloff, dB/oct ("rolloffNoiseExp")',
                  value = permittedValues['rolloffNoiseExp','default'],
                  min = permittedValues['rolloffNoiseExp', 'low'],
                  max = permittedValues['rolloffNoiseExp', 'high'],
                  step = permittedValues['rolloffNoiseExp','step']),
                shinyBS::bsCollapsePanel(
                  "Show & modify formants manually",
                  tags$style(type="text/css", "textarea {width:100%}"),
                  # NB: this little hack ties the width of the following
                  # textarea to the width of the panel in which it is embedded;
                  # http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                  tags$textarea(
                    id="formantsNoise",
                    label= 'Exact formants for unvoiced part',
                    rows = 10, cols = 20, value = "",
                    placeholder ="list()")
                )
              ),
              mainPanel(
                width = 6,
                plotOutput('plotConsonant'),
                fluidRow(
                  radioButtons(
                    inputId = 'formantsNoise_spectrogram_or_spectrum',
                    label = "Filter preview (unvoiced)",
                    choices = c("Spectrogram" = "spectrogram",
                                "Spectrum" = "spectrum"),
                    selected = 'spectrum', inline = TRUE, width = NULL)
                )
              )
            )
          )
        )
      )
    ),

    column(
      width = 4,
      fluidRow(
        column(
          width = 3,
          actionButton(
            inputId = "generateAudio",
            label = "Generate",
            style = "color: blue; background-color: orange;")
        ),
        column(
          width = 5,
          uiOutput("htmlAudio")
        )
      ),
      fluidRow(
        plotOutput('spectrogram')
      ),
      fluidRow(
        radioButtons(
          inputId = 'spectrogram_or_spectrum',
          label = "Generated sound",
          choices = c("Spectrogram" = "spectrogram",
                      "Spectrum" = "spectrum"),
          selected = 'spectrogram', inline = TRUE, width = NULL)
      ),
      fluidRow(
        shinyBS::bsCollapse(
          id = "spec_controls",
          shinyBS::bsCollapsePanel(
            "Show spectrogram controls",
            sliderInput(
              'spec_ylim', 'Frequency range, kHz',
              value = c(0, permittedValues['spec_ylim', 'default']),
              min = permittedValues['spec_ylim', 'low'],
              max = permittedValues['spec_ylim', 'high'],
              step = permittedValues['spec_ylim', 'step']),
            sliderInput(
              'specWindowLength', 'Window length, ms',
              value = permittedValues['specWindowLength','default'],
              min = permittedValues['specWindowLength', 'low'],
              max = permittedValues['specWindowLength', 'high'],
              step = permittedValues['specWindowLength','step']),
            sliderInput(
              'specContrast', 'Contrast',
              value = permittedValues['specContrast','default'],
              min = permittedValues['specContrast', 'low'],
              max = permittedValues['specContrast', 'high'],
              step = permittedValues['specContrast','step']),
            sliderInput(
              'specBrightness', 'Brightness',
              value = permittedValues['specBrightness','default'],
              min = permittedValues['specBrightness', 'low'],
              max = permittedValues['specBrightness', 'high'],
              step = permittedValues['specBrightness','step']),
            radioButtons(
              inputId = 'spec_colorTheme',
              label = "Color scheme",
              choices = c("Seewave" = "seewave",
                          "Heat" = "heat.colors",
                          "Black & white" = "bw"),
              selected = 'bw', inline = TRUE, width = NULL),
            radioButtons(
              inputId = 'spec_method', label = "Method",
              choices = c("Spectrum" = "spectrum",
                          "Spectral derivative" = "spectralDerivative"),
              selected = 'spectrum', inline = TRUE, width = NULL),
            radioButtons(
              inputId= 'osc', label ="Oscillogram type",
              choices=c("Linear" = "linear",
                        "dB" = "dB",
                        "None" = "none"),
              selected = 'linear', inline = TRUE, width = NULL),
            sliderInput(
              'osc_heights', 'Relative size of oscillogram',
              value = permittedValues['oscHeight','default'],
              min = permittedValues['oscHeight', 'low'],
              max = permittedValues['oscHeight', 'high'],
              step = permittedValues['oscHeight','step'])
            # sliderInput('spec_median_smoothing', 'Median smoothing, bins',
            # value=3, min=1, max=9, step=2),
            # sliderInput('spec_zpad', 'Zero padding, final windowLength_points',
            # value=0, min=0, max=6144, step=512)
          )
        )
      )
    ),

    column(
      width = 2,
      tags$h2('Presets'),
      selectInput(
        inputId = 'speaker', label ="Speaker",
        choices = names(presets),
        selected = names(presets)[1]),
      selectInput(
        inputId = 'callType', label = "Call",
        choices = names(presets[[1]]),
        selected = names(presets[[1]])[1]),
      shinyBS::bsCollapsePanel(
        "Load new preset",
        tags$style(
          type = "text/css",
          "textarea {width:100%; font-size:50%}"),
        tags$textarea(
          id = "user_preset",
          label = 'Type in a new preset here',
          rows = 10, cols = 20, value = "",
          placeholder = "soundgen(...)"),
        actionButton(
          inputId = "import_preset",
          label = "Update sliders")
      ),

      tags$h2('Export'),
      downloadButton(outputId = "saveAudio", label = "Save audio"),
      tags$br(), tags$br(),
      shinyBS::bsCollapsePanel(
        "Export R code",
        tags$style(
          type = "text/css",
          "textarea {width:100%; font-size:50%}"),
        tags$textarea(
          id = "mycall",
          label = 'Copy-paste function call',
          rows = 10, cols = 20, value = "",
          placeholder ="soundgen()")
      ),
      actionButton("about", "About")
      # shinyBS:::bsPopover(id = 'about', title = NULL, content = 'Help',
      # placement = "right", trigger = "hover")
      # shinyBS has to be mentioned somewhere in ui,
      # otherwise addTooltip doesn't work in server
    )
  )
)
