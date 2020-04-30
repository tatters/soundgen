ui = fluidPage(
  # headerPanel('soundgen 1.1.0'),

  fluidRow(
    column(6,
           tabsetPanel(id='parGroup',
                       navbarMenu("Main",
                                  tabPanel("Syllables",
                                           sidebarLayout(
                                             sidebarPanel(
                                               sliderInput('sylLen', 'Syllable length, ms', value=permittedValues['sylLen','default'], min=permittedValues['sylLen', 'low'], max=permittedValues['sylLen', 'high'], step=permittedValues['sylLen','step']),
                                               sliderInput('nSyl', 'Number of syllables', value=permittedValues['nSyl','default'], min=permittedValues['nSyl', 'low'], max=permittedValues['nSyl', 'high'], step=permittedValues['nSyl','step']),
                                               sliderInput('pauseLen', 'Pause, ms', value=permittedValues['pauseLen','default'], min=permittedValues['pauseLen', 'low'], max=permittedValues['pauseLen', 'high'], step=permittedValues['pauseLen','step']),
                                               sliderInput('repeatBout', 'Repeat bout # times', value=permittedValues['repeatBout','default'], min=permittedValues['repeatBout', 'low'], max=permittedValues['repeatBout', 'high'], step=permittedValues['repeatBout','step']),
                                               width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotSyllables'),
                                               width=6
                                             )
                                           )
                                  ),

                                  tabPanel("Hypers",
                                           sidebarLayout(
                                             sidebarPanel(
                                               sliderInput('temperature', 'Temperature', value=permittedValues['temperature','default'], min=permittedValues['temperature', 'low'], max=permittedValues['temperature', 'high'], step=permittedValues['temperature','step']),
                                               sliderInput('maleFemale', 'Male-female *hyper*', value=permittedValues['maleFemale','default'], min=permittedValues['maleFemale', 'low'], max=permittedValues['maleFemale', 'high'], step=permittedValues['maleFemale','step']),
                                               sliderInput('creakyBreathy', 'Creaky-breathy *hyper*', value=permittedValues['creakyBreathy','default'], min=permittedValues['creakyBreathy', 'low'], max=permittedValues['creakyBreathy', 'high'], step=permittedValues['creakyBreathy','step']),
                                               width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotHypers'),
                                               width=6
                                             )
                                           )
                                  ),

                                  tabPanel("Settings",
                                           sidebarLayout(
                                             sidebarPanel(
                                               numericInput('samplingRate', 'Sampling rate, Hz', value=permittedValues['samplingRate','default'], min=permittedValues['samplingRate', 'low'], max=permittedValues['samplingRate', 'high'], step=permittedValues['samplingRate','step']),
                                               numericInput('windowLength', 'FFT window length, ms', value=permittedValues['windowLength','default'], min=permittedValues['windowLength', 'low'], max=permittedValues['windowLength', 'high'], step=permittedValues['windowLength','step']),
                                               numericInput('pitchSamplingRate', 'Pitch sampling rate, Hz', value=permittedValues['pitchSamplingRate', 'default'], min=permittedValues['pitchSamplingRate', 'low'], max=permittedValues['pitchSamplingRate', 'high'], step=permittedValues['pitchSamplingRate', 'step']),
                                               numericInput('dynamicRange', 'Dynamic range, dB', value=permittedValues['dynamicRange', 'default'], min=permittedValues['dynamicRange', 'low'], max=permittedValues['dynamicRange', 'high'], step=permittedValues['dynamicRange', 'step']),
                                               sliderInput('pitchFloorCeiling', 'Synthesized pitch range, Hz', value=c(permittedValues['pitch', 'low'], permittedValues['pitch', 'high']), min=1, max=8000, step=10),
                                               width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotSettings'),
                                               width=6
                                             )
                                           )
                                  )
                       ),

                       navbarMenu("Intonation",
                                  tabPanel("Intonation syllable",
                                           sidebarLayout(
                                             sidebarPanel(
                                               checkboxInput(inputId = 'generateVoiced', label = 'Generate voiced component?', value = TRUE),
                                               actionButton(inputId = "pitch_flatten", label = "Flatten pitch contour"),
                                               tableOutput("pitch_anchors"),
                                               width=6),
                                             mainPanel(
                                               plotOutput('plotIntonation', click = "plotIntonation_click", dblclick = dblclickOpts(id = "plotIntonation_dblclick")),
                                               sliderInput('pitchRange', 'Plotted pitch range, Hz', value=c(75,150), min=permittedValues['pitch', 'low'], max=permittedValues['pitch', 'high'], step=permittedValues['pitch', 'step']),
                                               width=6)
                                           )
                                  ),

                                  tabPanel("Intonation global",
                                           sidebarLayout(
                                             sidebarPanel(
                                               actionButton(inputId = "pitch_flattenGlobal", label = "Flatten pitch contour"),
                                               tableOutput("pitch_anchorsGlobal"), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotIntonationGlobal', click = "plotIntonation_clickGlobal", dblclick = dblclickOpts(id = "plotIntonation_dblclickGlobal")), width=6
                                             )
                                           )
                                  ),

                                  tabPanel("Vibrato",
                                           sidebarLayout(
                                             sidebarPanel(
                                               sliderInput('vibratoFreq', 'Vibrato rate, Hz', value=permittedValues['vibratoFreq','default'], min=permittedValues['vibratoFreq', 'low'], max=permittedValues['vibratoFreq', 'high'], step=permittedValues['vibratoFreq','step']),
                                               sliderInput('vibratoDep', 'Vibrato depth, semitones', value=permittedValues['vibratoDep','default'], min=permittedValues['vibratoDep', 'low'], max=permittedValues['vibratoDep', 'high'], step=permittedValues['vibratoDep','step']),
                                               width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotVibrato'),
                                               width=6
                                             )
                                           )
                                  )
                       ),

                       navbarMenu("Amplitude",
                                  tabPanel("Amplitude syllable",
                                           sidebarLayout(
                                             sidebarPanel(
                                               sliderInput ('attackLen', 'Attack length, ms', value=permittedValues['attackLen','default'], min=permittedValues['attackLen', 'low'], max=permittedValues['attackLen', 'high'], step=permittedValues['attackLen','step']),
                                               actionButton(inputId = "ampl_syl_flatten", label = "Flatten amplitude envelope"),
                                               tableOutput("ampl_syl_anchors"), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotAmplSyl', click = "plotAmplSyl_click", dblclick = dblclickOpts(id = "plotAmplSyl_dblclick")), width=6
                                             )
                                           )
                                  ),

                                  tabPanel("Amplitude global",
                                           sidebarLayout(
                                             sidebarPanel(
                                               actionButton(inputId = "amplGlobal_flatten", label = "Flatten amplitude envelope"),
                                               tableOutput("amplGlobal_anchors"), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotAmplGlobal', click = "plotAmplGlobal_click", dblclick = dblclickOpts(id = "plotAmplGlobal_dblclick")), width=6
                                             )
                                           )
                                  ),

                                  tabPanel("Amplitude modulation",
                                           sidebarLayout(
                                             sidebarPanel(
                                               sliderInput('amDep', 'AM depth', value=permittedValues['amDep','default'], min=permittedValues['amDep', 'low'], max=permittedValues['amDep', 'high'], step=permittedValues['amDep','step']),
                                               sliderInput('amFreq', 'AM frequency, Hz', value=permittedValues['amFreq','default'], min=permittedValues['amFreq', 'low'], max=permittedValues['amFreq', 'high'], step=permittedValues['amFreq','step']),
                                               sliderInput('amShape', 'AM shape', value=permittedValues['amShape','default'], min=permittedValues['amShape', 'low'], max=permittedValues['amShape', 'high'], step=permittedValues['amShape','step']),
                                               width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotAM'),
                                               width=6
                                             )
                                           )
                                  )
                       ),

                       navbarMenu("Source",
                                  tabPanel("Glottal",
                                           sidebarLayout(
                                             sidebarPanel(
                                               sliderInput('rolloff', 'Source rolloff, dB/octave', value=permittedValues['rolloff','default'], min=permittedValues['rolloff', 'low'], max=permittedValues['rolloff', 'high'], step=permittedValues['rolloff','step']),
                                               shinyBS::bsCollapsePanel("Advanced",
                                                                        sliderInput('rolloffOct', 'Change of rolloff with frequency, dB/octave', value=permittedValues['rolloffOct','default'], min=permittedValues['rolloffOct', 'low'], max=permittedValues['rolloffOct', 'high'], step=permittedValues['rolloffOct','step']),
                                                                        sliderInput('rolloffKHz', 'Adjust rolloff per f0,  dB/kHz', value=permittedValues['rolloffKHz','default'], min=permittedValues['rolloffKHz', 'low'], max=permittedValues['rolloffKHz', 'high'], step=permittedValues['rolloffKHz','step']),
                                                                        sliderInput('rolloffParab', 'Parabolic rolloff adjustment, dB/octave', value=permittedValues['rolloffParab','default'], min=permittedValues['rolloffParab', 'low'], max=permittedValues['rolloffParab', 'high'], step=permittedValues['rolloffParab','step']),
                                                                        sliderInput('rolloffParabHarm', 'Harmonics boosted', value=permittedValues['rolloffParabHarm','default'], min=permittedValues['rolloffParabHarm', 'low'], max=permittedValues['rolloffParabHarm', 'high'], step=permittedValues['rolloffParabHarm','step']),
                                                                        sliderInput('glottis', 'Closed glottis, %', value=permittedValues['glottis', 'default'], min=permittedValues['glottis', 'low'], max=permittedValues['glottis', 'high'], step=permittedValues['glottis','step'])
                                               ), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotRolloff'), width=6
                                             )
                                           )
                                  ),

                                  tabPanel("Nonlinear effects",
                                           sidebarLayout(
                                             sidebarPanel(
                                               sliderInput('nonlinBalance', 'Balance between nonlinear regimes, %', value=permittedValues['nonlinBalance','default'], min=permittedValues['nonlinBalance', 'low'], max=permittedValues['nonlinBalance', 'high'], step=permittedValues['nonlinBalance','step']),
                                               sliderInput('shortestEpoch', 'Shortest epoch length', value=permittedValues['shortestEpoch','default'], min=permittedValues['shortestEpoch', 'low'], max=permittedValues['shortestEpoch', 'high'], step=permittedValues['shortestEpoch','step']),
                                               shinyBS::bsCollapsePanel("Advanced",
                                                                        sliderInput('subFreq', 'Target subharmonic frequency, Hz', value=permittedValues['subFreq','default'], min=permittedValues['subFreq', 'low'], max=permittedValues['subFreq', 'high'], step=permittedValues['subFreq','step']),
                                                                        sliderInput('subDep', 'Depth of subharmonics', value=permittedValues['subDep','default'], min=permittedValues['subDep', 'low'], max=permittedValues['subDep', 'high'], step=permittedValues['subDep','step']),                                                                                                                                            sliderInput('subWidth', 'Width of sidebands, Hz', value=permittedValues['subWidth','default'], min=permittedValues['subWidth', 'low'], max=permittedValues['subWidth', 'high'], step=permittedValues['subWidth','step']),
                                                                        sliderInput('jitterDep', 'Jitter depth, semitones', value=permittedValues['jitterDep','default'], min=permittedValues['jitterDep', 'low'], max=permittedValues['jitterDep', 'high'], step=permittedValues['jitterDep','step']),
                                                                        sliderInput('jitterLen', 'Jitter period, ms', value=permittedValues['jitterLen','default'], min=permittedValues['jitterLen', 'low'], max=permittedValues['jitterLen', 'high'], step=permittedValues['jitterLen','step']),
                                                                        sliderInput('shimmerDep', 'Shimmer depth, %', value=permittedValues['shimmerDep','default'], min=permittedValues['shimmerDep', 'low'], max=permittedValues['shimmerDep', 'high'], step=permittedValues['shimmerDep','step']),
                                                                        sliderInput('shimmerLen', 'Shimmer period, ms', value=permittedValues['shimmerLen','default'], min=permittedValues['shimmerLen', 'low'], max=permittedValues['shimmerLen', 'high'], step=permittedValues['shimmerLen','step'])
                                               ), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotNonlin'),
                                               width=6
                                             )
                                           )
                                  ),

                                  tabPanel("Unvoiced timing",
                                           sidebarLayout(
                                             sidebarPanel(
                                               actionButton(inputId = "noise_flatten", label = "Flatten contour"),
                                               tableOutput("noise_anchors"), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotUnvoiced', click = "plotUnvoiced_click", dblclick = dblclickOpts(id = "plotUnvoiced_dblclick")),
                                               sliderInput('noiseTime', 'Breathing start / end, ms', value=c(0, 300), min=-permittedValues['sylLen', 'high'] / 2, max=permittedValues['sylLen', 'high'], step=permittedValues['sylLen','step']),
                                               width=6
                                             )
                                           )
                                  )
                       ),

                       navbarMenu("Tract",
                                  tabPanel("Formants",
                                           sidebarLayout(
                                             sidebarPanel(
                                               sliderInput('formantDep', 'Formant prominence *hyper*', value=permittedValues['formantDep','default'], min=permittedValues['formantDep', 'low'], max=permittedValues['formantDep', 'high'], step=permittedValues['formantDep','step']),
                                               sliderInput('formantWidth', 'Formant width *hyper*', value=permittedValues['formantWidth','default'], min=permittedValues['formantWidth', 'low'], max=permittedValues['formantWidth', 'high'], step=permittedValues['formantWidth','step']),
                                               textInput('vowelString', label='String of vowel presets *hyper*', value = "a", width = NULL, placeholder ='uaaao'),
                                               shinyBS::bsCollapsePanel("Show & modify formants manually",
                                                                        tags$style(type="text/css", "textarea {width:100%}"), # NB: this little hack ties the width of the following textarea to the width of the panel in which it is embedded; see http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                                                                        tags$textarea(id="formants", label='Exact formants', rows=10, cols=20, value=NA, placeholder ="list()")
                                               ),
                                               shinyBS::bsCollapsePanel("Advanced",
                                                                        checkboxInput(inputId = 'estimateVTL', label = 'Estimate vocal tract length from formants?', value = FALSE),
                                                                        sliderInput('vocalTract', 'The length of vocal tract, cm', value=permittedValues['vocalTract', 'default'], min=permittedValues['vocalTract', 'low'], max=permittedValues['vocalTract', 'high'], step=permittedValues['vocalTract', 'step']),
                                                                        sliderInput('formantDepStoch', 'Added formants, dB', value=permittedValues['formantDepStoch','default'], min=permittedValues['formantDepStoch', 'low'], max=permittedValues['formantDepStoch', 'high'], step=permittedValues['formantDepStoch','step']),
                                                                        sliderInput('lipRad', 'Lip radiation, dB/oct', value=permittedValues['lipRad','default'], min=permittedValues['lipRad', 'low'], max=permittedValues['lipRad', 'high'], step=permittedValues['lipRad','step']),
                                                                        sliderInput('noseRad', 'Nose radiation, dB/oct', value=permittedValues['noseRad','default'], min=permittedValues['noseRad', 'low'], max=permittedValues['noseRad', 'high'], step=permittedValues['noseRad','step'])
                                               ),
                                               width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotFormants', click = "plotFormants_click", dblclick = dblclickOpts(id = "plotFormants_dblclick")),
                                               fluidRow(
                                                 radioButtons(inputId='formants_spectrogram_or_spectrum', label="Filter preview (voiced)", choices=c("Spectrogram"="spectrogram", "Spectrum"="spectrum", "Formant picker"="formantPicker"), selected='spectrum', inline=TRUE, width=NULL)
                                               ), width=6
                                             )
                                           )
                                  ),

                                  tabPanel("Mouth opening",
                                           sidebarLayout(
                                             sidebarPanel(
                                               actionButton(inputId = "mouth_flatten", label = "Flatten mouth opening contour"),
                                               tableOutput("mouth_anchors"),
                                               sliderInput('mouthOpenThres', 'Open lips at', value=permittedValues['mouthOpenThres','default'], min=permittedValues['mouthOpenThres', 'low'], max=permittedValues['mouthOpenThres', 'high'], step=permittedValues['mouthOpenThres','step']),
                                               width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotMouth', click = "plotMouth_click", dblclick = dblclickOpts(id = "plotMouth_dblclick")), width=6
                                             )
                                           )
                                  ),

                                  tabPanel("Unvoiced type",
                                           sidebarLayout(
                                             sidebarPanel(
                                               selectInput(inputId='noiseType', label="Presets", choices=c('Breathing'='b', 'Snuffling'='n', 'h'='h', 'sh'='x','f'='f', 's'='s'), selected='b'),
                                               sliderInput('rolloffNoise', 'Linear noise rolloff, dB/oct', value=permittedValues['rolloffNoise','default'], min=permittedValues['rolloffNoise', 'low'], max=permittedValues['rolloffNoise', 'high'], step=permittedValues['rolloffNoise','step']),
                                               sliderInput('rolloffNoiseExp', 'Exponential noise rolloff, dB/oct', value=permittedValues['rolloffNoiseExp','default'], min=permittedValues['rolloffNoiseExp', 'low'], max=permittedValues['rolloffNoiseExp', 'high'], step=permittedValues['rolloffNoiseExp','step']),
                                               shinyBS::bsCollapsePanel("Show & modify formants manually",
                                                                        tags$style(type="text/css", "textarea {width:100%}"), # NB: this little hack ties the width of the following textarea to the width of the panel in which it is embedded; see http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                                                                        tags$textarea(id="formantsNoise", label='Exact formants for unvoiced part', rows=10, cols=20, value="", placeholder ="list()")
                                               ),
                                               width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotConsonant'),
                                               fluidRow(
                                                 radioButtons(inputId='formantsNoise_spectrogram_or_spectrum', label="Filter preview (unvoiced)", choices=c("Spectrogram"="spectrogram", "Spectrum"="spectrum"), selected='spectrum', inline=TRUE, width=NULL)
                                               ),
                                               width=6
                                             )
                                           )
                                  )
                       )
           )
    ),

    column(4,
           fluidRow(
             column(3,
                    actionButton(inputId = "generateAudio", label = "Generate", style="color: blue; background-color: orange;")
             ),
             column(5,
                    uiOutput("htmlAudio")
             )
           ),
           fluidRow(
             plotOutput('spectrogram')
           ),
           fluidRow(
             radioButtons(inputId='spectrogram_or_spectrum', label="Generated sound", choices=c("Spectrogram"="spectrogram", "Spectrum"="spectrum"), selected='spectrogram', inline=TRUE, width=NULL)
           ),
           fluidRow(
             shinyBS::bsCollapse(id="spec_controls",
                                 shinyBS::bsCollapsePanel("Show spectrogram controls",
                                                          sliderInput('spec_ylim', 'Frequency range, kHz', value=c(0,5), min=0, max=22, step=1),
                                                          sliderInput('specWindowLength', 'Window length, ms', value=permittedValues['specWindowLength','default'], min=permittedValues['specWindowLength', 'low'], max=permittedValues['specWindowLength', 'high'], step=permittedValues['specWindowLength','step']),                                               sliderInput('specContrast', 'Contrast', value=permittedValues['specContrast','default'], min=permittedValues['specContrast', 'low'], max=permittedValues['specContrast', 'high'], step=permittedValues['specContrast','step']),
                                                          sliderInput('specBrightness', 'Brightness', value=permittedValues['specBrightness','default'], min=permittedValues['specBrightness', 'low'], max=permittedValues['specBrightness', 'high'], step=permittedValues['specBrightness','step']),
                                                          radioButtons(inputId='spec_colorTheme', label="Color scheme", choices=c("Seewave"="seewave", "Heat"="heat.colors", "Black & white"="bw"), selected='bw', inline=TRUE, width=NULL),
                                                          radioButtons(inputId='spec_method', label="Method", choices=c("Spectrum"="spectrum", "Spectral derivative"="spectralDerivative"), selected='spectrum', inline=TRUE, width=NULL),
                                                          radioButtons(inputId='osc', label="Oscillogram type", choices=c("Linear"="linear", "dB"="dB", "None"="none"), selected='linear', inline=TRUE, width=NULL),
                                                          sliderInput('osc_heights', 'Relative size of oscillogram', value=permittedValues['oscHeight','default'], min=permittedValues['oscHeight', 'low'], max=permittedValues['oscHeight', 'high'], step=permittedValues['oscHeight','step'])
                                                          # sliderInput('spec_median_smoothing', 'Median smoothing, bins', value=3, min=1, max=9, step=2),
                                                          # sliderInput('spec_zpad', 'Zero padding, final windowLength_points', value=0, min=0, max=6144, step=512)
                                 )
             )
           )
    ),

    column(2,
           tags$h2('Presets'),
           selectInput(inputId='speaker', label="Speaker", choices=names(presets), selected=names(presets)[1]),
           selectInput(inputId='callType', label="Call", choices=names(presets[[1]]), selected=names(presets[[1]])[1]),
           shinyBS::bsCollapsePanel("Load new preset",
                                    tags$style(type="text/css", "textarea {width:100%; font-size:50%}"),
                                    tags$textarea(id="user_preset", label='Type in a new preset here', rows=10, cols=20, value="", placeholder ="soundgen(...)"),
                                    actionButton(inputId = "import_preset", label = "Update sliders")
           ),

           tags$h2('Export'),
           downloadButton(outputId = "saveAudio", label = "Save audio"),
           tags$br(), tags$br(),
           shinyBS::bsCollapsePanel("Export R code",
                                    tags$style(type="text/css", "textarea {width:100%; font-size:50%}"), # NB: this little hack ties the width of the following textarea to the width of the panel in which it is embedded; see http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                                    tags$textarea(id="mycall", label='Copy-paste function call', rows=10, cols=20, value="", placeholder ="soundgen()")
           ),
           actionButton("about", "About")
           # shinyBS:::bsPopover(id='about', title=NULL, content='Help', placement="right", trigger="hover")  # shinyBS has to be mentioned somewhere in ui, otherwise addTooltip doesn't work in server
    )
  )
)
