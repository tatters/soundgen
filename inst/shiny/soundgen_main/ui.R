ui = fluidPage(
  # headerPanel('soundgen 1.0.0'),

  fluidRow(
    column(6,
           tabsetPanel(id='parGroup',
                       navbarMenu("Main",
                                  tabPanel("Syllables",
                                           sidebarLayout(
                                             sidebarPanel(
                                               sliderInput('sylLen', 'Syllable length, ms', value=permittedValues['sylLen','default'], min=permittedValues['sylLen', 'low'], max=permittedValues['sylLen', 'high'], step=permittedValues['sylLen','step']),
                                               shinyBS:::bsPopover(id='sylLen', title=NULL, content='Average duration of a continuous VOICED syllable (unvoiced noise is added separately and may fill in the pauses)', placement="right", trigger="hover"),
                                               sliderInput('nSyl', 'Number of syllables', value=permittedValues['nSyl','default'], min=permittedValues['nSyl', 'low'], max=permittedValues['nSyl', 'high'], step=permittedValues['nSyl','step']),
                                               shinyBS:::bsPopover(id='nSyl', title=NULL, content='Each sound consists of one or several syllables separated by pauses', placement="right", trigger="hover"),
                                               sliderInput('pauseLen', 'Pause, ms', value=permittedValues['pauseLen','default'], min=permittedValues['pauseLen', 'low'], max=permittedValues['pauseLen', 'high'], step=permittedValues['pauseLen','step']),
                                               shinyBS:::bsPopover(id='pauseLen', title=NULL, content='Average pause between syllables', placement="right", trigger="hover"),
                                               sliderInput('repeatBout', 'Repeat bout # times', value=permittedValues['repeatBout','default'], min=permittedValues['repeatBout', 'low'], max=permittedValues['repeatBout', 'high'], step=permittedValues['repeatBout','step']),
                                               shinyBS:::bsPopover(id='repeatBout', title=NULL, content='Play the whole bout several times with a specified pause', placement="right", trigger="hover"),width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotSyllables'), width=6
                                             )
                                           )
                                  ),

                                  tabPanel("Hypers",
                                           sidebarLayout(
                                             sidebarPanel(
                                               sliderInput('temperature', 'Temperature', value=permittedValues['temperature','default'], min=permittedValues['temperature', 'low'], max=permittedValues['temperature', 'high'], step=permittedValues['temperature','step']),
                                               shinyBS:::bsPopover(id='temperature', title=NULL, content='Stochasticity within each syllable', placement="right", trigger="hover"),
                                               sliderInput('maleFemale', 'Male-female *hyper*', value=permittedValues['maleFemale','default'], min=permittedValues['maleFemale', 'low'], max=permittedValues['maleFemale', 'high'], step=permittedValues['maleFemale','step']),
                                               shinyBS:::bsPopover(id='maleFemale', title=NULL, content='Adjusts vocal tract length, pitch contour, and formants to imitate larger/smaller body size', placement="right", trigger="hover"),
                                               sliderInput('creakyBreathy', 'Creaky-breathy *hyper*', value=permittedValues['creakyBreathy','default'], min=permittedValues['creakyBreathy', 'low'], max=permittedValues['creakyBreathy', 'high'], step=permittedValues['creakyBreathy','step']),
                                               shinyBS:::bsPopover(id='creakyBreathy', title=NULL, content='Changes a bunch of parameters to make the VOICED component either constricted (creaky) or breathy', placement="right", trigger="hover"), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotHypers'), width=6
                                             )
                                           )
                                  ),

                                  tabPanel("Settings",
                                           sidebarLayout(
                                             sidebarPanel(
                                               numericInput('samplingRate', 'Sampling rate, Hz', value=permittedValues['samplingRate','default'], min=permittedValues['samplingRate', 'low'], max=permittedValues['samplingRate', 'high'], step=permittedValues['samplingRate','step']),
                                               shinyBS:::bsPopover(id='samplingRate', title=NULL, content='The number of points per second of audio. Higher = better quality; lower = faster. Can be any integer, not necessarily a power of two.', placement="right", trigger="hover"),
                                               numericInput('windowLength', 'FFT window length, ms', value=permittedValues['windowLength','default'], min=permittedValues['windowLength', 'low'], max=permittedValues['windowLength', 'high'], step=permittedValues['windowLength','step']),
                                               shinyBS:::bsPopover(id='windowLength', title=NULL, content='The length of window for performing FFT - inverse FFT when filtering the source.', placement="right", trigger="hover"),
                                               numericInput('pitchSamplingRate', 'Pitch sampling rate, Hz', value=3500, min=100, max=44000, step=100),
                                               shinyBS:::bsPopover(id='pitchSamplingRate', title=NULL, content='The number of considered F0 values per second of audio. Should be >= pitchCeiling for best quality', placement="right", trigger="hover"),
                                               numericInput('throwaway', 'Dynamic range, dB', value=permittedValues['throwaway', 'default'], min=permittedValues['throwaway', 'low'], max=permittedValues['throwaway', 'high'], step=permittedValues['throwaway', 'step']),
                                               shinyBS:::bsPopover(id='throwaway', title=NULL, content='Discard everything below this amplitude', placement="right", trigger="hover"),
                                               sliderInput('pitchFloorCeiling', 'Synthesized pitch range, Hz', value=c(permittedValues['pitch', 'low'],permittedValues['pitch', 'high']), min=25, max=8000, step=25),
                                               shinyBS:::bsPopover(id='pitchFloorCeiling', title=NULL, content='Sets the bounds of fundamental frequency for synthesis', placement="right", trigger="hover"), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotSettings'), width=6
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
                                               shinyBS:::bsPopover(id='pitch_flatten', title=NULL, content='Revert to a flat intonation contour with pitch equal to the first (left) anchor', placement="right", trigger="hover"),
                                               tableOutput("pitch_anchors"),
                                               width=6),
                                             mainPanel(
                                               plotOutput('plotIntonation', click = "plotIntonation_click", dblclick = dblclickOpts(id = "plotIntonation_dblclick")),
                                               sliderInput('pitchRange', 'Plotted pitch range, Hz', value=c(75,150), min=permittedValues['pitch', 'low'], max=permittedValues['pitch', 'high'], step=permittedValues['pitch', 'step']),
                                               shinyBS:::bsPopover(id='pitchRange', title=NULL, content='Set upper / lower limit separately or drag in between the markers to shift both limits simultaneously', placement="right", trigger="hover"),
                                               width=6)
                                           )
                                  ),

                                  tabPanel("Intonation global",
                                           sidebarLayout(
                                             sidebarPanel(
                                               actionButton(inputId = "pitch_flattenGlobal", label = "Flatten pitch contour"),
                                               shinyBS:::bsPopover(id='pitch_flattenGlobal', title=NULL, content='No global pitch modulation from syllable to syllable', placement="right", trigger="hover"),
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
                                               shinyBS:::bsPopover(id='vibratoFreq', title=NULL, content='Frequency of regular FM', placement="right", trigger="hover"),
                                               sliderInput('vibratoDep', 'Vibrato depth, semitones', value=permittedValues['vibratoDep','default'], min=permittedValues['vibratoDep', 'low'], max=permittedValues['vibratoDep', 'high'], step=permittedValues['vibratoDep','step']),
                                               shinyBS:::bsPopover(id='vibratoDep', title=NULL, content='Depth of regular FM', placement="right", trigger="hover"), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotVibrato'), width=6
                                             )
                                           )
                                  )
                       ),

                       navbarMenu("Amplitude",
                                  tabPanel("Amplitude syllable",
                                           sidebarLayout(
                                             sidebarPanel(
                                               actionButton(inputId = "ampl_syl_flatten", label = "Flatten amplitude envelope"),
                                               shinyBS:::bsPopover(id='ampl_syl_flatten', title=NULL, content='Same amplitude over the entire syllable', placement="right", trigger="hover"),
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
                                               shinyBS:::bsPopover(id='amplGlobal_flatten', title=NULL, content='Same amplitude over the entire bout', placement="right", trigger="hover"),
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
                                               sliderInput ('attackLen', 'Attack length, ms', value=permittedValues['attackLen','default'], min=permittedValues['attackLen', 'low'], max=permittedValues['attackLen', 'high'], step=permittedValues['attackLen','step']),
                                               shinyBS:::bsPopover(id='attackLen', title=NULL, content='Does the voice start/end abruptly or with a "fade-in/out"?', placement="right", trigger="hover"),
                                               sliderInput('amDep', 'AM depth', value=permittedValues['amDep','default'], min=permittedValues['amDep', 'low'], max=permittedValues['amDep', 'high'], step=permittedValues['amDep','step']),
                                               shinyBS:::bsPopover(id='amDep', title=NULL, content='Depth of amplitude modulation', placement="right", trigger="hover"),
                                               sliderInput('amFreq', 'AM frequency, Hz', value=permittedValues['amFreq','default'], min=permittedValues['amFreq', 'low'], max=permittedValues['amFreq', 'high'], step=permittedValues['amFreq','step']),
                                               shinyBS:::bsPopover(id='amFreq', title=NULL, content='Frequency of amplitude modulation', placement="right", trigger="hover"),
                                               sliderInput('amShape', 'AM shape', value=permittedValues['amShape','default'], min=permittedValues['amShape', 'low'], max=permittedValues['amShape', 'high'], step=permittedValues['amShape','step']),
                                               shinyBS:::bsPopover(id='amShape', title=NULL, content='Shape of amplitude modulation: 0 = ~sine, -1 = notches, +1 = clicks', placement="right", trigger="hover"), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotAM'), width=6
                                             )
                                           )
                                  )
                       ),

                       navbarMenu("Source",
                                  tabPanel("Rolloff",
                                           sidebarLayout(
                                             sidebarPanel(
                                               sliderInput('rolloff', 'Source rolloff, dB/octave', value=permittedValues['rolloff','default'], min=permittedValues['rolloff', 'low'], max=permittedValues['rolloff', 'high'], step=permittedValues['rolloff','step']),
                                               shinyBS:::bsPopover(id='rolloff', title=NULL, content='Loss of energy in harmonics relative to fundamental frequency (F0); low values emphasize F0', placement="right", trigger="hover"),
                                               shinyBS::bsCollapsePanel("Advanced",
                                                                        sliderInput('rolloffOct', 'Change of rolloff with frequency, dB/octave', value=permittedValues['rolloffOct','default'], min=permittedValues['rolloffOct', 'low'], max=permittedValues['rolloffOct', 'high'], step=permittedValues['rolloffOct','step']),
                                                                        shinyBS:::bsPopover(id='rolloffOct', title=NULL, content='Negative: rolloff is progressively steeper for higher frequencies', placement="right", trigger="hover"),
                                                                        sliderInput('rolloffKHz', 'Adjust rolloff per f0,  dB/kHz', value=permittedValues['rolloffKHz','default'], min=permittedValues['rolloffKHz', 'low'], max=permittedValues['rolloffKHz', 'high'], step=permittedValues['rolloffKHz','step']),
                                                                        shinyBS:::bsPopover(id='rolloffKHz', title=NULL, content='Steeper/gentler basic rolloff as f0 varies', placement="right", trigger="hover"),
                                                                        sliderInput('rolloffParab', 'Parabolic rolloff adjustment, dB/octave', value=permittedValues['rolloffParab','default'], min=permittedValues['rolloffParab', 'low'], max=permittedValues['rolloffParab', 'high'], step=permittedValues['rolloffParab','step']),
                                                                        shinyBS:::bsPopover(id='rolloffParab', title=NULL, content='Parabolic boost to the first ... harmonics, dB', placement="right", trigger="hover"),
                                                                        sliderInput('rolloffParabHarm', 'Harmonics boosted', value=permittedValues['rolloffParabHarm','default'], min=permittedValues['rolloffParabHarm', 'low'], max=permittedValues['rolloffParabHarm', 'high'], step=permittedValues['rolloffParabHarm','step']),
                                                                        shinyBS:::bsPopover(id='rolloffParabHarm', title=NULL, content='Apply a parabolic boost to ... harmonics. See manual for demo', placement="right", trigger="hover")
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
                                               shinyBS:::bsPopover(id='nonlinBalance', title=NULL, content='3 regimes of nonlinear effects: none / subharmonics / subharmonics + jitter', placement="right", trigger="hover"),
                                               sliderInput('nonlinDep', 'Depth of nonlinear effects, % *hyper*', value=permittedValues['nonlinDep','default'], min=permittedValues['nonlinDep', 'low'], max=permittedValues['nonlinDep', 'high'], step=permittedValues['nonlinDep','step']),
                                               shinyBS:::bsPopover(id='nonlinDep', title=NULL, content='Modulates the strength of nonlinear effects specified in "Advanced" below, when these effects are added', placement="right", trigger="hover"),
                                               sliderInput('shortestEpoch', 'Shortest epoch length', value=permittedValues['shortestEpoch','default'], min=permittedValues['shortestEpoch', 'low'], max=permittedValues['shortestEpoch', 'high'], step=permittedValues['shortestEpoch','step']),
                                               shinyBS:::bsPopover(id='shortestEpoch', title=NULL, content='Change nonlinear regime no sooner than after ... ms', placement="right", trigger="hover"),
                                               shinyBS::bsCollapsePanel("Advanced",
                                                                        sliderInput('subFreq', 'Target subharmonic frequency, Hz', value=permittedValues['subFreq','default'], min=permittedValues['subFreq', 'low'], max=permittedValues['subFreq', 'high'], step=permittedValues['subFreq','step']),
                                                                        shinyBS:::bsPopover(id='subFreq', title=NULL, content='The approximate target frequency of subharmonics; the actual frequency is forced to be a fraction of f0 at every time point', placement="right", trigger="hover"),
                                                                        sliderInput('subDep', 'Width of sidebands, Hz', value=permittedValues['subDep','default'], min=permittedValues['subDep', 'low'], max=permittedValues['subDep', 'high'], step=permittedValues['subDep','step']),
                                                                        shinyBS:::bsPopover(id='subDep', title=NULL, content='Width of subharmonic sidebands, ie the strength of subharmonics depending on their distance from F-harmonics', placement="right", trigger="hover"),
                                                                        sliderInput('jitterDep', 'Jitter depth, semitones', value=permittedValues['jitterDep','default'], min=permittedValues['jitterDep', 'low'], max=permittedValues['jitterDep', 'high'], step=permittedValues['jitterDep','step']),
                                                                        shinyBS:::bsPopover(id='jitterDep', title=NULL, content='Random variation in F0 per glottal cycle', placement="right", trigger="hover"),
                                                                        sliderInput('jitterLen', 'Jitter period, ms', value=permittedValues['jitterLen','default'], min=permittedValues['jitterLen', 'low'], max=permittedValues['jitterLen', 'high'], step=permittedValues['jitterLen','step']),
                                                                        shinyBS:::bsPopover(id='jitterLen', title=NULL, content='The pitch jumps every ... ms. Low ~ harsh noise, high ~ shaky voice', placement="right", trigger="hover"),
                                                                        sliderInput('shimmerDep', 'Shimmer depth, %', value=permittedValues['shimmerDep','default'], min=permittedValues['shimmerDep', 'low'], max=permittedValues['shimmerDep', 'high'], step=permittedValues['shimmerDep','step']),
                                                                        shinyBS:::bsPopover(id='shimmerDep', title=NULL, content='Random variation in amplitude per glottal cycle', placement="right", trigger="hover")
                                               ), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotNonlin'), width=6
                                             )
                                           )
                                  ),

                                  tabPanel("Unvoiced timing",
                                           sidebarLayout(
                                             sidebarPanel(
                                               actionButton(inputId = "noise_flatten", label = "Flatten contour"),
                                               shinyBS:::bsPopover(id='noise_flatten', title=NULL, content='Revert to a flat contour with amplitude equal to the first (left) anchor', placement="right", trigger="hover"),
                                               tableOutput("noise_anchors"), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotUnvoiced', click = "plotUnvoiced_click", dblclick = dblclickOpts(id = "plotUnvoiced_dblclick")),
                                               sliderInput('noiseTime', 'Breathing start / end, ms', value=c(0, 300), min=-permittedValues['sylLen', 'high'] / 2, max=permittedValues['sylLen', 'high'], step=permittedValues['sylLen','step']),
                                               shinyBS:::bsPopover(id='noiseTime', title=NULL, content='Timing of respiration noise relative to the voiced component', placement="right", trigger="hover"), width=6
                                             )
                                           )
                                  )
                       ),

                       navbarMenu("Tract",
                                  tabPanel("Formants",
                                           sidebarLayout(
                                             sidebarPanel(
                                               sliderInput('formantDep', 'Formant prominence *hyper*', value=permittedValues['formantDep','default'], min=permittedValues['formantDep', 'low'], max=permittedValues['formantDep', 'high'], step=permittedValues['formantDep','step']),
                                               shinyBS:::bsPopover(id='formantDep', title=NULL, content='Multiply formant amplitudes by ... (>1 = emphasize vowel quality)', placement="right", trigger="hover"),

                                               textInput('vowelString', label='String of vowel presets *hyper*', value = "a", width = NULL, placeholder ='uaaao'),
                                               shinyBS:::bsPopover(id='vowelString', title=NULL, content="Implemented presets: a, o, i, e, u, 0 (schwa)", placement="right", trigger="hover"),
                                               shinyBS::bsCollapsePanel("Show & modify formants manually",
                                                                        tags$style(type="text/css", "textarea {width:100%}"), # NB: this little hack ties the width of the following textarea to the width of the panel in which it is embedded; see http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                                                                        tags$textarea(id="formants", label='Exact formants', rows=10, cols=20, value=NA, placeholder ="list()")
                                               ),
                                               shinyBS::bsCollapsePanel("Advanced",
                                                                        checkboxInput(inputId = 'estimateVTL', label = 'Estimate vocal tract length from formants?', value = TRUE),
                                                                        shinyBS:::bsPopover(id='estimateVTL', title=NULL, content='If TRUE, user-specified formants trump user-specified vocal tract length', placement="right", trigger="hover"),
                                                                        sliderInput('vocalTract', 'The length of vocal tract, cm', value=permittedValues['vocalTract', 'default'], min=permittedValues['vocalTract', 'low'], max=permittedValues['vocalTract', 'high'], step=permittedValues['vocalTract', 'step']),
                                                                        shinyBS:::bsPopover(id='vocalTract', title=NULL, content='Affects default formant dispersion at temperature>0', placement="right", trigger="hover"),
                                                                        sliderInput('formantDepStoch', 'Added formants, dB', value=permittedValues['formantDepStoch','default'], min=permittedValues['formantDepStoch', 'low'], max=permittedValues['formantDepStoch', 'high'], step=permittedValues['formantDepStoch','step']),
                                                                        shinyBS:::bsPopover(id='formantDepStoch', title=NULL, content='Amplitude of extra formants added on top of user-specified ones based on the length of vocal tract', placement="right", trigger="hover"),
                                                                        sliderInput('rolloffLip', 'Lip radiation, dB/oct', value=permittedValues['rolloffLip','default'], min=permittedValues['rolloffLip', 'low'], max=permittedValues['rolloffLip', 'high'], step=permittedValues['rolloffLip','step']),
                                                                        shinyBS:::bsPopover(id='rolloffLip', title=NULL, content='Rolloff due to lip radiation', placement="right", trigger="hover"),
                                                                        sliderInput('rolloffNose', 'Nose radiation, dB/oct', value=permittedValues['rolloffNose','default'], min=permittedValues['rolloffNose', 'low'], max=permittedValues['rolloffNose', 'high'], step=permittedValues['rolloffNose','step']),
                                                                        shinyBS:::bsPopover(id='rolloffNose', title=NULL, content='Rolloff due to nose radiation: added instead of lip radiation when the mouth is closed', placement="right", trigger="hover")
                                               ), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotFormants'),
                                               fluidRow(
                                                 radioButtons(inputId='formants_spectrogram_or_spectrum', label="Filter preview (voiced)", choices=c("Spectrogram"="spectrogram", "Spectrum"="spectrum"), selected='spectrum', inline=TRUE, width=NULL)
                                               ), width=6
                                             )
                                           )
                                  ),

                                  tabPanel("Mouth opening",
                                           sidebarLayout(
                                             sidebarPanel(
                                               actionButton(inputId = "mouth_flatten", label = "Flatten mouth opening contour"),
                                               shinyBS:::bsPopover(id='mouth_flatten', title=NULL, content='Revert to a flat mouth opening contour with opening degree equal at the first (left) anchor', placement="right", trigger="hover"),
                                               tableOutput("mouth_anchors"),
                                               sliderInput('mouthOpenThres', 'Open lips at', value=permittedValues['mouthOpenThres','default'], min=permittedValues['mouthOpenThres', 'low'], max=permittedValues['mouthOpenThres', 'high'], step=permittedValues['mouthOpenThres','step']),
                                               shinyBS:::bsPopover(id='mouthOpenThres', title=NULL, content='The degree of mouth opening at which lips separate and start to radiate', placement="right", trigger="hover"),
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
                                               shinyBS:::bsPopover(id='noiseType', title=NULL, content="Breathing = glottal noise (same formants as for voiced part); snuffling = breathing through the nose; h / s / sh / f = sibilants", placement="right", trigger="hover"),
                                               sliderInput('rolloffNoise', 'Noise rolloff, dB/oct', value=permittedValues['rolloffNoise','default'], min=permittedValues['rolloffNoise', 'low'], max=permittedValues['rolloffNoise', 'high'], step=permittedValues['rolloffNoise','step']),
                                               shinyBS:::bsPopover(id='rolloffNoise', title=NULL, content='Rolloff of the noise component (affects both breathing and supra-glottal noise)', placement="right", trigger="hover"),
                                               shinyBS::bsCollapsePanel("Show & modify formants manually",
                                                                        tags$style(type="text/css", "textarea {width:100%}"), # NB: this little hack ties the width of the following textarea to the width of the panel in which it is embedded; see http://stackoverflow.com/questions/32640875/r-shiny-tie-textarea-width-to-wellpanel-width
                                                                        tags$textarea(id="formantsNoise", label='Exact formants for unvoiced part', rows=10, cols=20, value="", placeholder ="list()")
                                               ), width=6
                                             ),
                                             mainPanel(
                                               plotOutput('plotConsonant'),
                                               fluidRow(
                                                 radioButtons(inputId='formantsNoise_spectrogram_or_spectrum', label="Filter preview (unvoiced)", choices=c("Spectrogram"="spectrogram", "Spectrum"="spectrum"), selected='spectrum', inline=TRUE, width=NULL)
                                               ), width=6
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
                    uiOutput("myAudio")
             )
           ),
           fluidRow(
             plotOutput('spectrogram')
           ),
           fluidRow(
             radioButtons(inputId='spectrogram_or_spectrum', label="Generated sound", choices=c("Spectrogram"="spectrogram", "Spectrum"="spectrum"), selected='spectrogram', inline=TRUE, width=NULL)
           ),
           fluidRow(
             sliderInput('specWindowLength', 'Window length, ms', value=permittedValues['specWindowLength','default'], min=permittedValues['specWindowLength', 'low'], max=permittedValues['specWindowLength', 'high'], step=permittedValues['specWindowLength','step'])
           ),
           fluidRow(
             shinyBS::bsCollapse(id="spec_controls",
                                 shinyBS::bsCollapsePanel("Show spectrogram controls",
                                                          shinyBS:::bsPopover(id='specWindowLength', title=NULL, content='Window length for FFT transform (Gaussian)', placement="below", trigger="hover"),
                                                          sliderInput('spec_ylim', 'Frequency range, kHz', value=c(0,5), min=0, max=22, step=1),
                                                          radioButtons(inputId='spec_colorTheme', label="Color scheme", choices=c("Seewave"="seewave", "Heat"="heat.colors", "Black & white"="bw"), selected='bw', inline=TRUE, width=NULL),
                                                          radioButtons(inputId='spec_method', label="Method", choices=c("Spectrum"="spectrum", "Spectral derivative"="spectralDerivative"), selected='spectrum', inline=TRUE, width=NULL),
                                                          sliderInput('specContrast', 'Contrast', value=permittedValues['specContrast','default'], min=permittedValues['specContrast', 'low'], max=permittedValues['specContrast', 'high'], step=permittedValues['specContrast','step']),
                                                          shinyBS:::bsPopover(id='specContrast', title=NULL, content='Regulates the contrast of the spectrogram', placement="below", trigger="hover"),
                                                          sliderInput('specBrightness', 'Brightness', value=permittedValues['specBrightness','default'], min=permittedValues['specBrightness', 'low'], max=permittedValues['specBrightness', 'high'], step=permittedValues['specBrightness','step']),
                                                          shinyBS:::bsPopover(id='specBrightness', title=NULL, content='Regulates the brightness of the spectrogram', placement="below", trigger="hover")
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
    )
  )
)
