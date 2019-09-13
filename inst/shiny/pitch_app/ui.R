ui = fluidPage(
  # headerPanel('...'),

  fluidRow(
    column(3,
           tabsetPanel(id='parGroup',
                       navbarMenu("In",
                                  tabPanel("STFT",
                                           sliderInput('windowLength', 'Window length, ms', value=defaults_analyze['windowLength','default'], min=defaults_analyze['windowLength', 'low'], max=defaults_analyze['windowLength', 'high'], step=defaults_analyze['windowLength','step']),
                                           sliderInput('overlap', 'Overlap, %', value=defaults_analyze['overlap','default'], min=defaults_analyze['overlap', 'low'], max=defaults_analyze['overlap', 'high'], step=defaults_analyze['overlap','step']),
                                           sliderInput('dynamicRange', 'Dynamic range, dB', value=defaults_analyze['dynamicRange','default'], min=defaults_analyze['dynamicRange', 'low'], max=defaults_analyze['dynamicRange', 'high'], step=defaults_analyze['dynamicRange','step']),
                                           sliderInput('zp', 'Zero padding, points 2 ^ n', value=defaults_analyze['zp','default'], min=defaults_analyze['zp','low'], max=defaults_analyze['zp','high'], step=defaults_analyze['zp','step']),
                                           selectInput('wn', 'Window type', choices = c('bartlett', 'blackman', 'flattop', 'gaussian', 'hamming', 'hanning', 'rectangle'), selected = 'gaussian', multiple = FALSE)
                                  ),
                                  tabPanel("Voicing",
                                           sliderInput('silence', 'Silence threshold', value=defaults_analyze['silence', 'default'], min=defaults_analyze['silence', 'low'], max=defaults_analyze['silence', 'high'], step=defaults_analyze['silence', 'step']),
                                           sliderInput('entropyThres', 'Entropy threshold', value=defaults_analyze['entropyThres', 'default'], min=defaults_analyze['entropyThres', 'low'], max=defaults_analyze['entropyThres', 'high'], step=defaults_analyze['entropyThres', 'step']),
                                           sliderInput('nCands', 'Candidates per method', value=defaults_analyze['nCands', 'default'], min=defaults_analyze['nCands', 'low'], max=defaults_analyze['nCands', 'high'], step=defaults_analyze['nCands', 'step']),
                                           sliderInput('minVoicedCands', 'Min candidates for voiced', value=defaults_analyze['minVoicedCands', 'default'], min=defaults_analyze['minVoicedCands', 'low'], max=defaults_analyze['minVoicedCands', 'high'], step=defaults_analyze['minVoicedCands', 'step'])
                                  ),

                                  tabPanel("Priors",
                                           sliderInput('pitchFloor', 'Pitch floor, Hz', value=defaults_analyze['pitchFloor', 'default'], min=defaults_analyze['pitchFloor', 'low'], max=defaults_analyze['pitchFloor', 'high'], step=defaults_analyze['pitchFloor', 'step']),
                                           sliderInput('pitchCeiling', 'Pitch ceiling, Hz', value=defaults_analyze['pitchCeiling', 'default'], min=defaults_analyze['pitchCeiling', 'low'], max=defaults_analyze['pitchCeiling', 'high'], step=defaults_analyze['pitchCeiling', 'step']),
                                           sliderInput('priorMean', 'Expected pitch (priorMean), Hz', value=defaults_analyze['priorMean', 'default'], min=defaults_analyze['priorMean', 'low'], max=defaults_analyze['priorMean', 'high'], step=defaults_analyze['priorMean', 'step']),
                                           sliderInput('priorSD', 'Expected range (priorSD), semitones', value=defaults_analyze['priorSD', 'default'], min=defaults_analyze['priorSD', 'low'], max=defaults_analyze['priorSD', 'high'], step=defaults_analyze['priorSD', 'step'])
                                  )
                       ),

                       navbarMenu("Trackers",
                                  tabPanel("Lowest dominant frequency",
                                           sliderInput('domThres', 'Dominant frequency threshold', value=defaults_analyze['domThres', 'default'], min=defaults_analyze['domThres', 'low'], max=defaults_analyze['domThres', 'high'], step=defaults_analyze['domThres', 'step']),
                                           sliderInput('domSmooth', 'Width of smoothing interval, Hz', value=defaults_analyze['domSmooth', 'default'], min=defaults_analyze['domSmooth', 'low'], max=defaults_analyze['domSmooth', 'high'], step=defaults_analyze['domSmooth', 'step'])
                                  ),

                                  tabPanel("Autocorrelation",
                                           sliderInput('autocorThres', 'Autocorrelation threshold', value=defaults_analyze['autocorThres', 'default'], min=defaults_analyze['autocorThres', 'low'], max=defaults_analyze['autocorThres', 'high'], step=defaults_analyze['autocorThres', 'step']),
                                           sliderInput('autocorSmooth', 'Width of smoothing interval, bins', value=defaults_analyze['autocorSmooth', 'default'], min=defaults_analyze['autocorSmooth', 'low'], max=defaults_analyze['autocorSmooth', 'high'], step=defaults_analyze['autocorSmooth', 'step'])
                                  ),

                                  tabPanel("Cepstrum",
                                           sliderInput('cepThres', 'Cepstrum threshold', value=defaults_analyze['cepThres', 'default'], min=defaults_analyze['cepThres', 'low'], max=defaults_analyze['cepThres', 'high'], step=defaults_analyze['cepThres', 'step']),
                                           sliderInput('cepSmooth', 'Width of smoothing interval, Hz', value=defaults_analyze['cepSmooth', 'default'], min=defaults_analyze['cepSmooth', 'low'], max=defaults_analyze['cepSmooth', 'high'], step=defaults_analyze['cepSmooth', 'step']),
                                           sliderInput('cepZp', 'Cepstral zero padding, 2 ^ n', value=defaults_analyze['cepZp', 'default'], min=defaults_analyze['cepZp', 'low'], max=defaults_analyze['cepZp', 'high'], step=defaults_analyze['cepZp', 'step'])
                                  ),

                                  tabPanel("Ratio of harmonics",
                                           sliderInput('specThres', 'Spectral threshold', value=defaults_analyze['specThres', 'default'], min=defaults_analyze['specThres', 'low'], max=defaults_analyze['specThres', 'high'], step=defaults_analyze['specThres', 'step']),
                                           sliderInput('specPeak', 'Spectral peak height', value=defaults_analyze['specPeak', 'default'], min=defaults_analyze['specPeak', 'low'], max=defaults_analyze['specPeak', 'high'], step=defaults_analyze['specPeak', 'step']),
                                           sliderInput('specHNRslope', 'Slope of HNR discount', value=defaults_analyze['specHNRslope', 'default'], min=defaults_analyze['specHNRslope', 'low'], max=defaults_analyze['specHNRslope', 'high'], step=defaults_analyze['specHNRslope', 'step']),
                                           sliderInput('specSmooth', 'Width of window for finding harmonics, Hz', value=defaults_analyze['specSmooth', 'default'], min=defaults_analyze['specSmooth', 'low'], max=defaults_analyze['specSmooth', 'high'], step=defaults_analyze['specSmooth', 'step']),
                                           sliderInput('specMerge', 'Margin for merging candidates, Hz', value=defaults_analyze['specMerge', 'default'], min=defaults_analyze['specMerge', 'low'], max=defaults_analyze['specMerge', 'high'], step=defaults_analyze['specMerge', 'step']),
                                           sliderInput('specSinglePeakCert', 'Certainty of single-ratio candidates', value=defaults_analyze['specSinglePeakCert', 'default'], min=defaults_analyze['specSinglePeakCert', 'low'], max=defaults_analyze['specSinglePeakCert', 'high'], step=defaults_analyze['specSinglePeakCert', 'step'])
                                  )
                       ),

                       navbarMenu("Out",
                                  tabPanel("Path",
                                           selectInput('pathfinding', 'Pathfinding method', choices = c('none', 'fast', 'slow'), selected = 'fast', multiple = FALSE),
                                           sliderInput('certWeight', 'Certainty weight', value=defaults_analyze['certWeight', 'default'], min=defaults_analyze['certWeight', 'low'], max=defaults_analyze['certWeight', 'high'], step=defaults_analyze['certWeight', 'step']),
                                           sliderInput('shortestSyl', 'Shortest syllable, ms', value=defaults_analyze['shortestSyl', 'default'], min=defaults_analyze['shortestSyl', 'low'], max=defaults_analyze['shortestSyl', 'high'], step=defaults_analyze['shortestSyl', 'step']),
                                           sliderInput('shortestPause', 'Shortest pause, ms', value=defaults_analyze['shortestPause', 'default'], min=defaults_analyze['shortestPause', 'low'], max=defaults_analyze['shortestPause', 'high'], step=defaults_analyze['shortestPause', 'step'])
                                  ),

                                  tabPanel("Smoothing",
                                           sliderInput ('smooth', 'Median smoothing (0 = none)', value=defaults_analyze['smooth','default'], min=defaults_analyze['smooth', 'low'], max=defaults_analyze['smooth', 'high'], step=defaults_analyze['smooth','step']),
                                           sliderInput('interpolWin', 'Interpolation window, ms (0 = none)', value=defaults_analyze['interpolWin', 'default'], min=defaults_analyze['interpolWin', 'low'], max=defaults_analyze['interpolWin', 'high'], step=defaults_analyze['interpolWin', 'step']),
                                           sliderInput('interpolTol', 'Interpolation tolerance', value=defaults_analyze['interpolTol', 'default'], min=defaults_analyze['interpolTol', 'low'], max=defaults_analyze['interpolTol', 'high'], step=defaults_analyze['interpolTol', 'step']),
                                           sliderInput('interpolCert', 'Interpolation certainty', value=defaults_analyze['interpolCert', 'default'], min=defaults_analyze['interpolCert', 'low'], max=defaults_analyze['interpolCert', 'high'], step=defaults_analyze['interpolCert', 'step'])
                                  ),

                                  tabPanel("Plot",
                                           sliderInput('spec_ylim', 'Frequency range, kHz', value=c(0, defaults_analyze['spec_ylim','default']), min=defaults_analyze['spec_ylim', 'low'], max=defaults_analyze['spec_ylim','high'], step=defaults_analyze['spec_ylim','step']),
                                           sliderInput('spec_cex', 'Point size', value = 2, min = .25, max = 4, step = .25),
                                           radioButtons(inputId='spec_colorTheme', label="Color scheme", choices=c("Seewave"="seewave", "Heat"="heat.colors", "Black & white"="bw"), selected='bw', inline=TRUE, width=NULL),
                                           sliderInput('specContrast', 'Contrast', value=defaults_analyze['specContrast','default'], min=defaults_analyze['specContrast', 'low'], max=defaults_analyze['specContrast', 'high'], step=defaults_analyze['specContrast','step']),
                                           sliderInput('specBrightness', 'Brightness', value=defaults_analyze['specBrightness','default'], min=defaults_analyze['specBrightness', 'low'], max=defaults_analyze['specBrightness', 'high'], step=defaults_analyze['specBrightness','step'])
                                  )
                       )
           )
    ),

    column(9,
           fluidRow(
             column(3,
                    fileInput(inputId = "loadAudio", label = NULL, multiple = TRUE, buttonLabel = 'Load audio', placeholder = '...', width = "200px")
             ),
             column(4,
                    uiOutput("fileN"),
                    actionButton(inputId = "lastFile", label = "Last", style="background-color: lightgray;"),
                    actionButton(inputId = "nextFile", label = "Next", style="background-color: lightgray;")
             ),
             column(3,
                    uiOutput("myAudio")
             ),
             column(2,
                    downloadButton(outputId = "saveRes", label = "", style="color: blue; background-color: orange;"),
                    actionButton('about', label = '?'),
                    shinyBS:::bsPopover(id='about', title=NULL, content='Help', placement="right", trigger="hover")  # shinyBS has to be mentioned somewhere in ui, otherwise addTooltip doesn't work in server
             )
           ),
           fluidRow(
             checkboxGroupInput('pitchMethods', label = 'Pitch methods', choiceValues = c('dom', 'autocor', 'cep', 'spec'), choiceNames = c('Lowest dominant frequency', 'Autocorrelation', 'Cepstrum', 'Ratio of harmonics'), selected = c('dom', 'autocor'), inline = TRUE)
           ),
           fluidRow(
             column(3,
                    radioButtons('spectro_clickAct', label = 'Left click action: ', choiceNames = c('Add anchor', 'Select'), choiceValues = c('addCand', 'select'), selected = 'addCand', inline = TRUE)
             ),
             column(3,
                    htmlOutput('pitchAtCursor', inline = TRUE)
             ),
             column(3,
                    actionButton(inputId = "selection_play", label = HTML("<img src='icons/play.png' width = '20px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = "selection_unvoice", label = HTML("<img src='icons/unvoice.png' width = '20px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = "selection_voice", label = HTML("<img src='icons/voice.png' width = '20px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = "selection_octaveUp", label = HTML("<img src='icons/octaveUp.png' width = '20px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = "selection_octaveDown", label = HTML("<img src='icons/octaveDown.png' width = '20px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = "selection_setPrior", label = "Prior", style = "padding: 2px 2px;")
             ),
             column(3,
                    actionButton(inputId = 'scrollLeft', label = HTML("<img src='icons/backward.png' width = '20px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = 'zoomOut', label = HTML("<img src='icons/zoomOut.png' width = '20px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = "selection_zoomToSel", label = HTML("<img src='icons/zoomSel.png' width = '20px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = 'zoomIn', label = HTML("<img src='icons/zoomIn.png' width = '20px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = 'scrollRight', label = HTML("<img src='icons/forward.png' width = '20px'>"), style = "padding: 2px 2px;")
             )
           ),

           fluidRow(
             plotOutput('spectrogram', click = "spectrogram_click", dblclick = dblclickOpts(id = "spectrogram_dblclick"), hover = hoverOpts(id = "spectrogram_hover"), brush = brushOpts(id = 'spectrogram_brush', resetOnNew = TRUE))  # , style = "max-width: 66vw; overflow-x: auto;"
           ),

           fluidRow(
             # status bar here
           )
    )
  )
)
