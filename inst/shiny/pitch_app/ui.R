ui = fluidPage(
  # headerPanel('...'),

  fluidRow(
    column(4,
           tabsetPanel(id='parGroup',
                       navbarMenu("Input",
                                  tabPanel("STFT",
                                           sliderInput('windowLength', 'Window length, ms', value=defaults_analyze['windowLength','default'], min=defaults_analyze['windowLength', 'low'], max=defaults_analyze['windowLength', 'high'], step=defaults_analyze['windowLength','step']),
                                           shinyBS:::bsPopover(id='windowLength', title=NULL, content='Length of STFT window, ms. Larger values improve frequency resolution at the expense of time resolution', placement="right", trigger="hover"),
                                           sliderInput('overlap', 'Overlap, %', value=defaults_analyze['overlap','default'], min=defaults_analyze['overlap', 'low'], max=defaults_analyze['overlap', 'high'], step=defaults_analyze['overlap','step']),
                                           shinyBS:::bsPopover(id='overlap', title=NULL, content='Overlap between analysis frames, %', placement="right", trigger="hover"),
                                           sliderInput('dynamicRange', 'Dynamic range, dB', value=defaults_analyze['dynamicRange','default'], min=defaults_analyze['dynamicRange', 'low'], max=defaults_analyze['dynamicRange', 'high'], step=defaults_analyze['dynamicRange','step']),
                                           shinyBS:::bsPopover(id='dynamicRange', title=NULL, content='Dynamic range of spectrogram, dB', placement="right", trigger="hover"),
                                           sliderInput('zp', 'Zero padding, points 2 ^ n', value=defaults_analyze['zp','default'], min=defaults_analyze['zp','low'], max=defaults_analyze['zp','high'], step=defaults_analyze['zp','step']),
                                           shinyBS:::bsPopover(id='zp', title=NULL, content='Zero padding of STFT window (improves frequency resolution): 8 means 2^8 = 256, etc.', placement="right", trigger="hover"),
                                           selectInput('wn', 'Window type', choices = c('bartlett', 'blackman', 'flattop', 'gaussian', 'hamming', 'hanning', 'rectangle'), selected = 'gaussian', multiple = FALSE),
                                           shinyBS:::bsPopover(id='wn', title=NULL, content='Type of STFT window', placement="right", trigger="hover")
                                  )
                       ),

                       navbarMenu("Candidates",
                                  tabPanel("General",
                                           sliderInput('silence', 'Silence threshold', value=defaults_analyze['silence', 'default'], min=defaults_analyze['silence', 'low'], max=defaults_analyze['silence', 'high'], step=defaults_analyze['silence', 'step']),
                                           shinyBS:::bsPopover(id='silence', title=NULL, content='Frames with RMS below silence threshold are not analyzed', placement="right", trigger="hover"),
                                           sliderInput('entropyThres', 'Entropy threshold', value=defaults_analyze['entropyThres', 'default'], min=defaults_analyze['entropyThres', 'low'], max=defaults_analyze['entropyThres', 'high'], step=defaults_analyze['entropyThres', 'step']),
                                           shinyBS:::bsPopover(id='entropyThres', title=NULL, content='Frames with Weiner entropy above entropy threshold are ignored when searching for pitch candidates', placement="right", trigger="hover"),
                                           sliderInput('nCands', 'Candidates per method', value=defaults_analyze['nCands', 'default'], min=defaults_analyze['nCands', 'low'], max=defaults_analyze['nCands', 'high'], step=defaults_analyze['nCands', 'step']),
                                           shinyBS:::bsPopover(id='nCands', title=NULL, content='Maximum number of pitch candidates to use per method', placement="right", trigger="hover"),
                                           sliderInput('minVoicedCands', 'Min candidates for voiced', value=defaults_analyze['minVoicedCands', 'default'], min=defaults_analyze['minVoicedCands', 'low'], max=defaults_analyze['minVoicedCands', 'high'], step=defaults_analyze['minVoicedCands', 'step']),
                                           shinyBS:::bsPopover(id='minVoicedCands', title=NULL, content='Minimum number of pitch candidates that have to be defined to consider a frame voiced', placement="right", trigger="hover")
                                  ),

                                  tabPanel("Priors",
                                           sliderInput('pitchFloor', 'Pitch floor, Hz', value=defaults_analyze['pitchFloor', 'default'], min=defaults_analyze['pitchFloor', 'low'], max=defaults_analyze['pitchFloor', 'high'], step=defaults_analyze['pitchFloor', 'step']),
                                           shinyBS:::bsPopover(id='pitchFloor', title=NULL, content='No candidates below this absolute threshold', placement="right", trigger="hover"),
                                           sliderInput('pitchCeiling', 'Pitch ceiling, Hz', value=defaults_analyze['pitchCeiling', 'default'], min=defaults_analyze['pitchCeiling', 'low'], max=defaults_analyze['pitchCeiling', 'high'], step=defaults_analyze['pitchCeiling', 'step']),
                                           shinyBS:::bsPopover(id='pitchCeiling', title=NULL, content='No candidates above this absolute threshold', placement="right", trigger="hover"),
                                           sliderInput('priorMean', 'Expected pitch (priorMean), Hz', value=defaults_analyze['priorMean', 'default'], min=defaults_analyze['priorMean', 'low'], max=defaults_analyze['priorMean', 'high'], step=defaults_analyze['priorMean', 'step']),
                                           shinyBS:::bsPopover(id='priorMean', title=NULL, content='Candidates close to this value are prioritized (how close is determined by priorSD)', placement="right", trigger="hover"),
                                           sliderInput('priorSD', 'Expected range (priorSD), semitones', value=defaults_analyze['priorSD', 'default'], min=defaults_analyze['priorSD', 'low'], max=defaults_analyze['priorSD', 'high'], step=defaults_analyze['priorSD', 'step']),
                                           shinyBS:::bsPopover(id='priorSD', title=NULL, content='Determines the width of expected pitch range (standard deviation of gamma distribution around priorMean)', placement="right", trigger="hover")
                                  ),

                                  tabPanel("Lowest dominant frequency",
                                           sliderInput('domThres', 'Dominant frequency threshold', value=defaults_analyze['domThres', 'default'], min=defaults_analyze['domThres', 'low'], max=defaults_analyze['domThres', 'high'], step=defaults_analyze['domThres', 'step']),
                                           shinyBS:::bsPopover(id='domThres', title=NULL, content='Minimum amplitude of dominant frequency', placement="right", trigger="hover"),
                                           sliderInput('domSmooth', 'Width of smoothing interval, bins', value=defaults_analyze['domSmooth', 'default'], min=defaults_analyze['domSmooth', 'low'], max=defaults_analyze['domSmooth', 'high'], step=defaults_analyze['domSmooth', 'step']),
                                           shinyBS:::bsPopover(id='domSmooth', title=NULL, content='Width of smoothing interval (in bins) for finding the lowest dominant frequency band', placement="right", trigger="hover")
                                  ),

                                  tabPanel("Autocorrelation",
                                           sliderInput('autocorThres', 'Autocorrelation threshold', value=defaults_analyze['autocorThres', 'default'], min=defaults_analyze['autocorThres', 'low'], max=defaults_analyze['autocorThres', 'high'], step=defaults_analyze['autocorThres', 'step']),
                                           shinyBS:::bsPopover(id='autocorThres', title=NULL, content='Voicing threshold for autocorrelation algorithm', placement="right", trigger="hover"),
                                           sliderInput('autocorSmooth', 'Width of smoothing interval, bins', value=defaults_analyze['autocorSmooth', 'default'], min=defaults_analyze['autocorSmooth', 'low'], max=defaults_analyze['autocorSmooth', 'high'], step=defaults_analyze['autocorSmooth', 'step']),
                                           shinyBS:::bsPopover(id='autocorSmooth', title=NULL, content='Width of smoothing interval (in bins) for finding peaks in the autocorrelation function', placement="right", trigger="hover")
                                  ),

                                  tabPanel("Cepstrum",
                                           sliderInput('cepThres', 'Cepstrum threshold', value=defaults_analyze['cepThres', 'default'], min=defaults_analyze['cepThres', 'low'], max=defaults_analyze['cepThres', 'high'], step=defaults_analyze['cepThres', 'step']),
                                           shinyBS:::bsPopover(id='cepThres', title=NULL, content='Voicing threshold for cepstral algorithm', placement="right", trigger="hover"),
                                           sliderInput('cepSmooth', 'Width of smoothing interval, bins', value=defaults_analyze['cepSmooth', 'default'], min=defaults_analyze['cepSmooth', 'low'], max=defaults_analyze['cepSmooth', 'high'], step=defaults_analyze['cepSmooth', 'step']),
                                           shinyBS:::bsPopover(id='cepSmooth', title=NULL, content='Width of smoothing interval (in bins) for finding peaks in the cepstrum', placement="right", trigger="hover"),
                                           sliderInput('cepZp', 'Cepstral zero padding, 2 ^ n', value=defaults_analyze['cepZp', 'default'], min=defaults_analyze['cepZp', 'low'], max=defaults_analyze['cepZp', 'high'], step=defaults_analyze['cepZp', 'step']),
                                           shinyBS:::bsPopover(id='cepZp', title=NULL, content='Length of cepstral window after zero padding: 8 means 2^8 = 256, etc.', placement="right", trigger="hover")
                                  ),

                                  tabPanel("Ratio of harmonics",
                                           sliderInput('specThres', 'Spectral threshold', value=defaults_analyze['specThres', 'default'], min=defaults_analyze['specThres', 'low'], max=defaults_analyze['specThres', 'high'], step=defaults_analyze['specThres', 'step']),
                                           shinyBS:::bsPopover(id='specThres', title=NULL, content='Voicing threshold for Ba-Na algorithm', placement="right", trigger="hover"),
                                           sliderInput('specPeak', 'Spectral peak height', value=defaults_analyze['specPeak', 'default'], min=defaults_analyze['specPeak', 'low'], max=defaults_analyze['specPeak', 'high'], step=defaults_analyze['specPeak', 'step']),
                                           shinyBS:::bsPopover(id='specPeak', title=NULL, content='Minimum amplitude of harmonics considered pitch candidates', placement="right", trigger="hover"),
                                           sliderInput('specHNRslope', 'Slope of HNR discount', value=defaults_analyze['specHNRslope', 'default'], min=defaults_analyze['specHNRslope', 'low'], max=defaults_analyze['specHNRslope', 'high'], step=defaults_analyze['specHNRslope', 'step']),
                                           shinyBS:::bsPopover(id='specHNRslope', title=NULL, content='0 = same threshold regardless of HNR; positive = lower threshold in noisy sounds', placement="right", trigger="hover"),
                                           sliderInput('specSmooth', 'Width of window for finding harmonics, Hz', value=defaults_analyze['specSmooth', 'default'], min=defaults_analyze['specSmooth', 'low'], max=defaults_analyze['specSmooth', 'high'], step=defaults_analyze['specSmooth', 'step']),
                                           shinyBS:::bsPopover(id='specSmooth', title=NULL, content='Width of window for detecting harmonics in the spectrum, Hz', placement="right", trigger="hover"),
                                           sliderInput('specMerge', 'Margin for merging candidates, Hz', value=defaults_analyze['specMerge', 'default'], min=defaults_analyze['specMerge', 'low'], max=defaults_analyze['specMerge', 'high'], step=defaults_analyze['specMerge', 'step']),
                                           shinyBS:::bsPopover(id='specMerge', title=NULL, content='Pitch candidates within specMerge semitones are merged with boosted certainty', placement="right", trigger="hover"),
                                           sliderInput('specSinglePeakCert', 'Certainty of single-ratio candidates', value=defaults_analyze['specSinglePeakCert', 'default'], min=defaults_analyze['specSinglePeakCert', 'low'], max=defaults_analyze['specSinglePeakCert', 'high'], step=defaults_analyze['specSinglePeakCert', 'step']),
                                           shinyBS:::bsPopover(id='specSinglePeakCert', title=NULL, content='If pitch is calculated based on a single harmonic ratio (as opposed to several ratios converging on the same candidate), its certainty is taken to be specSinglePeakCert', placement="right", trigger="hover")
                                  )
                       ),

                       navbarMenu("Postprocessing",
                                  tabPanel("Path",
                                           selectInput('pathfinding', 'Pathfinding method', choices = c('none', 'fast', 'slow'), selected = 'fast', multiple = FALSE),
                                           shinyBS:::bsPopover(id='pathfinding', title=NULL, content="Method of finding the optimal path through pitch candidates: 'none' = best candidate per frame, 'fast' = simple heuristic, 'slow' = annealing (initial analysis only)", placement="right", trigger="hover"),
                                           sliderInput('certWeight', 'Certainty weight', value=defaults_analyze['certWeight', 'default'], min=defaults_analyze['certWeight', 'low'], max=defaults_analyze['certWeight', 'high'], step=defaults_analyze['certWeight', 'step']),
                                           shinyBS:::bsPopover(id='certWeight', title=NULL, content='Specifies how much we prioritize the certainty of pitch candidates vs. pitch jumps', placement="right", trigger="hover"),
                                           sliderInput('shortestSyl', 'Shortest syllable, ms', value=defaults_analyze['shortestSyl', 'default'], min=defaults_analyze['shortestSyl', 'low'], max=defaults_analyze['shortestSyl', 'high'], step=defaults_analyze['shortestSyl', 'step']),
                                           shinyBS:::bsPopover(id='shortestSyl', title=NULL, content='Shorter voiced segments (ms) will be treated as voiceless or merged with longer segments', placement="right", trigger="hover"),
                                           sliderInput('shortestPause', 'Shortest pause, ms', value=defaults_analyze['shortestPause', 'default'], min=defaults_analyze['shortestPause', 'low'], max=defaults_analyze['shortestPause', 'high'], step=defaults_analyze['shortestPause', 'step']),
                                           shinyBS:::bsPopover(id='shortestPause', title=NULL, content="The smallest gap between voiced syllables (ms) that means they shouldn't be merged", placement="right", trigger="hover")
                                  ),

                                  tabPanel("Smoothing",
                                           sliderInput ('smooth', 'Median smoothing (0 = none)', value=defaults_analyze['smooth','default'], min=defaults_analyze['smooth', 'low'], max=defaults_analyze['smooth', 'high'], step=defaults_analyze['smooth','step']),
                                           shinyBS:::bsPopover(id='smooth', title=NULL, content='Amount of median smoothing', placement="right", trigger="hover"),
                                           sliderInput('snakeStep', 'Snake step (0 = none)', value=0, min=defaults_analyze['snakeStep', 'low'], max=defaults_analyze['snakeStep', 'high'], step=defaults_analyze['snakeStep', 'step']),
                                           shinyBS:::bsPopover(id='snakeStep', title=NULL, content='Use the snake algorithm to minimize the elastic force acting on pitch contour', placement="right", trigger="hover"),
                                           sliderInput('interpolWin', 'Interpolation window (0 = none)', value=defaults_analyze['interpolWin', 'default'], min=defaults_analyze['interpolWin', 'low'], max=defaults_analyze['interpolWin', 'high'], step=defaults_analyze['interpolWin', 'step']),
                                           shinyBS:::bsPopover(id='interpolWin', title=NULL, content="", placement="right", trigger="hover"),
                                           sliderInput('interpolTol', 'Interpolation tolerance', value=defaults_analyze['interpolTol', 'default'], min=defaults_analyze['interpolTol', 'low'], max=defaults_analyze['interpolTol', 'high'], step=defaults_analyze['interpolTol', 'step']),
                                           shinyBS:::bsPopover(id='interpolTol', title=NULL, content="", placement="right", trigger="hover"),
                                           sliderInput('interpolCert', 'Interpolation certainty', value=defaults_analyze['interpolCert', 'default'], min=defaults_analyze['interpolCert', 'low'], max=defaults_analyze['interpolCert', 'high'], step=defaults_analyze['interpolCert', 'step']),
                                           shinyBS:::bsPopover(id='interpolCert', title=NULL, content="", placement="right", trigger="hover")
                                  )
                       )
           )
    ),

    column(8,
           fluidRow(
             column(5,
                    fileInput(inputId = "loadAudio", label = NULL, multiple = TRUE, buttonLabel = 'Load audio', placeholder = 'No file selected')
             ),
             column(4,
                    uiOutput("myAudio")
             ),
             column(2,
                    downloadButton(outputId = "saveRes", label = "Save results", style="color: blue; background-color: orange;")
             ),
             column(1,
                    actionButton('about', label = '?')
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
                    htmlOutput('spectro_hover', inline = TRUE)
             ),
             column(2,
                    actionButton(inputId = "lastFile", label = "Last", style="background-color: lightgray;"),
                    actionButton(inputId = "nextFile", label = "Next", style="background-color: lightgray;")
             ),
             column(4,
                    actionButton(inputId = 'scrollLeft', label = '<'),
                    actionButton(inputId = 'zoomIn', label = '+'),
                    actionButton(inputId = 'zoomOut', label = '-'),
                    actionButton(inputId = 'scrollRight', label = '>')
             )
           ),

           fluidRow(
             plotOutput('spectrogram', click = "spectrogram_click", dblclick = dblclickOpts(id = "spectrogram_dblclick"), hover = hoverOpts(id = "spectrogram_hover"), brush = brushOpts(id = 'spectrogram_brush', resetOnNew = TRUE))  # , style = "max-width: 66vw; overflow-x: auto;"
           ),

           fluidRow(
             HTML('<h4>Operations with selection: '),
             actionButton(inputId = "selection_play", label = "Play", inline = TRUE),
             shinyBS:::bsPopover(id='selection_play', title=NULL, content='Play selection', placement="right", trigger="hover"),
             actionButton(inputId = "selection_unvoice", label = "Unvoice", inline = TRUE),
             shinyBS:::bsPopover(id='selection_unvoice', title=NULL, content='Treat selection as unvoiced', placement="right", trigger="hover"),
             actionButton(inputId = "selection_voice", label = "Undo\nunvoice", inline = TRUE),
             shinyBS:::bsPopover(id='selection_voice', title=NULL, content='Undo treating selection as unvoiced', placement="right", trigger="hover"),
             actionButton(inputId = "selection_octaveUp", label = "Octave\nUP", inline = TRUE),
             shinyBS:::bsPopover(id='selection_octaveUp', title=NULL, content='Raise pitch for selection by an octave', placement="right", trigger="hover"),
             actionButton(inputId = "selection_octaveDown", label = "Octave\nDOWN", inline = TRUE),
             shinyBS:::bsPopover(id='selection_octaveDown', title=NULL, content='Lower pitch for selection by an octave', placement="right", trigger="hover"),
             actionButton(inputId = "selection_setPrior", label = "Set prior", inline = TRUE),
             shinyBS:::bsPopover(id='selection_setPrior', title=NULL, content='Set a prior on expected pitch values corresponding to the selected frequency range', placement="right", trigger="hover"),
             actionButton(inputId = "selection_zoomToSel", label = "Zoom\nto sel", inline = TRUE),
             shinyBS:::bsPopover(id='selection_zoomToSel', title=NULL, content='Zoom to selection (time axis only)', placement="right", trigger="hover"),
             HTML('</h4>')
           ),

           fluidRow(
             shinyBS::bsCollapse(id="spec_controls",
                                 shinyBS::bsCollapsePanel("Show spectrogram controls",
                                                          column(6,
                                                                 sliderInput('spec_xlim', 'Time range, ms', value=c(0, 1000), min=0, max=1000, step=100),
                                                                 sliderInput('spec_ylim', 'Frequency range, kHz', value=c(0, defaults_analyze['spec_ylim','default']), min=defaults_analyze['spec_ylim', 'low'], max=defaults_analyze['spec_ylim','high'], step=defaults_analyze['spec_ylim','step'])
                                                          ),
                                                          column(6,
                                                                 radioButtons(inputId='spec_colorTheme', label="Color scheme", choices=c("Seewave"="seewave", "Heat"="heat.colors", "Black & white"="bw"), selected='bw', inline=TRUE, width=NULL),
                                                                 sliderInput('specContrast', 'Contrast', value=defaults_analyze['specContrast','default'], min=defaults_analyze['specContrast', 'low'], max=defaults_analyze['specContrast', 'high'], step=defaults_analyze['specContrast','step']),
                                                                 shinyBS:::bsPopover(id='specContrast', title=NULL, content='Regulates the contrast of the spectrogram', placement="below", trigger="hover"),
                                                                 sliderInput('specBrightness', 'Brightness', value=defaults_analyze['specBrightness','default'], min=defaults_analyze['specBrightness', 'low'], max=defaults_analyze['specBrightness', 'high'], step=defaults_analyze['specBrightness','step']),
                                                                 shinyBS:::bsPopover(id='specBrightness', title=NULL, content='Regulates the brightness of the spectrogram', placement="below", trigger="hover")
                                                          )
                                 )
             )
           )
    )
  )
)
