# pitch_app()
#
ui = fluidPage(
  # headerPanel('...'),
  tags$script('
    $(document).on("keydown", function (e) {
       Shiny.onInputChange("userPressedSmth", e.which + Math.random() / 3);
       // w/o Math.random() only the first of a series of identical
       // keydown events is sent to server()
    });

    // prevent spacebar from activating the last pressed button
    // see https://stackoverflow.com/questions/22280139/prevent-space-button-from-triggering-any-other-button-click-in-jquery
    $(document).keyup(function(event) {
      if(event.which === 32) {
  	    event.preventDefault();
      }
    });
  '),

  tags$head(
    tags$style(".buttonBlock {padding: 2px 2px; display: block}"),
    tags$style(".buttonInline {padding: 2px 2px;}"),
    tags$style(".buttonFile {background-color: lightgray; padding: 4px 10px; margin: 0; font-weight: bold;}"),
    tags$style("#plotDiv {position: relative}"),  # wrapper for plots has to have position relative
    tags$style(".plotUnder {position: absolute; top: 0; bottom: 0; right: 0; left: 0; height: 500px;}")
  ),

  shinyjs::useShinyjs(),  # needed to make the side panel collapsible
  # see https://stackoverflow.com/questions/46352156/r-shiny-resizing-the-mainpanel-window-when-i-minimize-the-sidebarpanel?rq=1
  # alternative: https://rstudio.github.io/shinydashboard

  # use an external javascript with any function I need to write myself
  # (eg for playing the audio)
  shinyjs::extendShinyjs(
    script = 'www/jsFun.js',
    functions = c('playme_js', 'stopAudio_js', 'clearBrush')
  ),

  fluidRow(
    column(
      width = 3,
      id = "Sidebar",
      tabsetPanel(
        id='parGroup',
        navbarMenu(
          "In",
          tabPanel(
            "STFT",
            actionButton(
              'reset_to_def',
              label = 'Reset ALL to defaults'),
            checkboxInput(
              'normalizeInput',
              'Normalize for peak amplitude',
              value = TRUE),
            numericInput(
              'windowLength',
              'Window length, ms ("windowLength")',
              value = defaults_analyze['windowLength', 'default'],
              min = defaults_analyze['windowLength', 'low'],
              max = defaults_analyze['windowLength', 'high'],
              step = defaults_analyze['windowLength', 'step']),
            sliderInput(
              'overlap',
              'Overlap, % ("overlap")',
              value = defaults_analyze['overlap', 'default'],
              min = defaults_analyze['overlap', 'low'],
              max = defaults_analyze['overlap', 'high'],
              step = defaults_analyze['overlap', 'step']),
            sliderInput(
              'dynamicRange',
              'Dynamic range, dB ("dynamicRange")',
              value = defaults_analyze['dynamicRange', 'default'],
              min = defaults_analyze['dynamicRange', 'low'],
              max = defaults_analyze['dynamicRange', 'high'],
              step = defaults_analyze['dynamicRange', 'step']),
            sliderInput(
              'zp',
              'Zero padding, points 2 ^ n ("zp")',
              value = defaults_analyze['zp', 'default'],
              min = defaults_analyze['zp', 'low'],
              max = defaults_analyze['zp', 'high'],
              step = defaults_analyze['zp', 'step']),
            selectInput(
              'wn',
              'Window type ("wn")',
              choices = c('bartlett', 'blackman', 'flattop', 'gaussian',
                          'hamming', 'hanning', 'rectangle'),
              selected = 'gaussian', multiple = FALSE)
          ),

          tabPanel(
            "Voicing",
            numericInput(
              'silence',
              'Silence threshold (0 to 1) ("silence")',
              value = defaults_analyze['silence', 'default'],
              min = defaults_analyze['silence', 'low'],
              max = defaults_analyze['silence', 'high'],
              step = defaults_analyze['silence', 'step']),
            sliderInput(
              'entropyThres',
              'Entropy threshold (0 to 1) ("entropyThres")',
              value = defaults_analyze['entropyThres', 'default'],
              min = defaults_analyze['entropyThres', 'low'],
              max = defaults_analyze['entropyThres', 'high'],
              step = defaults_analyze['entropyThres', 'step']),
            sliderInput(
              'nCands',
              'Candidates per method ("nCands")',
              value = defaults_analyze['nCands', 'default'],
              min = defaults_analyze['nCands', 'low'],
              max = defaults_analyze['nCands', 'high'],
              step = defaults_analyze['nCands', 'step']),
            sliderInput(
              'minVoicedCands',
              'Min candidates for voiced ("minVoicedCands")',
              value = defaults_analyze['minVoicedCands', 'default'],
              min = defaults_analyze['minVoicedCands', 'low'],
              max = defaults_analyze['minVoicedCands', 'high'],
              step = defaults_analyze['minVoicedCands', 'step'])
          ),

          tabPanel(
            "Priors",
            numericInput(
              'pitchFloor',
              'Pitch floor, Hz ("pitchFloor")',
              value = defaults_analyze['pitchFloor', 'default'],
              min = defaults_analyze['pitchFloor', 'low'],
              max = defaults_analyze['pitchFloor', 'high'],
              step = defaults_analyze['pitchFloor', 'step']),
            numericInput(
              'pitchCeiling',
              'Pitch ceiling, Hz ("pitchCeiling")',
              value = defaults_analyze['pitchCeiling', 'default'],
              min = defaults_analyze['pitchCeiling', 'low'],
              max = defaults_analyze['pitchCeiling', 'high'],
              step = defaults_analyze['pitchCeiling', 'step']),
            numericInput(
              'priorMean',
              'Expected pitch (priorMean), Hz ("priorMean")',
              value = defaults_analyze['priorMean', 'default'],
              min = defaults_analyze['priorMean', 'low'],
              max = defaults_analyze['priorMean', 'high'],
              step = defaults_analyze['priorMean', 'step']),
            numericInput(
              'priorSD',
              'Expected range (priorSD), semitones ("priorSD")',
              value = defaults_analyze['priorSD', 'default'],
              min = defaults_analyze['priorSD', 'low'],
              max = defaults_analyze['priorSD', 'high'],
              step = defaults_analyze['priorSD', 'step'])
          )
        ),

        navbarMenu(
          "Trackers",
          tabPanel(
            "Which to use",
            checkboxGroupInput(
              'pitchMethods',
              label = 'Pitch tracking methods ("pitchMethods")',
              choiceValues = c('dom', 'autocor', 'cep', 'spec', 'hps'),
              choiceNames = c('Lowest dominant frequency ("dom")',
                              'Autocorrelation ("autocor")',
                              'Cepstrum ("cep")',
                              'Ratio of harmonics ("spec")',
                              'Harmonic product spectrum ("hps")'),
              selected = c('dom', 'autocor'))
          ),

          tabPanel(
            'Lowest dominant frequency ("dom")',
            sliderInput(
              'domThres',
              'Dominant frequency threshold ("domThres")',
              value = defaults_analyze['domThres', 'default'],
              min = defaults_analyze['domThres', 'low'],
              max = defaults_analyze['domThres', 'high'],
              step = defaults_analyze['domThres', 'step']),
            sliderInput(
              'domSmooth',
              'Width of smoothing interval, Hz ("domSmooth")',
              value = defaults_analyze['domSmooth', 'default'],
              min = defaults_analyze['domSmooth', 'low'],
              max = defaults_analyze['domSmooth', 'high'],
              step = defaults_analyze['domSmooth', 'step'])
          ),

          tabPanel(
            'Autocorrelation ("autocor")',
            sliderInput(
              'autocorThres',
              'Voicing threshold ("autocorThres")',
              value = defaults_analyze['autocorThres', 'default'],
              min = defaults_analyze['autocorThres', 'low'],
              max = defaults_analyze['autocorThres', 'high'],
              step = defaults_analyze['autocorThres', 'step']),
            sliderInput(
              'autocorSmooth',
              'Width of smoothing interval, bins ("autocorSmooth")',
              value = defaults_analyze['autocorSmooth', 'default'],
              min = defaults_analyze['autocorSmooth', 'low'],
              max = defaults_analyze['autocorSmooth', 'high'],
              step = defaults_analyze['autocorSmooth', 'step']),
            sliderInput(
              'autocorUpsample',
              'Upsample resolution, Hz ("autocorUpsample")',
              value = defaults_analyze['autocorUpsample', 'default'],
              min = defaults_analyze['autocorUpsample', 'low'],
              max = defaults_analyze['autocorUpsample', 'high'],
              step = defaults_analyze['autocorUpsample', 'step']),
            sliderInput(
              'autocorBestPeak',
              'Best candidate ("autocorBestPeak")',
              value = defaults_analyze['autocorBestPeak', 'default'],
              min = defaults_analyze['autocorBestPeak', 'low'],
              max = defaults_analyze['autocorBestPeak', 'high'],
              step = defaults_analyze['autocorBestPeak', 'step'])
          ),

          tabPanel(
            'Cepstrum ("cep")',
            sliderInput(
              'cepThres',
              'Voicing threshold ("cepThres")',
              value = defaults_analyze['cepThres', 'default'],
              min = defaults_analyze['cepThres', 'low'],
              max = defaults_analyze['cepThres', 'high'],
              step = defaults_analyze['cepThres', 'step']),
            sliderInput(
              'cepSmooth',
              'Width of smoothing interval, Hz ("cepSmooth")',
              value = defaults_analyze['cepSmooth', 'default'],
              min = defaults_analyze['cepSmooth', 'low'],
              max = defaults_analyze['cepSmooth', 'high'],
              step = defaults_analyze['cepSmooth', 'step']),
            sliderInput(
              'cepZp',
              'Cepstral zero padding, 2 ^ n ("cepZp")',
              value = defaults_analyze['cepZp', 'default'],
              min = defaults_analyze['cepZp', 'low'],
              max = defaults_analyze['cepZp', 'high'],
              step = defaults_analyze['cepZp', 'step'])
          ),

          tabPanel(
            'Ratio of harmonics ("spec")',
            sliderInput(
              'specThres',
              'Voicing threshold ("specThres")',
              value = defaults_analyze['specThres', 'default'],
              min = defaults_analyze['specThres', 'low'],
              max = defaults_analyze['specThres', 'high'],
              step = defaults_analyze['specThres', 'step']),
            sliderInput(
              'specPeak',
              'Spectral peak height ("specPeak")',
              value = defaults_analyze['specPeak', 'default'],
              min = defaults_analyze['specPeak', 'low'],
              max = defaults_analyze['specPeak', 'high'],
              step = defaults_analyze['specPeak', 'step']),
            sliderInput(
              'specHNRslope',
              'Slope of HNR discount ("specHNRslope")',
              value = defaults_analyze['specHNRslope', 'default'],
              min = defaults_analyze['specHNRslope', 'low'],
              max = defaults_analyze['specHNRslope', 'high'],
              step = defaults_analyze['specHNRslope', 'step']),
            sliderInput(
              'specSmooth',
              'Width of window for finding harmonics, Hz ("specSmooth")',
              value = defaults_analyze['specSmooth', 'default'],
              min = defaults_analyze['specSmooth', 'low'],
              max = defaults_analyze['specSmooth', 'high'],
              step = defaults_analyze['specSmooth', 'step']),
            sliderInput(
              'specMerge',
              'Margin for merging candidates, Hz ("specMerge")',
              value = defaults_analyze['specMerge', 'default'],
              min = defaults_analyze['specMerge', 'low'],
              max = defaults_analyze['specMerge', 'high'],
              step = defaults_analyze['specMerge', 'step']),
            sliderInput(
              'specSinglePeakCert',
              'Certainty of single-ratio candidates ("specSinglePeakCert")',
              value = defaults_analyze['specSinglePeakCert', 'default'],
              min = defaults_analyze['specSinglePeakCert', 'low'],
              max = defaults_analyze['specSinglePeakCert', 'high'],
              step = defaults_analyze['specSinglePeakCert', 'step'])
          ),

          tabPanel(
            'Harmonic product spectrum ("hps")',
            sliderInput(
              'hpsThres',
              'Voicing threshold ("hpsThres")',
              value = defaults_analyze['hpsThres', 'default'],
              min = defaults_analyze['hpsThres', 'low'],
              max = defaults_analyze['hpsThres', 'high'],
              step = defaults_analyze['hpsThres', 'step']),
            sliderInput(
              'hpsNum',
              'The number of folds ("hpsNum")',
              value = defaults_analyze['hpsNum', 'default'],
              min = defaults_analyze['hpsNum', 'low'],
              max = defaults_analyze['hpsNum', 'high'],
              step = defaults_analyze['hpsNum', 'step']),
            sliderInput(
              'hpsNorm',
              'Inflation of hps pitch certainty ("hpsNorm")',
              value = defaults_analyze['hpsNorm', 'default'],
              min = defaults_analyze['hpsNorm', 'low'],
              max = defaults_analyze['hpsNorm', 'high'],
              step = defaults_analyze['hpsNorm', 'step']),
            sliderInput(
              'hpsPenalty',
              'Penalty for low-frequency candidates ("hpsPenalty")',
              value = defaults_analyze['hpsPenalty', 'default'],
              min = defaults_analyze['hpsPenalty', 'low'],
              max = defaults_analyze['hpsPenalty', 'high'],
              step = defaults_analyze['hpsPenalty', 'step'])
          )
        ),

        navbarMenu(
          "Out",
          tabPanel(
            "Output",
            checkboxGroupInput(
              'summaryFun',
              label = 'Summary function ("summaryFun")',
              choiceValues = c('mean', 'sd', 'median', 'min', 'max'),
              choiceNames = c('Mean', 'SD', 'Median', 'Min', 'Max'),
              selected = c('mean', 'sd')),
            textInput(
              'summaryFun_text',
              label = 'Manual summary function',
              value = '',
              placeholder = 'function(x) diff(range(x, na.rm = TRUE))')
          ),

          tabPanel(
            "Path",
            checkboxInput(
              'automPathUpdate',
              'Update path automatically',
              value = TRUE),
            selectInput(
              'pathfinding',
              'Pathfinding method ("pathfinding")',
              choices = c('none', 'fast', 'slow'),
              selected = 'fast', multiple = FALSE),
            sliderInput(
              'certWeight',
              'Certainty weight ("certWeight")',
              value = defaults_analyze['certWeight', 'default'],
              min = defaults_analyze['certWeight', 'low'],
              max = defaults_analyze['certWeight', 'high'],
              step = defaults_analyze['certWeight', 'step']),
            numericInput(
              'shortestSyl',
              'Shortest syllable, ms ("shortestSyl")',
              value = defaults_analyze['shortestSyl', 'default'],
              min = defaults_analyze['shortestSyl', 'low'],
              max = defaults_analyze['shortestSyl', 'high'],
              step = defaults_analyze['shortestSyl', 'step']),
            numericInput(
              'shortestPause',
              'Shortest pause, ms ("shortestPause")',
              value = defaults_analyze['shortestPause', 'default'],
              min = defaults_analyze['shortestPause', 'low'],
              max = defaults_analyze['shortestPause', 'high'],
              step = defaults_analyze['shortestPause', 'step'])
          ),

          tabPanel(
            "Smoothing",
            sliderInput (
              'smooth',
              'Median smoothing (0 = none) ("smooth")',
              value = defaults_analyze['smooth', 'default'],
              min = defaults_analyze['smooth', 'low'],
              max = defaults_analyze['smooth', 'high'],
              step = defaults_analyze['smooth', 'step']),
            numericInput(
              'interpolWin',
              'Interpolation window, ms (0 = none) ("interpolWin")',
              value = defaults_analyze['interpolWin', 'default'],
              min = defaults_analyze['interpolWin', 'low'],
              max = defaults_analyze['interpolWin', 'high'],
              step = defaults_analyze['interpolWin', 'step']),
            numericInput(
              'interpolTol',
              'Interpolation tolerance ("interpolTol")',
              value = defaults_analyze['interpolTol', 'default'],
              min = defaults_analyze['interpolTol', 'low'],
              max = defaults_analyze['interpolTol', 'high'],
              step = defaults_analyze['interpolTol', 'step']),
            sliderInput(
              'interpolCert',
              'Interpolation certainty ("interpolCert")',
              value = defaults_analyze['interpolCert', 'default'],
              min = defaults_analyze['interpolCert', 'low'],
              max = defaults_analyze['interpolCert', 'high'],
              step = defaults_analyze['interpolCert', 'step'])
          ),

          tabPanel(
            "Spectrogram",
            sliderInput(
              'spec_ylim',
              'Frequency range, kHz ("ylim")',
              value=c(0, defaults_analyze['spec_ylim', 'default']),
              min = defaults_analyze['spec_ylim', 'low'],
              max = defaults_analyze['spec_ylim', 'high'],
              step = defaults_analyze['spec_ylim', 'step']),
            sliderInput(
              'spec_maxPoints',
              'Max number of pixels, 10^',
              value = defaults_analyze['spec_maxPoints', 'default'],
              min = defaults_analyze['spec_maxPoints', 'low'],
              max = defaults_analyze['spec_maxPoints', 'high'],
              step = defaults_analyze['spec_maxPoints', 'step']),
            numericInput(
              'spec_cex',
              'Point size ("pitchPlot=list(cex=...")',
              value = defaults_analyze['spec_cex', 'default'],
              min = defaults_analyze['spec_cex', 'low'],
              max = defaults_analyze['spec_cex', 'high'],
              step = defaults_analyze['spec_cex', 'step']),
            radioButtons(
              inputId = 'spec_colorTheme',
              label = 'Color scheme ("colorTheme")',
              choices = c("Seewave" = "seewave",
                          "Heat" = "heat.colors",
                          "Black & white" = "bw"),
              selected = 'bw', inline = TRUE, width = NULL),
            sliderInput(
              'specContrast',
              'Contrast ("contrast")',
              value = defaults_analyze['specContrast', 'default'],
              min = defaults_analyze['specContrast', 'low'],
              max = defaults_analyze['specContrast', 'high'],
              step = defaults_analyze['specContrast', 'step']),
            sliderInput(
              'specBrightness',
              'Brightness ("brightness")',
              value = defaults_analyze['specBrightness', 'default'],
              min = defaults_analyze['specBrightness', 'low'],
              max = defaults_analyze['specBrightness', 'high'],
              step = defaults_analyze['specBrightness', 'step'])
          ),

          tabPanel(
            "Oscillogram",
            selectInput(
              'osc',
              'Oscillogram type ("osc")',
              choices = c('none', 'linear', 'dB'),
              selected = 'linear', multiple = FALSE),
            sliderInput(
              'osc_height',
              'Oscillogram height, px ("heights")',
              value = defaults_analyze['osc_height', 'default'],
              min = defaults_analyze['osc_height', 'low'],
              max = defaults_analyze['osc_height', 'high'],
              step = defaults_analyze['osc_height', 'step']),
            sliderInput(
              'osc_maxPoints',
              'Max number of pixels, 10^',
              value = defaults_analyze['osc_maxPoints', 'default'],
              min = defaults_analyze['osc_maxPoints', 'low'],
              max = defaults_analyze['osc_maxPoints', 'high'],
              step = defaults_analyze['osc_maxPoints', 'step'])
          )
        )
      )
    ),  # end of column "Sidebar"

    column(
      width = 9,
      id = "Main",
      fluidRow(
        column(
          width = 1,
          bsButton("showpanel", label = '', icon = icon("bars"),
                   type = "toggle", value = TRUE)
        ),

        column(
          width = 3,
          fileInput(
            inputId = "loadAudio",
            label = NULL, multiple = TRUE,
            buttonLabel = 'Load audio',
            placeholder = '...', width = "175px")
        ),

        column(
          width = 6,
          tags$div(
            actionButton(
              inputId = "lastFile", label = "<<",
              class = "buttonFile"),
            tags$strong(uiOutput("fileN", inline = TRUE)),
            actionButton(
              inputId = "nextFile", label = ">>",
              class = "buttonFile")
          ),
          selectInput('fileList', label = NULL, choices = list())
        ),

        column(
          2,
          downloadButton(
            outputId = "saveRes",
            label = "",
            style="color: blue; background-color: orange;"),
          actionButton('about', label = '?'),
          shinyBS:::bsPopover(
            id = 'about', title = NULL, content = 'Help',
            placement = "right", trigger = "hover")
          # shinyBS has to be mentioned somewhere in ui,
          # otherwise addTooltip doesn't work in server
        )
      ),

      fluidRow(
        column(
          width = 1,
          actionButton(
            inputId = 'zoomIn_freq',
            label = HTML("<img src='icons/zoomIn.png' width = '25px'>"),
            style = "padding: 2px 2px; display: block"),
          actionButton(
            inputId = 'zoomOut_freq',
            label = HTML("<img src='icons/zoomOut.png' width = '25px'>"),
            style = "padding: 2px 2px; display: block")
        ),

        column(
          width = 3,
          radioButtons(
            'spectro_clickAct',
            label = 'Left click action: ',
            choiceNames = c('Anchor', 'Select'),
            choiceValues = c('addCand', 'select'),
            selected = 'addCand', inline = TRUE)
        ),

        column(
          width = 5,
          actionButton(
            inputId = "selection_stop",
            label = HTML("<img src='icons/stop.png' width = '25px'>"),
            class = "buttonInline"),
          actionButton(
            inputId = "selection_play",
            label = HTML("<img src='icons/play.png' width = '25px'>"),
            class = "buttonInline"),
          actionButton(
            inputId = "selection_unvoice",
            label = HTML("<img src='icons/unvoice.png' width = '25px'>"),
            class = "buttonInline"),
          actionButton(
            inputId = "selection_voice",
            label = HTML("<img src='icons/voice.png' width = '25px'>"),
            class = "buttonInline"),
          actionButton(
            inputId = "selection_octaveUp",
            label = HTML("<img src='icons/octaveUp.png' width = '25px'>"),
            class = "buttonInline"),
          actionButton(
            inputId = "selection_octaveDown",
            label = HTML("<img src='icons/octaveDown.png' width = '25px'>"),
            class = "buttonInline"),
          actionButton(
            inputId = "selection_setPrior",
            label = HTML("<img src='icons/prior.png' width = '25px'>"),
            class = "buttonInline"),
          actionButton(
            inputId = "button_pathUpdate",
            label = HTML("<img src='icons/update.png' width = '25px'>"),
            class = "buttonInline")
        ),

        column(
          width = 3,
          actionButton(
            inputId = 'scrollLeft',
            label = HTML("<img src='icons/backward.png' width = '25px'>"),
            class = "buttonInline"),
          actionButton(
            inputId = 'zoomOut',
            label = HTML("<img src='icons/zoomOut.png' width = '25px'>"),
            class = "buttonInline"),
          actionButton(
            inputId = "zoomToSel",
            label = HTML("<img src='icons/zoomSel.png' width = '25px'>"),
            class = "buttonInline"),
          actionButton(
            inputId = 'zoomIn',
            label = HTML("<img src='icons/zoomIn.png' width = '25px'>"),
            class = "buttonInline"),
          actionButton(
            inputId = 'scrollRight',
            label = HTML("<img src='icons/forward.png' width = '25px'>"),
            class = "buttonInline")
        )
      ),

      fluidRow(
        tags$div(
          id = 'plotDiv',

          plotOutput(
            'spectrogram', height = '500px'
          ),

          tags$div(
            class = 'plotUnder',
            plotOutput(
              'specSlider', height = '500px'
            )
          ),

          tags$div(
            class = 'plotUnder',
            plotOutput(
              'specOver', height = '500px',
              click = "spectrogram_click",
              dblclick = dblclickOpts(id = "spectrogram_dblclick"),
              hover = hoverOpts(id = "spectrogram_hover"),
              brush = brushOpts(id = 'spectrogram_brush', opacity = 0.25, resetOnNew = FALSE))
          ),

          plotOutput('oscillogram', height = 'auto')
        )
      )

      #fluidRow(
      # htmlOutput('statusBar')  # status bar here
      #),
    )  # end of column "Main"
  )  # end of fluidRow
)
