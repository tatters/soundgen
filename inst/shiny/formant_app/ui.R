# formant_app()
#
ui = fluidPage(
  # css
  tags$head(
    shiny::includeCSS("www/formant_app.css")
  ),

  # js
  includeScript("www/formant_app.js"),
  includeScript("www/formant_app_shinyjs.js"),
  shinyjs::useShinyjs(),
  # handy for calling js functions from R, eg for a collapsible side panel - see
  # https://stackoverflow.com/questions/46352156/r-shiny-resizing-the-mainpanel-window-when-i-minimize-the-sidebarpanel?rq=1
  # alternative: https://rstudio.github.io/shinydashboard

  # import some js functions to be invoked from R with shinyjs
  # (eg for playing the audio)
  shinyjs::extendShinyjs(
    script = 'www/formant_app_shinyjs.js',
    functions = c('playme_js', 'stopAudio_js', 'clearBrush', 'inheritSize', 'scrollBar')
  ),

  # html
  tags$div(
    id = 'gridCont',

    tags$div(
      id = 'left',

      fluidRow(
        column(
          width = 4,
          id = "Sidebar",
          tabsetPanel(
            id = 'parGroup',
            navbarMenu(
              "Analysis",
              tabPanel(
                "General",
                actionButton(
                  'reset_to_def',
                  label = 'Reset ALL to defaults'),
                radioButtons(
                  'audioMethod',
                  label = "Play audio with",
                  choices = list('Browser' = 'Browser', 'R' = 'R'),
                  selected = 'Browser', inline = TRUE, width = NULL
                ),
                checkboxInput(
                  'normalizeInput',
                  'Normalize for peak amplitude',
                  value = TRUE),
              ),

              tabPanel(
                "LPC",
                sliderInput(
                  'nFormants',
                  'Number of formants',
                  value = def_form['nFormant', 'default'],
                  min = def_form['nFormant', 'low'],
                  max = def_form['nFormant', 'high'],
                  step = def_form['nFormant', 'step']),
                numericInput(
                  'silence',
                  'Silence threshold (0 to 1)',
                  value = def_form['silence', 'default'],
                  min = def_form['silence', 'low'],
                  max = def_form['silence', 'high'],
                  step = def_form['silence', 'step']),
                selectInput(
                  'summaryFun',
                  'Average measure per annotated region',
                  choices = list('Median' = 'median',
                                 'Mean' = 'mean'),
                  selected = 'median'
                ),
                textInput(
                  'coeffs',
                  'Number of LPC coefficients',
                  value = ''),
                sliderInput(
                  'minformant',
                  'Minimum formant frequency',
                  value = def_form['minformant', 'default'],
                  min = def_form['minformant', 'low'],
                  max = def_form['minformant', 'high'],
                  step = def_form['minformant', 'step']),
                sliderInput(
                  'maxbw',
                  'Maximum formant bandwidth',
                  value = def_form['maxbw', 'default'],
                  min = def_form['maxbw', 'low'],
                  max = def_form['maxbw', 'high'],
                  step = def_form['maxbw', 'step'])
              ),

              tabPanel(
                "Windowing",
                numericInput(
                  'windowLength_lpc',
                  'Window length, ms',
                  value = def_form['windowLength_lpc', 'default'],
                  min = def_form['windowLength_lpc', 'low'],
                  max = def_form['windowLength_lpc', 'high'],
                  step = def_form['windowLength_lpc', 'step']),
                numericInput(
                  'step_lpc',
                  'Step, ms',
                  value = def_form['step_lpc', 'default'],
                  min = def_form['step_lpc', 'low'],
                  max = def_form['step_lpc', 'high'],
                  step = def_form['step_lpc', 'step']),
                sliderInput(
                  'overlap_lpc',
                  'Overlap, %',
                  value = def_form['overlap_lpc', 'default'],
                  min = def_form['overlap_lpc', 'low'],
                  max = def_form['overlap_lpc', 'high'],
                  step = def_form['overlap_lpc', 'step']),
                sliderInput(
                  'dynamicRange_lpc',
                  'Dynamic range, dB',
                  value = def_form['dynamicRange_lpc', 'default'],
                  min = def_form['dynamicRange_lpc', 'low'],
                  max = def_form['dynamicRange_lpc', 'high'],
                  step = def_form['dynamicRange_lpc', 'step']),
                sliderInput(
                  'zp_lpc',
                  'Zero padding, points 2 ^ n',
                  value = def_form['zp_lpc', 'default'],
                  min = def_form['zp', 'low'],
                  max = def_form['zp_lpc', 'high'],
                  step = def_form['zp_lpc', 'step']),
                selectInput(
                  'wn_lpc',
                  'Window type',
                  choices = c('bartlett', 'blackman', 'flattop', 'gaussian',
                              'hamming', 'hanning', 'rectangle'),
                  selected = 'gaussian', multiple = FALSE)
              ),

              tabPanel(
                "Vocal tract",
                selectInput(
                  'vtl_method',
                  'Method for estimating VTL',
                  choices = list('Regression' = 'regression',
                                 'Mean formant dispersion' = 'meanDispersion',
                                 'Mean formant' = 'meanFormant'),
                  selected = 'regression'
                ),

                numericInput(
                  'speedSound',
                  'Speed of sound, cm/s',
                  value = '35400',
                  min = 1, max = 100000, step = 1
                ),

                checkboxInput(
                  'interceptZero',
                  'Set intercept to zero',
                  value = TRUE),
              )
            ),

            navbarMenu(
              "Plotting",
              tabPanel(
                "Spectrogram",
                sliderInput(
                  'spec_ylim',
                  'Frequency range, kHz',
                  value = c(0, def_form['spec_ylim', 'default']),
                  min = def_form['spec_ylim', 'low'],
                  max = def_form['spec_ylim', 'high'],
                  step = def_form['spec_ylim', 'step']),
                numericInput(
                  'windowLength',
                  'Window length, ms',
                  value = def_form['windowLength', 'default'],
                  min = def_form['windowLength', 'low'],
                  max = def_form['windowLength', 'high'],
                  step = def_form['windowLength', 'step']),
                numericInput(
                  'step',
                  'Step, ms',
                  value = def_form['step', 'default'],
                  min = def_form['step', 'low'],
                  max = def_form['step', 'high'],
                  step = def_form['step', 'step']),
                sliderInput(
                  'overlap',
                  'Overlap, %',
                  value = def_form['overlap', 'default'],
                  min = def_form['overlap', 'low'],
                  max = def_form['overlap', 'high'],
                  step = def_form['overlap', 'step']),
                sliderInput(
                  'dynamicRange',
                  'Dynamic range, dB',
                  value = def_form['dynamicRange', 'default'],
                  min = def_form['dynamicRange', 'low'],
                  max = def_form['dynamicRange', 'high'],
                  step = def_form['dynamicRange', 'step']),
                radioButtons(
                  inputId = 'spec_colorTheme',
                  label = 'Color scheme',
                  choices = c("Seewave" = "seewave",
                              "Heat" = "heat.colors",
                              "Black & white" = "bw"),
                  selected = 'bw', inline = TRUE, width = NULL),
                sliderInput(
                  'specContrast',
                  'Contrast',
                  value = def_form['specContrast', 'default'],
                  min = def_form['specContrast', 'low'],
                  max = def_form['specContrast', 'high'],
                  step = def_form['specContrast', 'step']),
                sliderInput(
                  'specBrightness',
                  'Brightness',
                  value = def_form['specBrightness', 'default'],
                  min = def_form['specBrightness', 'low'],
                  max = def_form['specBrightness', 'high'],
                  step = def_form['specBrightness', 'step']),
                shinyBS::bsCollapsePanel(
                  "Advanced",
                  sliderInput(
                    'spec_cex',
                    'Point size of formant tracks',
                    value = 1,
                    min = 0,
                    max = 3,
                    step = .1),
                  textInput(
                    'spec_col',
                    'Color of formant tracks',
                    value = '#FF0000FF', # red
                    placeholder = '#FF0000FF'
                  ),
                  sliderInput(
                    'zp',
                    'Zero padding, points 2 ^ n',
                    value = def_form['zp', 'default'],
                    min = def_form['zp', 'low'],
                    max = def_form['zp', 'high'],
                    step = def_form['zp', 'step']),
                  selectInput(
                    'wn',
                    'Window type',
                    choices = c('bartlett', 'blackman', 'flattop', 'gaussian',
                                'hamming', 'hanning', 'rectangle'),
                    selected = 'gaussian', multiple = FALSE),
                  sliderInput(
                    'spec_maxPoints',
                    'Max number of pixels, 10^',
                    value = def_form['spec_maxPoints', 'default'],
                    min = def_form['spec_maxPoints', 'low'],
                    max = def_form['spec_maxPoints', 'high'],
                    step = def_form['spec_maxPoints', 'step'])
                )
              ),

              tabPanel(
                "Oscillogram",
                selectInput(
                  'osc',
                  'Oscillogram type',
                  choices = c('linear', 'dB'),
                  selected = 'linear', multiple = FALSE),
                sliderInput(
                  'osc_maxPoints',
                  'Max number of pixels, 10^',
                  value = def_form['osc_maxPoints', 'default'],
                  min = def_form['osc_maxPoints', 'low'],
                  max = def_form['osc_maxPoints', 'high'],
                  step = def_form['osc_maxPoints', 'step'])
              ),

              tabPanel(
                "Spectrum",
                sliderInput(
                  'spectrum_xlim',
                  'Frequency range, kHz',
                  value = c(0, def_form['spectrum_xlim', 'default']),
                  min = def_form['spectrum_xlim', 'low'],
                  max = def_form['spectrum_xlim', 'high'],
                  step = def_form['spectrum_xlim', 'step']),
                sliderInput(
                  'spectrum_len',
                  'Resolution, points',
                  value = def_form['spectrum_len', 'default'],
                  min = def_form['spectrum_len', 'low'],
                  max = def_form['spectrum_len', 'high'],
                  step = def_form['spectrum_len', 'step'])
              ),

              tabPanel("Annotations")
            )
          )
        ),  # end of column "sidebar"

        column(
          width = 8,
          id ="Main",
          fluidRow(
            column(
              width = 1,
              bsButton(
                "showpanel", label = '', icon = icon("bars"),
                type = "toggle", value = FALSE)
            ),

            column(
              width = 3,
              fileInput(
                inputId = "loadAudio", label = NULL,
                # accept = c('audio/wav', 'audio/wave', 'audio/x-wave', 'audio/vnd.wave',
                #            'audio/mpeg', 'audio/mpeg4-generic', 'audio/mpeg3', 'audio/x-mpeg-3',
                #            'video/mpeg', 'video/x-mpeg'),
                multiple = TRUE, buttonLabel = 'Load audio',
                placeholder = '...')
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
              width = 2,
              uiOutput("htmlAudio"),  # not actually shown
              downloadButton(
                outputId = "saveRes", label = "",
                style="color: blue; background-color: orange;"),
              actionButton(
                'about', label = '?'),
              shinyBS:::bsPopover
              (id = 'about', title = NULL, content = 'Help',
                placement = "right", trigger = "hover")
              # shinyBS has to be mentioned somewhere in ui,
              # otherwise addTooltip doesn't work in server
            )
          ),

          fluidRow(
            column(
              width = 2,
              actionButton(
                inputId = 'zoomOut_freq',
                label = HTML("<img src='icons/zoomOut.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = 'zoomIn_freq',
                label = HTML("<img src='icons/zoomIn.png' width = '20px'>"),
                class = "buttonInline")
            ),

            column(
              width = 4,
              actionButton(
                inputId = "selection_stop",
                label = HTML("<img src='icons/stop.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = "selection_play",
                label = HTML("<img src='icons/play.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = "selection_annotate",
                label = HTML("<img src='icons/annotate.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = "selection_delete",
                label = HTML("<img src='icons/delete.png' width = '20px'>"),
                class = "buttonInline")
            ),

            column(
              width = 6,
              actionButton(
                inputId = 'scrollLeft',
                label = HTML("<img src='icons/backward.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = 'zoomOut',
                label = HTML("<img src='icons/zoomOut.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = "zoomToSel",
                label = HTML("<img src='icons/zoomSel.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = 'zoomIn',
                label = HTML("<img src='icons/zoomIn.png' width = '20px'>"),
                class = "buttonInline"),
              actionButton(
                inputId = 'scrollRight',
                label = HTML("<img src='icons/forward.png' width = '20px'>"),
                class = "buttonInline")
            )
          ),

          fluidRow(
            tags$div(
              id = 'plotRow',

              tags$div(
                id = 'specDiv',
                plotOutput('spectrogram'),
                plotOutput('specSlider'),
                plotOutput(
                  'specOver',
                  click = "spectrogram_click",
                  dblclick = dblclickOpts(id = "spectrogram_dblclick"),
                  hover = hoverOpts(id = "spectrogram_hover"),
                  brush = brushOpts(id = 'spectrogram_brush', opacity = 0.25, resetOnNew = FALSE))
              ),

              tags$div(
                id = 'scrollBarCont',
                tags$div(
                  id = 'scrollBar'
                )
              ),

              plotOutput(
                'oscillogram', height = '100px'  # auto to inherit from css in the header
              ),

              plotOutput(
                'ann_plot', height = '60px',
                click = "ann_click",
                dblclick = dblclickOpts(id = "ann_dblclick")
              )
            )
          )
        )
      )
    ),

    tags$div(
      id = 'right',
      fluidRow(
        column(
          width = 10,
          uiOutput('fButtons', style = "height: 60px;")
        ),
        column(
          width = 2,
          actionButton(
            inputId = 'synthBtn',
            label = HTML("<img src='icons/synth.png' width = '30px'>"),
            class = "buttonInline"
          )
        )
      ),

      fluidRow(
        tags$div(
          id = 'spectrumDiv',
          plotOutput(
            'spectrum',
            height = "300px",
            click = "spectrum_click",
            dblclick = dblclickOpts(id = "spectrum_dblclick"),
            hover = hoverOpts(id = "spectrum_hover")),
          tags$div(
            id = 'spectrum_smoothDiv',
            sliderInput(
              'spectrum_smooth',
              'Smoothing',
              value = def_form['spectrum_smooth', 'default'],
              min = def_form['spectrum_smooth', 'low'],
              max = def_form['spectrum_smooth', 'high'],
              step = def_form['spectrum_smooth', 'step'],
              width = '200px')
          )
        )
      ),

      fluidRow(
        tags$div(
          id = 'fmtSpaceDiv',
          plotOutput(
            'fmtSpace', height = "175px")
        )
      ),

      fluidRow(
        tableOutput('ann_table')  # dataTableOutput('ann_table')
      )
    )
  )
)
