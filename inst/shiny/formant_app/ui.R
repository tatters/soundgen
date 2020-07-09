# formant_app()
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

  shinyjs::useShinyjs(),  # needed to make the side panel collapsible
  # see https://stackoverflow.com/questions/46352156/r-shiny-resizing-the-mainpanel-window-when-i-minimize-the-sidebarpanel?rq=1
  # alternative: https://rstudio.github.io/shinydashboard

  fluidRow(
    column(
      width = 3,
      id = "Sidebar",
      tabsetPanel(
        id = 'parGroup',
        navbarMenu(
          "Analysis",
          tabPanel(
            "LPC",
            actionButton(
              'reset_to_def',
              label = 'Reset ALL to defaults'),
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
              selected = 'gaussian', multiple = FALSE),
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
              choices = c('none', 'linear', 'dB'),
              selected = 'linear', multiple = FALSE),
            sliderInput(
              'osc_height',
              'Oscillogram height, px',
              value = def_form['osc_height', 'default'],
              min = def_form['osc_height', 'low'],
              max = def_form['osc_height', 'high'],
              step = def_form['osc_height', 'step']),
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
              'spectrum_len',
              'Resolution, points',
              value = def_form['spectrum_len', 'default'],
              min = def_form['spectrum_len', 'low'],
              max = def_form['spectrum_len', 'high'],
              step = def_form['spectrum_len', 'step'])
          ),

          tabPanel("Annotations",

          )
        )
      )
    ),  # end of column "sidebar"

    column(
      width = 9,
      id ="Main",
      fluidRow(
        column(
          1,
          bsButton(
            "showpanel", label = '', icon = icon("bars"),
            type = "toggle", value = FALSE)
        ),
        column(
          3,
          fileInput(
            inputId = "loadAudio", label = NULL,
            multiple = TRUE, buttonLabel = 'Load audio',
            placeholder = '...', width = "175px")
        ),
        column(
          3,
          uiOutput("fileN"),
          actionButton(
            inputId = "lastFile", label = "Last",
            style="background-color: lightgray;"),
          actionButton(
            inputId = "nextFile", label = "Next",
            style="background-color: lightgray;")
        ),
        column(
          3,
          uiOutput("htmlAudio")
        ),
        column(
          2,
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
          1,
          actionButton(
            inputId = 'zoomIn_freq',
            label = HTML("<img src='icons/zoomIn.png' width = '25px'>"),
            style = "padding: 2px 2px; display: block"),
          actionButton(
            inputId = 'zoomOut_freq',
            label = HTML("<img src='icons/zoomOut.png' width = '25px'>"),
            style = "padding: 2px 2px; display: block"),
        ),
        column(
          1,
          actionButton(
            inputId = "selection_play",
            label = HTML("<img src='icons/play.png' width = '25px'>"),
            style = "padding: 2px 2px;"),
          actionButton(
            inputId = "selection_delete",
            label = HTML("<img src='icons/delete.png' width = '25px'>"),
            style = "padding: 2px 2px;")
        ),
        column(
          1,
          # htmlOutput('pitchAtCursor', inline = TRUE)
        ),
        column(
          2,
          uiOutput('fRadios')
        ),
        column(
          2,
          actionButton(
            inputId = 'scrollLeft',
            label = HTML("<img src='icons/backward.png' width = '25px'>"),
            style = "padding: 2px 2px;"),
          actionButton(
            inputId = 'zoomOut',
            label = HTML("<img src='icons/zoomOut.png' width = '25px'>"),
            style = "padding: 2px 2px;"),
          actionButton(
            inputId = "zoomToSel",
            label = HTML("<img src='icons/zoomSel.png' width = '25px'>"),
            style = "padding: 2px 2px;"),
          actionButton(
            inputId = 'zoomIn',
            label = HTML("<img src='icons/zoomIn.png' width = '25px'>"),
            style = "padding: 2px 2px;"),
          actionButton(
            inputId = 'scrollRight',
            label = HTML("<img src='icons/forward.png' width = '25px'>"),
            style = "padding: 2px 2px;")
        ),
        column(
          2,
          # htmlOutput('spectrum_cursor', inline = TRUE)
        ),
        column(
          2,
          # htmlOutput('spectrum_peak', inline = TRUE)
        ),
        column(
          1,
          # add smth if needed
        )
      ),
      fluidRow(
        column(
          width = 7,
          absolutePanel(height = '500px',
            top = 0, left = 0, right = 0,
            plotOutput('adjaPlot', height = '500px',
            click = "spectrogram_click",
            dblclick = dblclickOpts(id = "spectrogram_dblclick"),
            hover = hoverOpts(id = "spectrogram_hover"),
            brush = brushOpts(id = 'spectrogram_brush', opacity = 0, resetOnNew = FALSE))
          ),
          plotOutput(
            'spectrogram', height = '500px'),
          # , style = "max-width: 66vw; overflow-x: auto;"

          plotOutput(
            'oscillogram', height = '100px'),
          plotOutput(
            'ann_plot', height = '100px',
            click = "ann_click",
            dblclick = dblclickOpts(id = "ann_dblclick"))
        ),
        column(
          plotOutput(
            'spectrum',
            height = '500px',
            click = "spectrum_click",
            dblclick = dblclickOpts(id = "spectrum_dblclick"),
            hover = hoverOpts(id = "spectrum_hover"),
            brush = brushOpts(id = 'spectrum_brush', resetOnNew = TRUE)),
          sliderInput(
            'spectrum_smooth',
            'Smoothing',
            value = def_form['spectrum_smooth', 'default'],
            min = def_form['spectrum_smooth', 'low'],
            max = def_form['spectrum_smooth', 'high'],
            step = def_form['spectrum_smooth', 'step']),
          tableOutput('ann_table'),
          width = 5
        )
      )

      #fluidRow(
      # htmlOutput('statusBar')  # status bar here
      #)
    )  # end of column "main"
  )
)
