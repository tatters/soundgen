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
    column(id = "Sidebar",
           tabsetPanel(id='parGroup',
                       navbarMenu("Analysis",
                                  tabPanel("LPC",
                                           actionButton('reset_to_def', label = 'Reset ALL to defaults'),
                                           sliderInput('nFormants', 'Number of formants', value=4, min=1, max=10, step=1),
                                           numericInput('silence', 'Silence threshold (0 to 1)', value=defaults_analyze['silence', 'default'], min=defaults_analyze['silence', 'low'], max=defaults_analyze['silence', 'high'], step=defaults_analyze['silence', 'step']),
                                           textInput('coeffs', 'Number of LPC coefficients', value=''),
                                           sliderInput('minformant', 'Minimum formant frequency', value=200, min=10, max=5000, step=10),
                                           sliderInput('maxbw', 'Maximum formant bandwidth', value=600, min=10, max=5000, step=10)
                                  ),

                                  tabPanel("Windowing",
                                           numericInput('windowLength_lpc', 'Window length, ms', value=10, min=defaults_analyze['windowLength', 'low'], max=defaults_analyze['windowLength', 'high'], step=defaults_analyze['windowLength','step']),
                                           sliderInput('overlap_lpc', 'Overlap, %', value=defaults_analyze['overlap','default'], min=defaults_analyze['overlap', 'low'], max=defaults_analyze['overlap', 'high'], step=defaults_analyze['overlap','step']),
                                           sliderInput('dynamicRange_lpc', 'Dynamic range, dB', value=defaults_analyze['dynamicRange','default'], min=defaults_analyze['dynamicRange', 'low'], max=defaults_analyze['dynamicRange', 'high'], step=defaults_analyze['dynamicRange','step']),
                                           sliderInput('zp_lpc', 'Zero padding, points 2 ^ n', value=defaults_analyze['zp','default'], min=defaults_analyze['zp','low'], max=defaults_analyze['zp','high'], step=defaults_analyze['zp','step']),
                                           selectInput('wn_lpc', 'Window type', choices = c('bartlett', 'blackman', 'flattop', 'gaussian', 'hamming', 'hanning', 'rectangle'), selected = 'gaussian', multiple = FALSE),
                                  )
                       ),

                       navbarMenu("Plotting",
                                  tabPanel("Spectrogram",
                                           sliderInput('spec_ylim', 'Frequency range, kHz', value=c(0, defaults_analyze['spec_ylim','default']), min=defaults_analyze['spec_ylim', 'low'], max=defaults_analyze['spec_ylim','high'], step=defaults_analyze['spec_ylim','step']),
                                           numericInput('windowLength', 'Window length, ms', value=10, min=defaults_analyze['windowLength', 'low'], max=defaults_analyze['windowLength', 'high'], step=defaults_analyze['windowLength','step']),
                                           sliderInput('overlap', 'Overlap, %', value=defaults_analyze['overlap','default'], min=defaults_analyze['overlap', 'low'], max=defaults_analyze['overlap', 'high'], step=defaults_analyze['overlap','step']),
                                           sliderInput('dynamicRange', 'Dynamic range, dB', value=defaults_analyze['dynamicRange','default'], min=defaults_analyze['dynamicRange', 'low'], max=defaults_analyze['dynamicRange', 'high'], step=defaults_analyze['dynamicRange','step']),
                                           radioButtons(inputId='spec_colorTheme', label='Color scheme', choices=c("Seewave"="seewave", "Heat"="heat.colors", "Black & white"="bw"), selected='bw', inline=TRUE, width=NULL),
                                           sliderInput('specContrast', 'Contrast', value=defaults_analyze['specContrast','default'], min=defaults_analyze['specContrast', 'low'], max=defaults_analyze['specContrast', 'high'], step=defaults_analyze['specContrast','step']),
                                           sliderInput('specBrightness', 'Brightness', value=defaults_analyze['specBrightness','default'], min=defaults_analyze['specBrightness', 'low'], max=defaults_analyze['specBrightness', 'high'], step=defaults_analyze['specBrightness','step']),
                                           shinyBS::bsCollapsePanel("Advanced",
                                                                    sliderInput('zp', 'Zero padding, points 2 ^ n', value=defaults_analyze['zp','default'], min=defaults_analyze['zp','low'], max=defaults_analyze['zp','high'], step=defaults_analyze['zp','step']),
                                                                    selectInput('wn', 'Window type', choices = c('bartlett', 'blackman', 'flattop', 'gaussian', 'hamming', 'hanning', 'rectangle'), selected = 'gaussian', multiple = FALSE),
                                                                    sliderInput('maxPoints_spec', 'Max number of pixels, 10^', value=5.5, min=3, max=7, step=.25)
                                           )
                                  ),

                                  tabPanel("Oscillogram",
                                           selectInput('osc', 'Oscillogram type ("osc")', choices = c('none', 'linear', 'dB'), selected = 'linear', multiple = FALSE),
                                           sliderInput('osc_height', 'Oscillogram height, px ("heights")', value=100, min=25, max=500, step=25),
                                           sliderInput('maxPoints_osc', 'Max number of pixels, 10^', value=5, min=3, max=7, step=.5)
                                  ),

                                  tabPanel("Spectrum",
                                           sliderInput('spectrum_len', 'Resolution, points', value=500, min=100, max=5000, step=25)
                                  ),

                                  tabPanel("Annotations",

                                  )
                       )
           ),
           width = 3
    ),

    column(id ="Main",
           fluidRow(
             column(1,
                    bsButton("showpanel", label = '', icon = icon("bars"), type = "toggle", value = FALSE)
             ),
             column(3,
                    fileInput(inputId = "loadAudio", label = NULL, multiple = TRUE, buttonLabel = 'Load audio', placeholder = '...', width = "175px")
             ),
             column(3,
                    uiOutput("fileN"),
                    actionButton(inputId = "lastFile", label = "Last", style="background-color: lightgray;"),
                    actionButton(inputId = "nextFile", label = "Next", style="background-color: lightgray;")
             ),
             column(3,
                    uiOutput("htmlAudio")
             ),
             column(2,
                    downloadButton(outputId = "saveRes", label = "", style="color: blue; background-color: orange;"),
                    actionButton('about', label = '?'),
                    shinyBS:::bsPopover(id='about', title=NULL, content='Help', placement="right", trigger="hover")  # shinyBS has to be mentioned somewhere in ui, otherwise addTooltip doesn't work in server
             )
           ),

           fluidRow(
             column(1,
                    actionButton(inputId = 'zoomIn_freq', label = HTML("<img src='icons/zoomIn.png' width = '25px'>"), style = "padding: 2px 2px; display: block"),
                    actionButton(inputId = 'zoomOut_freq', label = HTML("<img src='icons/zoomOut.png' width = '25px'>"), style = "padding: 2px 2px; display: block"),
             ),
             column(1,
                    actionButton(inputId = "selection_play", label = HTML("<img src='icons/play.png' width = '25px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = "selection_delete", label = HTML("<img src='icons/delete.png' width = '25px'>"), style = "padding: 2px 2px;")
             ),
             column(1,
                    htmlOutput('pitchAtCursor', inline = TRUE)
             ),
             column(2,
                    uiOutput('fRadios')
             ),
             column(2,
                    actionButton(inputId = 'scrollLeft', label = HTML("<img src='icons/backward.png' width = '25px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = 'zoomOut', label = HTML("<img src='icons/zoomOut.png' width = '25px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = "zoomToSel", label = HTML("<img src='icons/zoomSel.png' width = '25px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = 'zoomIn', label = HTML("<img src='icons/zoomIn.png' width = '25px'>"), style = "padding: 2px 2px;"),
                    actionButton(inputId = 'scrollRight', label = HTML("<img src='icons/forward.png' width = '25px'>"), style = "padding: 2px 2px;")
             ),
             column(2,
                    htmlOutput('spectrum_cursor', inline = TRUE)
             ),
             column(2,
                    htmlOutput('spectrum_peak', inline = TRUE)
             ),
             column(1,
                    # add smth if needed
             )
           ),
           fluidRow(
             column(
               plotOutput('spectrogram', height = '500px', click = "spectrogram_click", dblclick = dblclickOpts(id = "spectrogram_dblclick"), hover = hoverOpts(id = "spectrogram_hover"), brush = brushOpts(id = 'spectrogram_brush', resetOnNew = TRUE)),  # , style = "max-width: 66vw; overflow-x: auto;"
               plotOutput('oscillogram', height = '100px'),
               plotOutput('ann_plot', height = '100px', click = "ann_click", dblclick = dblclickOpts(id = "ann_dblclick")),
               width = 7
             ),
             column(
               plotOutput('spectrum', height = '500px', click = "spectrum_click", dblclick = dblclickOpts(id = "spectrum_dblclick"), hover = hoverOpts(id = "spectrum_hover"), brush = brushOpts(id = 'spectrum_brush', resetOnNew = TRUE)),
               sliderInput('spectrum_smooth', 'Smoothing', value=-1, min=-2, max=0, step=.05),
               tableOutput('ann_table'),
               width = 5
             )
           ),

           #fluidRow(
           # htmlOutput('statusBar')  # status bar here
           #),
           width = 9
    )
  )
)
