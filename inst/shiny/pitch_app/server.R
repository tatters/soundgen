server = function(input, output, session) {
  myPars = list()

  observeEvent(input$loadAudio, {
    myPars$myAudio_path = input$loadAudio$datapath
    myPars$myAudio_type = input$loadAudio$type
    temp_audio = tuneR::readWave(input$loadAudio$datapath)
    myPars$myAudio = as.numeric(temp_audio@left)
    myPars$samplingRate = temp_audio@samp.rate

    output$spectrogram = renderPlot({
      spectrogram(
        myPars$myAudio_path,
        windowLength = input$windowLength,
        step = input$step,
        overlap = input$overlap,
        wn = input$wn,
        zp = input$zp,
        osc = FALSE,
        xlab = 'Time, ms', ylab = 'Frequency, kHz',
        main = 'Spectrogram',
        contrast = input$specContrast,
        brightness = input$specBrightness,
        colorTheme = input$spec_colorTheme,
        ylim = c(input$spec_ylim[1], input$spec_ylim[2])
      )
    })
    # playme(myPars$myAudio_path)
    output$myAudio = renderUI(
      tags$audio(src = myPars$myAudio_path, type = myPars$myAudio_type, autoplay = NA, controls = NA)  # refuses to work - try with shinyFiles library
    )
  })

  # observeEvent(input$about, {
  #   id <<- showNotification(
  #     ui = paste0("SoundGen ", packageVersion('soundgen'), ". Load/detach library(shinyBS) to show/hide tips. Project home page: http://cogsci.se/soundgen.html. Contact me at andrey.anikin / at / rambler.ru. Thank you!"),
  #     duration = 10,
  #     closeButton = TRUE,
  #     type = 'default'
  #   )
  # })
}
