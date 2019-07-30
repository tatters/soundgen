# TODO: add a routine for manually adding/removing pitch candidates; enter them as a new pitchMethod ("manual"), with very high certainty (eg 5, 10, 1000 - test). Rerun pathfinder each time something is modified; the resulting contour may still not pass through the manual candidates despite their high certainty, so after pathfinder replace pitch values at the updated contour with the manually specified values

# Also: check why it crashes with pitch cepstral

server = function(input, output, session) {
  myPars = reactiveValues()

  output$spectrogram = renderPlot({
    spectrogram(
      input$loadAudio$datapath,
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
    addPitchCands(
      pitchCands = myPars$df$pitchCands,
      pitchCert = myPars$df$pitchCert,
      pitchSource = myPars$df$pitchSource,
      pitch = myPars$df$result$pitch,
      # candPlot = candPlot,
      # pitchPlot = pitchPlot,
      addToExistingPlot = TRUE,
      showLegend = TRUE,
      ylim = c(input$spec_ylim[1], input$spec_ylim[2])
    )
  })

  observeEvent(input$loadAudio, {
    myPars$myAudio_path = input$loadAudio$datapath
    myPars$myAudio_type = input$loadAudio$type
    temp_audio = tuneR::readWave(input$loadAudio$datapath)
    myPars$myAudio = as.numeric(temp_audio@left)
    myPars$samplingRate = temp_audio@samp.rate

    # playme(myPars$myAudio_path)
    output$myAudio = renderUI(
      tags$audio(src = myPars$myAudio_path, type = myPars$myAudio_type, autoplay = NA, controls = NA)  # refuses to work - try with shinyFiles library
    )
  })

  obs_anal = observe({
    if (!is.null(input$loadAudio$datapath)) {
      myPars$df = analyze(
        input$loadAudio$datapath,
        windowLength = input$windowLength,
        step = input$step,
        overlap = input$overlap,
        wn = input$wn,
        zp = input$zp,
        dynamicRange = input$dynamicRange,
        silence = input$silence,
        entropyThres = input$entropyThres,
        nFormants = 1,
        pitchMethods = input$pitchMethods,
        pitchFloor = input$pitchFloor,
        pitchCeiling = input$pitchCeiling,
        priorMean = input$priorMean,
        priorSD = input$priorSD,
        priorPlot = FALSE,
        nCands = input$nCands,
        minVoicedCands = input$minVoicedCands,
        domThres = input$domThres,
        domSmooth = input$domSmooth,
        autocorThres = input$autocorThres,
        autocorSmooth = input$autocorSmooth,
        cepThres = input$cepThres,
        cepSmooth = input$cepSmooth,
        cepZp = input$cepZp,
        specThres = input$specThres,
        specPeak = input$specPeak,
        specSinglePeakCert = input$specSinglePeakCert,
        specHNRslope = input$specHNRslope,
        specSmooth = input$specSmooth,
        specMerge = input$specMerge,
        shortestSyl = input$shortestSyl,
        shortestPause = input$shortestPause,
        interpolWin = input$interpolWin,
        interpolTol = input$interpolTol,
        interpolCert = input$interpolCert,
        pathfinding = input$pathfinding,
        # annealPars = list(maxit = 5000, temp = 1000),
        certWeight = input$certWeight,
        snakeStep = input$snakeStep,
        snakePlot = FALSE,
        smooth = input$smooth,
        smoothVars = c('pitch'),
        summary = 'extended',
        plot = FALSE
      )
      windowLength_points = floor(input$windowLength / 1000 * myPars$samplingRate / 2) * 2
      myPars$X = seq(1, max(1, (length(myPars$myAudio) - windowLength_points)),
                     length.out = nrow(myPars$df$result)) / myPars$samplingRate * 1000 + input$windowLength / 2

      # add: update defaults that depend on samplingRate, eg cepSmooth
    }
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
