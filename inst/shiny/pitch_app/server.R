# TODO: add a routine for manually adding/removing pitch candidates; enter them as a new pitchMethod ("manual"), with very high certainty (eg 5, 10, 1000 - test). Rerun pathfinder each time something is modified; the resulting contour may still not pass through the manual candidates despite their high certainty, so after pathfinder replace pitch values at the updated contour with the manually specified values

# Also: check why it crashes with pitch cepstral

server = function(input, output, session) {
  myPars = reactiveValues()
  myPars$startedManual = FALSE

  output$spectrogram = renderPlot({
    print('running spectrogram')
    spectrogram(  # instead of re-loading the file every time, save the spectrogram matrix and re-draw manually with filled.contour.mod
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
    print('running load audio')
    myPars$myAudio_path = input$loadAudio$datapath
    myPars$myAudio_type = input$loadAudio$type
    temp_audio = tuneR::readWave(input$loadAudio$datapath)
    myPars$myAudio = as.numeric(temp_audio@left)
    myPars$samplingRate = temp_audio@samp.rate
    myPars$dur = length(temp_audio@left) / temp_audio@samp.rate * 1000

    # playme(myPars$myAudio_path)
    output$myAudio = renderUI(
      tags$audio(src = myPars$myAudio_path, type = myPars$myAudio_type, autoplay = NA, controls = NA)  # refuses to work - try with shinyFiles library
    )
  })

  obs_anal = observeEvent(input$loadAudio$datapath, {
    print('running anal')
    if (!is.null(input$loadAudio$datapath)) {
      temp_anal = analyze(
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

      # add a new category for manual pitch values
      temp_anal$pitchCands = rbind(temp_anal$pitchCands,
                                   rep(NA, ncol(temp_anal$pitchCands)))
      temp_anal$pitchSource = rbind(temp_anal$pitchSource,
                                    rep('manual', ncol(temp_anal$pitchSource)))
      temp_anal$pitchCert = rbind(temp_anal$pitchCert,
                                  rep(NA, ncol(temp_anal$pitchCert)))

      myPars$df = temp_anal
      windowLength_points = floor(input$windowLength / 1000 * myPars$samplingRate / 2) * 2
      myPars$X = seq(1, max(1, (length(myPars$myAudio) - windowLength_points)),
                     length.out = nrow(myPars$df$result)) / myPars$samplingRate * 1000 + input$windowLength / 2
      # add: update defaults that depend on samplingRate, eg cepSmooth
    }
  })

  obs_pitch = observeEvent(myPars$df$pitchCands, {
    if (length(myPars$df$pitchCands) > 0) {
      print('running voiced segments')
      myPars$voicedSegments = findVoicedSegments(
        myPars$df$pitchCands,
        shortestSyl = input$shortestSyl,
        shortestPause = input$shortestPause,
        minVoicedCands = input$minVoicedCands,
        pitchMethods = input$pitchMethods,
        step = input$step,
        samplingRate = input$samplingRate
      )

      # for each syllable, impute NA's and find a nice path through pitch candidates
      myPars$pitch = rep(NA, ncol(myPars$df$pitchCands))
      if (nrow(myPars$voicedSegments) > 0) {
        # if we have found at least one putatively voiced syllable
        for (syl in 1:nrow(myPars$voicedSegments)) {
          myseq = myPars$voicedSegments$segmentStart[syl]:myPars$voicedSegments$segmentEnd[syl]
          # print(myseq)
          # print(myPars$df$pitchCands[, myseq, drop = FALSE])
          # compute the optimal path through pitch candidates
          myPars$pitch[myseq] = pathfinder(
            pitchCands = myPars$df$pitchCands[, myseq, drop = FALSE],
            pitchCert = myPars$df$pitchCert[, myseq, drop = FALSE],
            certWeight = input$certWeight,
            pathfinding = input$pathfinding,
            interpolWin = input$interpolWin,
            interpolTol = input$interpolTol,
            interpolCert = input$interpolCert,
            snakeStep = input$snakeStep,
            snakePlot = FALSE
          )
        }
      }
    }
  })



  observeEvent(input$spectrogram_click, {
    myPars$startedManual = TRUE
    print('running spectrogram_click')
    if (length(myPars$df$pitchCands) > 0) {
      # click_x = input$spectrogram_click$x / myPars$dur  # ranges 0 to 1
      # click_y = round(input$spectrogram_click$y, 3)
      closest_frame = which.min(abs(as.numeric(colnames(myPars$df$pitchCands)) - input$spectrogram_click$x))
      # print(c(closest_frame, click_x, click_y))
      # create a manual pitch estimate for the closest frame with the clicked value
      myPars$df$pitchCands[nrow(myPars$df$pitchCands), closest_frame] = round(input$spectrogram_click$y * 1000, 3)
      myPars$df$pitchCert[nrow(myPars$df$pitchCert), closest_frame] = 5
    }
    print(myPars$df$pitchCands[, 1:10])
  })

  # observeEvent(input$spectrogram_dblclick, {
  #   ref = data.frame(value = myPars$df$pitchCands[nrow(myPars$df$pitchCands), ])
  #   ref$time = as.numeric(colnames(myPars$df$pitchCands))
  #   closestPoint = nearPoints(ref, input$spectrogram_dblclick,  xvar = 'time',
  #                             yvar = 'value', threshold = 100000, maxpoints = 1)
  #   idx = as.numeric(rownames(closestPoint))
  #   # we can remove any anchor except the first and the last (because ampl
  #   # opening at start and end of sound has to be defined)
  #   if (length(idx) > 0) {
  #     myPars$df$pitchCands[nrow(myPars$df$pitchCands), idx] = NA
  #   }
  # })

  # observeEvent(input$about, {
  #   id <<- showNotification(
  #     ui = paste0("SoundGen ", packageVersion('soundgen'), ". Load/detach library(shinyBS) to show/hide tips. Project home page: http://cogsci.se/soundgen.html. Contact me at andrey.anikin / at / rambler.ru. Thank you!"),
  #     duration = 10,
  #     closeButton = TRUE,
  #     type = 'default'
  #   )
  # })
}
