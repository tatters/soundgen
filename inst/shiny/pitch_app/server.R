# TODO: manually added pitch values should affect syllable structure; voicing bar; add comments to clarify that the last row of pitchCands is always "manual" (removed re-ordering of pitch candidates by frequency - check that works for all pathfinding methods); preserve manual values when some pars change and re-run analyze(); display last added manual value OR current mouse position as "main" above spectrogram; add spectrogram controls to control pitch candidates (pch, cex, etc.); add zoom; export pitch contour; handle folders as input; pathfinding "slow" should either respect manual or be disabled; fix audio playback

server = function(input, output, session) {
  myPars = reactiveValues()

  observeEvent(input$loadAudio, {
    print('running load audio')
    # myPars$pitch = NULL
    # myPars$pitchCands = null
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

    # instead of re-loading the file every time, save the spectrogram matrix and re-draw manually with filled.contour.mod
    myPars$spec = spectrogram(
      input$loadAudio$datapath,
      windowLength = input$windowLength,
      step = input$step,
      overlap = input$overlap,
      wn = input$wn,
      zp = input$zp,
      contrast = input$specContrast,
      brightness = input$specBrightness,
      output = 'processed'
    )
  })

  output$spectrogram = renderPlot({
    print('running spectrogram')
    if (input$spec_colorTheme == 'bw') {
      color.palette = function(x) gray(seq(from = 1, to = 0, length = x))
    } else if (input$spec_colorTheme == 'seewave') {
      color.palette = seewave::spectro.colors
    } else {
      colFun = match.fun(input$spec_colorTheme)
      color.palette = function(x) rev(colFun(x))
    }
    filled.contour.mod(
      x = as.numeric(colnames(myPars$spec)),
      y = as.numeric(rownames(myPars$spec)),
      z = t(myPars$spec),
      levels = seq(0, 1, length = 30),
      color.palette = color.palette,
      xlim = c(0, tail(as.numeric(colnames(myPars$spec)), 1)),
      xlab = 'Time, ms', ylab = 'Frequency, kHz',
      main = '',
      ylim = c(input$spec_ylim[1], input$spec_ylim[2])
    )

    addPitchCands(
      pitchCands = myPars$pitchCands$freq,
      pitchCert = myPars$pitchCands$cert,
      pitchSource = myPars$pitchCands$source,
      pitch = myPars$pitch,
      # candPlot = candPlot,
      # pitchPlot = pitchPlot,
      addToExistingPlot = TRUE,
      showLegend = TRUE,
      ylim = c(input$spec_ylim[1], input$spec_ylim[2])
    )
  })

  obs_anal = observe({
    if (!is.null(input$loadAudio$datapath)) {
      print('running anal')
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
        nFormants = 0,     # disable formant tracking
        SPL_measured = 0,  # disable loudness analysis
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
      temp_anal$pitchCands$freq = rbind(temp_anal$pitchCands$freq,
                                        rep(NA, ncol(temp_anal$pitchCands$freq)))
      temp_anal$pitchCands$cert = rbind(temp_anal$pitchCands$cert,
                                        rep(NA, ncol(temp_anal$pitchCands$cert)))
      temp_anal$pitchCands$source = rbind(temp_anal$pitchCands$source,
                                          rep('manual', ncol(temp_anal$pitchCands$source)))

      # isolate(myPars$pitchCands <- temp_anal$pitchCands)  # to avoid re-running analyze when myPars$pitchCands changes as manual pitch values are added?
      myPars$pitchCands = temp_anal$pitchCands
      windowLength_points = floor(input$windowLength / 1000 * myPars$samplingRate / 2) * 2
      myPars$X = seq(1, max(1, (length(myPars$myAudio) - windowLength_points)),
                     length.out = nrow(temp_anal$result)) / myPars$samplingRate * 1000 + input$windowLength / 2
      # add: update defaults that depend on samplingRate, eg cepSmooth
      isolate(obs_pitch())
    }
  })

  obs_pitch = function() {
    print('running obs_pitch')
    if (length(myPars$pitchCands$freq) > 0) {
      myPars$voicedSegments = findVoicedSegments(
        myPars$pitchCands$freq,
        shortestSyl = input$shortestSyl,
        shortestPause = input$shortestPause,
        minVoicedCands = input$minVoicedCands,
        pitchMethods = input$pitchMethods,
        step = input$step,
        samplingRate = input$samplingRate
      )

      # for each syllable, impute NA's and find a nice path through pitch candidates
      myPars$pitch = rep(NA, ncol(myPars$pitchCands$freq))
      if (nrow(myPars$voicedSegments) > 0) {
        # if we have found at least one putatively voiced syllable
        for (syl in 1:nrow(myPars$voicedSegments)) {
          myseq = myPars$voicedSegments$segmentStart[syl]:myPars$voicedSegments$segmentEnd[syl]
          # print(myseq)
          # print(myPars$pitchCands$freq[, myseq, drop = FALSE])
          # compute the optimal path through pitch candidates
          myPars$pitch[myseq] = pathfinder(
            pitchCands = myPars$pitchCands$freq[, myseq, drop = FALSE],
            pitchCert = myPars$pitchCands$cert[, myseq, drop = FALSE],
            pitchSource = myPars$pitchCands$source[, myseq, drop = FALSE],
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

      ## Median smoothing of pitch contour
      if (input$smooth > 0) {
        points_per_sec = length(myPars$pitch) / myPars$dur * 1000
        # smooth of 1 means that smoothing window is ~100 ms
        smoothing_ww = round(input$smooth * points_per_sec / 10, 0)
        # the larger smooth, the heavier the smoothing (lower tolerance
        # threshold before values are replaced by median over smoothing window).
        # smooth of 1 gives smoothingThres of 4 semitones
        smoothingThres = 4 / input$smooth
        #print(myPars$pitchCands$source)
        manual_frames = as.vector(apply(myPars$pitchCands$freq, 2, function(x) !is.na(tail(x, 1))))
        myPars$pitch = medianSmoother(as.data.frame(myPars$pitch),
                                      smoothing_ww = smoothing_ww,
                                      smoothingThres = smoothingThres,
                                      inviolable = manual_frames)[, 1]
      }
    }
  }

  observeEvent(input$spectrogram_click, {
    if (length(myPars$pitchCands$freq) > 0) {
      closest_frame = which.min(abs(as.numeric(colnames(myPars$pitchCands$freq)) - input$spectrogram_click$x))
      # create a manual pitch estimate for the closest frame with the clicked value
      myPars$pitchCands$freq[nrow(myPars$pitchCands$freq), closest_frame] = round(input$spectrogram_click$y * 1000, 3)
      myPars$pitchCands$cert[nrow(myPars$pitchCands$cert), closest_frame] = 1
      obs_pitch()
    }
  })

  observeEvent(input$spectrogram_dblclick, {
    if (any(myPars$pitchCands$source == 'manual')) {
      closest_frame = which.min(abs(as.numeric(colnames(myPars$pitchCands$freq)) - input$spectrogram_dblclick$x))
      if (length(closest_frame) > 0) {
        myPars$pitchCands$freq[nrow(myPars$pitchCands$freq), closest_frame] = NA
        obs_pitch()
      }
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
