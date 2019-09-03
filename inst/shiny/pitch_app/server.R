# TODO: plot manual pitch values; action buttons for doing smth with selection (octave up/down, devoice, set prior based on selection, etc. ...); manually added pitch values should affect syllable structure (extend pitch contour into previously unvoiced regions); add spectrogram controls to control pitch candidates (pch, cex, etc.); add zoom; export pitch contour; handle folders as input; make the side pane with par-s collapsible; distinguish between unspecified manual values and manually unvoiced values (currently both are NA) - a sort of "inviolable"

server = function(input, output, session) {
  # clean-up of www/ folder: remove all files except temp.wav
  files = list.files('www/')
  files = files[files != 'temp.wav']
  for (f in files){
    file.remove(paste0('www/', f))
  }

  myPars = reactiveValues()
  myPars$myAudio_path = NULL
  myPars$pitch = NULL       # pitch contour
  myPars$bp = NULL          # selected points
  myPars$anyManual = FALSE  # keep track of whether any manual pitch values have been added
  myPars$manual = data.frame(frame = NA, freq = NA)[-1, ]
  myPars$manualUnv = numeric()
  myPars$clicksAfterBrushing = 2

  observeEvent(input$loadAudio, {
    print('running load audio')
    myPars$pitch = NULL
    myPars$pitchCands = NULL
    myPars$myAudio_path = input$loadAudio$datapath
    myPars$myAudio_type = input$loadAudio$type
    myPars$temp_audio = tuneR::readWave(input$loadAudio$datapath)
    myPars$myAudio = as.numeric(myPars$temp_audio@left)
    myPars$samplingRate = myPars$temp_audio@samp.rate
    myPars$dur = length(myPars$temp_audio@left) / myPars$temp_audio@samp.rate * 1000

    # instead of re-loading the file every time, save the spectrogram matrix and re-draw manually with filled.contour.mod
    extractSpectrogram()
  })

  extractSpectrogram = reactive({
    if (!is.null(myPars$myAudio)) {
      myPars$spec = spectrogram(
        myPars$myAudio,
        samplingRate = myPars$samplingRate,
        windowLength = input$windowLength,
        step = input$step,
        wn = input$wn,
        zp = input$zp,
        contrast = input$specContrast,
        brightness = input$specBrightness,
        output = 'processed'
      )
    }
  })

  saveAudio = observeEvent(myPars$temp_audio, {
    print('running saveAudio')
    # Method: saves a temporary audio file in 'www/'. This is a workaround since
    # html tag for some reason cannot play myPars$myAudio_path (although feeding
    # it to spectrogram works - so probably only works within R). Alternatives:
    # soundgen::play() or shinyFiles library

    # first remove the previous sound file to avoid cluttering up the www/ folder
    if (!is.null(myPars$myfile)){
      file.remove(paste0('www/', myPars$myfile))
    }
    randomID = paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = '')
    myPars$myfile = paste0(randomID, '.wav')
    # this is the new sound file. NB: has to be saved in www/ !!!
    seewave::savewav(myPars$temp_audio,
                     f = myPars$samplingRate,
                     filename = paste0('www/', myPars$myfile))
    output$myAudio = renderUI(
      tags$audio(src = myPars$myfile, type = myPars$myAudio_type, autoplay = NA, controls = NA)
    )
  })

  output$spectrogram = renderPlot({
    print('drawing spectrogram')
    par(mar = c(2, 2, 0.5, 2))  # no need to save use's graphical par-s - they revert to orig on exit
    if (is.null(myPars$myAudio_path) | is.null(myPars$spec)) {
      plot(1:10, type = 'n', bty = 'n', axes = FALSE, xlab = '', ylab = '')
      text(x = 5, y = 5, labels = 'Upload an audio file to begin...')
    } else {
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
    }
  })

  hover_label = reactive({
    hover_temp = input$spectrogram_hover
    if (!is.null(hover_temp) & !is.null(myPars$myAudio_path)) {
      label = paste('<h4>Pitch at cursor: ', round(hover_temp$y * 1000), 'Hz</h4>')
    } else {
      label = '<h4>Pitch at cursor: </h4>'
    }
    return(label)
  })
  output$spectro_hover = renderUI(HTML(hover_label()))

  brush = observeEvent(input$spectrogram_brush, {
    print('running brush')
    myPars$clicksAfterBrushing = 0
    myPars$pitch_df = data.frame(
      time = as.numeric(colnames(myPars$pitchCands$freq)),
      freq = myPars$pitch / 1000
    )
    myPars$bp = brushedPoints(myPars$pitch_df,
                              brush = input$spectrogram_brush,
                              xvar = 'time', yvar = 'freq',
                              allRows = TRUE)
    # for ex., to unvoice selection: myPars$pitch[myPars$bp[, 'selected_'] == TRUE] = NA
    # print(bp)
  })

  obs_anal = observe({
    if (!is.null(input$loadAudio$datapath)) {
      print('running anal')
      temp_anal = analyze(
        input$loadAudio$datapath,
        windowLength = input$windowLength,
        step = input$step,
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

      myPars$pitchCands = temp_anal$pitchCands
      windowLength_points = floor(input$windowLength / 1000 * myPars$samplingRate / 2) * 2
      myPars$X = seq(
        1,
        max(1, (length(myPars$myAudio) - windowLength_points)),
        length.out = nrow(temp_anal$result)
      ) / myPars$samplingRate * 1000 + input$windowLength / 2
      # add: update defaults that depend on samplingRate, eg cepSmooth

      # if running analyze() for the same audio, preserve the old manual values
      # (if any) and paste them back in
      isolate({
        if (!is.null(myPars$pitch) &
            myPars$anyManual) {
          # if the number of frames has changed (new windowLengh or step),
          # up/downsample the manual pitch contour accordingly
          len_old = length(myPars$pitch)  # !!! switch to myPars$manual
          len_new = ncol(myPars$pitchCands$freq)
          pitch_newLen = rep(NA, len = len_new)
          idx_old = which(!is.na(myPars$pitch))
          idx_new = round(idx_old * len_new / len_old)
          pitch_newLen[idx_new] = myPars$pitch[idx_old]
          myPars$pitch = pitch_newLen
          myPars$pitchCands$freq[nrow(myPars$pitchCands$freq), ] = myPars$pitch
        }
        obs_pitch()  # run pathfinder
      })
    }
  })

  obs_pitch = function() {
    print('running obs_pitch')
    if (length(myPars$pitchCands$freq) > 0) {
      myPars$voicedSegments = findVoicedSegments(
        myPars$pitchCands$freq,
        manualV = myPars$manual$frame,
        manualUnv = myPars$manualUnv,
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
          manual_syl = myPars$manual[myPars$manual$frame %in% myseq, ]
          manual_syl$frame = manual_syl$frame - myseq[1] + 1  # adjust manual idx to syllable
          # compute the optimal path through pitch candidates
          myPars$pitch[myseq] = pathfinder(
            pitchCands = myPars$pitchCands$freq[, myseq, drop = FALSE],
            pitchCert = myPars$pitchCands$cert[, myseq, drop = FALSE],
            manual = manual_syl,
            certWeight = input$certWeight,
            pathfinding = ifelse(input$pathfinding == 'slow',
                                 'fast',  # slow doesn't work well with manual cand-s
                                 input$pathfinding),
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
    myPars$clicksAfterBrushing = myPars$clicksAfterBrushing + 1
    if (length(myPars$pitchCands$freq) > 0 & myPars$clicksAfterBrushing > 2) {
      session$resetBrush("spectrogram_brush")  # doesn't reset automatically for some reason
      closest_frame = which.min(abs(as.numeric(colnames(myPars$pitchCands$freq)) - input$spectrogram_click$x))
      # create a manual pitch estimate for the closest frame with the clicked value
      myPars$manual = rbind(myPars$manual, data.frame(
        frame = closest_frame,
        freq = round(input$spectrogram_click$y * 1000, 3)
      ))
      obs_pitch()
    }
  })

  observeEvent(input$spectrogram_dblclick, {
    if (nrow(myPars$manual) > 0) {
      closest_frame = which.min(abs(as.numeric(colnames(myPars$pitchCands$freq)) -
                                      input$spectrogram_dblclick$x))
      if (length(closest_frame) > 0) {
        idx_rem = which(myPars$manual$frame == closest_frame)
        if (length(idx_rem) > 0) {
          myPars$manual = myPars$manual[-idx_rem, ]
        }
        obs_pitch()
      }
    }
  })

  observeEvent(input$selection_unvoice, {
    print('Unvoicing selection')
    if (!is.null(myPars$bp)) {
      # myPars$pitch[myPars$bp[, 'selected_'] == TRUE] = NA
      myPars$manualUnv = c(myPars$manualUnv, which(myPars$bp[, 'selected_']))
      obs_pitch()
    }
  })

  observeEvent(input$selection_voice, {
    print('Voicing selection')
    if (!is.null(myPars$bp) & length(myPars$manualUnv) > 0) {
      # myPars$pitch[myPars$bp[, 'selected_'] == TRUE] = NA
      idx_rem = which(myPars$manualUnv %in% myPars$bp[, 'selected_'])
      myPars$manualUnv = myPars$manualUnv[-idx_rem]
      obs_pitch()
    }
  })

  observeEvent(input$selection_octaveUp, {
    print('Selection octave up')
    if (!is.null(myPars$bp)) {
      myPars$pitch[myPars$bp[, 'selected_'] == TRUE] = myPars$pitch[myPars$bp[, 'selected_'] == TRUE] * 2
    }
  })

  observeEvent(input$selection_octaveDown, {
    print('Selection octave down')
    if (!is.null(myPars$bp)) {
      myPars$pitch[myPars$bp[, 'selected_'] == TRUE] = myPars$pitch[myPars$bp[, 'selected_'] == TRUE] / 2
    }
  })

  observeEvent(input$selection_setPrior, {
    print('Setting prior')
    if (!is.null(input$spectrogram_brush)) {
      pr = c(input$spectrogram_brush$ymin, input$spectrogram_brush$ymax) * 1000
      pr[pr < input$pitchFloor] = input$pitchFloor
      pr[pr > input$pitchCeiling] = input$pitchCeiling
      meanPr = mean(pr)
      sdPr = round((HzToSemitones(pr[2]) - HzToSemitones(mean(pr))) / 2, 1)
      updateSliderInput(session, 'priorMean', value = meanPr)
      updateSliderInput(session, 'priorSD', value = sdPr)
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
