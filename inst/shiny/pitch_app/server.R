# TODO: manually added pitch values should affect syllable structure; add spectrogram controls to control pitch candidates (pch, cex, etc.); add zoom; export pitch contour; handle folders as input; action buttons for doing smth with selection (octave up/down, devoice, set prior based on selection, etc. ...)

server = function(input, output, session) {
  # clean-up of www/ folder: remove all files except temp.wav
  files = list.files('www/')
  files = files[files != 'temp.wav']
  for (f in files){
    file.remove(paste0('www/', f))
  }

  myPars = reactiveValues()
  myPars$myAudio_path = NULL
  myPars$pitch = NULL
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

      # isolate({
      # hover_temp = input$spectrogram_hover
      # if (!is.null(hover_temp)) {
      #   abline(v = hover_temp$x, lty = 2)
      #   abline(h = hover_temp$y, lty = 2)
      #   text(x = hover_temp$x,
      #        y = hover_temp$y,
      #        labels = paste(round(hover_temp$y * 1000), 'Hz'),
      #        adj = c(1, 1))
      # text(x = 0,
      #      y = hover_temp$y,
      #      labels = paste(round(hover_temp$x), 'ms'),
      #      adj = c(0.5, 0))
      # print(c(hover_temp$x, hover_temp$y))
      # }
      # })
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
    bp = brushedPoints(myPars$pitch_df,
                       brush = input$spectrogram_brush,
                       xvar = 'time', yvar = 'freq',
                       allRows = TRUE)
    # for ex., to unvoice selection: myPars$pitch[bp[, 'selected_'] == TRUE] = NA
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

      # if running analyze() for the same audio, preserve the old manual values
      # and paste them back in
      isolate({
        if (!is.null(myPars$pitch)) {
          # if the number of frames has changed (new windowLengh or step),
          # up/downsample the manual pitch contour accordingly
          len_old = length(myPars$pitch)
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
