# TODO: add spectrogram controls to control pitch candidates (pch, cex, etc.); handle folders as input; make the side pane with par-s collapsible; check pathfinding_slow with manual

server = function(input, output, session) {
  myPars = reactiveValues()
  myPars$zoomFactor = 2
  myPars$out = NULL         # for storing the output

  # clean-up of www/ folder: remove all files except temp.wav
  files = list.files('www/')
  files = files[files != 'temp.wav']
  for (f in files){
    file.remove(paste0('www/', f))
  }

  reset = function() {
    myPars$pitch = NULL       # pitch contour
    myPars$pitchCands = NULL
    myPars$bp = NULL          # selected points
    myPars$manual = data.frame(frame = NA, freq = NA)[-1, ]
    myPars$manualUnv = numeric()
  }

  observeEvent(input$loadAudio, {
    print('running load audio')
    myPars$n = 1
    myPars$nFiles = nrow(input$loadAudio)
    reset()
    readAudio(1)
    # instead of re-loading the file every time, save the spectrogram matrix
    # and re-draw manually with filled.contour.mod
    extractSpectrogram()
  })

  readAudio = function(i) {
    temp = input$loadAudio[i, ]
    myPars$myAudio_filename = temp$name
    myPars$myAudio_path = temp$datapath
    myPars$myAudio_type = temp$type
    myPars$temp_audio = tuneR::readWave(temp$datapath)
    myPars$myAudio = as.numeric(myPars$temp_audio@left)
    myPars$samplingRate = myPars$temp_audio@samp.rate
    myPars$dur = round(length(myPars$temp_audio@left) / myPars$temp_audio@samp.rate * 1000)
    updateSliderInput(session, inputId = 'spec_xlim', value = c(0, myPars$dur), max = myPars$dur)
  }

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
        xlim = input$spec_xlim, # c(0, tail(as.numeric(colnames(myPars$spec)), 1)),
        xlab = 'Time, ms', ylab = 'Frequency, kHz',
        main = '',
        ylim = c(input$spec_ylim[1], input$spec_ylim[2])
      )
      # add manual values to the list of pitch candidates for seamless plotting
      n = ncol(myPars$pitchCands$freq)
      if (n > 0 & nrow(myPars$manual) > 0) {
        temp_freq = rep(NA, n)
        temp_freq[myPars$manual$frame] = myPars$manual$freq
        temp_freq = rbind(myPars$pitchCands$freq, temp_freq)
        temp_cert = rbind(myPars$pitchCands$cert, rep(1, n))  # change 1 to input$manualCert
        temp_source = rbind(myPars$pitchCands$source, rep('manual', n))
      } else {
        temp_freq = myPars$pitchCands$freq
        temp_cert = myPars$pitchCands$cert
        temp_source = myPars$pitchCands$source
      }
      addPitchCands(
        pitchCands = temp_freq,
        pitchCert = temp_cert,
        pitchSource = temp_source,
        pitch = myPars$pitch,
        addToExistingPlot = TRUE,
        showLegend = TRUE,
        ylim = c(input$spec_ylim[1], input$spec_ylim[2])
      )
    }
  })

  obs_anal = observe({
    if (!is.null(input$loadAudio$datapath)) {
      print('running anal')
      temp_anal = analyze(
        myPars$myAudio_path,
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
            nrow(myPars$manual) > 0) {
          # if the number of frames has changed (new windowLengh or step),
          # up/downsample manual pitch candidates accordingly
          len_old = length(myPars$pitch)  # !!! switch to myPars$manual
          len_new = ncol(myPars$pitchCands$freq)
          myPars$manual$frame = ceiling(myPars$manual$frame * len_new / len_old)
          # pitch_newLen = rep(NA, len = len_new)
          # idx_old = which(!is.na(myPars$pitch))
          # idx_new = round(idx_old * len_new / len_old)
          # pitch_newLen[idx_new] = myPars$pitch[idx_old]
          # myPars$pitch = pitch_newLen
          # myPars$pitchCands$freq[nrow(myPars$pitchCands$freq), ] = myPars$pitch
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
            pitchSource = myPars$pitchCands$source[, myseq, drop = FALSE],
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
        myPars$pitch = medianSmoother(as.data.frame(myPars$pitch),
                                      smoothing_ww = smoothing_ww,
                                      smoothingThres = smoothingThres,
                                      inviolable = myPars$manual$frame)[, 1]
      }
    }
  }

  observeEvent(input$spectrogram_click, {
    if (length(myPars$pitchCands$freq) > 0 & input$spectro_clickAct == 'addCand') {
      session$resetBrush("spectrogram_brush")  # doesn't reset automatically for some reason
      closest_frame = which.min(abs(
        as.numeric(colnames(myPars$pitchCands$freq)) - input$spectrogram_click$x))
      # create a manual pitch estimate for the closest frame with the clicked value
      new_freq = round(input$spectrogram_click$y * 1000, 3)
      if (closest_frame %in% myPars$manual$frame) {
        myPars$manual$freq[myPars$manual$frame == closest_frame] = new_freq
      } else {
        myPars$manual = rbind(myPars$manual,
                              data.frame(frame = closest_frame, freq = new_freq))
      }
      myPars$manual = myPars$manual[order(myPars$manual$frame), ]  # just to keep things tidy
      # if this frame was manually flagged as unvoiced, remove this flag
      idx_rem = which(myPars$manualUnv == closest_frame)
      if (length(idx_rem) > 0) myPars$manualUnv = myPars$manualUnv[-idx_rem]
      obs_pitch()
    }
  })

  observeEvent(input$spectrogram_dblclick, {
    if (nrow(myPars$manual) > 0) {
      closest_frame = which.min(abs(as.numeric(colnames(myPars$pitchCands$freq)) -
                                      input$spectrogram_dblclick$x))
      if (length(closest_frame) > 0) {
        idx_rem = which(myPars$manual$frame == closest_frame)
        if (length(idx_rem) > 0) myPars$manual = myPars$manual[-idx_rem, ]
        obs_pitch()
      }
    }
  })

  observeEvent(input$selection_unvoice, {
    print('Unvoicing selection')
    if (!is.null(myPars$bp) & length(myPars$brush_sel_xy) > 0) {
      myPars$manualUnv = c(myPars$manualUnv, myPars$brush_sel_xy)
      # remove manual anchors within selection, if any
      idx_rem = which(myPars$manual$frame %in% myPars$manualUnv)
      if (length(idx_rem) > 0) myPars$manual = myPars$manual[-idx_rem, ]
      obs_pitch()
    }
  })

  observeEvent(input$selection_voice, {
    print('Voicing selection')
    if (!is.null(myPars$bp) &
        length(myPars$brush_sel_x) > 0 &
        length(myPars$manualUnv) > 0) {
      idx_rem = which(myPars$manualUnv %in% myPars$brush_sel_x)
      if (length(idx_rem) > 0) myPars$manualUnv = myPars$manualUnv[-idx_rem]
      obs_pitch()
    }
  })

  observeEvent(input$selection_octaveUp, {
    print('Selection octave up')
    if (!is.null(myPars$bp) & length(myPars$brush_sel_xy) > 0) {
      # remove previous manual cands in selection
      idx_rem = which(myPars$manual$frame %in% myPars$brush_sel_xy)
      if (length(idx_rem) > 0) myPars$manual = myPars$manual[-idx_rem, ]
      # add the new ones
      myPars$manual = rbind(myPars$manual, data.frame(
        frame = myPars$brush_sel_xy,
        freq = myPars$pitch[myPars$brush_sel_xy] * 2
      ))
      # make sure we stay within pitchFloor/pitchCeiling
      myPars$manual[myPars$manual < input$pitchFloor] = input$pitchFloor
      myPars$manual[myPars$manual > input$pitchCeiling] = input$pitchCeiling
      obs_pitch()
    }
  })

  observeEvent(input$selection_octaveDown, {
    print('Selection octave down')
    if (!is.null(myPars$bp) & length(myPars$brush_sel_xy) > 0) {
      # remove previous manual cands in selection
      idx_rem = which(myPars$manual$frame %in% myPars$brush_sel_xy)
      if (length(idx_rem) > 0) myPars$manual = myPars$manual[-idx_rem, ]
      # add the new ones
      myPars$manual = rbind(myPars$manual, data.frame(
        frame = myPars$brush_sel_xy,
        freq = myPars$pitch[myPars$brush_sel_xy] / 2
      ))
      # make sure we stay within pitchFloor/pitchCeiling
      myPars$manual[myPars$manual < input$pitchFloor] = input$pitchFloor
      myPars$manual[myPars$manual > input$pitchCeiling] = input$pitchCeiling
      obs_pitch()
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
    myPars$pitch_df = data.frame(
      time = as.numeric(colnames(myPars$pitchCands$freq)),
      freq = myPars$pitch / 1000
    )
    myPars$bp = brushedPoints(myPars$pitch_df,
                              brush = input$spectrogram_brush,
                              xvar = 'time', yvar = 'freq',
                              allRows = TRUE)
    myPars$brush_sel_xy = which(myPars$bp[, 'selected_'] == TRUE)  # selected pitch points
    myPars$brush_sel_x = which(myPars$pitch_df$time > input$spectrogram_brush$xmin &
                                 myPars$pitch_df$time < input$spectrogram_brush$xmax)  # selected frames (along x axis)
    # for ex., to unvoice selection: myPars$pitch[myPars$bp[, 'selected_'] == TRUE] = NA
    # print(bp)
  })

  changeZoom = function(coef) {
    midpoint = mean(input$spec_xlim)
    halfRan = diff(input$spec_xlim) / 2 / coef
    newLeft = max(0, midpoint - halfRan)
    newRight = min(myPars$dur, midpoint + halfRan)
    updateSliderInput(session, inputId = 'spec_xlim', value = c(newLeft, newRight))
  }
  observeEvent(input$zoomIn, changeZoom(myPars$zoomFactor))
  observeEvent(input$zoomOut, changeZoom(1 / myPars$zoomFactor))
  observeEvent(input$selection_zoomToSel, {
    if (!is.null(myPars$bp)) {
      updateSliderInput(session, inputId = 'spec_xlim',
                        value = round(c(input$spectrogram_brush$xmin,
                                        input$spectrogram_brush$xmax)))
    }
  })

  shiftFrame = function(direction) {
    ran = diff(input$spec_xlim)
    if (direction == 'left') {
      newLeft = max(0, input$spec_xlim[1] - ran)
      newRight = newLeft + ran
    } else if (direction == 'right') {
      newRight = min(myPars$dur, input$spec_xlim[2] + ran)
      newLeft = newRight - ran
    }
    updateSliderInput(session, 'spec_xlim', value = c(newLeft, newRight))
  }
  observeEvent(input$scrollLeft, shiftFrame('left'))
  observeEvent(input$scrollRight, shiftFrame('right'))

  done = function() {
    new = data.frame(
      file = basename(myPars$myAudio_filename),
      pitch = paste0('{', paste(round(myPars$pitch), collapse = ', '), '}')
    )
    if (is.null(myPars$out)) {
      myPars$out = new
    } else {
      idx = which(myPars$out$file == new$file)
      if (length(idx) == 1) {
        myPars$out$pitch[idx] = new$pitch
      } else {
        myPars$out = rbind(myPars$out, new)
      }
    }
  }

  nextFile = function() {
    done()
    if (myPars$n < myPars$nFiles) {
      myPars$n = myPars$n + 1
      reset()
      readAudio(myPars$n)
      extractSpectrogram()
    }
  }
  observeEvent(input$nextFile, nextFile())

  lastFile = function() {
    done()
    if (myPars$n > 1) {
      myPars$n = myPars$n - 1
      reset()
      readAudio(myPars$n)
      extractSpectrogram()
      # re-load the manual pitch contour for the previous file - maybe remember myPars$manual instead
      # pitch_temp = as.character(myPars$out$pitch[myPars$n])
      # pitch_temp = substr(pitch_temp, 2, (nchar(pitch_temp) - 1))
      # pitch_temp = eval(parse(text = noquote(paste0('c(', pitch_temp, ')'))))
      # myPars$pitch = pitch_temp
    }
  }
  observeEvent(input$lastFile, lastFile())

  output$saveRes = downloadHandler(
    filename = function() 'output.csv',
    content = function(filename) {
      done()  # finalize the last file
      write.csv(myPars$out, filename, row.names = FALSE)
    }
  )
  # observeEvent(input$about, {
  #   id <<- showNotification(
  #     ui = paste0("SoundGen ", packageVersion('soundgen'), ". Load/detach library(shinyBS) to show/hide tips. Project home page: http://cogsci.se/soundgen.html. Contact me at andrey.anikin / at / rambler.ru. Thank you!"),
  #     duration = 10,
  #     closeButton = TRUE,
  #     type = 'default'
  #   )
  # })
}
