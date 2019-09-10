# TODO: maybe add a status bar somewhere that prints what function is executing; maybe prior from sel should affect only current file (?); add spectrogram controls to control pitch candidates (pch, cex, etc.); make the side pane with par-s collapsible; maybe remember manual anchors when moving back and forth between multiple files; maybe integrate with analyze() so manual pitch contour is taken into account when calculating %voiced and energy above f0 (new arg to analyze, re-run analyze at done() in pitch_app())

server = function(input, output, session) {
  myPars = reactiveValues()
  myPars$zoomFactor = 2     # zoom buttons ch+ange zoom by this factor
  myPars$print = TRUE       # if TRUE, some functions print a meassage to the console when called
  myPars$out = NULL         # for storing the output
  myPars$drawSpec = TRUE

  # clean-up of www/ folder: remove all files except temp.wav
  files = list.files('www/')
  files = files[files != 'temp.wav']
  for (f in files){
    file.remove(paste0('www/', f))
  }

  reset = function() {
    myPars$pitch = NULL       # pitch contour
    myPars$pitchCands = NULL  # matrix of pitch candidates
    myPars$bp = NULL          # selected points (under brush)
    myPars$manual = data.frame(frame = NA, freq = NA)[-1, ]  # manually added pitch anchors
    myPars$manualUnv = numeric()                             # manually unvoiced frames
    myPars$drawSpec = FALSE   # prevent the spectrogram from being redrawn needlessly
    # (only draw it after extracting it)
  }

  observeEvent(input$loadAudio, {
    if (myPars$print) print('Loading audio...')
    myPars$n = 1   # file number in queue
    myPars$nFiles = nrow(input$loadAudio)  # number of uploaded files in queue
    reset()
    readAudio(1)  # read the first sound in queue
  })

  readAudio = function(i) {
    # reads an audio file with tuneR::readWave
    temp = input$loadAudio[i, ]
    myPars$myAudio_filename = temp$name
    myPars$myAudio_path = temp$datapath
    myPars$myAudio_type = temp$type

    extension = substr(myPars$myAudio_filename,
                       nchar(myPars$myAudio_filename) - 2, nchar(myPars$myAudio_filename))
    if (extension == 'wav' | extension == 'WAV') {
      myPars$temp_audio = tuneR::readWave(temp$datapath)
    } else if (extension == 'mp3' | extension == 'MP3') {
      myPars$temp_audio = tuneR::readMP3(temp$datapath)
    } else {
      warning('Input not recognized: must be a numeric vector or wav/mp3 file')
    }

    myPars$myAudio = as.numeric(myPars$temp_audio@left)
    myPars$samplingRate = myPars$temp_audio@samp.rate
    myPars$dur = round(length(myPars$temp_audio@left) / myPars$temp_audio@samp.rate * 1000)
    myPars$spec_xlim = c(0, myPars$dur)
    # for the first audio only, update autocorSmooth
    # to a default that depends on samplingRate
    if (i == 1) {
      updateSliderInput(session, inputId = 'autocorSmooth',
                      value = 2 * ceiling(7 * myPars$samplingRate / 44100 / 2) - 1)
    }
  }

  extractSpectrogram = observe({
    if (myPars$print) print('Extracting spectrogram...')
    # Instead of re-loading the file every time, save the spectrogram matrix
    # and re-draw manually with filled.contour.mod
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
        output = 'processed',
        plot = FALSE
      )
      myPars$drawSpec = TRUE
    }
  })

  writeAudioFile = observeEvent(myPars$temp_audio, {
    if (myPars$print) print('Writing audio file...')
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
    if (myPars$drawSpec == TRUE) {
      if (myPars$print) print('Drawing spectrogram...')
      par(mar = c(2, 2, 0.5, 2))  # no need to save user's graphical par-s - revert to orig on exit
      if (is.null(myPars$myAudio_path) | is.null(myPars$spec)) {
        plot(1:10, type = 'n', bty = 'n', axes = FALSE, xlab = '', ylab = '')
        text(x = 5, y = 5, labels = 'Upload wav/mp3 file(s) to begin...\nSuggested max duration ~10 s')
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
          xlim = myPars$spec_xlim,
          xlab = 'Time, ms', ylab = 'Frequency, kHz',
          main = '',
          ylim = c(input$spec_ylim[1], input$spec_ylim[2])
        )
        # add manual values to the list of pitch candidates for seamless plotting
        n = ncol(myPars$pitchCands$freq)
        # if (length(n>0) == 0 | length(nrow(myPars$manual)>0) == 0) browser()
        if (!is.null(myPars$pitchCands) &&
            (n > 0 & nrow(myPars$manual) > 0)) {
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
          ylim = c(input$spec_ylim[1], input$spec_ylim[2]),
          candPlot = list(cex = input$spec_cex)
        )
        # Add text label of file name / number
        file_lab = myPars$myAudio_filename
        if (myPars$nFiles > 1) {
          file_lab = paste0(file_lab, '\nFile ', myPars$n, ' of ', myPars$nFiles)
        }
        ran_x = myPars$spec_xlim[2] - myPars$spec_xlim[1]
        ran_y = input$spec_ylim[2] - input$spec_ylim[1]
        text(x = myPars$spec_xlim[1] + ran_x * .01,
             y = input$spec_ylim[2] - ran_y * .01,
             labels = file_lab,
             adj = c(0, 1))  # left, top
      }
    }
  })

  obs_anal = observe({
    # analyze the file (executes every time a slider with arg value is changed)
    if (!is.null(input$loadAudio$datapath)) {
      if (myPars$print) print('Calling analyze()...')
      myPars$step = input$windowLength * (1 - input$overlap / 100)
      temp_anal = analyze(
        myPars$myAudio_path,
        windowLength = input$windowLength,
        step = myPars$step,
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
        snakeStep = 0,  # doesn't work with manual
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
        }
        obs_pitch()  # run pathfinder
      })
    }
  })

  obs_pitch = function() {
    if (myPars$print) print('Looking for pitch contour with obs_pitch()')
    if (length(myPars$pitchCands$freq) > 0) {
      myPars$voicedSegments = findVoicedSegments(
        myPars$pitchCands$freq,
        manualV = myPars$manual$frame,
        manualUnv = myPars$manualUnv,
        shortestSyl = input$shortestSyl,
        shortestPause = input$shortestPause,
        minVoicedCands = input$minVoicedCands,
        pitchMethods = input$pitchMethods,
        step = myPars$step,
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
            interpolWin_bin = ceiling(input$interpolWin / myPars$step),
            interpolTol = input$interpolTol,
            interpolCert = input$interpolCert,
            snakeStep = 0,
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


  ## Clicking events
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
    closest_frame = which.min(abs(as.numeric(colnames(myPars$pitchCands$freq)) -
                                    input$spectrogram_dblclick$x))
    if (length(closest_frame) > 0) {
      # remove manual anchor for this frame, if any
      idx_rem = which(myPars$manual$frame == closest_frame)
      if (length(idx_rem) > 0) myPars$manual = myPars$manual[-idx_rem, ]
      # mark the frame as unvoiced if it's not already marked as unvoiced
      if (!closest_frame %in% myPars$manualUnv)
        myPars$manualUnv = c(myPars$manualUnv, closest_frame)
      # re-run pitch contour
      obs_pitch()
    }
  })

  ## Buttons for operations with selection
  observeEvent(input$selection_play, {
    if (!is.null(input$spectrogram_brush) & length(myPars$brush_sel_x) > 0) {
      from = myPars$brush_sel_x[1] / length(myPars$pitch) * myPars$dur / 1000
      to = tail(myPars$brush_sel_x, 1) / length(myPars$pitch) * myPars$dur / 1000
    } else {
      from = myPars$spec_xlim[1] / 1000
      to = myPars$spec_xlim[2] / 1000
    }
    playme(myPars$myAudio_path, from = from, to = to)
  })

  observeEvent(input$selection_unvoice, {
    if (myPars$print) print('Unvoicing selection...')
    if (!is.null(myPars$bp) & length(myPars$brush_sel_xy) > 0) {
      # NB: play forgets the previous selection, but other buttons do not,
      # hence myPars$bp instead of input$spectrogram_brush
      myPars$manualUnv = c(myPars$manualUnv, myPars$brush_sel_xy)
      # remove manual anchors within selection, if any
      idx_rem = which(myPars$manual$frame %in% myPars$manualUnv)
      if (length(idx_rem) > 0) myPars$manual = myPars$manual[-idx_rem, ]
      obs_pitch()
    }
  })

  observeEvent(input$selection_voice, {
    if (myPars$print) print('Voicing selection...')
    if (!is.null(myPars$bp) &
        length(myPars$brush_sel_x) > 0 &
        length(myPars$manualUnv) > 0) {
      idx_rem = which(myPars$manualUnv %in% myPars$brush_sel_x)
      if (length(idx_rem) > 0) myPars$manualUnv = myPars$manualUnv[-idx_rem]
      obs_pitch()
    }
  })

  observeEvent(input$selection_octaveUp, {
    if (myPars$print) print('Selection octave up...')
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
      myPars$manual$freq[myPars$manual$freq < input$pitchFloor] = input$pitchFloor
      myPars$manual$freq[myPars$manual$freq > input$pitchCeiling] = input$pitchCeiling
      obs_pitch()
    }
  })

  observeEvent(input$selection_octaveDown, {
    if (myPars$print) print('Selection octave down...')
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
      myPars$manual$freq[myPars$manual$freq < input$pitchFloor] = input$pitchFloor
      myPars$manual$freq[myPars$manual$freq > input$pitchCeiling] = input$pitchCeiling
      obs_pitch()
    }
  })

  observeEvent(input$selection_setPrior, {
    if (myPars$print) print('Setting prior...')
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
    if (myPars$print) print('Running brush()...')
    myPars$pitch_df = data.frame(
      time = as.numeric(colnames(myPars$pitchCands$freq)),
      freq = myPars$pitch / 1000
    )
    myPars$bp = brushedPoints(myPars$pitch_df,
                              brush = input$spectrogram_brush,
                              xvar = 'time', yvar = 'freq',
                              allRows = TRUE)
    # selected pitch points
    myPars$brush_sel_xy = which(myPars$bp[, 'selected_'] == TRUE)
    # selected frames (along x axis)
    myPars$brush_sel_x = which(myPars$pitch_df$time > input$spectrogram_brush$xmin &
                                 myPars$pitch_df$time < input$spectrogram_brush$xmax)
  })

  changeZoom = function(coef) {
    midpoint = mean(myPars$spec_xlim)
    halfRan = diff(myPars$spec_xlim) / 2 / coef
    newLeft = max(0, midpoint - halfRan)
    newRight = min(myPars$dur, midpoint + halfRan)
    myPars$spec_xlim = c(newLeft, newRight)
  }
  observeEvent(input$zoomIn, changeZoom(myPars$zoomFactor))
  observeEvent(input$zoomOut, changeZoom(1 / myPars$zoomFactor))
  observeEvent(input$selection_zoomToSel, {
    if (!is.null(myPars$bp)) {
      myPars$spec_xlim = round(c(input$spectrogram_brush$xmin, input$spectrogram_brush$xmax))
    }
  })

  shiftFrame = function(direction) {
    ran = diff(myPars$spec_xlim)
    if (direction == 'left') {
      newLeft = max(0, myPars$spec_xlim[1] - ran)
      newRight = newLeft + ran
    } else if (direction == 'right') {
      newRight = min(myPars$dur, myPars$spec_xlim[2] + ran)
      newLeft = newRight - ran
    }
    myPars$spec_xlim = c(newLeft, newRight)
  }
  observeEvent(input$scrollLeft, shiftFrame('left'))
  observeEvent(input$scrollRight, shiftFrame('right'))

  done = function() {
    # meaning we have finished editing pitch contour for a sound - prepares the output
    new = data.frame(
      file = basename(myPars$myAudio_filename),
      dur = myPars$dur,
      time = paste0('{', paste(round(myPars$X), collapse = ', '), '}'),
      pitch = paste0('{', paste(round(myPars$pitch), collapse = ', '), '}'),
      stringsAsFactors = FALSE
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
    }
  }
  observeEvent(input$nextFile, nextFile())

  lastFile = function() {
    done()
    if (myPars$n > 1) {
      myPars$n = myPars$n - 1
      reset()
      readAudio(myPars$n)
      # todo: re-load the manual pitch contour for the previous file -
      # remember myPars$manual and myPars$manualUnv
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

  observeEvent(input$about, {
    id <<- showNotification(
      ui = paste0("Manual pitch editor: soundgen ", packageVersion('soundgen'), ". Left-click to add/correct a pitch anchor, double-click to remove/unvoice the frame. Load/detach library(shinyBS) to show/hide tips. Project home page with extra tips: http://cogsci.se/soundgen.html. Contact me at andrey.anikin / at / rambler.ru. Thank you!"),
      duration = 10,
      closeButton = TRUE,
      type = 'default'
    )
  })
}
