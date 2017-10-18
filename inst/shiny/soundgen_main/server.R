server = function(input, output, session) {
  # clean-up of www/ folder: remove all files except temp.wav
  files = list.files('www/')
  files = files[files != 'temp.wav']
  for (f in files){
    file.remove(paste0('www/', f))
  }

  ## S E T U P
  myPars = reactiveValues('myfile' = NULL,
                          'sound' = as.numeric(tuneR::readWave('www/temp.wav')@left),
                          # w/o as.numeric we get integers and spec complains
                          'pitchAnchors' = defaults$pitchAnchors,
                          'pitchAnchorsGlobal' = defaults$pitchAnchorsGlobal,
                          'noiseAnchors' = defaults$noiseAnchors,
                          'mouthAnchors' = defaults$mouthAnchors,
                          'amplAnchors' = defaults$amplAnchors,
                          'amplAnchorsGlobal' = defaults$amplAnchorsGlobal,
                          'formants' = defaults$formants,
                          'formantsNoise' = NA,
                          'updateDur' = TRUE,
                          'loaded_presets' = list(),
                          'sylDur_previous' = defaults$sylLen
  )

  durTotal = reactive({
    # the duration of the entire bout without noise,
    # calculated as the sum of voiced syllables and pauses
    ifelse(input$nSyl == 1,
           input$sylLen,
           (input$sylLen * input$nSyl + input$pauseLen * (input$nSyl - 1)))
  })

  durSyl_withNoise = reactive({ # the duration of a single syllable with noise
    ifelse(!sum(myPars$noiseAnchors$value > input$throwaway) > 0,
           input$sylLen,
           min(0, myPars$noiseAnchors$time[1]) +
             max(input$sylLen,
                 myPars$noiseAnchors$time[length(myPars$noiseAnchors$time)]))
  })



  ## R E S E T T I N G
  sliders_to_reset = c('')

  # This key function is EXTREMELY bug-prone - careful with what you change!
  # The right order is crucial
  reset_all = reactive({
    # print('running reset_all()')
    myPars$updateDur = FALSE # to prevent duration-related settings in myPars
    # from being updated by event listener observeEvent(input$sylLen)
    # when a new preset is loaded

    # first reset everything to defaults
    for (v in rownames(permittedValues)[1:which(rownames(permittedValues) == 'rolloffNoise')]) {
      updateSliderInput(session, v, value = permittedValues[v,'default'])
    }
    lists_to_default = c('pitchAnchors', 'pitchAnchorsGlobal', 'mouthAnchors',
                         'noiseAnchors', 'amplAnchors', 'amplAnchorsGlobal',
                         'formants', 'formantsNoise')
    for (v in lists_to_default) {
      myPars[[v]] = defaults[[v]]
    }

    # ...then load the partial list of presets that are specified (≠ default)
    # for this speaker and call type
    if (length(myPars$loaded_presets) >= 1) {
      # the last user-uploaded preset
      preset = try(myPars$loaded_presets[[length(myPars$loaded_presets)]], silent = TRUE)
    } else {
      # a preset from the library
      preset_text = presets[[input$speaker]] [[input$callType]]
      preset_text = substr(preset_text, 9, nchar(preset_text))  # remove 'soundgen('
      preset_text = paste0('list', preset_text)  # start with 'list('
      preset = try(eval(parse(text = preset_text)), silent = TRUE)
    }
    if (class(preset) == 'list') {
      if(class(preset$formants) == 'character') {
        preset$vowelString = preset$formants  # in case formants = 'aui' etc
      }

      sliders_to_reset = names(preset)[which(names(preset) %in% names(input))]
      for (v in sliders_to_reset) {
        try(updateSliderInput(session, v, value = as.numeric(preset[[v]])))
      }

      # reformat anchors from the preset
      for (anchor in c('pitchAnchors', 'pitchAnchorsGlobal', 'glottisAnchors',
                       'amplAnchors', 'amplAnchorsGlobal', 'mouthAnchors')) {
        if (!is.null(preset[[anchor]]) && !is.na(preset[[anchor]])) {
          preset[[anchor]] = reformatAnchors(preset[[anchor]])
        }
      }
      if (is.numeric(preset$noiseAnchors) && length(preset$noiseAnchors) > 0) {
        preset$noiseAnchors = data.frame(
          time = seq(0,
                     ifelse(is.numeric(preset$sylLen),
                            preset$sylLen,
                            permittedValues['sylLen', 'default']),
                     length.out = max(2, length(preset$noiseAnchors))),
          value = preset$noiseAnchors
        )
      }

      myPars_to_reset = names(myPars)[which(names(myPars) %in% names(preset))]
      for (v in myPars_to_reset) {
        myPars[[v]] = preset[[v]]
      }

      if (length(myPars$noiseAnchors) > 1) {
        updateSliderInput(session, 'noiseTime', value = range(myPars$noiseAnchors$time))
      }

      # special cases
      if (!is.null(preset$pitchAnchors)) {
        if (any(is.na(preset$pitchAnchors))) {
          updateCheckboxInput(session, 'generateVoiced', value = FALSE)
        } else {
          updateCheckboxInput(session, 'generateVoiced', value = TRUE)
          updateSliderInput(session, 'pitchRange',
                            value = c(round(min(preset$pitchAnchors$value) / 2 ^ (1 / 12), 0),
                                      round(max(preset$pitchAnchors$value) * 2 ^ (1 / 12), 0)))
        }
      }

      if(!is.null(preset$vowelString)) {
        updateTextInput(session, inputId = 'vowelString',
                        value = preset$vowelString)
        updateVowels()
      } else if (is.null(preset$vowelString) & !is.null(preset$formants)) {
        updateTextInput(session, inputId = 'vowelString', value = '')
        updateTextInput(session, inputId = 'formants',
                        value = as.character(call('print', preset$formants)[2]))
        myPars$formants = preset$formants
      } else { # if both are NULL
        updateTextInput(session, inputId = 'vowelString', value = '')
        # updateVowels()
      }

      if(!is.null(preset$noiseType)) {
        updateSelectInput(session, inputId = 'noiseType',
                          value = preset$noiseType)
        updateNoise()
      } else if (is.null(preset$noiseType) &
                 !is.null(preset$formantsNoise)) {
        updateTextInput(session, inputId = 'noiseType', value = '')
        updateTextInput(session, inputId = 'formantsNoise',
                        value = as.character(call('print', preset$formantsNoise)[2]))
        myPars$formantsNoise = preset$formantsNoise
      } else { # if both are NULL
        updateTextInput(session, inputId = 'noiseType', value = 'b')
        updateNoise()
      }

      if (!is.list(preset$noiseAnchors) & is.numeric(preset$sylLen)) {
        myPars$noiseAnchors = data.frame(
          time = c(0, preset$sylLen),
          value = c(input$throwaway, input$throwaway)
        )
        myPars$sylDur_previous = input$sylLen
      }

      if (is.numeric(preset$vocalTract)) {
        updateSliderInput(session, inputId = 'vocalTract', value = preset$vocalTract)
        updateCheckboxInput(session, inputId = 'estimateVTL', value = FALSE)
      } else {
        updateCheckboxInput(session, inputId = 'estimateVTL', value = TRUE)
      }

      if (is.list(preset$glottisAnchors)) {
        updateSliderInput(session, inputId = 'glottisAnchors', value = mean(preset$glottisAnchors$value))
      }
    }
  })

  observeEvent(input$callType, {
    myPars$loaded_presets = list()  # remove user-uploaded preset
    reset_all()
  })

  observeEvent(input$speaker, {
    myPars$loaded_presets = list()  # remove user-uploaded preset
    # update available call types for this speaker specified in presets,
    # except the last call type, which is reserved for formants
    updateSelectInput(session, inputId = 'callType',
                      choices = head(names(presets[[input$speaker]]), -1),
                      selected = head(names(presets[[input$speaker]]), 1))
    # NB: this triggers observeEvent(input$callType), and that in turn triggers reset_all()
    updateSelectInput(session, inputId = 'noiseType',
                      choices = noiseType_alternatives())

  })

  noiseType_alternatives = reactive({
    cons = names(presets[[input$speaker]]$Formants$consonants)
    choices = list(Breathing = 'b')
    if (!is.null(cons) && length(cons) > 0) {
      lbls = sapply(presets[[input$speaker]]$Formants$consonants, function(x) x$label)
      choices = c(choices, as.list(cons))
      names(choices)[2:length(choices)] = lbls
    }
    choices
  })

  observeEvent(input$formants, {
    if (length(input$formants) > 0) {
      try({myPars$formants = eval(parse(text = input$formants))})
      # overrides vowelString
    }
  })

  observeEvent(input$vowelString, {
    updateVowels()
  })

  updateVowels = reactive({
    if (nchar(input$vowelString) > 0) {
      try({
        converted = soundgen:::convertStringToFormants(input$vowelString,
                                                       speaker = input$speaker)
        if (sum(unlist(converted)) > 0) { # if the converted formant list is not empty
          myPars$formants = converted
          # (...otherwise don't change myPars$formants to prevent crashing)
        }
        updateTextInput(session, inputId = 'formants',
                        value = as.character(call('print', converted)[2]))
      })
    }
  })

  observeEvent(input$formantsNoise, {
    if (length(input$formantsNoise) > 0) {
      try({myPars$formantsNoise =
        eval(parse(text = input$formantsNoise))}) # overrides chosen consonant
    }
  })

  observeEvent(input$noiseType, {
    updateNoise()
  })

  updateNoise = reactive({
    if (input$noiseType == 'b') {  # breathing
      myPars$formantsNoise = NA
      updateTextInput(session, inputId = 'formantsNoise', value = 'NA')
    } else if (nchar(input$noiseType) > 0) {  # TODO - check if this always works!!!
      n = presets[[input$speaker]]$Formants$consonants[input$noiseType] [[1]]
      myPars$formantsNoise = n[3:length(n)]
      updateSliderInput(session, inputId = 'rolloffNoise',
                        value = n[['rolloffNoise']])
      updateTextInput(session, inputId = 'formantsNoise',
                      value = as.character(call('print', myPars$formantsNoise)[2]))
    }
  })

  observeEvent(input$sylLen, {
    # has to be updated manually, b/c noiseAnchors are the only time anchors
    # expressed in ms rather than 0 to 1 (b/c we don't want to rescale
    # pre-syllable aspiration depending on the syllable duration)
    if (myPars$updateDur == TRUE) {
      # doesn't run if updateDur == FALSE (set to F in reset_all())
      scale_coef = input$sylLen / myPars$sylDur_previous
      myPars$noiseAnchors$time[myPars$noiseAnchors$time > 0] =
        round(myPars$noiseAnchors$time[myPars$noiseAnchors$time > 0] * scale_coef)
      # rescale positive time anchors, but not negative ones
      # (ie the length of pre-syllable aspiration does not
      # vary as the syllable length changes - just doesn't seem to make sense)
      updateSliderInput(session, inputId = 'noiseTime',
                        value = range(myPars$noiseAnchors$time))
      myPars$sylDur_previous = input$sylLen  # track the previous value
    }
    myPars$updateDur = TRUE  # execute after the first change (resetting)
  })

  vocalTract = reactive({
    ifelse(input$estimateVTL, NA, input$vocalTract)
  })

  ## P I T C H
  observeEvent(input$pitchFloorCeiling, {
    updateSliderInput(session, inputId = 'pitchRange',
                      min = input$pitchFloorCeiling[1],
                      max = input$pitchFloorCeiling[2])
  })

  observeEvent(input$generateVoiced, {
    if (input$generateVoiced == FALSE) {
      myPars$pitchAnchors = NULL
      myPars$pitchAnchorsGlobal = NULL
    } else {
      myPars$pitchAnchors = defaults$pitchAnchors
      myPars$pitchAnchorsGlobal = defaults$pitchAnchorsGlobal
    }
  })

  output$plotIntonation = renderPlot({
    myPitchContour()
  })

  myPitchContour = reactive({
    if (is.list(myPars$pitchAnchors)) {
      pitch_y_lwr = min(input$pitchRange[1], min(myPars$pitchAnchors$value) / 1.1)
      pitch_y_upr = max(input$pitchRange[2], max(myPars$pitchAnchors$value) * 1.1)
      getSmoothContour(anchors = myPars$pitchAnchors,
                       len = input$sylLen * permittedValues['pitch', 'high'] / 1000,
                       ylim = c(pitch_y_lwr, pitch_y_upr),
                       valueFloor = input$pitchFloorCeiling[1],
                       valueCeiling = input$pitchFloorCeiling[2],
                       samplingRate = permittedValues['pitch', 'high'],
                       thisIsPitch = TRUE, plot = TRUE)
    } else {
      plot(1:10, 1:10, type = 'n', xlab = '', ylab = '', axes = FALSE)
      text(x = 5, y = 5, labels = 'No voiced component', adj = .5, col = 'blue', cex = 1)
    }
  })

  observeEvent(input$plotIntonation_click, {
    if (is.list(myPars$pitchAnchors)) {
      click_x = round(input$plotIntonation_click$x / input$sylLen, 2)
      click_y = round(semitonesToHz(input$plotIntonation_click$y))
      # if the click is below or above thresholds, move within thresholds
      if (click_y < permittedValues['pitch', 'low']) {
        click_y = permittedValues['pitch', 'low']
      }
      if (click_y > permittedValues['pitch', 'high']) {
        click_y = permittedValues['pitch', 'high']
      }

      closest_point_in_time = which.min(abs(myPars$pitchAnchors$time - click_x))
      delta_x = abs(myPars$pitchAnchors$time[closest_point_in_time] - click_x)
      # if the click is near (within ±5% of the time range) an existing anchor
      # point, we update the pitch of this anchor according to click location (and
      # the time as well, unless it is the first or the last anchor)
      if (delta_x < 0.05) {
        myPars$pitchAnchors$value[closest_point_in_time] = click_y
        if (closest_point_in_time != 1 &&
            closest_point_in_time != length(myPars$pitchAnchors$time)) {
          myPars$pitchAnchors$time[closest_point_in_time] = click_x
        }
      } else { # otherwise, we simply add the new point as another anchor
        myPars[['pitchAnchors']] = data.frame (
          'time' = c(myPars$pitchAnchors$time, click_x),
          'value' = c(myPars$pitchAnchors$value, click_y)
        ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
      }
      # sort the updated dataframe of pitch anchors to make sure the point are in
      # the right order (otherwise it's hard to keep track of which are the first
      # and last anchors - and we have to, since those cannot be removed)
      idx_order = order(myPars$pitchAnchors$time)
      myPars$pitchAnchors$time = myPars$pitchAnchors$time[idx_order]
      myPars$pitchAnchors$value = myPars$pitchAnchors$value[idx_order]
    }
  })

  observeEvent(input$plotIntonation_dblclick, {
    if (is.list(myPars$pitchAnchors)) {
      ref = as.data.frame(myPars[['pitchAnchors']])
      ref$time = ref$time * input$sylLen
      closestPoint = nearPoints(ref, input$plotIntonation_dblclick,
                                xvar = 'time', yvar = 'value',
                                threshold = 100000, maxpoints = 1)
      idx = as.numeric(rownames(closestPoint))
      if (length(idx) > 0 && idx != 1 && idx != length(myPars$pitchAnchors$time)) {
        # we can remove any anchor except the first and the last (because pitch at
        # start and end of sound has to be defined)
        myPars[['pitchAnchors']] = data.frame(
          'time' = myPars$pitchAnchors$time[-idx],
          'value' = myPars$pitchAnchors$value[-idx]
        )
      }
    }
  })

  observeEvent(input$pitch_flatten, {
    # flat pitch equal to the first pitch anchor
    if (is.list(myPars$pitchAnchors)) {
    myPars[['pitchAnchors']] = data.frame('time' = c(0,1),
                                          'value' = rep(myPars$pitchAnchors$value[1], 2))
    }
  })

  output$pitch_anchors = renderTable(expr = {
    if (is.list(myPars$pitchAnchors)) {
      data.frame(
        'Time, ms' = round(myPars$pitchAnchors$time * input$sylLen, 0),
        'Pitch, Hz' = round(myPars$pitchAnchors$value, 0))
    } else {
      'None'
    }
  },
  digits = 0, align = 'c', rownames = FALSE )



  ## P I T C H   G L O B A L
  output$plotIntonationGlobal = renderPlot({
    myPitchContourGlobal()
  })

  myPitchContourGlobal <- reactive({
    if (input$nSyl > 1 && is.list(myPars$pitchAnchorsGlobal)) {
      soundgen:::getDiscreteContour(
        anchors = myPars$pitchAnchorsGlobal,
        len = input$nSyl,
        method = 'spline',
        plot = TRUE,
        ylab = 'Pitch delta, semitones',
        valueFloor = permittedValues['pitchDeltas', 'low'],
        valueCeiling = permittedValues['pitchDeltas', 'high'],
        ylim = c(permittedValues['pitchDeltas', 'low'],
                 permittedValues['pitchDeltas', 'high'])
      )
    } else {
      plot(1:10, 1:10, type = 'n', xlab = '', ylab = '', axes = FALSE)
      text(x = 5, y = 5, labels = 'Need >1 syllable!', adj = .5, col = 'blue', cex = 1)
    }
  })

  observeEvent(input$plotIntonation_clickGlobal, {
    if (is.list(myPars$pitchAnchorsGlobal)) {
      timeRange = input$nSyl - 1
      click_x = (input$plotIntonation_clickGlobal$x - 1) / timeRange  # ranges 0 to 1
      click_y = round(input$plotIntonation_clickGlobal$y, 1)
      # if the click is below or above thresholds, move within thresholds
      if (click_y < permittedValues['pitchDeltas', 'low']) {
        click_y = permittedValues['pitchDeltas', 'low']
      }
      if (click_y > permittedValues['pitchDeltas', 'high']) {
        click_y = permittedValues['pitchDeltas', 'high']
      }

      closest_point_in_time = which.min(abs(myPars$pitchAnchorsGlobal$time - click_x))
      delta_x = abs(myPars$pitchAnchorsGlobal$time[closest_point_in_time] - click_x)
      # if the click is near (within ±20% of the time range) an existing anchor
      # point, we update the pitch of this anchor according to click location (and
      # the time as well, unless it is the first or the last anchor)
      if (delta_x < 0.2) {
        myPars$pitchAnchorsGlobal$value[closest_point_in_time] = click_y
        if (closest_point_in_time != 1 &&
            closest_point_in_time != length(myPars$pitchAnchorsGlobal$time)) {
          myPars$pitchAnchorsGlobal$time[closest_point_in_time] = click_x
        }
      }  else { # otherwise, we simply add the new point as another anchor
        myPars[['pitchAnchorsGlobal']] = data.frame (
          'time' = c(myPars$pitchAnchorsGlobal$time, click_x),
          'value' = c(myPars$pitchAnchorsGlobal$value, click_y)
        ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
      }
      # sort the updated dataframe of pitch anchors to make sure the point are in the right order (otherwise it's hard to keep track of which are the first and last anchors - and we have to, since those cannot be removed)
      idx_order = order(myPars$pitchAnchorsGlobal$time)
      myPars$pitchAnchorsGlobal$time = myPars$pitchAnchorsGlobal$time[idx_order]
      myPars$pitchAnchorsGlobal$value = myPars$pitchAnchorsGlobal$value[idx_order]
    }
  })

  observeEvent(input$plotIntonation_dblclickGlobal, {
    if (is.list(myPars$pitchAnchorsGlobal)) {
      ref = as.data.frame(myPars[['pitchAnchorsGlobal']])
      ref$time = ref$time * (input$nSyl - 1) + 1
      closestPoint = nearPoints(ref, input$plotIntonation_dblclickGlobal,
                                xvar = 'time', yvar = 'value',
                                threshold = 100000, maxpoints = 1)
      idx = as.numeric(rownames(closestPoint))
      if (length(idx) > 0 && idx != 1 &&
          idx != length(myPars$pitchAnchorsGlobal$time)) {
        # we can remove any anchor except the first and the last (because pitch at
        # start and end of sound has to be defined)
        myPars[['pitchAnchorsGlobal']] = data.frame(
          'time' = myPars$pitchAnchorsGlobal$time[-idx],
          'value' = myPars$pitchAnchorsGlobal$value[-idx]
        )
      }
    }
  })

  observeEvent(input$pitch_flattenGlobal, {
    # flat pitch modulation across syllables
    if (is.list(myPars$pitchAnchorsGlobal)) {
      myPars[['pitchAnchorsGlobal']] = data.frame('time' = c(0,1),
                                                  'value' = c(0,0))
    }
  })


  output$pitch_anchorsGlobal = renderTable(expr = {
    if (is.list(myPars$pitchAnchorsGlobal)) {
      data.frame(
        'Time 0 to 1' = round(myPars$pitchAnchorsGlobal$time, 2),
        'Pitch delta, semitones' = round(myPars$pitchAnchorsGlobal$value, 0))
    } else {
      'None'
    }
  },
  digits = 2, align = 'c', rownames = FALSE)


  ## UNVOICED
  output$plotUnvoiced = renderPlot({
    myUnvoicedContour()
  })

  myUnvoicedContour = reactive({
    br_xlim_low = min(input$noiseTime[1], 0)
    br_xlim_high = max(input$noiseTime[2], input$sylLen)
    br_ylim_low = permittedValues['noiseAmpl', 'low']
    br_ylim_high = permittedValues['noiseAmpl', 'high']
    nTicks = length(seq(br_ylim_low, br_ylim_high, by = 20)) - 1
    getSmoothContour(anchors = myPars$noiseAnchors,
                     xlim = c(br_xlim_low, br_xlim_high),
                     ylim = c(br_ylim_low, br_ylim_high),
                     voiced = input$sylLen,
                     contourLabel = 'noise',
                     valueFloor = br_ylim_low,
                     valueCeiling = br_ylim_high,
                     yaxp = c(br_ylim_low, br_ylim_high, nTicks),
                     plot = TRUE)
  })

  observeEvent(input$plotUnvoiced_click, {
    click_x = round(input$plotUnvoiced_click$x)
    click_y = round(input$plotUnvoiced_click$y)
    # if the click is outside the allowed range of y, re-interpret the click as within the range
    if (click_y < permittedValues['noiseAmpl', 'low']) {
      click_y = permittedValues['noiseAmpl', 'low']
    }
    if (click_y > permittedValues['noiseAmpl', 'high']) {
      click_y = permittedValues['noiseAmpl', 'high']
    }

    closest_point_in_time = which.min(abs(myPars$noiseAnchors$time - click_x))
    delta_x = abs(myPars$noiseAnchors$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor
    # point, we update the ampl of this anchor according to click location and time
    if (delta_x < 0.05 * durSyl_withNoise()) {
      myPars$noiseAnchors$value[closest_point_in_time] = click_y
      myPars$noiseAnchors$time[closest_point_in_time] = click_x
    } else { # otherwise, we simply add the new point as another anchor
      myPars[['noiseAnchors']] = data.frame (
        'time' = c(myPars$noiseAnchors$time, click_x),
        'value' = c(myPars$noiseAnchors$value, click_y)
      ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of pitch anchors to make sure the point are in
    # the right order (otherwise it's hard to keep track of which are the first
    # and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$noiseAnchors$time)
    myPars$noiseAnchors$time = myPars$noiseAnchors$time[idx_order]
    myPars$noiseAnchors$value = myPars$noiseAnchors$value[idx_order]
  })

  observeEvent(input$plotUnvoiced_dblclick, {
    closestPoint = nearPoints(as.data.frame(myPars[['noiseAnchors']]),
                              input$plotUnvoiced_dblclick, xvar = 'time',
                              yvar = 'value', threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    if (length(idx) > 0 && length(myPars$noiseAnchors$time) > 2) {
      # we can remove any anchor, as long as there will be at least two anchors
      # left (to know what noise duration should be)
      myPars[['noiseAnchors']] = data.frame(
        'time' = myPars$noiseAnchors$time[-idx],
        'value' = myPars$noiseAnchors$value[-idx]
      )
    }
  })

  observeEvent(input$noise_flatten, {
    # flat pitch equal to the first pitch anchor
    myPars[['noiseAnchors']] = data.frame(
      'time' = myPars$noiseAnchors$time[c(1,length(myPars$noiseAnchors$time))],
      'value' = rep(myPars$noiseAnchors$value[1],2)
    )})

  output$noise_anchors = renderTable(expr = data.frame(
    'Time, ms' = round(myPars$noiseAnchors$time, 0),
    'Amplitude, dB' = round(myPars$noiseAnchors$value, 0),
    row.names = 1:length(myPars$noiseAnchors$time)),
    digits = 0, align = 'c', rownames = FALSE)


  ## MOUTH OPENING
  output$plotMouth = renderPlot({
    myMouthOpening()
  })

  myMouthOpening = reactive({
    getSmoothContour(
      anchors = myPars$mouthAnchors,
      len = durSyl_withNoise() / 1000 * 1000,
      samplingRate = 1000,
      contourLabel = 'mouth',
      xlim = c(0, durSyl_withNoise()),
      xaxs = "i",
      ylim = c(permittedValues['mouthOpening', 'low'], permittedValues['mouthOpening', 'high']),
      valueFloor = permittedValues['mouthOpening', 'low'],
      valueCeiling = permittedValues['mouthOpening', 'high'],
      plot = TRUE)
    # ylab = 'Mouth opening (0.5 = neutral)')
    # xaxs = "i" to enforce exact axis limits, otherwise we exceed the range.
    # OR: xlim = range(myPars$noiseAnchors$time)
  })

  observeEvent(input$plotMouth_click, {
    click_x = round(round(input$plotMouth_click$x) / durSyl_withNoise(), 2)
    click_y = round(input$plotMouth_click$y, 2)
    # if the click is outside the allowed range of y, re-interpret the click
    # as within the range
    if (click_y < permittedValues['mouthOpening', 'low']) {
      click_y = permittedValues['mouthOpening', 'low']
    }
    if (click_y > permittedValues['mouthOpening', 'high']) {
      click_y = permittedValues['mouthOpening', 'high']
    }

    closest_point_in_time = which.min(abs(myPars$mouthAnchors$time - click_x))
    delta_x = abs(myPars$mouthAnchors$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor
    # point, we update the pitch of this anchor according to click location and time
    if (delta_x < 0.05) {
      myPars$mouthAnchors$value[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 &
          closest_point_in_time != length(myPars$mouthAnchors$time)) {
        myPars$mouthAnchors$time[closest_point_in_time] = click_x
      }
    } else { # otherwise, we simply add the new point as another anchor
      myPars[['mouthAnchors']] = data.frame (
        'time' = c(myPars$mouthAnchors$time, click_x),
        'value' = c(myPars$mouthAnchors$value, click_y)
      ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of pitch anchors to make sure the point are in
    # the right order (otherwise it's hard to keep track of which are the first
    # and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$mouthAnchors$time)
    myPars$mouthAnchors$time = myPars$mouthAnchors$time[idx_order]
    myPars$mouthAnchors$value = myPars$mouthAnchors$value[idx_order]
  })

  observeEvent(input$plotMouth_dblclick, {
    ref = as.data.frame(myPars[['mouthAnchors']])
    ref$time = ref$time * durSyl_withNoise()
    closestPoint = nearPoints(ref, input$plotMouth_dblclick, xvar = 'time',
                              yvar = 'value', threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    # we can remove any anchor except the first and the last (because mouth
    # opening at start and end of sound has to be defined)
    if (length(idx) > 0 && idx != 1 && idx != length(myPars$mouthAnchors$time)) {
      myPars[['mouthAnchors']] = data.frame('time' = myPars$mouthAnchors$time[-idx],
                                            'value' = myPars$mouthAnchors$value[-idx])
    }
  })

  observeEvent(input$mouth_flatten, {
    myPars[['mouthAnchors']] = data.frame('time' = c(0,1),
                                          'value' = c(.5,.5))  # default mouth opening
  })

  output$mouth_anchors = renderTable(expr = data.frame(
    'Time, ms' = as.integer(round(myPars$mouthAnchors$time * durSyl_withNoise())),
    'Open' = myPars$mouthAnchors$value,
    row.names = 1:length(myPars$mouthAnchors$time)),
    digits = 2, align = 'c', rownames = FALSE)


  ## AMPLITUDE ENVELOPE LOCAL (PER VOICED SYLLABLE)
  output$plotAmplSyl = renderPlot({
    amplEnvelope_syl()
  })

  amplEnvelope_syl = reactive({
    getSmoothContour(anchors = myPars$amplAnchors,
                     xaxs = "i",
                     xlim = c(0, input$sylLen),
                     ylim = c(0, -input$throwaway),
                     valueFloor = 0, valueCeiling = -input$throwaway,
                     len = input$sylLen / 1000 * 1000,
                     samplingRate = 1000, plot = TRUE)
    # xaxs = "i" to enforce exact axis limits, otherwise we exceed the range
  })

  observeEvent(input$plotAmplSyl_click, {
    click_x = round (round(input$plotAmplSyl_click$x)/input$sylLen,2)
    click_y = round(input$plotAmplSyl_click$y)
    # if the click is outside the allowed range of y, re-interpret the click
    # as within the range
    if (click_y < 0) click_y = 0
    if (click_y > -input$throwaway) click_y = -input$throwaway

    closest_point_in_time = which.min(abs(myPars$amplAnchors$time - click_x))
    delta_x = abs(myPars$amplAnchors$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor point,
    # we update the anchor according to click location and time
    if (delta_x < 0.05) {
      myPars$amplAnchors$value[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 &
          closest_point_in_time != length(myPars$amplAnchors$time)) {
        myPars$amplAnchors$time[closest_point_in_time] = click_x
      }
    } else {  # otherwise, we simply add the new point as another anchor
      myPars[['amplAnchors']] = data.frame (
        'time' = c(myPars$amplAnchors$time, click_x),
        'value' = c(myPars$amplAnchors$value, click_y)
      ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of anchors to make sure the point are in the
    # right order (otherwise it's hard to keep track of which are the first and
    # last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$amplAnchors$time)
    myPars$amplAnchors$time = myPars$amplAnchors$time[idx_order]
    myPars$amplAnchors$value = myPars$amplAnchors$value[idx_order]
  })

  observeEvent(input$plotAmplSyl_dblclick, {
    ref = as.data.frame(myPars[['amplAnchors']])
    ref$time = ref$time * input$sylLen
    closestPoint = nearPoints(ref, input$plotAmplSyl_dblclick, xvar = 'time',
                              yvar = 'value', threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    # we can remove any anchor except the first and the last (because ampl
    # opening at start and end of sound has to be defined)
    if (length(idx) > 0 && idx != 1 && idx != length(myPars$amplAnchors$time)) {
      myPars[['amplAnchors']] = data.frame('time' = myPars$amplAnchors$time[-idx],
                                           'value' = myPars$amplAnchors$value[-idx])
    }
  })

  observeEvent(input$ampl_syl_flatten, {
    # flat ampl equal to the first ampl anchor
    myPars[['amplAnchors']] = data.frame('time' = c(0,1),
                                         'value' = rep(myPars$amplAnchors$value[1], 2))
  })

  output$ampl_syl_anchors = renderTable(expr = data.frame(
    'Time, ms' = as.integer(round(myPars$amplAnchors$time * input$sylLen, 0)),
    'Amplitude' = myPars$amplAnchors$value,
    row.names = 1:length(myPars$amplAnchors$time)),
    digits = 0, align = 'c', rownames = FALSE)


  ## AMPLITUDE ENVELOPE GLOBAL (PER BOUT)
  output$plotAmplGlobal = renderPlot({
    amplEnvelopeGlobal()
  })

  amplEnvelopeGlobal = reactive({
    if (input$nSyl > 1) {
      getSmoothContour(anchors = myPars$amplAnchorsGlobal,
                       xaxs = "i", xlim = c(0, durTotal()),
                       ylim = c(0, -input$throwaway),
                       valueFloor = 0,
                       valueCeiling = -input$throwaway,
                       len = durTotal() / 1000 * 100,
                       samplingRate = 100, plot = TRUE)
    } else {
      plot(1:10, 1:10, type = 'n', xlab = '', ylab = '', axes = FALSE)
      text(x = 5, y = 5, labels = 'Need >1 syllable!', adj = .5, col = 'blue', cex = 1)
    }
  })

  observeEvent(input$plotAmplGlobal_click, {
    click_x = round(input$plotAmplGlobal_click$x / durTotal(), 2)
    click_y = round(input$plotAmplGlobal_click$y)
    # if the click is outside the allowed range of y, re-interpret the click as within the range
    if (click_y < 0) click_y = 0
    if (click_y > -input$throwaway) click_y = -input$throwaway

    closest_point_in_time = which.min(abs(myPars$amplAnchorsGlobal$time - click_x))
    delta_x = abs(myPars$amplAnchorsGlobal$time[closest_point_in_time] - click_x)
    # if the click is near (within ±5% of the time range) an existing anchor
    # point, we update the pitch of this anchor according to click location and time
    if (delta_x < 0.05) {
      myPars$amplAnchorsGlobal$value[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 &
          closest_point_in_time != length(myPars$amplAnchorsGlobal$time)) {
        myPars$amplAnchorsGlobal$time[closest_point_in_time] =
          input$plotAmplGlobal_click$x
      }
    } else {  # otherwise, we simply add the new point as another anchor
      myPars[['amplAnchorsGlobal']] = data.frame(
        'time' = c(myPars$amplAnchorsGlobal$time, click_x),
        'value' = c(myPars$amplAnchorsGlobal$value, click_y)
      ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of anchors to make sure the point are in the
    # right order (otherwise it's hard to keep track of which are the first and
    # last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$amplAnchorsGlobal$time)
    myPars$amplAnchorsGlobal$time = myPars$amplAnchorsGlobal$time[idx_order]
    myPars$amplAnchorsGlobal$value = myPars$amplAnchorsGlobal$value[idx_order]
  })

  observeEvent(input$plotAmplGlobal_dblclick, {
    ref = as.data.frame(myPars[['amplAnchorsGlobal']])
    ref$time = ref$time * durTotal()
    closestPoint = nearPoints(ref, input$plotAmplGlobal_dblclick,  xvar = 'time',
                              yvar = 'value', threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    # we can remove any anchor except the first and the last (because ampl
    # opening at start and end of sound has to be defined)
    if (length(idx) > 0 && idx != 1 &&
        idx != length(myPars$amplAnchorsGlobal$time)) {
      myPars[['amplAnchorsGlobal']] = data.frame(
        'time' = myPars$amplAnchorsGlobal$time[-idx],
        'value' = myPars$amplAnchorsGlobal$value[-idx]
      )
    }
  })

  observeEvent(input$amplGlobal_flatten, {
    # flat ampl equal to the first ampl anchor
    myPars[['amplAnchorsGlobal']] = data.frame('time' = c(0,1),
                                               'value' = rep(myPars$amplAnchorsGlobal$value[1], 2))
  })

  output$amplGlobal_anchors = renderTable(expr = data.frame(
    'Time, ms' = as.integer(round(myPars$amplAnchorsGlobal$time * durTotal(), 0)),
    'Amplitude' = myPars$amplAnchorsGlobal$value,
    row.names = 1:length(myPars$amplAnchorsGlobal$time)),
    digits = 0, align = 'c', rownames = FALSE)


  ## O T H E R    P L O T S
  output$plotSyllables = renderPlot({
    soundgen:::divideIntoSyllables(sylLen = input$sylLen,
                                   nSyl = input$nSyl,
                                   pauseLen = input$pauseLen,
                                   sylDur_min = permittedValues['sylLen', 'low'],
                                   sylDur_max = permittedValues['sylLen', 'high'],
                                   pauseDur_min = permittedValues['pauseLen', 'low'],
                                   pauseDur_max = permittedValues['pauseLen', 'high'],
                                   temperature = input$temperature, plot = TRUE)
  })

  output$plotHypers = renderPlot({
    soundgen:::divideIntoSyllables(sylLen = input$sylLen,
                                   nSyl = input$nSyl,
                                   pauseLen = input$pauseLen,
                                   sylDur_min = permittedValues['sylLen', 'low'],
                                   sylDur_max = permittedValues['sylLen', 'high'],
                                   pauseDur_min = permittedValues['pauseLen', 'low'],
                                   pauseDur_max = permittedValues['pauseLen', 'high'],
                                   temperature = input$temperature, plot = TRUE)
  })

  output$plotSettings = renderPlot({
    soundgen:::divideIntoSyllables(sylLen = input$sylLen,
                                   nSyl = input$nSyl,
                                   pauseLen = input$pauseLen,
                                   sylDur_min = permittedValues['sylLen', 'low'],
                                   sylDur_max = permittedValues['sylLen', 'high'],
                                   pauseDur_min = permittedValues['pauseLen', 'low'],
                                   pauseDur_max = permittedValues['pauseLen', 'high'],
                                   temperature = input$temperature, plot = TRUE)
  })

  output$plotVibrato = renderPlot({
    plot(x = 1:input$sylLen,
         y = input$vibratoDep * sin(2 * pi * (1:input$sylLen) *
                                      input$vibratoFreq / 1000),
         ylim = c(-permittedValues['vibratoDep', 'high'],
                  permittedValues['vibratoDep', 'high']),
         type = 'l',
         xlab = 'Time, ms',
         ylab = 'F0 delta, semitones')
  })

  output$plotRolloff = renderPlot({
    # seewave::meanspec(myPars$sound, f = input$samplingRate, dB = 'max0',
    #   wl = floor(input$specWindowLength*input$samplingRate/1000/2)*2,
    #   flim = c(0,10), main = 'Spectrum')
    getRolloff(pitch_per_gc = range(myPars$pitchAnchors$value),
      rolloff = input$rolloff,
      rolloffOct = input$rolloffOct,
      rolloffParab = input$rolloffParab,
      rolloffParabHarm = input$rolloffParabHarm,
      rolloffKHz = input$rolloffKHz,
      baseline = 200,
      throwaway = input$throwaway,
      samplingRate = input$samplingRate,
      plot = TRUE
    )
  })

  output$plotFormants = renderPlot({
    nr = floor(input$specWindowLength * input$samplingRate / 1000 / 2)
    if (input$formants_spectrogram_or_spectrum == 'spectrum') {
      s = getSpectralEnvelope(nr = nr,
                              nc = 100,
                              formants = myPars$formants,
                              formantDep = input$formantDep,
                              lipRad = input$lipRad,
                              mouthAnchors = myPars$mouthAnchors,
                              vocalTract = vocalTract(),
                              temperature = input$temperature,
                              formantDepStoch = input$formantDepStoch,
                              samplingRate = input$samplingRate,
                              plot = FALSE
      )
      lta = apply(s, 1, mean)
      freqs = seq(1, round(input$samplingRate / 2), length.out = nr)
      plot(freqs, 20 * log10(lta), type = 'l', xlab = 'Frequency, Hz', ylab = 'Power, dB')
    } else {
      getSpectralEnvelope(nr = nr,
                          nc = 100,
                          formants = myPars$formants,
                          formantDep = input$formantDep,
                          lipRad = input$lipRad,
                          mouthAnchors = myPars$mouthAnchors,
                          vocalTract = vocalTract(),
                          temperature = input$temperature,
                          formantDepStoch = input$formantDepStoch,
                          samplingRate = input$samplingRate,
                          plot = TRUE,
                          duration = durSyl_withNoise(),
                          xlab = 'Time, ms',
                          ylab = 'Frequency, kHz',
                          colorTheme = input$spec_colorTheme
      )
    }
  })

  output$plotAM = renderPlot({
    sig = soundgen:::getSigmoid(len = input$sylLen,
                                samplingRate = 1000,
                                freq = input$amFreq,
                                shape = input$amShape)
    trill = (.5 - sig) * input$amDep
    plot(x = 1:input$sylLen,
         y = trill,
         ylim = c(-permittedValues['amDep', 'high'],
                  permittedValues['amDep', 'high']),
         type = 'l',
         xlab = 'Time, ms',
         ylab = 'Amplitude delta, %')
  })

  output$plotNonlin = renderPlot({
    # see source.R, "get a random walk for intra-syllable variation"
    rw = soundgen:::zeroOne(soundgen:::getRandomWalk(
      len = 100,
      rw_range = input$temperature,
      trend = c(.5, -.5), # randomWalk_trendStrength
      rw_smoothing = .3
    )) * 100
    rw_bin = soundgen:::getIntegerRandomWalk(
      rw,
      nonlinBalance = input$nonlinBalance,
      minLength = ceiling(input$shortestEpoch / 1000 * myPitchContour()),
      plot = FALSE  # for some reason fails to plot
    )
    q1 = soundgen:::noiseThresholdsDict$q1[input$nonlinBalance + 1]
    q2 = soundgen:::noiseThresholdsDict$q2[input$nonlinBalance + 1]
    rw_bin_100 = rw_bin
    rw_bin_100[rw_bin_100 == 1] = q1
    rw_bin_100[rw_bin_100 == 2] = q2

    timeseq = seq(0, input$sylLen, length.out = length(rw))
    plot(x = timeseq,
         y = rw, ylim = c(0, 110), type = 'l', lwd = 1,
         xlab = 'Time', ylab = 'Latent non-linearity', main = 'Random walk')
    points(x = timeseq,
           y = rw_bin_100, type = 'l', lwd = 4, col = 'blue')
    lines(x = c(0, input$sylLen), y = c(q1, q1), lty = 3, lwd = 2, col = 'red')
    text(x = 0, y = q1 + 2, labels = 'subh', pos = 4)
    lines(x = c(0, input$sylLen), y = c(q2, q2), lty = 3, lwd = 2, col = 'red')
    text(x = 0, y = q2 + 2, labels = 'subh + jitter', pos = 4)
  })

  output$plotConsonant = renderPlot({
    if (is.null(myPars$formantsNoise) || is.na(myPars$formantsNoise)) {
      plot(1:10, 1:10, type = 'n', xlab = '', ylab = '', axes = FALSE)
      text(x = 5, y = 5, labels = 'Same filter as for voiced', adj = .5, col = 'blue', cex = 1)
    } else {
      nr = floor(input$specWindowLength * input$samplingRate / 1000 / 2)
      if (input$formantsNoise_spectrogram_or_spectrum == 'spectrum') {
        s = getSpectralEnvelope(nr = nr,
                                nc = 100,
                                formants = myPars$formantsNoise,
                                formantDep = input$formantDep,
                                lipRad = input$lipRad,
                                mouthAnchors = myPars$mouthAnchors,
                                vocalTract = input$vocalTract,
                                temperature = input$temperature,
                                formantDepStoch = input$formantDepStoch,
                                samplingRate = input$samplingRate,
                                plot = FALSE
        )
        lta = apply(s, 1, mean)
        freqs = seq(1, round(samplingRate / 2), length.out = nr)
        plot(freqs, 20 * log10(lta), type = 'l', xlab = 'Frequency, Hz', ylab = 'Power, dB')
      } else {
        getSpectralEnvelope(nr = nr,
                            nc = 100,
                            formants = myPars$formantsNoise,
                            formantDep = input$formantDep,
                            lipRad = input$lipRad,
                            mouthAnchors = myPars$mouthAnchors,
                            vocalTract = input$vocalTract,
                            temperature = input$temperature,
                            formantDepStoch = input$formantDepStoch,
                            samplingRate = input$samplingRate,
                            plot = TRUE,
                            duration = durSyl_withNoise(),
                            xlab = 'Time, ms',
                            ylab = 'Frequency, kHz',
                            colorTheme = input$spec_colorTheme
        )
      }
    }
  })

  output$spectrogram = renderPlot({
    if (input$spectrogram_or_spectrum == 'spectrogram') {
      spectrogram(myPars$sound,
                  samplingRate = input$samplingRate,
                  wn = 'gaussian', windowLength = input$specWindowLength,
                  step = round(input$specWindowLength / 4),
                  osc = TRUE, xlab = 'Time, ms', ylab = 'Frequency, kHz',
                  main = 'Spectrogram', contrast = input$specContrast,
                  brightness = input$specBrightness,
                  colorTheme = input$spec_colorTheme,
                  method = input$spec_method,
                  ylim = c(input$spec_ylim[1], input$spec_ylim[2]))
    } else {
      seewave::meanspec(myPars$sound, f = input$samplingRate, dB = 'max0',
                        wl = floor(input$specWindowLength * input$samplingRate / 1000 / 2) * 2,
                        flim = c(input$spec_ylim[1], input$spec_ylim[2]),
                        alim = c(input$throwaway, 0),
                        main = 'Spectrum')
    }
  })

  ## A U D I O
  # create a string with the call to soundgen() with the par values from the UI
  mycall = reactive({
    arg_list = list(
      repeatBout = input$repeatBout,
      nSyl = input$nSyl,
      sylLen = input$sylLen,
      pauseLen = input$pauseLen,
      pitchAnchors = myPars$pitchAnchors,
      pitchAnchorsGlobal = myPars$pitchAnchorsGlobal,
      glottisAnchors = input$glottisAnchors,
      temperature = input$temperature,
      maleFemale = input$maleFemale,
      creakyBreathy = input$creakyBreathy,
      nonlinBalance = input$nonlinBalance,
      nonlinDep = input$nonlinDep,
      jitterDep = input$jitterDep,
      jitterLen = input$jitterLen,
      vibratoFreq = input$vibratoFreq,
      vibratoDep = input$vibratoDep,
      shimmerDep = input$shimmerDep,
      attackLen = input$attackLen,
      rolloff = input$rolloff,
      rolloffOct = input$rolloffOct,
      rolloffParab = input$rolloffParab,
      rolloffParabHarm = input$rolloffParabHarm,
      rolloffKHz = input$rolloffKHz,
      lipRad = input$lipRad,
      noseRad = input$noseRad,
      mouthOpenThres = input$mouthOpenThres,
      formants = myPars$formants,
      formantDep = input$formantDep,
      formantDepStoch = input$formantDepStoch,
      vocalTract = vocalTract(),
      subFreq = input$subFreq,
      subDep = input$subDep,
      shortestEpoch = input$shortestEpoch,
      amDep = input$amDep,
      amFreq = input$amFreq,
      amShape = input$amShape,
      noiseAnchors = myPars$noiseAnchors,
      formantsNoise = myPars$formantsNoise,
      rolloffNoise = input$rolloffNoise,
      mouthAnchors = myPars$mouthAnchors,
      amplAnchors = myPars$amplAnchors,
      amplAnchorsGlobal = myPars$amplAnchorsGlobal,
      samplingRate = input$samplingRate,
      windowLength = input$windowLength,
      pitchFloor = input$pitchFloorCeiling[1],
      pitchCeiling = input$pitchFloorCeiling[2],
      pitchSamplingRate = input$pitchSamplingRate,
      throwaway = input$throwaway
    )
    # simplify arg_list by removing values that are the same as defaults
    idx_same = apply(matrix(1:length(arg_list)), 1, function(x) {
      temp = all.equal(arg_list[[x]],
                       defaults[[names(arg_list)[x]]],
                       check.attributes = FALSE)
      if (class(temp) == 'character') temp = FALSE
      temp
    })
    not_defaults = which(idx_same != TRUE)
    arg_list = arg_list[not_defaults]
    arg_list
  })

  # show simplified function call as string to user for copy-pasting
  observeEvent(mycall(),
               updateTextInput(session, inputId = 'mycall',
                               value = {
                                 temp = as.character(call('print', mycall())[2])
                                 paste0('soundgen', substr(temp, 5, nchar(temp)))
                               })
  )

  output$myAudio = renderUI(
    tags$audio(src = "temp.wav", type = "audio/wav", autoplay = NA, controls = NA)
  )

  observeEvent(input$generateAudio, {
    generate()
  })

  generate = reactive({
    # first remove the previous sound file to avoid cluttering up the www/ folder
    if (!is.null(myPars$myfile)){
      file.remove(paste0('www/', myPars$myfile))
    }
    myPars$sound = do.call('soundgen', mycall()) # eval(parse(text = mycall()))  # generate audio
    randomID = paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = '')
    myPars$myfile = paste0(randomID, '.wav')
    # this is the new sound file. NB: has to be saved in www/ !!!
    seewave::savewav(myPars$sound, f = input$samplingRate,
                     filename = paste0('www/', myPars$myfile))
    output$myAudio = renderUI(
      tags$audio(src = myPars$myfile, type = "audio/wav", autoplay = NA, controls = NA)
    )
  })

  output$saveAudio = downloadHandler(
    filename = function() as.character(myPars$myfile), # to have '.csv' instead of '.wav'
    content = function(filename) {
      seewave::savewav(myPars$sound, f = input$samplingRate, filename = filename)
    }
  )

  observeEvent(input$import_preset, {
    # replace "soundgen" with "list" and parse
    new_preset_text = substr(input$user_preset, 9, nchar(input$user_preset))
    new_preset_text = paste0('list', new_preset_text)
    new_preset_list = try(eval(parse(text = new_preset_text)), silent = TRUE)

    # create a new preset
    new_presetID = paste(sample(c(letters, 0:9), 8, replace = TRUE),
                         collapse = '')
    myPars$loaded_presets[[new_presetID]] = new_preset_list

    # update sliders
    reset_all()
    mycall()
  })

  observeEvent(input$about, {
    id <<- showNotification(
      ui = 'SoundGen 1.1.0, 2017. Load/detach library(shinyBS) to show/hide tips. Project home page: http://cogsci.se/soundgen.html. Contact me at andrey.anikin / at / rambler.ru. Thank you!',
      duration = 10,
      closeButton = TRUE,
      type = 'default'
    )
  })
}
