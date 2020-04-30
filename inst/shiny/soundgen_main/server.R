formantsPerVowel = data.frame(  # Ladefoged 2012 "Vowels & consonants, 3rd ed.", p. 43
  phoneme = c('bee', 'bid', 'bed', 'bad', 'bod', 'bud', 'bawd', 'hood', 'hoo'),
  f1 = c(250, 380, 550, 650, 720, 620, 570, 430, 300),
  f2 = c(2300, 1950, 1800, 1750, 1100, 1200, 850, 1050, 880)
)

server = function(input, output, session) {
  # clean-up of www/ folder: remove all files except temp.wav
  files = list.files('www/', pattern = '.wav')
  files = files[files != 'temp.wav']
  for (f in files){
    file.remove(paste0('www/', f))
  }

  ## S E T U P
  myPars = reactiveValues('myfile' = NULL,
                          'sound' = as.numeric(tuneR::readWave('www/temp.wav')@left),
                          # w/o as.numeric we get integers and spec complains
                          'pitch' = defaults$pitch,
                          'pitchGlobal' = defaults$pitchGlobal,
                          'noise' = defaults$noise,
                          'mouth' = defaults$mouth,
                          'ampl' = defaults$ampl,
                          'amplGlobal' = defaults$amplGlobal,
                          'formants' = defaults$formants,
                          'formantsPicked' = c(NA, NA),
                          'formantsNoise' = NA,
                          'updateDur' = TRUE,
                          'loaded_presets' = list(),
                          'sylDur_previous' = defaults$sylLen,
                          updateVTL = FALSE
  )

  durTotal = reactive({
    # the duration of the entire bout without noise,
    # calculated as the sum of voiced syllables and pauses
    ifelse(input$nSyl == 1,
           input$sylLen,
           (input$sylLen * input$nSyl + input$pauseLen * (input$nSyl - 1)))
  })

  durSyl_withNoise = reactive({ # the duration of a single syllable with noise
    ifelse(!sum(myPars$noise$value > -input$dynamicRange) > 0,
           input$sylLen,
           min(0, myPars$noise$time[1]) +
             max(input$sylLen,
                 myPars$noise$time[length(myPars$noise$time)]))
  })



  ## R E S E T T I N G
  sliders_to_reset = c('')

  # This key function is EXTREMELY bug-prone - careful with what you change!
  # The right order is crucial
  reset_all = reactive({
    # print('running reset_all()')
    myPars$formantsPicked = c(NA, NA)
    myPars$updateDur = FALSE # to prevent duration-related settings in myPars
    # from being updated by event listener observeEvent(input$sylLen)
    # when a new preset is loaded
    myPars$updateVTL = FALSE

    # first reset everything to defaults
    for (v in rownames(permittedValues)[1:which(rownames(permittedValues) == 'rolloffNoiseExp')]) {
      updateSliderInput(session, v, value = permittedValues[v,'default'])
    }
    lists_to_default = c('pitch', 'pitchGlobal', 'mouth',
                         'noise', 'ampl', 'amplGlobal',
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
        if (is.numeric(preset[[v]])) {
          new_value = preset[[v]][1]  # the first value if a vector
        } else if (is.list(preset[[v]])) {
          if (!names(preset)[v] %in% c('formants', 'formantsNoise')) {
            v1 = try(preset[[v]]$value[1])
            if (class(v1) == 'try-error') {
              print(preset[[v]])
            } else {
              new_value = v1  # the first value if a df of anchors
            }
          }
        } else {
          new_value = NULL
        }
        if (length(new_value) > 0) {
          try(updateSliderInput(session, v, value = new_value))
        }
      }

      # reformat anchors from the preset
      for (anchor in c('pitch', 'pitchGlobal', 'glottis',
                       'ampl', 'amplGlobal', 'mouth')) {
        if (is.numeric(preset[[anchor]]) | is.list(preset[[anchor]])) {
          preset[[anchor]] = soundgen:::reformatAnchors(preset[[anchor]])
        }
      }
      if (is.numeric(preset$noise)) {
        if (length(preset$noise) > 0) {
          preset$noise = data.frame(
            time = seq(0,
                       ifelse(is.numeric(preset$sylLen),
                              preset$sylLen,
                              permittedValues['sylLen', 'default']),
                       length.out = max(2, length(preset$noise))),
            value = preset$noise
          )
        }
      }

      myPars_to_reset = names(myPars)[which(names(myPars) %in% names(preset))]
      for (v in myPars_to_reset) {
        myPars[[v]] = preset[[v]]
      }

      if (length(myPars$noise) > 1) {
        updateSliderInput(session, 'noiseTime', value = range(myPars$noise$time))
      }

      # special cases
      if (is.numeric(preset$sylLen)) {
        # update "previous" sylLen for scaling the syllable
        myPars$sylDur_previous = preset$sylLen
      } else {
        myPars$sylDur_previous = defaults$sylLen
      }

      if (!is.null(preset$pitch)) {
        if (any(is.na(preset$pitch))) {
          updateCheckboxInput(session, 'generateVoiced', value = FALSE)
        } else {
          updateCheckboxInput(session, 'generateVoiced', value = TRUE)
          updateSliderInput(session, 'pitchRange',
                            value = c(round(min(preset$pitch$value) / 1.1, 0),
                                      round(max(preset$pitch$value) * 1.1, 0)))
        }
      } else {
        updateSliderInput(session, 'pitchRange',
                          value = c(70, 250))
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

      if (!is.list(preset$noise) & is.numeric(preset$sylLen)) {
        myPars$noise = data.frame(
          time = c(0, preset$sylLen),
          value = c(-input$dynamicRange, -input$dynamicRange)
        )
        myPars$sylDur_previous = input$sylLen
      }

      if (is.numeric(preset$vocalTract)) {
        updateSliderInput(session, inputId = 'vocalTract', value = preset$vocalTract)
        updateCheckboxInput(session, inputId = 'estimateVTL', value = FALSE)
      } else {
        updateCheckboxInput(session, inputId = 'estimateVTL', value = TRUE)
      }

      if (is.list(preset$glottis)) {
        updateSliderInput(session, inputId = 'glottis', value = mean(preset$glottis$value))
      }
    }

    # update VTL if preset contains formants, but does not contain an explicit VTL value
    if ((is.numeric(preset$formants) |
         is.list(preset$formants) |
         is.character(preset$formants)) &
        !is.numeric(preset$vocalTract)) {
      v = estimateVTL(preset$formants)
      if (is.numeric(v)) {
        if (v < permittedValues['vocalTract', 'low']) v = permittedValues['vocalTract', 'low']
        if (v > permittedValues['vocalTract', 'high']) v = permittedValues['vocalTract', 'high']
        updateSliderInput(session, inputId = 'vocalTract', value = v)
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
    if (!is.null(cons)) {
      if (length(cons) > 0) {
        lbls = sapply(presets[[input$speaker]]$Formants$consonants, function(x) x$label)
        choices = c(choices, as.list(cons))
        names(choices)[2:length(choices)] = lbls
      }
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
        if (!class(converted) == 'logical') {  # not NA
          if (sum(unlist(converted)) > 0) {  # if the converted formant list is not empty
            myPars$formants = converted
            # (...otherwise don't change myPars$formants to prevent crashing)
          }
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
      n = presets[[input$speaker]]$Formants$consonants[input$noiseType][[1]]
      myPars$formantsNoise = n[3:length(n)]
      updateSliderInput(session, inputId = 'rolloffNoise',
                        value = n[['rolloffNoise']])
      updateTextInput(session, inputId = 'formantsNoise',
                      value = paste0('list(', toString(myPars$formantsNoise), ')'))
    }
  })

  observeEvent(input$sylLen, {
    # has to be updated manually, b/c noise are the only time anchors
    # expressed in ms rather than 0 to 1 (b/c we don't want to rescale
    # pre-syllable aspiration depending on the syllable duration)
    if (myPars$updateDur == TRUE) {
      # doesn't run if updateDur == FALSE (set to F in reset_all())
      myPars$noise$time = soundgen:::scaleNoiseAnchors(
        noiseTime = myPars$noise$time,
        sylLen_old = myPars$sylDur_previous,
        sylLen_new = input$sylLen
      )
      updateSliderInput(session, inputId = 'noiseTime',
                        value = range(myPars$noise$time))
      myPars$sylDur_previous = input$sylLen  # track the previous value
    }
    myPars$updateDur = TRUE  # execute after the first change (resetting)
  })

  vocalTract = reactive({
    ifelse(input$estimateVTL, NA, input$vocalTract)
  })

  observeEvent({
    input$estimateVTL
    myPars$formants}, {
      if (myPars$updateVTL & input$estimateVTL) {
        v = estimateVTL(myPars$formants)
        if (is.numeric(v)) {
          if (v < permittedValues['vocalTract', 'low']) v = permittedValues['vocalTract', 'low']
          if (v > permittedValues['vocalTract', 'high']) v = permittedValues['vocalTract', 'high']
          updateSliderInput(session, inputId = 'vocalTract', value = v)
        }
      }
      myPars$updateVTL = TRUE
    })

  # observeEvent(input$estimateVTL, {
  #   vocalTract_est()
  # })


  ## P I T C H
  updatePitchRange = reactive({
    updateSliderInput(session, 'pitchRange',
                      value = c(min(input$pitchRange[1], min(myPars$pitch$value) / 1.1),
                                max(input$pitchRange[2], max(myPars$pitch$value) * 1.1)))
  })

  observeEvent(input$pitchFloorCeiling, {
    updateSliderInput(session, inputId = 'pitchRange',
                      min = input$pitchFloorCeiling[1],
                      max = input$pitchFloorCeiling[2])
  })

  # observeEvent(myPars$pitch, updatePitchRange())

  observeEvent(input$generateVoiced, {
    if (input$generateVoiced == FALSE) {
      myPars$pitch = NULL
      myPars$pitchGlobal = NULL
    } else {
      myPars$pitch = defaults$pitch
      myPars$pitchGlobal = defaults$pitchGlobal
    }
  })

  output$plotIntonation = renderPlot({
    myPitchContour()
  })

  myPitchContour = reactive({
    if (is.list(myPars$pitch)) {
      # pitch_y_lwr = min(input$pitchRange[1], min(myPars$pitch$value) / 1.1)
      # pitch_y_upr = max(input$pitchRange[2], max(myPars$pitch$value) * 1.1)
      getSmoothContour(anchors = myPars$pitch,
                       len = input$sylLen * input$pitchRange[2]/ 1000,
                       ylim = input$pitchRange,
                       valueFloor = input$pitchFloorCeiling[1],
                       valueCeiling = input$pitchFloorCeiling[2],
                       samplingRate = input$pitchRange[2],
                       thisIsPitch = TRUE, plot = TRUE)
    } else {
      plot(1:10, 1:10, type = 'n', xlab = '', ylab = '', axes = FALSE)
      text(x = 5, y = 5, labels = 'No voiced component', adj = .5, col = 'blue', cex = 1)
    }
  })

  observeEvent(input$plotIntonation_click, {
    if (is.list(myPars$pitch)) {
      click_x = round(input$plotIntonation_click$x / input$sylLen, 2)
      click_y = round(semitonesToHz(input$plotIntonation_click$y))
      # if the click is below or above thresholds, move within thresholds
      if (click_y < input$pitchRange[1]) {
        click_y = input$pitchRange[1]
      }
      if (click_y > input$pitchRange[2]) {
        click_y = input$pitchRange[2]
      }

      closest_point_in_time = which.min(abs(myPars$pitch$time - click_x))
      delta_x = abs(myPars$pitch$time[closest_point_in_time] - click_x)
      # if the click is near (within 5% of the time range) an existing anchor
      # point, we update the pitch of this anchor according to click location (and
      # the time as well, unless it is the first or the last anchor)
      if (delta_x < 0.05) {
        myPars$pitch$value[closest_point_in_time] = click_y
        if (closest_point_in_time != 1 &
            closest_point_in_time != length(myPars$pitch$time)) {
          myPars$pitch$time[closest_point_in_time] = click_x
        }
      } else { # otherwise, we simply add the new point as another anchor
        myPars[['pitch']] = data.frame (
          'time' = c(myPars$pitch$time, click_x),
          'value' = c(myPars$pitch$value, click_y)
        ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
      }
      # sort the updated dataframe of pitch anchors to make sure the point are in
      # the right order (otherwise it's hard to keep track of which are the first
      # and last anchors - and we have to, since those cannot be removed)
      idx_order = order(myPars$pitch$time)
      myPars$pitch$time = myPars$pitch$time[idx_order]
      myPars$pitch$value = myPars$pitch$value[idx_order]
      updatePitchRange()
    }
  })

  observeEvent(input$plotIntonation_dblclick, {
    if (is.list(myPars$pitch)) {
      ref = as.data.frame(myPars[['pitch']])
      ref$time = ref$time * input$sylLen
      ref$value = semitonesToHz(ref$value)
      closestPoint = nearPoints(ref, input$plotIntonation_dblclick,
                                xvar = 'time', yvar = 'value',
                                threshold = 100000, maxpoints = 1)
      idx = as.numeric(rownames(closestPoint))
      if (length(idx) > 0 & idx != 1 & idx != length(myPars$pitch$time)) {
        # we can remove any anchor except the first and the last (because pitch at
        # start and end of sound has to be defined)
        myPars[['pitch']] = data.frame(
          'time' = myPars$pitch$time[-idx],
          'value' = myPars$pitch$value[-idx]
        )
        updatePitchRange()
      }
    }
  })

  observeEvent(input$pitch_flatten, {
    # flat pitch equal to the first pitch anchor
    if (is.list(myPars$pitch)) {
      myPars[['pitch']] = data.frame('time' = c(0,1),
                                     'value' = rep(myPars$pitch$value[1], 2))
    }
  })

  output$pitch_anchors = renderTable(expr = {
    if (is.list(myPars$pitch)) {
      data.frame(
        'Time, ms' = round(myPars$pitch$time * input$sylLen, 0),
        'Pitch, Hz' = round(myPars$pitch$value, 0))
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
    if (input$nSyl > 1 & is.list(myPars$pitchGlobal)) {
      soundgen:::getDiscreteContour(
        anchors = myPars$pitchGlobal,
        len = input$nSyl,
        interpol = 'spline',
        plot = TRUE,
        ylab = 'Semitones',
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
    if (is.list(myPars$pitchGlobal)) {
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

      closest_point_in_time = which.min(abs(myPars$pitchGlobal$time - click_x))
      delta_x = abs(myPars$pitchGlobal$time[closest_point_in_time] - click_x)
      # if the click is near (within 20% of the time range) an existing anchor
      # point, we update the pitch of this anchor according to click location (and
      # the time as well, unless it is the first or the last anchor)
      if (delta_x < 0.2) {
        myPars$pitchGlobal$value[closest_point_in_time] = click_y
        if (closest_point_in_time != 1 &
            closest_point_in_time != length(myPars$pitchGlobal$time)) {
          myPars$pitchGlobal$time[closest_point_in_time] = click_x
        }
      }  else { # otherwise, we simply add the new point as another anchor
        myPars[['pitchGlobal']] = data.frame (
          'time' = c(myPars$pitchGlobal$time, click_x),
          'value' = c(myPars$pitchGlobal$value, click_y)
        ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
      }
      # sort the updated dataframe of pitch anchors to make sure the point are in the right order (otherwise it's hard to keep track of which are the first and last anchors - and we have to, since those cannot be removed)
      idx_order = order(myPars$pitchGlobal$time)
      myPars$pitchGlobal$time = myPars$pitchGlobal$time[idx_order]
      myPars$pitchGlobal$value = myPars$pitchGlobal$value[idx_order]
    }
  })

  observeEvent(input$plotIntonation_dblclickGlobal, {
    if (is.list(myPars$pitchGlobal)) {
      ref = as.data.frame(myPars[['pitchGlobal']])
      ref$time = ref$time * (input$nSyl - 1) + 1
      closestPoint = nearPoints(ref, input$plotIntonation_dblclickGlobal,
                                xvar = 'time', yvar = 'value',
                                threshold = 100000, maxpoints = 1)
      idx = as.numeric(rownames(closestPoint))
      if (length(idx) > 0 & idx != 1 &
          idx != length(myPars$pitchGlobal$time)) {
        # we can remove any anchor except the first and the last (because pitch at
        # start and end of sound has to be defined)
        myPars[['pitchGlobal']] = data.frame(
          'time' = myPars$pitchGlobal$time[-idx],
          'value' = myPars$pitchGlobal$value[-idx]
        )
      }
    }
  })

  observeEvent(input$pitch_flattenGlobal, {
    # flat pitch modulation across syllables
    if (is.list(myPars$pitchGlobal)) {
      myPars[['pitchGlobal']] = data.frame('time' = c(0,1),
                                           'value' = c(0,0))
    }
  })


  output$pitch_anchorsGlobal = renderTable(expr = {
    if (is.list(myPars$pitchGlobal)) {
      data.frame(
        'Time 0 to 1' = round(myPars$pitchGlobal$time, 2),
        'Adjustment, semitones' = round(myPars$pitchGlobal$value, 0))
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
    br_ylim_low = -input$dynamicRange  # permittedValues['noiseAmpl', 'low']
    br_ylim_high = permittedValues['noiseAmpl', 'high']
    nTicks = length(seq(br_ylim_low, br_ylim_high, by = 20)) - 1
    getSmoothContour(anchors = myPars$noise,
                     normalizeTime = FALSE,
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

    closest_point_in_time = which.min(abs(myPars$noise$time - click_x))
    delta_x = abs(myPars$noise$time[closest_point_in_time] - click_x)
    # if the click is near (within 5% of the time range) an existing anchor
    # point, we update the ampl of this anchor according to click location and time
    if (delta_x < 0.05 * durSyl_withNoise()) {
      myPars$noise$value[closest_point_in_time] = click_y
      myPars$noise$time[closest_point_in_time] = click_x
    } else { # otherwise, we simply add the new point as another anchor
      myPars[['noise']] = data.frame(
        'time' = c(myPars$noise$time, click_x),
        'value' = c(myPars$noise$value, click_y)
      ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of pitch anchors to make sure the point are in
    # the right order (otherwise it's hard to keep track of which are the first
    # and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$noise$time)
    myPars$noise$time = myPars$noise$time[idx_order]
    myPars$noise$value = myPars$noise$value[idx_order]
  })

  observeEvent(input$plotUnvoiced_dblclick, {
    closestPoint = nearPoints(as.data.frame(myPars[['noise']]),
                              input$plotUnvoiced_dblclick, xvar = 'time',
                              yvar = 'value', threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    if (length(idx) > 0 & length(myPars$noise$time) > 2) {
      # we can remove any anchor, as long as there will be at least two anchors
      # left (to know what noise duration should be)
      myPars[['noise']] = data.frame(
        'time' = myPars$noise$time[-idx],
        'value' = myPars$noise$value[-idx]
      )
    }
  })

  observeEvent(input$noise_flatten, {
    # flat pitch equal to the first pitch anchor
    myPars[['noise']] = data.frame(
      'time' = myPars$noise$time[c(1,length(myPars$noise$time))],
      'value' = rep(myPars$noise$value[1],2)
    )})

  output$noise_anchors = renderTable(expr = data.frame(
    'Time, ms' = round(myPars$noise$time, 0),
    'Amplitude, dB' = round(myPars$noise$value, 0),
    row.names = 1:length(myPars$noise$time)),
    digits = 0, align = 'c', rownames = FALSE)


  ## MOUTH OPENING
  output$plotMouth = renderPlot({
    myMouthOpening()
  })

  myMouthOpening = reactive({
    getSmoothContour(
      anchors = myPars$mouth,
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
    # OR: xlim = range(myPars$noise$time)
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

    closest_point_in_time = which.min(abs(myPars$mouth$time - click_x))
    delta_x = abs(myPars$mouth$time[closest_point_in_time] - click_x)
    # if the click is near (within 5% of the time range) an existing anchor
    # point, we update the pitch of this anchor according to click location and time
    if (delta_x < 0.05) {
      myPars$mouth$value[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 &
          closest_point_in_time != length(myPars$mouth$time)) {
        myPars$mouth$time[closest_point_in_time] = click_x
      }
    } else { # otherwise, we simply add the new point as another anchor
      myPars[['mouth']] = data.frame (
        'time' = c(myPars$mouth$time, click_x),
        'value' = c(myPars$mouth$value, click_y)
      ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of pitch anchors to make sure the point are in
    # the right order (otherwise it's hard to keep track of which are the first
    # and last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$mouth$time)
    myPars$mouth$time = myPars$mouth$time[idx_order]
    myPars$mouth$value = myPars$mouth$value[idx_order]
  })

  observeEvent(input$plotMouth_dblclick, {
    ref = as.data.frame(myPars[['mouth']])
    ref$time = ref$time * durSyl_withNoise()
    closestPoint = nearPoints(ref, input$plotMouth_dblclick, xvar = 'time',
                              yvar = 'value', threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    # we can remove any anchor except the first and the last (because mouth
    # opening at start and end of sound has to be defined)
    if (length(idx) > 0 & idx != 1 & idx != length(myPars$mouth$time)) {
      myPars[['mouth']] = data.frame('time' = myPars$mouth$time[-idx],
                                     'value' = myPars$mouth$value[-idx])
    }
  })

  observeEvent(input$mouth_flatten, {
    myPars[['mouth']] = data.frame('time' = c(0,1),
                                   'value' = c(.5,.5))  # default mouth opening
  })

  output$mouth_anchors = renderTable(expr = data.frame(
    'Time, ms' = as.integer(round(myPars$mouth$time * durSyl_withNoise())),
    'Open' = myPars$mouth$value,
    row.names = 1:length(myPars$mouth$time)),
    digits = 2, align = 'c', rownames = FALSE)


  ## AMPLITUDE ENVELOPE LOCAL (PER VOICED SYLLABLE)
  output$plotAmplSyl = renderPlot({
    amplEnvelope_syl()
  })

  amplEnvelope_syl = reactive({
    getSmoothContour(anchors = myPars$ampl,
                     xaxs = "i",
                     xlim = c(0, input$sylLen),
                     ylim = c(-input$dynamicRange, 0),
                     valueFloor = -input$dynamicRange,
                     valueCeiling = 0,
                     len = input$sylLen / 1000 * 1000,
                     samplingRate = 1000, plot = TRUE)
    # xaxs = "i" to enforce exact axis limits, otherwise we exceed the range
  })

  observeEvent(input$plotAmplSyl_click, {
    click_x = round(round(input$plotAmplSyl_click$x)/input$sylLen,2)
    click_y = round(input$plotAmplSyl_click$y)
    # if the click is outside the allowed range of y, re-interpret the click
    # as within the range
    if (click_y < -input$dynamicRange) click_y = -input$dynamicRange
    if (click_y > 0) click_y = 0

    closest_point_in_time = which.min(abs(myPars$ampl$time - click_x))
    delta_x = abs(myPars$ampl$time[closest_point_in_time] - click_x)
    # if the click is near (within 5% of the time range) an existing anchor point,
    # we update the anchor according to click location and time
    if (delta_x < 0.05) {
      myPars$ampl$value[closest_point_in_time] = click_y
      if (closest_point_in_time != 1 &
          closest_point_in_time != length(myPars$ampl$time)) {
        myPars$ampl$time[closest_point_in_time] = click_x
      }
    } else {  # otherwise, we simply add the new point as another anchor
      myPars[['ampl']] = data.frame (
        'time' = c(myPars$ampl$time, click_x),
        'value' = c(myPars$ampl$value, click_y)
      ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
    }
    # sort the updated dataframe of anchors to make sure the point are in the
    # right order (otherwise it's hard to keep track of which are the first and
    # last anchors - and we have to, since those cannot be removed)
    idx_order = order(myPars$ampl$time)
    myPars$ampl$time = myPars$ampl$time[idx_order]
    myPars$ampl$value = myPars$ampl$value[idx_order]
  })

  observeEvent(input$plotAmplSyl_dblclick, {
    ref = as.data.frame(myPars[['ampl']])
    ref$time = ref$time * input$sylLen
    closestPoint = nearPoints(ref, input$plotAmplSyl_dblclick, xvar = 'time',
                              yvar = 'value', threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    # we can remove any anchor except the first and the last (because ampl
    # opening at start and end of sound has to be defined)
    if (length(idx) > 0 & idx != 1 & idx != length(myPars$ampl$time)) {
      myPars[['ampl']] = data.frame('time' = myPars$ampl$time[-idx],
                                    'value' = myPars$ampl$value[-idx])
    }
  })

  observeEvent(input$ampl_syl_flatten, {
    # flat ampl equal to the first ampl anchor
    myPars[['ampl']] = data.frame('time' = c(0, 1),
                                  'value' = rep(myPars$ampl$value[1], 2))
  })

  output$ampl_syl_anchors = renderTable(expr = data.frame(
    'Time, ms' = as.integer(round(myPars$ampl$time * input$sylLen, 0)),
    'Amplitude' = myPars$ampl$value,
    row.names = 1:length(myPars$ampl$time)),
    digits = 0, align = 'c', rownames = FALSE)


  ## AMPLITUDE ENVELOPE GLOBAL (PER BOUT)
  output$plotAmplGlobal = renderPlot({
    amplEnvelopeGlobal()
  })

  amplEnvelopeGlobal = reactive({
    if (input$nSyl > 1  & is.list(myPars$amplGlobal)) {
      soundgen:::getDiscreteContour(
        anchors = myPars$amplGlobal,
        len = input$nSyl,
        interpol = 'spline',
        ylab = 'dB',
        ylim = c(-input$dynamicRange / 2, input$dynamicRange / 2),
        valueFloor = -input$dynamicRange / 2,
        valueCeiling = input$dynamicRange / 2,
        plot = TRUE
      )
    } else {
      plot(1:10, 1:10, type = 'n', xlab = '', ylab = '', axes = FALSE)
      text(x = 5, y = 5, labels = 'Need >1 syllable!', adj = .5, col = 'blue', cex = 1)
    }
  })

  observeEvent(input$plotAmplGlobal_click, {
    if (is.list(myPars$amplGlobal)) {
      timeRange = input$nSyl - 1
      click_x = (input$plotAmplGlobal_click$x - 1) / timeRange  # ranges 0 to 1
      click_y = round(input$plotAmplGlobal_click$y)
      # if the click is outside the allowed range of y, re-interpret the click as within the range
      if (click_y < (-input$dynamicRange / 2)) click_y = -input$dynamicRange / 2
      if (click_y > (input$dynamicRange / 2)) click_y = input$dynamicRange / 2

      closest_point_in_time = which.min(abs(myPars$amplGlobal$time - click_x))
      delta_x = abs(myPars$amplGlobal$time[closest_point_in_time] - click_x)
      # if the click is near (within 20% of the time range) an existing anchor
      # point, we update the pitch of this anchor according to click location and time
      if (delta_x < 0.2) {
        myPars$amplGlobal$value[closest_point_in_time] = click_y
        if (closest_point_in_time != 1 &
            closest_point_in_time != length(myPars$amplGlobal$time)) {
          myPars$amplGlobal$time[closest_point_in_time] =
            click_x
        }
      } else {  # otherwise, we simply add the new point as another anchor
        myPars[['amplGlobal']] = data.frame(
          'time' = c(myPars$amplGlobal$time, click_x),
          'value' = c(myPars$amplGlobal$value, click_y)
        ) # convoluted, but otherwise problems with unwanted dataframe-list conversion, etc
      }
      # sort the updated dataframe of anchors to make sure the point are in the
      # right order (otherwise it's hard to keep track of which are the first and
      # last anchors - and we have to, since those cannot be removed)
      idx_order = order(myPars$amplGlobal$time)
      myPars$amplGlobal$time = myPars$amplGlobal$time[idx_order]
      myPars$amplGlobal$value = myPars$amplGlobal$value[idx_order]
    }
  })

  observeEvent(input$plotAmplGlobal_dblclick, {
    ref = as.data.frame(myPars[['amplGlobal']])
    ref$time = ref$time * (input$nSyl - 1) + 1
    closestPoint = nearPoints(ref, input$plotAmplGlobal_dblclick,  xvar = 'time',
                              yvar = 'value', threshold = 100000, maxpoints = 1)
    idx = as.numeric(rownames(closestPoint))
    # we can remove any anchor except the first and the last (because ampl
    # opening at start and end of sound has to be defined)
    if (length(idx) > 0 & idx != 1 &
        idx != length(myPars$amplGlobal$time)) {
      myPars[['amplGlobal']] = data.frame(
        'time' = myPars$amplGlobal$time[-idx],
        'value' = myPars$amplGlobal$value[-idx]
      )
    }
  })

  observeEvent(input$amplGlobal_flatten, {
    # flat ampl equal to the first ampl anchor
    myPars[['amplGlobal']] = data.frame('time' = c(0, 1),
                                        'value' = c(0, 0))
  })

  output$amplGlobal_anchors = renderTable(expr = {
    if (is.list(myPars$amplGlobal)) {
      data.frame(
        'Time 0 to 1' = round(myPars$amplGlobal$time, 2),
        'Adjustment, dB' = round(myPars$amplGlobal$value, 0))
    } else {
      'None'
    }
  },
  digits = 2, align = 'c', rownames = FALSE)


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
    getRolloff(pitch_per_gc = range(myPars$pitch$value),
               rolloff = input$rolloff,
               rolloffOct = input$rolloffOct,
               rolloffParab = input$rolloffParab,
               rolloffParabHarm = input$rolloffParabHarm,
               rolloffKHz = input$rolloffKHz,
               baseline = 200,
               dynamicRange = input$dynamicRange,
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
                              formantDepStoch = input$formantDepStoch,
                              formantWidth = input$formantWidth,
                              lipRad = input$lipRad,
                              mouth = myPars$mouth,
                              vocalTract = vocalTract(),
                              temperature = input$temperature,
                              samplingRate = input$samplingRate,
                              plot = FALSE
      )
      lta = apply(s, 1, mean)
      freqs = seq(1, round(input$samplingRate / 2), length.out = nr)
      plot(freqs, 20 * log10(lta), type = 'l', xlab = 'Frequency, Hz',
           ylab = 'Power, dB', xlim = c(input$spec_ylim[1], input$spec_ylim[2]) * 1000)
    } else if (input$formants_spectrogram_or_spectrum == 'spectrogram') {
      getSpectralEnvelope(nr = nr,
                          nc = 100,
                          formants = myPars$formants,
                          formantDep = input$formantDep,
                          formantDepStoch = input$formantDepStoch,
                          formantWidth = input$formantWidth,
                          lipRad = input$lipRad,
                          mouth = myPars$mouth,
                          vocalTract = vocalTract(),
                          temperature = input$temperature,
                          samplingRate = input$samplingRate,
                          plot = TRUE,
                          duration = durSyl_withNoise(),
                          xlab = 'Time, ms',
                          ylab = 'Frequency, kHz',
                          ylim = input$spec_ylim,
                          colorTheme = input$spec_colorTheme
      )
    } else if (input$formants_spectrogram_or_spectrum == 'formantPicker') {
      plot(formantsPerVowel$f1, formantsPerVowel$f2, type = 'n',
           xlab = 'F1, Hz', ylab = 'F2, Hz',
           xlim = c(100, 1000), ylim = c(400, 2900))
      text(formantsPerVowel$f1, formantsPerVowel$f2,
           labels = formantsPerVowel$phoneme, col = 'blue')
      mtext(paste('F1 = ', myPars$formantsPicked[1]), side = 3, line = 1)
      mtext(paste('F2 = ', myPars$formantsPicked[2]), side = 3, line = 0)
      if(!any(is.na(myPars$formantsPicked))) {
        points(myPars$formantsPicked[1], myPars$formantsPicked[2],
               pch = 4, cex = 2, lwd = 5, col = rgb(1, 0, 0, alpha = .5))
      }
    }
  })

  observeEvent(input$plotFormants_click, {
    if (input$formants_spectrogram_or_spectrum == 'formantPicker') {
      # prevent VTL from being calculated based on these formants, since f1-f2 are not enough
      updateCheckboxInput(session, inputId = 'estimateVTL', value = FALSE)
      myPars$updateVTL = FALSE
      myPars$formantsPicked = round(c(input$plotFormants_click$x, input$plotFormants_click$y))
      myPars$formants = myPars$formantsPicked
      updateTextInput(session, inputId = 'formants',
                      value = paste0('list(f1 = ', myPars$formantsPicked[1],
                                     ', f2 = ', myPars$formantsPicked[2], ')'))
      updateTextInput(session, inputId = 'vowelString',
                      value = '')

    }
  })

  observeEvent(input$plotFormants_dblclick, {
    if (input$formants_spectrogram_or_spectrum == 'formantPicker') {
      myPars$formantsPicked = c(NA, NA)
      myPars$formants = NA
      updateTextInput(session, inputId = 'formants',
                      value = '')
      updateTextInput(session, inputId = 'vowelString',
                      value = '')
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
      plot = TRUE
    )
  })

  output$plotConsonant = renderPlot({
    if (is.numeric(myPars$formantsNoise) |
        is.list(myPars$formantsNoise) |
        is.character(myPars$formantsNoise)) {
      nr = floor(input$specWindowLength * input$samplingRate / 1000 / 2)
      if (input$formantsNoise_spectrogram_or_spectrum == 'spectrum') {
        s = getSpectralEnvelope(nr = nr,
                                nc = 100,
                                formants = myPars$formantsNoise,
                                formantDep = input$formantDep,
                                formantDepStoch = 0,
                                formantWidth = input$formantWidth,
                                lipRad = input$lipRad,
                                mouth = myPars$mouth,
                                vocalTract = input$vocalTract,
                                temperature = input$temperature,
                                samplingRate = input$samplingRate,
                                plot = FALSE
        )
        lta = apply(s, 1, mean)
        freqs = seq(1, round(input$samplingRate / 2), length.out = nr)
        plot(freqs, 20 * log10(lta), type = 'l', xlab = 'Frequency, Hz',
             ylab = 'dB', xlim = c(input$spec_ylim[1], input$spec_ylim[2]) * 1000)
      } else {
        getSpectralEnvelope(nr = nr,
                            nc = 100,
                            formants = myPars$formantsNoise,
                            formantDep = input$formantDep,
                            formantDepStoch = 0,
                            formantWidth = input$formantWidth,
                            lipRad = input$lipRad,
                            mouth = myPars$mouth,
                            vocalTract = vocalTract(),
                            temperature = input$temperature,
                            samplingRate = input$samplingRate,
                            plot = TRUE,
                            duration = durSyl_withNoise(),
                            xlab = 'Time, ms',
                            ylab = 'Frequency, kHz',
                            ylim = input$spec_ylim,
                            colorTheme = input$spec_colorTheme
        )
      }
    } else {
      plot(1:10, 1:10, type = 'n', xlab = '', ylab = '', axes = FALSE)
      text(x = 5, y = 5, labels = 'Same filter as for voiced', adj = .5, col = 'blue', cex = 1)
    }
  })

  output$spectrogram = renderPlot({
    if (input$spectrogram_or_spectrum == 'spectrogram') {
      if (input$osc_heights < 0) {
        heights = c(-input$osc_heights, 1)
      } else if (input$osc_heights == 0) {
        heights = c(1, 1)
      } else {
        heights = c(1, input$osc_heights)
      }
      spectrogram(
        myPars$sound,
        samplingRate = input$samplingRate,
        wn = 'gaussian', windowLength = input$specWindowLength,
        step = round(input$specWindowLength / 4),
        osc = input$osc == 'linear',
        osc_dB = input$osc == 'dB',
        heights = heights,
        xlab = 'Time, ms', ylab = 'Frequency, kHz',
        main = 'Spectrogram', contrast = input$specContrast,
        brightness = input$specBrightness,
        colorTheme = input$spec_colorTheme,
        method = input$spec_method,
        ylim = c(input$spec_ylim[1], input$spec_ylim[2])
      )
    } else {
      seewave::meanspec(myPars$sound, f = input$samplingRate, dB = 'max0',
                        wl = floor(input$specWindowLength * input$samplingRate / 1000 / 2) * 2,
                        flim = c(input$spec_ylim[1], input$spec_ylim[2]),
                        alim = c(-input$dynamicRange, 0),
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
      pitch = myPars$pitch,
      pitchGlobal = myPars$pitchGlobal,
      glottis = input$glottis,
      temperature = input$temperature,
      maleFemale = input$maleFemale,
      creakyBreathy = input$creakyBreathy,
      nonlinBalance = input$nonlinBalance,
      jitterDep = input$jitterDep,
      jitterLen = input$jitterLen,
      vibratoFreq = input$vibratoFreq,
      vibratoDep = input$vibratoDep,
      shimmerDep = input$shimmerDep,
      shimmerLen = input$shimmerLen,
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
      formantWidth = input$formantWidth,
      vocalTract = vocalTract(),
      subFreq = input$subFreq,
      subDep = input$subDep,
      subWidth = input$subWidth,
      shortestEpoch = input$shortestEpoch,
      amDep = input$amDep,
      amFreq = input$amFreq,
      amShape = input$amShape,
      noise = myPars$noise,
      formantsNoise = myPars$formantsNoise,
      rolloffNoise = input$rolloffNoise,
      rolloffNoiseExp = input$rolloffNoiseExp,
      mouth = myPars$mouth,
      ampl = myPars$ampl,
      amplGlobal = myPars$amplGlobal,
      samplingRate = input$samplingRate,
      windowLength = input$windowLength,
      pitchFloor = input$pitchFloorCeiling[1],
      pitchCeiling = input$pitchFloorCeiling[2],
      pitchSamplingRate = input$pitchSamplingRate,
      dynamicRange = input$dynamicRange
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

  output$htmlAudio = renderUI(
    tags$audio(src = "temp.wav", type = "audio/wav",
               autoplay = NA, controls = NA,
               style="transform: scale(0.75); transform-origin: 0 0;")
  )

  observeEvent(input$generateAudio, {
    generate()
  })

  generate = reactive({
    withProgress(message = 'Synthesizing the sound...', value = 0.5, {
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
      output$htmlAudio = renderUI(
        tags$audio(src = myPars$myfile, type = "audio/wav", autoplay = NA, controls = NA)
      )
    })
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
    if (nchar(new_preset_text) > 0) {
      new_preset_text = paste0('list', new_preset_text)
      new_preset_list = try(eval(parse(text = new_preset_text)), silent = TRUE)

      # create a new preset
      new_presetID = paste(sample(c(letters, 0:9), 8, replace = TRUE),
                           collapse = '')
      myPars$loaded_presets[[new_presetID]] = new_preset_list

      # update sliders
      reset_all()
      mycall()
    }
  })

  observeEvent(input$about, {
    id <<- showNotification(
      ui = paste0("Interactive voice synthesizer: soundgen ", packageVersion('soundgen'), ". More info: http://cogsci.se/soundgen.html"),
      duration = 10,
      closeButton = TRUE,
      type = 'default'
    )
  })

  ## TOOLTIPS - have to be here instead of UI b/c otherwise problems with regulating delay
  # (see https://stackoverflow.com/questions/47477237/delaying-and-expiring-a-shinybsbstooltip)
  # Main / syllables
  shinyBS::addTooltip(session, id='sylLen', title = 'Average duration of a continuous VOICED syllable (unvoiced noise is added separately and may fill in the pauses)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='nSyl', title = 'Each sound consists of one or several syllables separated by pauses', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='pauseLen', title = 'Average pause between syllables', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='repeatBout', title = 'Play the whole bout several times with a specified pause', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

  # Main / hypers
  shinyBS::addTooltip(session, id='temperature', title = 'Stochasticity within each syllable', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='maleFemale', title = 'Adjusts vocal tract length, pitch contour, and formants to imitate larger/smaller body size', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='creakyBreathy', title = 'Changes a bunch of parameters to make the VOICED component either constricted (creaky) or breathy', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

  # Main / settings
  shinyBS::addTooltip(session, id='samplingRate', title = 'The number of points per second of audio. Higher = better quality; lower = faster. Can be any integer, not necessarily a power of two.', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='windowLength', title = 'The length of window for performing FFT - inverse FFT when filtering the source.', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='pitchSamplingRate', title = 'The number of considered F0 values per s of audio. Set up to samplingRate for max precision, but at least >= pitchCeiling', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='dynamicRange', title = 'Discard everything more than dynamicRange dB under maximum amplitude', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='pitchFloorCeiling', title = 'Sets the bounds of fundamental frequency for synthesis', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

  # Intonation
  shinyBS::addTooltip(session, id='pitch_flatten', title = 'Revert to a flat intonation contour with pitch equal to the first (left) anchor', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='pitchRange', title = 'Set upper / lower limit separately or drag in between the markers to shift both limits simultaneously', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='pitch_flattenGlobal', title = 'No global pitch modulation from syllable to syllable', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='vibratoFreq', title = 'Frequency of regular FM', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='vibratoDep', title = 'Depth of regular FM', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

  # Amplitude
  shinyBS::addTooltip(session, id='attackLen', title = 'Does the voice start/end abruptly or with a "fade-in/out"?', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='ampl_syl_flatten', title = 'Same amplitude over the entire syllable', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='amplGlobal_flatten', title = 'Same amplitude over the entire bout', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='amDep', title = 'Depth of amplitude modulation', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='amFreq', title = 'Frequency of amplitude modulation', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='amShape', title = 'Shape of amplitude modulation: 0 = ~sine, -1 = notches, +1 = clicks', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

  # Source / glottal
  shinyBS::addTooltip(session, id='rolloff', title = 'Loss of energy in harmonics relative to fundamental frequency (F0); low values emphasize F0', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='rolloffOct', title = 'Negative: rolloff is progressively steeper for higher frequencies', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='rolloffKHz', title = 'Steeper/gentler basic rolloff as f0 varies', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='rolloffParab', title = 'Parabolic boost to the first ... harmonics, dB', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='rolloffParabHarm', title = 'Apply a parabolic boost to ... harmonics. See vignette and ?getRolloff', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='glottis', title = 'Proportion of time glottis is closed relative to F0 period; adds silences between glottal pulses', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

  # Source / nonlinear
  shinyBS::addTooltip(session, id='nonlinBalance', title = '3 regimes of nonlinear effects: none / subharmonics / subharmonics + jitter', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='shortestEpoch', title = 'Change nonlinear regime no sooner than after ... ms', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='subFreq', title = 'The approximate target frequency of subharmonics; the actual frequency is forced to be a fraction of f0 at every time point', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='subDep', title = 'Width of subharmonic sidebands, ie the strength of subharmonics depending on their distance from F-harmonics', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='jitterDep', title = 'Random variation in F0 per glottal cycle', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='jitterLen', title = 'The pitch jumps every ... ms. Low ~ harsh noise, high ~ shaky voice', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='shimmerDep', title = 'Random variation in amplitude per glottal cycle', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='shimmerLen', title = 'The amplitude jumps every ... ms. Low ~ harsh noise, high ~ shaky voice', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='noise_flatten', title = 'Revert to a flat contour with amplitude equal to the first (left) anchor', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='noiseTime', title = 'Timing of respiration noise relative to the voiced component', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

  # Tract / formants
  shinyBS::addTooltip(session, id='formantDep', title = 'Multiply formant amplitudes by ... (>1 = emphasize vowel quality)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='formantWidth', title = 'Multiply formant bandwidths by ... (>1 = nasalized or muffled)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='vowelString', title = "Implemented presets: a, o, i, e, u, 0 (schwa)", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='estimateVTL', title = 'If TRUE, user-specified formants trump user-specified vocal tract length', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='vocalTract', title = 'Affects default formant spacing at temperature>0', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='formantDepStoch', title = 'Amplitude of extra formants added on top of user-specified ones based on the length of vocal tract', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='lipRad', title = 'Rolloff due to lip radiation', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='noseRad', title = 'Rolloff due to nose radiation: added instead of lip radiation when the mouth is closed', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

  # Tract / mouth opening & unvoiced type
  shinyBS::addTooltip(session, id='mouth_flatten', title = 'Revert to a flat mouth opening contour with opening degree equal to the first (left) anchor', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='mouthOpenThres', title = 'The degree of mouth opening at which lips separate and start to radiate', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='noiseType', title = "Breathing = glottal noise (same formants as for voiced part); snuffling = breathing through the nose; h / s / sh / f = sibilants", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='rolloffNoise', title = 'Linear rolloff of the noise component, dB/kHz above 2 kHz (affects both breathing and supra-glottal noise)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='rolloffNoiseExp', title = 'Exponential rolloff of the noise component, dB/oct (affects both breathing and supra-glottal noise)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

  # spectrogram controls
  shinyBS::addTooltip(session, id='specWindowLength', title = 'Window length for FFT transform (Gaussian)', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='osc', title = 'Plot oscillogram on a linear or dB scale?', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='osc_heights', title = 'Relative size of spectrogram vs oscillogram', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='specContrast', title = 'Regulates the contrast of the spectrogram', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
  shinyBS::addTooltip(session, id='specBrightness', title = 'Regulates the brightness of the spectrogram', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))

}
