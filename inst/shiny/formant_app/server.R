# formant_app()
#
# To do: check & debug with real tasks; LPC saves all avail formants - check beh when changing nFormants across annotations & files; from-to in play sometimes weird (stops audio while cursor is still moving); highlight smts disappears in ann_table (buggy! tricky!); load audio upon session start; maybe arbitrary number of annotation tiers

# Start with a fresh R session and run the command options(shiny.reactlog=TRUE)
# Then run your app in a show case mode: runApp('inst/shiny/formant_app', display.mode = "showcase")
# At any time you can hit Ctrl+F3 (or for Mac users, Command+F3) in your web browser to launch the reactive log visualization.


# # tip: to read the output, do smth like:
# a = read.csv('~/Downloads/output.csv', stringsAsFactors = FALSE)
# as.numeric(unlist(strsplit(a$pitch, ',')))

# shinyBS needs to be included as a dependency (instead of just "import"):
# see https://stackoverflow.com/questions/52649138/including-shinybs-in-a-package

server = function(input, output, session) {
  # make overlaid plots resizable (js fix)
  shinyjs::js$inheritSize(parentDiv = 'specDiv')

  # set max upload file size to 30 MB
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)

  myPars = reactiveValues(
    zoomFactor = 2,     # zoom buttons change time zoom by this factor
    zoomFactor_freq = 1.5,  # same for frequency
    print = TRUE,       # if TRUE, some functions print a meassage to the console when called
    drawSpec = TRUE,
    shinyTip_show = 1000,      # delay until showing a tip (ms)
    shinyTip_hide = 0,         # delay until hiding a tip (ms)
    initDur = 2000,            # initial duration to analyze (ms)
    out_fTracks = list(),      # a list for storing formant tracks per file
    out_spects = list(),       # a list for storing spectrograms
    selectedF = 'f1',          # preselect F1 for correction
    slider_ms = 50,            # how often to update play slider
    scrollFactor = .75,        # how far to scroll on arrow press/click
    wheelScrollFactor = .1,    # how far to scroll on mouse wheel (prop of xlim)
    cursor = 0,
    listenToFbtn = FALSE,      # buggy
    play = list(on = FALSE),
    debugQn = FALSE             # for debugging - click "?" to step into the code
  )

  # NB: using myPars$play$cursor for some reason invalidates the observer,
  # so it keeps executing as fast as it can - no idea why!

  # clean-up of www/ folder: remove all files except temp.wav
  # if (!dir.exists("www")) dir.create("www")  # otherwise trouble with shinyapps.io
  if (file.exists('www/temp.csv')) {
    showModal(modalDialog(
      title = "Unsaved data",
      "Found unsaved data from a prevous session. Append to the new output?",
      easyClose = TRUE,
      footer = tagList(
        actionButton("discard", "Discard"),
        actionButton("append", "Append")
      )
    ))
  }
  observeEvent(input$discard, {
    file.remove('www/temp.csv')
    removeModal()
  })
  observeEvent(input$append, {
    myPars$out = read.csv('www/temp.csv', stringsAsFactors = FALSE)
    removeModal()
  })

  files = list.files('www/', pattern = '.wav')
  files = files[files != 'temp.wav']
  for (f in files){
    file.remove(paste0('www/', f))
  }

  reset = function() {
    if (myPars$print) print('Resetting...')
    myPars$ann = NULL         # a dataframe of annotations for the current file
    myPars$currentAnn = NULL  # the idx of currently selected annotation
    myPars$bp = NULL          # selected points (under brush)
    myPars$spec = NULL
    myPars$spec_trimmed = NULL
    myPars$formantTracks = NULL
    myPars$formants = NULL
    myPars$analyzedUpTo = NULL
    myPars$selection = NULL
    myPars$cursor = 0
    myPars$spectrogram_brush = NULL
  }

  resetSliders = function() {
    if (myPars$print) print('Resetting sliders...')
    sliders_to_reset = names(input)[which(names(input) %in% rownames(def_form))]
    for (v in sliders_to_reset) {
      new_value = def_form[v, 'default']
      try(updateSliderInput(session, v, value = new_value))
      try(updateNumericInput(session, v, value = new_value))
      updateSelectInput(session, 'wn', selected = 'gaussian')
      updateSelectInput(session, 'wn_lpc', selected = 'gaussian')
      updateSliderInput(session, 'spec_ylim',
                        value = c(0, def_form['spec_ylim','default']))
      updateSliderInput(session, 'spectrum_xlim',
                        value = c(0, def_form['spectrum_xlim','default']))
      updateRadioButtons(session, 'spec_colorTheme', selected='bw')
      updateSelectInput(session, 'osc', selected = 'linear')
      updateSelectInput(session, 'vtl_method', selected = 'regression')
      updateSliderInput(session, 'speedSound', value = '35400')
      updateTextInput(session, 'coeffs', value = '')
      updateCheckboxInput(session, 'interceptZero', value = TRUE)
    }
  }
  observeEvent(input$reset_to_def, resetSliders())

  rbind_fill = function(df1, df2) {
    # fill missing columns with NAs, then rbind - handy in case nFormants changes
    if (!is.list(df1) || nrow(df1) == 0) return(df2)
    if (!is.list(df2) || nrow(df2) == 0) return(df1)
    df1[setdiff(names(df2), names(df1))] = NA
    df2[setdiff(names(df1), names(df2))] = NA
    return(rbind(df1, df2))
  }

  loadAudio = function() {
    # shinyjs::js$inheritSize(parentDiv = 'specDiv')
    if (myPars$print) print('Loading audio...')
    done()
    reset()  # also triggers done(), but done() needs to run first in case loadAudio
    # is re-executed (need to save myPars$ann --> myPars$out)

    # if output.csv is among the uploaded files, use the annotations in it
    ext = substr(input$loadAudio$name,
                 (nchar(input$loadAudio$name) - 2),
                 nchar(input$loadAudio$name))
    old_out_idx = which(ext == 'csv')[1]  # grab the first csv, if any
    if (!is.na(old_out_idx)) {
      user_ann = read.csv(input$loadAudio$datapath[old_out_idx], stringsAsFactors = FALSE)
      if (is.null(myPars$out)) {
        myPars$out = user_ann
      } else {
        myPars$out = rbind_fill(myPars$out, user_ann)
      }
    }

    # work only with audio files
    idx_audio = which(apply(matrix(input$loadAudio$type), 1, function(x) {
      grepl('audio', x, fixed = TRUE)
    }))
    if (length(idx_audio) > 0) {
      if (is.null(myPars$fileList)) {
        myPars$fileList = input$loadAudio[idx_audio, ]
        myPars$n = 1   # file number in queue
      } else {
        sameFiles = which(myPars$fileList$name %in% input$loadAudio$name)
        if (length(sameFiles) > 0) {
          message('Note: uploading the same audio file twice overwrites previous annotations')
          if (!is.null(myPars$out)) {
            myPars$out = myPars$out[!myPars$out$file %in% myPars$fileList$name[sameFiles]]
            if (length(myPars$out) == 0) myPars$out = NULL
          }
          myPars$fileList = myPars$fileList[-sameFiles, ]
        }
        myPars$n = nrow(myPars$fileList) + 1
        myPars$fileList = rbind(myPars$fileList, input$loadAudio[idx_audio, ])
      }
      myPars$nFiles = nrow(myPars$fileList)  # number of uploaded files in queue
      myPars$drawSpec = FALSE  # hold on plotting the spectrogram until after running analyze()
      choices = as.list(myPars$fileList$name)
      names(choices) = myPars$fileList$name
      if (input$fileList == myPars$fileList$name[myPars$n])
        readAudio(myPars$n)  # doesn't fire automatically if the same as before
      updateSelectInput(session, 'fileList',
                        choices = as.list(myPars$fileList$name),
                        selected = myPars$fileList$name[myPars$n])
    } else if(!is.na(old_out_idx)) {
      # only a new csv uploaded - just refresh the current file
      readAudio(myPars$n)
    }
  }
  observeEvent(input$loadAudio, loadAudio())

  observeEvent(input$showpanel, {
    if(input$showpanel == TRUE) {
      shinyjs::removeCssClass("Main", "col-sm-12")
      shinyjs::addCssClass("Main", "col-sm-8")
      shinyjs::show(id = "Sidebar")
      shinyjs::enable(id = "Sidebar")
    }
    else {
      shinyjs::removeCssClass("Main", "col-sm-8")
      shinyjs::addCssClass("Main", "col-sm-12")
      shinyjs::hide(id = "Sidebar")
    }
  })

  readAudio = function(i) {
    # reads an audio file with tuneR::readWave
    if (myPars$print) print('Reading audio...')
    temp = myPars$fileList[i, ]
    myPars$myAudio_filename = temp$name
    myPars$myAudio_path = temp$datapath
    myPars$myAudio_type = temp$type

    extension = substr(myPars$myAudio_filename,
                       nchar(myPars$myAudio_filename) - 2,
                       nchar(myPars$myAudio_filename))
    if (extension == 'wav' | extension == 'WAV') {
      myPars$temp_audio = tuneR::readWave(temp$datapath)
    } else if (extension == 'mp3' | extension == 'MP3') {
      myPars$temp_audio = tuneR::readMP3(temp$datapath)
    } else {
      warning('Input not recognized: must be a wav or mp3 file')
    }

    myPars$myAudio = as.numeric(myPars$temp_audio@left)
    myPars$ls = length(myPars$myAudio)
    myPars$samplingRate = myPars$temp_audio@samp.rate
    myPars$maxAmpl = 2 ^ (myPars$temp_audio@bit - 1)
    if (input$normalizeInput) {
      myPars$myAudio = myPars$myAudio / max(abs(myPars$myAudio)) * myPars$maxAmpl
    }
    myPars$nyquist = myPars$samplingRate / 2 / 1000
    updateSliderInput(session, 'spec_ylim',
                      value = c(0, min(def_form['spec_ylim', 'default'], myPars$nyquist)),
                      max = myPars$nyquist)
    updateSliderInput(session, 'spectrum_xlim',
                      value = c(0, min(def_form['spectrum_xlim', 'default'], myPars$nyquist)),
                      max = myPars$nyquist)
    myPars$dur = round(length(myPars$temp_audio@left) / myPars$temp_audio@samp.rate * 1000)
    myPars$time = seq(1, myPars$dur, length.out = myPars$ls)
    myPars$spec_xlim = c(0, min(myPars$initDur, myPars$dur))
    myPars$regionToAnalyze = myPars$spec_xlim

    # update info - file number ... out of ...
    updateSelectInput(session, 'fileList',
                      label = NULL,
                      selected = myPars$fileList$name[myPars$n])
    file_lab = paste0('File ', myPars$n, ' of ', myPars$nFiles)
    output$fileN = renderUI(HTML(file_lab))

    # if we've already worked with this file,
    # re-load the annotations and (if in current session) formant tracks + spec
    idx = which(myPars$out$file == myPars$myAudio_filename)
    if (length(idx) > 0) {
      myPars$ann = myPars$out[idx, ]
    } else {
      myPars$ann = NULL
    }
    if (!is.null(myPars$out_fTracks[[myPars$myAudio_filename]])) {
      myPars$formantTracks = myPars$out_fTracks[[myPars$myAudio_filename]]
      myPars$spec = myPars$out_specs[[myPars$myAudio_filename]]
      myPars$drawSpec = TRUE  # don't need to wait for analyze() to run
    }
    drawAnn()
    # drawAnnTbl()
  }

  extractSpectrogram = observe({
    # Instead of re-loading the file every time, could save the spectrogram
    # matrix and re-draw manually with soundgen:::filled.contour.mod
    if (!is.null(myPars$myAudio)) {  # & is.null(myPars$spec)
      if (myPars$print) print('Extracting spectrogram...')
      myPars$spec = spectrogram(
        myPars$myAudio,
        samplingRate = myPars$samplingRate,
        dynamicRange = input$dynamicRange,
        windowLength = input$windowLength,
        step = input$step,
        wn = input$wn,
        zp = 2 ^ input$zp,
        contrast = input$specContrast,
        brightness = input$specBrightness,
        output = 'processed',
        plot = FALSE
      )
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
    output$htmlAudio = renderUI(
      tags$audio(src = myPars$myfile, type = myPars$myAudio_type,
                 id = 'myAudio',
                 style = "display: none; transform: scale(0.75); transform-origin: 0 0;")
    )
  })


  # update the number of formant buttons
  output$fButtons = renderUI({
    c(
      list(actionButton(
        inputId = 'remF',
        label = HTML("<img src='icons/minus.png' width = '25px'>"), # '-',
        class = "buttonInline"
      )),
      lapply(1:input$nFormants, function(f) {  # need to return a list, hence lapply
        tags$div(
          id = paste0('fDiv_', f),
          class = ifelse(f == 1, 'fBox selected', 'fBox'),
          textInput(
            inputId = paste0('f', f, '_text'),
            label = paste0('F', f),
            value = '')
        )
      }),
      list(actionButton(
        inputId = 'addF',
        label = HTML("<img src='icons/plus.png' width = '25px'>"), # '+',
        class = "buttonInline"
      )),
      list(
        shinyBS:::bsPopover(id = 'fDiv_1', title = NULL, content = 'Click to select, type a number to correct or NA to treat as missing, empty to reset to default', placement = "right", trigger = "hover", options = list(delay = list(show = 1000, hide = 0))),
        shinyBS::bsPopover(id='remF', title = NULL, content = 'Remove one formant', placement = "right", trigger = "hover", options = list(delay = list(show = 1000, hide = 0))),
        shinyBS::bsPopover(id='addF', title = NULL, content = 'Add one formant', placement = "right", trigger = "hover", options = list(delay = list(show = 1000, hide = 0)))
      )
    )
  })

  observeEvent(input$addF, {
    updateSliderInput(session, 'nFormants',
                      value = input$nFormants + 1)
  })

  observeEvent(input$remF, {
    updateSliderInput(session, 'nFormants',
                      value = max(1, input$nFormants - 1))
  })

  # edit myPars$ann when formant freq is modified manually as text
  observeEvent(input$nFormants, {
    lapply(1:input$nFormants, function(f) {
      fn = paste0('f', f, '_text')
      observeEvent(input[[fn]], {
        hr()  # otherwise row highlight disappears, no idea why!
        if (isolate(myPars$listenToFbtn)) {
          v = input[[fn]]
          vn = suppressWarnings(as.numeric(v))
          if (v == '') {
            # empty string - reset to default
            myPars$ann[myPars$currentAnn, paste0('f', f)] = myPars$formants[f]
            updateTextInput(
              session, fn,
              value = as.character(myPars$formants[f]))
          } else if (!is.na(vn)) {
            # number - treat as manual correction
            myPars$ann[myPars$currentAnn, paste0('f', f)] = vn
          } else {
            # any other (invalid) input - treat as a missing formant (NA)
            myPars$ann[myPars$currentAnn, paste0('f', f)] = NA
          }
          updateVTL()
          hr()
        }
      })

      # add onclick event with shinyjs to select the formant to edit
      this_div = paste0('fDiv_', f)
      shinyjs::onclick(id = this_div, {
        myPars$selectedF = paste0('f', f)
        shinyjs::addCssClass(id = this_div, class = 'selected')
        other_divs = paste0('fDiv_', (1:input$nFormants)[-f])
        for (d in other_divs) {
          shinyjs::removeCssClass(id = d, class = 'selected')
        }
      })
      # start listening to formant buttons when one is in focus
      shinyjs::onevent('focusin', id = fn, {
        myPars$listenToFbtn = TRUE
        # print('LISTENING')
      })
    })
    avFmPerSel()
  })
  # focusout fires when it shouldn't, so we stop listening if the mouse is over
  # the main panel or the spectrum instead (ie basically anywhere else)
  shinyjs::onevent('hover', id = 'left', {
    myPars$listenToFbtn = FALSE
    # print('STOP LISTENING')
  })
  shinyjs::onevent('hover', id = 'spectrum', {
    myPars$listenToFbtn = FALSE
    # print('STOP LISTENING')
  })

  # Updating spec / osc stuff to speed up plotting
  observe({
    if (!is.null(myPars$myAudio)) {
      # if (myPars$print) print('Scaling audio...')
      if (input$osc == 'dB') {
        myPars$myAudio_scaled = osc(
          myPars$myAudio,
          dynamicRange = input$dynamicRange,
          dB = TRUE,
          maxAmpl = myPars$maxAmpl,
          plot = FALSE,
          returnWave = TRUE)
        myPars$ylim_osc = c(-2 * input$dynamicRange, 0)
      } else {
        myPars$myAudio_scaled = myPars$myAudio
        myPars$ylim_osc = c(-myPars$maxAmpl, myPars$maxAmpl)
      }
    }
  })

  observe({
    # Cut just the part of spec currently needed for plotting
    # (faster than plotting a huge matrix with xlim/ylim)
    if (!is.null(myPars$spec) & !is.null(myPars$myAudio_scaled)) {
      if (myPars$print) print('Trimming the spec & osc')
      # spec
      x = as.numeric(colnames(myPars$spec))
      idx_x = which(x >= (myPars$spec_xlim[1] / 1.05) &
                      x <= (myPars$spec_xlim[2] * 1.05))
      # 1.05 - a bit beyond b/c we use xlim/ylim and may get white space
      y = as.numeric(rownames(myPars$spec))
      idx_y = which(y >= (input$spec_ylim[1] / 1.05) &
                      y <= (input$spec_ylim[2] * 1.05))
      myPars$spec_trimmed = downsample_spec(
        myPars$spec[idx_y, idx_x],
        maxPoints = 10 ^ input$spec_maxPoints)
      # dim(myPars$spec_trimmed)

      # osc
      idx_s = max(1, (myPars$spec_xlim[1] / 1.05 * myPars$samplingRate / 1000)) :
        min(myPars$ls, (myPars$spec_xlim[2] * 1.05 * myPars$samplingRate / 1000))
      downs_osc = 10 ^ input$osc_maxPoints

      isolate({
        myPars$myAudio_trimmed = myPars$myAudio_scaled[idx_s]
        myPars$time_trimmed = myPars$time[idx_s]
        myPars$ls_trimmed = length(myPars$myAudio_trimmed)
        if (!is.null(myPars$myAudio_trimmed) &&
            myPars$ls_trimmed > downs_osc) {
          myseq = round(seq(1, myPars$ls_trimmed,
                            length.out = downs_osc))
          myPars$myAudio_trimmed = myPars$myAudio_trimmed[myseq]
          myPars$time_trimmed = myPars$time_trimmed[myseq]
          myPars$ls_trimmed = length(myseq)
        }
      })
      myPars$drawSpec = TRUE
    }
  })

  downsample_sound = function(x, maxPoints) {
    if (!is.null(myPars$myAudio_trimmed) &&
        myPars$ls_trimmed > (10 ^ input$osc_maxPoints)) {
      if (myPars$print) print('Downsampling osc')
      myseq = round(seq(1, myPars$ls_trimmed,
                        by = myPars$ls_trimmed / input$osc_maxPoints))
      myPars$myAudio_trimmed = myPars$myAudio_trimmed[myseq]
      myPars$ls_trimmed = length(myseq)
    }
  }

  downsample_spec = function(x, maxPoints) {
    lxy = nrow(x) * ncol(x)
    if (length(lxy) > 0 && lxy > maxPoints) {
      if (myPars$print) print('Downsampling spectrogram...')
      lx = ncol(x)  # time
      ly = nrow(x)  # freq
      downs = sqrt(lxy / maxPoints)
      seqx = round(seq(1, lx, length.out = lx / downs))
      seqy = round(seq(1, ly, length.out = ly / downs))
      out = x[seqy, seqx]
    } else {
      out = x
    }
    return(out)
  }

  observe({
    if (!is.null(myPars$spec)) {
      if (is.null(myPars$analyzedUpTo)) {
        myPars$regionToAnalyze = myPars$spec_xlim
        call = TRUE
      } else {
        if (myPars$analyzedUpTo < myPars$spec_xlim[2]) {
          myPars$regionToAnalyze = c(myPars$analyzedUpTo, myPars$spec_xlim[2])
          call = TRUE
        } else {
          call = FALSE
        }
      }
      if (call) extractFormants()
    }
  })

  observe({
    # keep track of the maximum number of formant ever analyzed
    # to make sure the output table has enough columns
    myPars$maxF = max(myPars$maxF,
                      input$nFormants,
                      length(colnames(myPars$ann)) - 6)  # just the formant columns
  })

  observeEvent(input$nFormants, {
    myPars$ff = paste0('f', 1:input$nFormants)
    myPars$f_col_names = paste0(myPars$ff, '_freq')
    if (!is.null(myPars$formantTracks)) {
      missingCols = myPars$f_col_names[which(!myPars$f_col_names %in% colnames(myPars$formantTracks))]
      if (length(missingCols > 0)) myPars$formantTracks[, missingCols] = NA
    }
    if (!is.null(myPars$ann)) {
      missingCols = myPars$ff[which(!myPars$ff %in% colnames(myPars$ann))]
      myPars$ann[, missingCols] = NA
    }
  })



  #################
  ### P L O T S ###
  #################

  ## SPECTROGRAM
  output$spectrogram = renderPlot({
    if (!is.null(myPars$spec) && myPars$drawSpec == TRUE) {
      if (myPars$print) print('Drawing spectrogram...')
      par(mar = c(0.2, 2, 0.5, 2))  # no need to save user's graphical par-s - revert to orig on exit
      if (is.null(myPars$spec)) {
        plot(1:10, type = 'n', bty = 'n', axes = FALSE, xlab = '', ylab = '')
        text(x = 5, y = 5,
             labels = 'Upload wav/mp3 file(s) to begin...\nSuggested max duration ~30 s')
      } else {
        if (input$spec_colorTheme == 'bw') {
          color.palette = function(x) gray(seq(from = 1, to = 0, length = x))
        } else if (input$spec_colorTheme == 'seewave') {
          color.palette = seewave::spectro.colors
        } else {
          colFun = match.fun(input$spec_colorTheme)
          color.palette = function(x) rev(colFun(x))
        }
        soundgen:::filled.contour.mod(
          x = as.numeric(colnames(myPars$spec_trimmed)),
          y = as.numeric(rownames(myPars$spec_trimmed)),
          z = t(myPars$spec_trimmed),
          levels = seq(0, 1, length = 30),
          color.palette = color.palette,
          xlim = myPars$spec_xlim,
          xaxt = 'n',
          xaxs = 'i', xlab = '',
          ylab = 'Frequency, kHz',
          main = '',
          ylim = input$spec_ylim
        )

        # Add text label of file name
        ran_x = myPars$spec_xlim[2] - myPars$spec_xlim[1]
        ran_y = input$spec_ylim[2] - input$spec_ylim[1]
        text(x = myPars$spec_xlim[1] + ran_x * .01,
             y = input$spec_ylim[2] - ran_y * .01,
             labels = myPars$myAudio_filename,
             adj = c(0, 1))  # left, top
      }
    }
  })

  observe({
    myPars$specOver_opts = list(
      xlim = myPars$spec_xlim, ylim = input$spec_ylim,
      xaxs = "i", yaxs = "i",
      bty = 'n', xaxt = 'n', yaxt = 'n',
      xlab = '', ylab = '')
  })

  output$specOver = renderPlot({
    if (!is.null(myPars$spec)) {
      par(mar = c(0.2, 2, 0.5, 2), bg = NA)
      # bg=NA makes the image transparent

      # empty plot to enable hover/click events for the spectrogram underneath
      do.call(plot, c(list(
        x = myPars$spec_xlim,
        y = input$spec_ylim,
        type = 'n'),
        myPars$specOver_opts))

      if (!is.null(myPars$spectrogram_hover)) {
        # horizontal line
        do.call(points, c(list(
          x = myPars$spec_xlim,
          y = rep(myPars$spectrogram_hover$y, 2),
          type = 'l', lty = 3),
          myPars$specOver_opts))
        # frequency label
        do.call(text, list(
          x = myPars$spec_xlim[1],
          y = myPars$spectrogram_hover$y,
          labels = myPars$spectrogram_hover$freq,
          adj = c(0, 0)))
        # vertical line
        do.call(points, list(
          x = rep(myPars$spectrogram_hover$x, 2),
          y = input$spec_ylim,
          type = 'l', lty = 3))
        # time label
        do.call(text, list(
          x = myPars$spectrogram_hover$x,
          y = input$spec_ylim[1] + .025 * diff(input$spec_ylim),
          labels = soundgen:::convert_sec_to_hms(myPars$spectrogram_hover$x / 1000, 3),
          adj = .5))
      }

      # Add a rectangle showing the current annotation
      if (!is.null(myPars$currentAnn)) {
        isolate({  # from myPars$ann
          rect(
            xleft = myPars$ann$from[myPars$currentAnn],
            xright = myPars$ann$to[myPars$currentAnn],
            ybottom = input$spec_ylim[1],
            ytop = input$spec_ylim[2],
            col = rgb(.2, .2, .2, alpha = .25),
            border = NA
          )
        })
      }

      # Add a rectangle showing the selected region
      if (!is.null(myPars$spectrogram_brush)) {
        rect(
          xleft = myPars$spectrogram_brush$xmin,
          xright = myPars$spectrogram_brush$xmax,
          ybottom = input$spec_ylim[1],
          ytop = input$spec_ylim[2],
          col = rgb(.2, .2, .2, alpha = .15),
          border = NA
        )
      }

      # Add formant tracks
      if (!is.null(myPars$formantTracks)) {
        for (f in 2:(input$nFormants + 1)) {
          points(myPars$formantTracks$time,
                 myPars$formantTracks[, f] / 1000,
                 pch = 16,
                 col = input$spec_col,
                 cex = input$spec_cex)
        }
      }
    }
  })

  output$specSlider = renderPlot({
    if (!is.null(myPars$spec)) {
      par(mar = c(0.2, 2, 0.5, 2), bg = NA)
      # bg=NA makes the image transparent

      if (myPars$cursor == 0) {
        # just a transparent plot
        do.call(plot, c(list(
          x = 1, type = 'n'),
          myPars$specOver_opts))
      } else {
        # horizontal line at current play time
        do.call(plot, c(list(
          x = rep(myPars$cursor, 2),
          y = input$spec_ylim,
          type = 'l'),
          myPars$specOver_opts))
      }
    }
  })

  observeEvent(input$spectrogram_hover, {
    if (!is.null(input$spectrogram_hover) & !is.null(myPars$spec)) {
      myPars$spectrogram_hover = input$spectrogram_hover
      cursor_hz = myPars$spectrogram_hover$y * 1000
      cursor_notes = soundgen::notesDict$note[round(HzToSemitones(cursor_hz)) + 1]
      myPars$spectrogram_hover$freq = paste0(
        round(myPars$spectrogram_hover$y * 1000), ' Hz (',
        cursor_notes, ')')
    }
  })

  shinyjs::onevent('mouseout', id = 'specOver', {
    # NB: more flexible and less mafan than juggling with the observer of
    # input$spectrogram_hover
    myPars$spectrogram_hover = NULL
    shinyjs::js$clearBrush(s = '_brush')
  } )

  observeEvent(input$spectrogram_click, {
    myPars$spectrogram_brush = NULL
    # shinyjs::js$clearBrush(s = '_brush')
    myPars$cursor = input$spectrogram_click$x
    if (!is.null(myPars$currentAnn)) {
      inside_sel = (myPars$ann$from[myPars$currentAnn] < input$spectrogram_click$x) &
        (myPars$ann$to[myPars$currentAnn] > input$spectrogram_click$x)
      if (inside_sel) {
        # update both myPars$ann and the corresponding formant button
        myPars$ann[myPars$currentAnn, myPars$selectedF] = round(input$spectrogram_click$y * 1000)
        updateTextInput(
          session, paste0(myPars$selectedF, '_text'),
          value = as.character(myPars$ann[myPars$currentAnn, myPars$selectedF]))
        updateVTL()
      }
    }
  })

  observeEvent(input$spectrogram_dblclick, {
    if (!is.null(myPars$spectrogram_brush)) {
      showModal(dataModal_new())
    }
  })

  observeEvent(input$spectrogram_brush, {
    if (!is.null(input$spectrogram_brush)) {
      myPars$spectrogram_brush = input$spectrogram_brush
    }
  })


  ## OSCILLOGRAM
  observe({
    output$oscillogram = renderPlot({
      if (!is.null(myPars$myAudio_trimmed)) {
        if (myPars$print) print('Drawing osc...')
        par(mar = c(2, 2, 0, 2))
        plot(myPars$time_trimmed,
             myPars$myAudio_trimmed,
             type = 'l',
             xlim = myPars$spec_xlim,
             ylim = myPars$ylim_osc,
             axes = FALSE, xaxs = "i", yaxs = "i", bty = 'o',
             xlab = 'Time, ms',
             ylab = '')
        box()
        # axis(side = 1)
        time_location = axTicks(1)
        time_labels = soundgen:::convert_sec_to_hms(time_location / 1000, 3)
        axis(side = 1, at = time_location, labels = time_labels)
        if (input$osc == 'dB') {
          axis(side = 4, at = seq(0, input$dynamicRange, by = 10))
          mtext("dB", side = 2, line = 3)
        }
        abline(h = 0, lty = 2)
      }
    }, execOnResize = TRUE)
  })


  ## SPECTRUM
  output$spectrum = renderPlot({
    if (!is.null(myPars$spectrum)) {
      if (myPars$print) print('Drawing spectrum...')
      par(mar = c(2, 2, 0.5, 0))
      xlim = input$spectrum_xlim # xlim = c(0, myPars$spectrum_freq_range[2])
      ylim = c(
        myPars$spectrum_ampl_range[1],
        # leave extra room for "smoothing" slider
        myPars$spectrum_ampl_range[2] + 1/4 * diff(myPars$spectrum_ampl_range)
      )
      plot(myPars$spectrum$freq,
           myPars$spectrum$ampl,
           xlab = '', ylab = '',
           xlim = xlim, ylim = ylim,
           xaxs = "i",
           type = 'l')
      # # fill in the AUC if needed
      # polygon(c(myPars$spectrum$freq, xlim[2], xlim[1]),
      #         c(myPars$spectrum$ampl, ylim[1], ylim[1]),
      #         border = NA,
      #         col = rgb(.5, .5, .5, .15))
      spectrum_peaks()

      # add a vertical line and freq label on hover
      if (!is.null(myPars$spectrum_hover)) {
        abline(v = myPars$spectrum_hover$x, lty = 2)
        text(x = myPars$spectrum_hover$x,
             y = myPars$spectrum_ampl_range[1],
             labels = myPars$spectrum_hover$cursor,
             adj = c(0, 0))
        # show the nearest spectral peak
        text(x = myPars$spectrum_hover$pf,
             y = myPars$spectrum_hover$pa,
             labels = myPars$spectrum_hover$peak,
             adj = c(0, 0))
        points(x = myPars$spectrum_hover$pf,
               y = myPars$spectrum_hover$pa, pch = 16)
      }

      # plot formant frequencies, if any
      if (!is.null(myPars$ann) &&
          any(is.numeric(myPars$currentAnn))) {
        ff = as.numeric(myPars$ann[myPars$currentAnn, myPars$ff])
      } else if (!is.null(myPars$formants) &&
                 any(!is.na(as.numeric(myPars$formants)))) {
        ff = as.numeric(myPars$formants)
      } else {
        ff = numeric(0)
      }

      if (length(ff) > 0) {
        for (i in 1:length(ff)) {
          f = ff[i] / 1000
          if (is.numeric(f) & any(!is.na(f))) {
            idx_f = which.min(abs(myPars$spectrum$freq - f))
            text(
              x = f,
              y = myPars$spectrum$ampl[idx_f] + 2,  # +2 to avoid overlap with cross
              labels = paste0('F', i),
              adj = c(.5, 0)
            )
            segments(
              x0 = f, y0 = ylim[1] - .04 * diff(ylim),
              x1 = f, y1 = myPars$spectrum$ampl[idx_f],
              lty = 3, col = 'lightgreen'
            )
          }
        }
      }
    }
  })

  observe({
    if (!is.null(myPars$spec_trimmed)) {
      if (myPars$print) print('Extracting spectrum of selection...')
      if (!is.null(myPars$selection) && length(myPars$selection) > 0) {
        # take the spectrum of selection (annotated region) from raw audio
        myPars$spectrum = try(as.list(soundgen:::getSmoothSpectrum(
          sound = myPars$selection,
          samplingRate = myPars$samplingRate,
          len = input$spectrum_len,
          loessSpan = 10 ^ input$spectrum_smooth
        )))
        # if (class(myPars$spectrum) == 'try-error') browser()
        # myPars$spectrum = list(
        #     freq = as.numeric(rownames(myPars$spec)),
        #     ampl = rowMeans(myPars$spec_trimmed)
        # )
      } else {
        # just use the visible part of the spectrogram, but all freq bins
        # (so not the same as myPars$spec_trimmed)
        time_stamps = as.numeric(colnames(myPars$spec))
        idx = which(time_stamps >= myPars$spec_xlim[1] &
                      time_stamps <= myPars$spec_xlim[2])
        if (length(idx) > 1) {
          ampl = as.numeric(rowMeans(myPars$spec[, idx, drop = FALSE]))
          ampl[ampl == 0] = 1e-16  # avoid -Inf in log-spectrum
          spec_temp = list(
            freq = as.numeric(rownames(myPars$spec[, idx])),
            ampl = 20 * log10(ampl)
          )
          myPars$spectrum = as.list(soundgen:::getSmoothSpectrum(
            spectrum = spec_temp,
            len = input$spectrum_len,
            loessSpan = 10 ^ input$spectrum_smooth
          ))
        }
      }

      isolate({
        myPars$spectrum_freq_range = range(myPars$spectrum$freq)
        myPars$spectrum_ampl_range = range(myPars$spectrum$ampl)
      })
    }
  })

  spectrum_peaks = reactive({
    # find peaks in the spectrum
    if (!is.null(myPars$spectrum)) {
      if (myPars$print) print('Looking for spectral peaks...')
      sp_zoo = zoo::as.zoo(myPars$spectrum$ampl)
      temp = zoo::rollapply(sp_zoo,
                            width = 3,
                            align = 'center',
                            function(x) which.max(x) == 2)
      myPars$spectrum_peaks = zoo::index(temp)[zoo::coredata(temp)]
    }
  })

  observeEvent(input$spectrum_click, {
    # update both myPars$ann and the corresponding formant button
    if (!is.null(myPars$currentAnn)) {
      myPars$ann[myPars$currentAnn, myPars$selectedF] = round(input$spectrum_click$x * 1000)
      updateTextInput(
        session, paste0(myPars$selectedF, '_text'),
        value = as.character(myPars$ann[myPars$currentAnn, myPars$selectedF]))
      updateVTL()
    }
  })

  observeEvent(input$spectrum_dblclick, {
    # update both myPars$ann and the corresponding formant button
    if (!is.null(myPars$currentAnn)) {
      pf_idx = which.min(abs(input$spectrum_dblclick$x -
                               myPars$spectrum$freq[myPars$spectrum_peaks]))
      pf = myPars$spectrum$freq[myPars$spectrum_peaks[pf_idx]]
      myPars$ann[myPars$currentAnn, myPars$selectedF] = round(pf * 1000)
      updateTextInput(
        session, paste0(myPars$selectedF, '_text'),
        value = as.character(myPars$ann[myPars$currentAnn, myPars$selectedF]))
    }
  })

  observeEvent(input$spectrum_hover, {
    if (!is.null(myPars$spectrum) & !is.null(input$spectrum_hover)) {
      myPars$spectrum_hover = data.frame(x = input$spectrum_hover$x,
                                         y = input$spectrum_hover$y)
      cursor_hz = round(input$spectrum_hover$x * 1000)
      cursor_notes = soundgen::notesDict$note[round(HzToSemitones(cursor_hz)) + 1]
      myPars$spectrum_hover$cursor = paste0(cursor_hz, 'Hz (', cursor_notes, ')')

      pf_idx = which.min(abs(myPars$spectrum_hover$x -
                               myPars$spectrum$freq[myPars$spectrum_peaks]))
      myPars$spectrum_hover$pa =myPars$spectrum$ampl[myPars$spectrum_peaks[pf_idx]]
      myPars$spectrum_hover$pf = myPars$spectrum$freq[myPars$spectrum_peaks[pf_idx]]
      nearest_peak_hz = round(myPars$spectrum_hover$pf * 1000)
      nearest_peak_notes = soundgen::notesDict$note[round(HzToSemitones(nearest_peak_hz)) + 1]
      myPars$spectrum_hover$peak = paste0(nearest_peak_hz, ' Hz (',
                                          nearest_peak_notes, ')')
    }
  })
  observeEvent(input$spectrum_smooth, {
    myPars$spectrum_hover = NULL
  })


  ## ANNOTATIONS
  drawAnn = function() {
    output$ann_plot = renderPlot({
      if (myPars$print) print('Drawing annotations...')
      isolate({
        if (!is.null(myPars$ann)) {
          if (nrow(myPars$ann) > 0) {
            par(mar = c(0, 2, 0, 2))
            plot(myPars$time_trimmed,
                 xlim = myPars$spec_xlim,
                 ylim = c(.2, .8),
                 type = 'n',
                 xaxs = "i", yaxs = "i",
                 bty = 'n',
                 axes = FALSE,
                 xlab = '', ylab = ''
            )
            for (i in 1:nrow(myPars$ann)) {
              r = rnorm(1, 0, .05)  # random vertical shift to avoid overlap
              segments(x0 = myPars$ann$from[i],
                       x1 = myPars$ann$to[i],
                       y0 = .5 + r, y1 = .5 + r, lwd = 2)
              segments(x0 = myPars$ann$from[i],
                       x1 = myPars$ann$from[i],
                       y0 = .45 + r, y1 = .55 + r, lwd = 2)
              segments(x0 = myPars$ann$to[i],
                       x1 = myPars$ann$to[i],
                       y0 = .45 + r, y1 = .55 + r, lwd = 2)
              middle_i = mean(as.numeric(myPars$ann[i, c('from', 'to')]))
              text(x = middle_i,
                   y = .5 + r,
                   labels = myPars$ann$label[i],
                   adj = c(.5, 0), cex = 1.5)
            }
            par(mar = c(0.2, 2, 0.5, 2))
          }
        } else if (!is.null(myPars$spec)) {
          par(mar = c(0, 2, 0, 2))
          plot(1:10,
               type = 'n',
               bty = 'n',
               axes = FALSE,
               xlab = '', ylab = '')
          text(5, 5,
               labels = paste('Select a region of spectrogram and double-click',
                              'to create an annotation'))
        }
      })
    })
  }
  observeEvent(myPars$spec_xlim, drawAnn())

  observeEvent(myPars$currentAnn, {
    if (!is.null(myPars$currentAnn)) {
      if (myPars$print) print('Updating selection...')
      hr()
      sel_points = as.numeric(round(myPars$ann[myPars$currentAnn, c('from', 'to')] /
                                      1000 * myPars$samplingRate))
      idx_points = sel_points[1]:sel_points[2]
      myPars$selection = myPars$myAudio[idx_points]
      # move the spec view to show the selected ann
      ann_dur = myPars$ann$to[myPars$currentAnn] -
        myPars$ann$from[myPars$currentAnn]
      mid_view = mean(myPars$spec_xlim)
      mid_ann = mean(as.numeric(myPars$ann[myPars$currentAnn, c('from', 'to')]))
      shift = mid_ann - mid_view
      if (myPars$ann$from[myPars$currentAnn] < myPars$spec_xlim[1] |
          myPars$ann$to[myPars$currentAnn] > myPars$spec_xlim[2]) {
        if (diff(myPars$spec_xlim) > ann_dur) {
          # the ann fits based on current zoom level
          myPars$spec_xlim[1] = max(0, myPars$spec_xlim[1] + shift)
          myPars$spec_xlim[2] = min(myPars$dur, myPars$spec_xlim[2] + shift)
        } else {
          # zoom out enough to show the whole ann
          half_span = ann_dur * 1.5 / 2
          myPars$spec_xlim[1] = max(0, mid_ann - half_span)
          myPars$spec_xlim[2] = min(myPars$dur, mid_ann + half_span)
        }
      }
    }
  })

  observeEvent(input$ann_click, {
    # select the annotation whose middle (label) is closest to the click
    if (!is.null(myPars$ann)) {
      ds = abs(input$ann_click$x - (myPars$ann$from + myPars$ann$to) / 2)
      myPars$currentAnn = which.min(ds)
      myPars$spectrogram_brush = list(xmin = myPars$ann$from[myPars$currentAnn],
                                      xmax = myPars$ann$to[myPars$currentAnn])
      myPars$cursor = myPars$ann$from[myPars$currentAnn]
      avFmPerSel()
    }
  })

  observeEvent(input$ann_dblclick, {
    # select and edit the double-clicked annotation
    if (!is.null(myPars$ann)) {
      ds = abs(input$ann_dblclick$x - (myPars$ann$from + myPars$ann$to) / 2)
      myPars$currentAnn = which.min(ds)
      showModal(dataModal_edit())
    }
  })

  dataModal_new = function() {
    modalDialog(
      textInput("annotation", "New annotation:",
                placeholder = '...some info...'
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_new", "OK")
      ),
      easyClose = TRUE
    )
  }

  observeEvent(input$ok_new, {
    if (myPars$print) print('Creating a new annotation...')
    new = data.frame(
      # idx = ifelse(is.null(myPars$ann), 1, nrow(myPars$ann) + 1),
      file = myPars$myAudio_filename,
      from = round(myPars$spectrogram_brush$xmin),
      to = round(myPars$spectrogram_brush$xmax),
      label = input$annotation,
      dF = NA, vtl = NA,
      stringsAsFactors = FALSE)
    new[, myPars$ff] = NA

    # depending on the history of changing input$nFormants / output.csv,
    # there may be more formants in myPars$ann than in the current sel
    if (is.null(myPars$ann)) {
      myPars$ann = new
    } else {
      myPars$ann = rbind_fill(myPars$ann, new)
    }

    # reorder and select the newly added annotation
    ord = order(myPars$ann$from)
    myPars$ann = myPars$ann[ord, ]
    myPars$currentAnn = which(ord == nrow(myPars$ann))

    # paste in formant frequencies
    avFmPerSel()
    myPars$ann[myPars$currentAnn, myPars$ff] = myPars$formants[myPars$f_col_names]
    updateFBtn(myPars$formants[myPars$f_col_names])
    updateVTL()

    # clear the selection, close the modal
    removeModal()
    drawAnn()
    avFmPerSel()
    # hr()

    # save a backup in case the app crashes before done() fires
    write.csv(rbind_fill(myPars$out, myPars$ann), 'www/temp.csv', row.names = FALSE)
  })

  updateFBtn = function(ff) {
    if (myPars$print) print('Updating formant buttons...')
    for (f in 1:input$nFormants) {
      updateTextInput(session, inputId = paste0('f', f, '_text'),
                      value = ff[f])
    }
  }

  dataModal_edit = function() {
    modalDialog(
      textInput("annotation", "New annotation:",
                placeholder = '...some info...'
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_edit", "OK")
      ),
      easyClose = TRUE
    )
  }

  observeEvent(input$ok_edit, {
    myPars$ann$label[myPars$currentAnn] = input$annotation
    removeModal()
    drawAnn()
    # drawAnnTbl()
  })



  ### ANALYZE
  extractFormants = reactive({
    if (!is.null(myPars$myAudio) & !is.null(myPars$regionToAnalyze)) {
      if (myPars$print) print('Extracting formants...')
      if (input$coeffs != '') {
        coeffs = as.numeric(input$coeffs)
      } else {
        coeffs = NULL
      }
      myPars$temp_anal = analyze(
        myPars$myAudio,
        samplingRate = myPars$samplingRate,
        from = myPars$regionToAnalyze[1] / 1000,
        to = myPars$regionToAnalyze[2] / 1000,
        scale = myPars$maxAmpl,
        windowLength = input$windowLength_lpc,
        step = input$step_lpc,
        wn = input$wn_lpc,
        zp = input$zp_lpc,
        dynamicRange = input$dynamicRange_lpc,
        silence = input$silence,
        formants = list(
          coeffs = coeffs,
          minformant = input$minformant,
          maxbw = input$maxbw,
          verify = FALSE
        ),
        pitchMethods = NULL,  # disable pitch tracking
        SPL_measured = 0,  # disable loudness analysis
        nFormants = NULL,  # save all available formants
        roughness = list(amRes = 0),  # no roughness analysis
        summaryFun = NULL,
        plot = FALSE
      )
      myPars$nMeasuredFmts = length(grep('_freq', colnames(myPars$temp_anal)))
      myPars$allF_colnames = paste0('f', 1:myPars$nMeasuredFmts, '_freq')
      myPars$temp_anal = myPars$temp_anal[, c('time', myPars$allF_colnames)]
      for (c in colnames(myPars$temp_anal)) {
        if (any(!is.na(myPars$temp_anal[, c]))) {
          # in case of all NAs
          myPars$temp_anal[, c] = round(myPars$temp_anal[, c])
        }
      }
      isolate({
        myPars$analyzedUpTo = myPars$regionToAnalyze[2]
        if (is.null(myPars$formantTracks)) {
          myPars$formantTracks = myPars$temp_anal
        } else {
          new_time_range = range(myPars$temp_anal$time)
          idx = which(myPars$formantTracks$time >= new_time_range[1] &
                        myPars$formantTracks$time <= new_time_range[2])
          if (length(idx) > 0) {
            myPars$formantTracks = myPars$formantTracks[-idx, ]
          }
          myPars$formantTracks = rbind_fill(myPars$formantTracks, myPars$temp_anal)
          # myPars$formantTracks = myPars$formantTracks[order(myPars$formantTracks$time), ]
        }
      })
    }
  })

  # if any of LPC settings change, we re-analyze the entire file
  observeEvent(
    c(input$nFormants, input$silence, input$coeffs, input$minformant,
      input$maxbw, input$windowLength_lpc, input$step_lpc,
      input$wn_lpc, input$zp_lpc, input$dynamicRange_lpc), {
        myPars$analyzedUpTo = 0
      },
    ignoreInit = TRUE
  )

  avFmPerSel = function() {
    # analyze annotated selection
    if (!is.null(myPars$currentAnn) & !is.null(myPars$formantTracks)) {
      if (myPars$print) print('Averaging formants in selection...')
      isolate({
        # don't want dependency on myPars$ann
        idx = which(
          myPars$formantTracks$time >= myPars$ann$from[myPars$currentAnn] &
            myPars$formantTracks$time <= myPars$ann$to[myPars$currentAnn]
        )
      })
      fMat = myPars$formantTracks[idx, 2:(input$nFormants + 1)]
      myPars$formants = apply(fMat, 2, function(x)
        round(do.call(input$summaryFun, list(x, na.rm = TRUE))))
      # myPars$bandwidth ?

      idx_f_na = which(is.na(myPars$ann[myPars$currentAnn, myPars$ff]))
      if (length(idx_f_na) > 0)
        myPars$ann[myPars$currentAnn, myPars$ff[idx_f_na]] = myPars$formants[idx_f_na]

      # fill in the formant boxes - note that we use the (possibly
      # user-modified) myPars$ann instead of myPars$formants
      updateFBtn(as.character(myPars$ann[myPars$currentAnn, myPars$ff]))
    }
  }
  observeEvent(myPars$formantTracks, avFmPerSel())

  observeEvent(myPars$ann, {
    if (myPars$print) print('Drawing ann_table...')
    if (!is.null(myPars$ann)) {
      ann_for_print = myPars$ann[, which(!colnames(myPars$ann) %in% c('X', 'file'))]
    } else {
      ann_for_print = '...waiting for some annotations...'
    }
    output$ann_table = renderTable(
      format(ann_for_print),
      align = 'c', striped = FALSE,
      bordered = TRUE, hover = FALSE, width = '100%'
    )
    hr()
  }, ignoreNULL = FALSE)

  hr = function() {
    if (!is.null(myPars$currentAnn)) {
      # Sys.sleep(.5)
      session$sendCustomMessage('highlightRow', myPars$currentAnn)
    }
  }

  updateVTL = function() {
    if (!is.null(myPars$ann[myPars$currentAnn, ]) &&
        any(!is.na(myPars$ann[myPars$currentAnn, myPars$ff]))) {
      if (myPars$print) print('Updating VTL...')
      fmts_ann = as.numeric(myPars$ann[myPars$currentAnn, myPars$ff])
      vtl_ann = estimateVTL(
        formants = fmts_ann,
        method = input$vtl_method,
        speedSound = input$speedSound,
        interceptZero = input$interceptZero,
        output = 'detailed'
      )
      # method = c('regression', 'meanDispersion', 'meanFormant')[1],
      # speedSound = 35400,
      try({
        vtl_ann$formantDispersion = round(vtl_ann$formantDispersion)
      }, silent = TRUE)
      try({
        vtl_ann$vocalTract = round(vtl_ann$vocalTract, 2)
      }, silent = TRUE)
      myPars$ann$dF[myPars$currentAnn] = vtl_ann$formantDispersion
      myPars$ann$vtl[myPars$currentAnn] = vtl_ann$vocalTract
      # drawAnnTbl()
      # hr()
    }
  }
  observeEvent(c(input$vtl_method, input$speedSound, input$interceptZero), updateVTL())

  observeEvent(input$tableRow, {
    if (!is.null(myPars$ann) && input$tableRow > 0) {
      myPars$currentAnn = input$tableRow
      myPars$spectrogram_brush = list(xmin = myPars$ann$from[myPars$currentAnn],
                                      xmax = myPars$ann$to[myPars$currentAnn])
      avFmPerSel()
    }
  }, ignoreInit = TRUE)


  ## Buttons for operations with selection
  startPlay = function() {
    if (!is.null(myPars$myAudio)) {
      if (!is.null(myPars$spectrogram_brush)) {
        myPars$play$from = myPars$spectrogram_brush$xmin / 1000
        myPars$play$to = myPars$spectrogram_brush$xmax / 1000
      } else {
        myPars$play$from = myPars$cursor / 1000 # myPars$spec_xlim[1] / 1000
        myPars$play$to = myPars$spec_xlim[2] / 1000
      }
      myPars$play$dur = myPars$play$to - myPars$play$from
      myPars$play$timeOn = proc.time()
      myPars$play$timeOff = myPars$play$timeOn + myPars$play$dur
      myPars$cursor_temp = myPars$cursor
      myPars$play$on = TRUE
      if (myPars$print) print('Playing selection...')

      # play selection
      if (input$audioMethod == 'Browser') {
        # play with javascript
        shinyjs::js$playme_js(  # need an external js script for this
          audio_id = 'myAudio',  # defined in UI
          from = myPars$play$from,
          to = myPars$play$to)
      } else {
        # or play with R:
        playme(myPars$myAudio_path, from = myPars$play$from, to = myPars$play$to)
      }
    }
  }
  observeEvent(c(input$selection_play), startPlay())  # add myPars$myAudio for autoplay

  stopPlay = function() {
    myPars$play$on = FALSE
    myPars$cursor = myPars$cursor_temp
    shinyjs::js$stopAudio_js(audio_id = 'myAudio')
  }
  observeEvent(input$selection_stop, stopPlay())

  observe({
    if (!is.null(myPars$play$on) && myPars$play$on) {
      time = proc.time()
      if (!is.null(myPars$slider_ms)) invalidateLater(myPars$slider_ms)
      if ((time - myPars$play$timeOff)[3] > 0) {
        myPars$play$on = FALSE
        myPars$cursor = myPars$cursor_temp  # reset to original cursor
      } else {
        myPars$cursor = as.numeric(
          myPars$play$from + time - myPars$play$timeOn
        )[3] * 1000  # [3] for "elapsed", ie "real" time
      }
    }
  })

  deleteSel = function() {
    if (!is.null(myPars$currentAnn)) {
      myPars$ann = myPars$ann[-myPars$currentAnn, ]
      myPars$selection = NULL
      myPars$currentAnn = NULL
      drawAnn()
      # drawAnnTbl()
    }
  }
  observeEvent(input$selection_delete, deleteSel())

  observeEvent(input$selection_annotate, {
    if (!is.null(myPars$spectrogram_brush)) {
      showModal(dataModal_new())
    }
  })

  # HOTKEYS
  observeEvent(input$userPressedSmth, {
    button_code = floor(input$userPressedSmth)
    # see https://keycode.info/
    if (button_code == 32) {                      # SPACEBAR (play / stop)
      if (myPars$play$on) stopPlay() else startPlay()
    } else if (button_code == 46) {               # DELETE (delete current annotation)
      deleteSel()
    } else if (button_code == 37) {               # ARROW LEFT (scroll left)
      shiftFrame('left', step = myPars$scrollFactor)
    } else if (button_code == 39) {               # ARROW RIGHT (scroll right)
      shiftFrame('right', step = myPars$scrollFactor)
    } else if (button_code == 38) {               # ARROW UP (horizontal zoom-in)
      changeZoom(myPars$zoomFactor)
    } else if (button_code == 83) {               # S (horizontal zoom to selection)
      #  zoomToSel()  # reserve alphanumeric for annotations
    } else if (button_code == 40) {               # ARROW DOWN (horizontal zoom-out)
      changeZoom(1 / myPars$zoomFactor)
    } else if (button_code %in% c(61, 107)) {     # + (vertical zoom-in)
      changeZoom_freq(1 / myPars$zoomFactor_freq)
    } else if (button_code %in% c(173, 109)) {    # - (vertical zoom-out)
      changeZoom_freq(myPars$zoomFactor_freq)
    } else if (button_code == 13) {               # ENTER (next file)
      # nextFile()   # disable b/c it's natural to press ENTER to close a modal win
    } else if (button_code == 8) {                # BACKSPACE (previous file)
      # lastFile()
    }
  })


  ## ZOOM
  changeZoom_freq = function(coef) {
    # midpoint = mean(input$spec_ylim)
    # halfRan = diff(input$spec_ylim) / 2 / coef
    # newLow = max(0, midpoint - halfRan)
    # newHigh = min(myPars$samplingRate / 2, midpoint + halfRan)
    updateSliderInput(session, 'spec_ylim', value = c(0, input$spec_ylim[2] * coef))
  }
  observeEvent(input$zoomIn_freq, changeZoom_freq(1 / myPars$zoomFactor_freq))
  observeEvent(input$zoomOut_freq, changeZoom_freq(myPars$zoomFactor_freq))
  observeEvent(input$spec_ylim, {
    updateSliderInput(session, 'spectrum_xlim', value = input$spec_ylim)
  })

  changeZoom = function(coef, toCursor = FALSE) {
    # intelligent zoom-in a la Audacity: midpoint moves closer to seletion/cursor
    if (!is.null(myPars$cursor) & toCursor) {
      if (!is.null(myPars$spectrogram_brush)) {
        midpoint = 3/4 * mean(c(myPars$spectrogram_brush$xmin,
                                myPars$spectrogram_brush$xmax)) +
          1/4 * mean(myPars$spec_xlim)
      } else {
        if (myPars$cursor > 0) {
          midpoint = 3/4 * myPars$cursor + 1/4 * mean(myPars$spec_xlim)
        } else {
          # when first opening a file, zoom in to the beginning
          midpoint = mean(myPars$spec_xlim) / coef
        }
      }
    } else {
      midpoint = mean(myPars$spec_xlim)
    }
    halfRan = diff(myPars$spec_xlim) / 2 / coef
    newLeft = max(0, midpoint - halfRan)
    newRight = min(myPars$dur, midpoint + halfRan)
    myPars$spec_xlim = c(newLeft, newRight)
  }
  observeEvent(input$zoomIn, changeZoom(myPars$zoomFactor, toCursor = TRUE))
  observeEvent(input$zoomOut, changeZoom(1 / myPars$zoomFactor))
  zoomToSel = function() {
    if (!is.null(myPars$spectrogram_brush)) {
      myPars$spec_xlim = round(c(myPars$spectrogram_brush$xmin,
                                 myPars$spectrogram_brush$xmax))
    }
  }
  observeEvent(input$zoomToSel, {
    zoomToSel()
  })

  shiftFrame = function(direction, step = 1) {
    ran = diff(myPars$spec_xlim)
    shift = ran * step
    if (direction == 'left') {
      newLeft = max(0, myPars$spec_xlim[1] - shift)
      newRight = newLeft + ran
    } else if (direction == 'right') {
      newRight = min(myPars$dur, myPars$spec_xlim[2] + shift)
      newLeft = newRight - ran
    }
    myPars$spec_xlim = c(newLeft, newRight)
    # update cursor when shifting frame, but not when zooming
    myPars$cursor = myPars$spec_xlim[1]
  }
  observeEvent(input$scrollLeft, shiftFrame('left', step = myPars$scrollFactor))
  observeEvent(input$scrollRight, shiftFrame('right', step = myPars$scrollFactor))

  moveSlider = observe({
    if (myPars$print) print('Moving slider')
    width = round(diff(myPars$spec_xlim) / myPars$dur * 100, 2)
    left = round(myPars$spec_xlim[1] / myPars$dur * 100, 2)
    shinyjs::js$scrollBar(  # need an external js script for this
      id = 'scrollBar',  # defined in UI
      width = paste0(width, '%'),
      left = paste0(left, '%'))
  })

  observeEvent(input$scrollBarLeft, {
    if (!is.null(myPars$spec)) {
      spec_span = diff(myPars$spec_xlim)
      scrollBarLeft_ms = input$scrollBarLeft * myPars$dur
      myPars$spec_xlim = c(max(0, scrollBarLeft_ms),
                           min(myPars$dur, scrollBarLeft_ms + spec_span))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$scrollBarMove, {
    direction = substr(input$scrollBarMove, 1, 1)
    if (direction == 'l') {
      shiftFrame('left', step = myPars$scrollFactor)
    } else if (direction == 'r') {
      shiftFrame('right', step = myPars$scrollFactor)
    }
  }, ignoreNULL = TRUE)

  observeEvent(input$scrollBarWheel, {
    direction = substr(input$scrollBarWheel, 1, 1)
    if (direction == 'l') {
      shiftFrame('left', step = myPars$wheelScrollFactor)
    } else if (direction == 'r') {
      shiftFrame('right', step = myPars$wheelScrollFactor)
    }
  }, ignoreNULL = TRUE)

  observeEvent(input$zoomWheel, {
    direction = substr(input$zoomWheel, 1, 1)
    if (direction == 'l') {
      changeZoom(1 / myPars$zoomFactor)
    } else if (direction == 'r') {
      changeZoom(myPars$zoomFactor, toCursor = TRUE)
    }
  }, ignoreNULL = TRUE)

  # step-overlap
  observeEvent(input$overlap, {
    # change step if overlap changes, but don't change step if windowLength changes
    step = round(input$windowLength * (1 - input$overlap / 100))
    if (input$step != step)
      updateNumericInput(session, 'step', value = step)
  }, ignoreInit = TRUE)
  observeEvent(c(input$step, input$windowLength), {
    # change overlap if step or windowLength change
    overlap = (1 - input$step / input$windowLength) * 100
    if (input$overlap != overlap)
      updateSliderInput(session, 'overlap', value = overlap)
  })

  observeEvent(input$overlap_lpc, {
    # change step if overlap changes, but don't change step if windowLength changes
    step_lpc = round(input$windowLength_lpc * (1 - input$overlap_lpc / 100))
    if (input$step_lpc != step_lpc)
      updateNumericInput(session, 'step_lpc', value = step_lpc)
  }, ignoreInit = TRUE)
  observeEvent(c(input$step_lpc, input$windowLength_lpc), {
    # change overlap if step or windowLength change
    overlap_lpc = (1 - input$step_lpc / input$windowLength_lpc) * 100
    if (input$overlap_lpc != overlap_lpc)
      updateSliderInput(session, 'overlap_lpc', value = overlap_lpc)
  })


  # SAVE OUTPUT
  done = function() {
    # meaning we are done with a sound - prepares the output
    if (myPars$print) print('Running done()...')
    if (!is.null(myPars$ann)) {
      if (is.null(myPars$out)) {
        myPars$out = myPars$ann
      } else {
        # remove previous records for this file, if any
        idx = which(myPars$out$file == myPars$ann$file[1])
        if (length(idx) > 0)
          myPars$out = myPars$out[-idx, ]

        # append annotations from the current audio
        myPars$out = rbind_fill(myPars$out, myPars$ann)
      }
      # keep track of formant tracks and spectrograms
      # to avoid analyzing them again if the user goes
      # back and forth between files
      myPars$out_fTracks[[myPars$myAudio_filename]] = myPars$formantTracks
      myPars$out_spects[[myPars$myAudio_filename]] = myPars$spec
    }
    if (!is.null(myPars$out)) {
      # re-order and save a backup
      myPars$out = myPars$out[order(myPars$out$file, myPars$out$from), ]
      write.csv(myPars$out, 'www/temp.csv', row.names = FALSE)
    }
  }

  observeEvent(input$fileList, {
    done()
    myPars$n = which(myPars$fileList$name == input$fileList)
    reset()
    if (length(myPars$n) == 1 && myPars$n > 0) readAudio(myPars$n)
  }, ignoreInit = TRUE)

  nextFile = function() {
    if (!is.null(myPars$myAudio_path)) {
      done()
      if (myPars$n < myPars$nFiles) {
        myPars$n = myPars$n + 1
        updateSelectInput(session, 'fileList',
                          selected = myPars$fileList$name[myPars$n])
        # ...which triggers observeEvent(input$fileList)
      }
    }

  }
  observeEvent(input$nextFile, nextFile())

  lastFile = function() {
    if (!is.null(myPars$myAudio_path)) {
      done()
      if (myPars$n > 1) {
        myPars$n = myPars$n - 1
        updateSelectInput(session, 'fileList',
                          selected = myPars$fileList$name[myPars$n])
      }
    }
  }
  observeEvent(input$lastFile, lastFile())

  output$saveRes = downloadHandler(
    filename = function() 'output.csv',
    content = function(filename) {
      done()  # finalize the last file
      write.csv(myPars$out, filename, row.names = FALSE)
      if (file.exists('www/temp.csv')) file.remove('www/temp.csv')
    }
  )

  observeEvent(input$about, {
    if (myPars$debugQn) {
      browser()  # back door for debugging)
    } else {
      showNotification(
        ui = paste0(
          "App for measuring formants in small annotated segments: soundgen ",
          packageVersion('soundgen'), ". Spectrogram: select an area and ",
          "double-click to add an annotation, left-click to correct a formant ",
          "measure. Spectrum: single-click to update a formant to cursor ",
          "position, double click to update to the closest spectral peak. More ",
          "info: ?formant_app and http://cogsci.se/soundgen.html"),
        duration = 20,
        closeButton = TRUE,
        type = 'default'
      )
    }
  })

  ### TOOLTIPS - have to be here instead of UI b/c otherwise problems with regulating delay
  # (see https://stackoverflow.com/questions/47477237/delaying-and-expiring-a-shinybsbstooltip)
  ## Analysis
  # LPC
  shinyBS::addTooltip(session, id='reset_to_def', title = 'Reset all settings to default values', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='audioMethod', title = "Play audio with javascript (recommended in Firefox, doesn't work in Chrome) or with R (browser-independent, but then the cursor doesn't move, and you can't stop playback)", placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='nFormants', title = 'Number of formants to analyze', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='silence', title = 'Frames below this threshold are not analyzed', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='coeffs', title = 'The number of LPC coefficients (see ?phonTools::findformants)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='minformant', title = 'Lowest accepted formant frequency (see ?phonTools::findformants)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='maxbw', title = 'Maximum accepted formant bandwidth (see ?phonTools::findformants)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))

  # Windowing
  shinyBS::addTooltip(session, id='windowLength_lpc', title = 'Length of STFT window for LPC analysis, ms. Independent of the window used for plotting the spectrogram', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='step_lpc', title = 'Step between analysis frames, ms', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='overlap_lpc', title = 'Overlap between analysis frames, %', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='dynamicRange_lpc', title = 'Dynamic range, dB', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='zp_lpc', title = 'Zero padding: 8 means 2^8 = 256, etc.', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='wn_lpc', title = 'Type of STFT window', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))

  # Vocal tract
  shinyBS::addTooltip(session, id='vtl_method', title = 'Method of calculating vocal tract length (VTL). See ?estimateVTL', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='speedSound', title = 'VTL estimate depends on the assumed speed of sound inside the vocal tract', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='interceptZero', title = 'Set the regression intercept to zero (i.e., assume a closed-open vocal tube model)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))

  ## Plotting
  # spectrogram
  shinyBS::addTooltip(session, id='spec_ylim', title = "Range of displayed frequencies, kHz", placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='windowLength', title = 'Length of STFT window for LPC analysis, ms. Independent of the window used for plotting the spectrogram', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='step', title = 'Step between analysis frames, ms', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='overlap', title = 'Overlap between analysis frames, %', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='dynamicRange', title = 'Dynamic range, dB', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='spec_cex', title = "Magnification coefficient controlling the size of points showing pitch candidates", placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='specContrast', title = 'Regulates the contrast of the spectrogram', placement="below", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='specBrightness', title = 'Regulates the brightness of the spectrogram', placement="below", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='zp', title = 'Zero padding: 8 means 2^8 = 256, etc.', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='wn', title = 'Type of STFT window', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='spec_maxPoints', title = 'The number of points to plot in the spectrogram (smaller = faster, but low resolution)', placement="below", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))

  # oscillogram
  shinyBS::addTooltip(session, id='osc', title = 'The type of oscillogram to show', placement="below", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='osc_maxPoints', title = 'The number of points to plot in the oscillogram (smaller = faster, but low resolution)', placement="below", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))

  # spectrum
  shinyBS::addTooltip(session, id='spectrum_xlim', title = 'Range of displayed frequencies, kHz (normally tied to spectrogram range)', placement="below", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='spectrum_len', title = 'The number of points to plot in the spectrum (smaller = faster, but low resolution)', placement="below", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))

  # action buttons
  shinyBS:::addTooltip(session, id='lastFile', title='Save and return to the previous file', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS:::addTooltip(session, id='nextFile', title='Save and proceed to the next file', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS:::addTooltip(session, id='selection_stop', title='Stop playback', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS:::addTooltip(session, id='selection_play', title='Play selection (SPACEBAR)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS:::addTooltip(session, id='selection_annotate', title='Create a new annotation (DOUBLE-CLICK)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS:::addTooltip(session, id='selection_delete', title='Remove annotation (DELETE)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='saveRes', title = 'Download results (see ?pitch_app for recovering unsaved data after a crash)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))

  # navigation / zoom
  shinyBS::addTooltip(session, id='zoomIn_freq', title = 'Zoom in frequency (+)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='zoomOut_freq', title = 'Zoom out frequency (-)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='scrollLeft', title = 'Scroll left (arrow LEFT)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='zoomOut', title = 'Zoom out time (arrow DOWN)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='zoomToSel', title = 'Zoom to selection (S)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='zoomIn', title = 'Zoom in time (arrow UP)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
  shinyBS::addTooltip(session, id='scrollRight', title = 'Scroll right (arrow RIGHT)', placement="right", trigger="hover", options = list(delay = list(show = 1000, hide = 0)))
}
