# # tip: to read the output, do smth like:
# a = read.csv('~/Downloads/output.csv', stringsAsFactors = FALSE)
# as.numeric(unlist(strsplit(a$pitch, ',')))

# shinyBS needs to be included as a dependency (instead of just "import"):
# see https://stackoverflow.com/questions/52649138/including-shinybs-in-a-package

server = function(input, output, session) {
    myPars = reactiveValues()
    myPars$zoomFactor = 2     # zoom buttons change time zoom by this factor
    myPars$zoomFactor_freq = 1.5  # same for frequency
    myPars$print = TRUE       # if TRUE, some functions print a meassage to the console when called
    myPars$out = NULL         # for storing the output
    myPars$drawSpec = TRUE
    myPars$shinyTip_show = 1000      # delay until showing a tip (ms)
    myPars$shinyTip_hide = 0         # delay until hiding a tip (ms)
    myPars$myAudio = NULL
    myPars$currentAnn = NULL
    myPars$ann = data.frame(from = NA, to = NA, label = NA)[-1, ]

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
        myPars$out = read.csv('www/temp.csv')
        removeModal()
    })

    files = list.files('www/', pattern = '.wav')
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

    resetSliders = function() {
        sliders_to_reset = names(input)[which(names(input) %in% rownames(defaults_analyze))]
        for (v in sliders_to_reset) {
            new_value = defaults_analyze[v, 'default']
            try(updateSliderInput(session, v, value = new_value))
            try(updateNumericInput(session, v, value = new_value))
            updateSelectInput(session, 'wn', selected = 'gaussian')
            updateCheckboxGroupInput(session, 'pitchMethods', selected = c('dom', 'autocor'))
            updateCheckboxGroupInput(session, 'summaryFun', selected = c('mean', 'sd'))
            updateTextInput(session, 'summaryFun_text', value = '')
            updateSelectInput(session, 'pathfinding', selected = 'fast')
            updateSliderInput(session, 'spec_ylim', value=c(0, defaults_analyze['spec_ylim','default']))
            updateRadioButtons(session, 'spec_colorTheme', selected='bw')
            updateSelectInput(session, 'osc', selected = 'linear')
        }
    }
    observeEvent(input$reset_to_def, resetSliders())


    observeEvent(input$loadAudio, {
        if (myPars$print) print('Loading audio...')
        done()  # save previous work, if any
        myPars$n = 1   # file number in queue
        myPars$nFiles = nrow(input$loadAudio)  # number of uploaded files in queue
        myPars$fileList = paste(input$loadAudio$name, collapse = ', ')
        # set up a list for storing manual anchors for each of uploaded files
        myPars$history = vector('list', length = myPars$nFiles)
        names(myPars$history) = input$loadAudio$name
        for (i in 1:length(myPars$history)) {
            myPars$history[[i]] = list(manual = NULL, manualUnv = NULL)
        }

        reset()
        readAudio(1)  # read the first sound in queue
    })

    observeEvent(input$showpanel, {
        if(input$showpanel == TRUE) {
            shinyjs::removeCssClass("Main", "col-sm-12")
            shinyjs::addCssClass("Main", "col-sm-9")
            shinyjs::show(id = "Sidebar")
            shinyjs::enable(id = "Sidebar")
        }
        else {
            shinyjs::removeCssClass("Main", "col-sm-9")
            shinyjs::addCssClass("Main", "col-sm-12")
            shinyjs::hide(id = "Sidebar")
        }
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
            warning('Input not recognized: must be a wav or mp3 file')
        }

        myPars$myAudio = as.numeric(myPars$temp_audio@left)
        myPars$ls = length(myPars$myAudio)
        myPars$samplingRate = myPars$temp_audio@samp.rate
        myPars$maxAmpl = 2 ^ (myPars$temp_audio@bit - 1)
        updateSliderInput(session, 'spec_ylim', max = myPars$samplingRate / 2 / 1000)  # check!!!
        myPars$dur = round(length(myPars$temp_audio@left) / myPars$temp_audio@samp.rate * 1000)
        myPars$time = seq(1, myPars$dur, length.out = myPars$ls)
        myPars$spec_xlim = c(0, myPars$dur)
        # for the first audio only, update autocorSmooth
        # to a default that depends on samplingRate
        if (i == 1) {
            updateSliderInput(session, inputId = 'autocorSmooth',
                              value = 2 * ceiling(7 * myPars$samplingRate / 44100 / 2) - 1)
        }

        # update info - file number ... out of ...
        file_lab = paste0('File ', myPars$n, ' of ', myPars$nFiles) # , ': ', myPars$myAudio_filename)
        output$fileN = renderUI(HTML(file_lab))

        # if we've already worked with this file in current session,
        # re-load the manual anchors
        hist = myPars$history[[myPars$myAudio_filename]]
        if (!is.null(hist$manual)) myPars$manual = hist$manual
        if (!is.null(hist$manualUnv)) myPars$manualUnv = hist$manualUnv
    }

    extractSpectrogram = observe({
        if (myPars$print) print('Extracting spectrogram...')
        # Instead of re-loading the file every time, save the spectrogram matrix
        # and re-draw manually with soundgen:::filled.contour.mod
        if (!is.null(myPars$myAudio)) {
            myPars$spec = spectrogram(
                myPars$myAudio,
                samplingRate = myPars$samplingRate,
                dynamicRange = input$dynamicRange,
                windowLength = input$windowLength,
                overlap = input$overlap,
                wn = input$wn,
                zp = 2 ^ input$zp,
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
        output$htmlAudio = renderUI(
            tags$audio(src = myPars$myfile, type = myPars$myAudio_type,
                       autoplay = NA, controls = NA,
                       style="transform: scale(0.75); transform-origin: 0 0;")
        )
    })


    # Updating spec / osc stuff to speed up plotting
    observe({
        if (!is.null(myPars$myAudio)) {
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
        if (!is.null(myPars$spec)) {
            if (myPars$print) print('Trimming the spec & osc')
            x = as.numeric(colnames(myPars$spec))
            idx_x = which(x >= (myPars$spec_xlim[1] / 1.05) &
                              x <= (myPars$spec_xlim[2] * 1.05))
            # 1.05 - a bit beyond b/c we use xlim/ylim and may get white space
            myPars$spec_trimmed = myPars$spec[, idx_x]
            idx_s = (myPars$spec_xlim[1] / 1.05 * myPars$samplingRate / 1000) :
                max(myPars$ls, (myPars$spec_xlim[2] / 1.05 * myPars$samplingRate / 1000))
            downs_spec = 10 ^ input$maxPoints_spec
            downs_osc = 10 ^ input$maxPoints_osc
            myPars$myAudio_trimmed = myPars$myAudio_scaled[idx_s]
            isolate({
                myPars$spec_trimmed = downsample_spec(myPars$spec_trimmed, downs_spec)
                myPars$ls_trimmed = length(myPars$myAudio_trimmed)
                myPars$time_trimmed = myPars$time[idx_s]
                if (!is.null(myPars$myAudio_trimmed) &&
                    myPars$ls_trimmed > downs_osc) {
                    if (myPars$print) print('Downsampling osc')
                    myseq = round(seq(1, myPars$ls_trimmed,
                                      length.out = downs_osc))
                    myPars$myAudio_trimmed = myPars$myAudio_trimmed[myseq]
                    myPars$time_trimmed = myPars$time_trimmed[myseq]
                    myPars$ls_trimmed = length(myseq)
                }
            })
        }
    })

    observe({
        if (!is.null(myPars$spec)) {
            if (myPars$print) print('Trimming the spec')
            y = as.numeric(rownames(myPars$spec))
            idx_y = which(y >= (input$spec_ylim[1] / 1.05) &
                              y <= (input$spec_ylim[2] * 1.05))
            myPars$spec_trimmed = downsample_spec(
                x = myPars$spec[idx_y, ],
                maxPoints = 10 ^ input$maxPoints_spec)
        }
    })

    downsample_sound = function(x, maxPoints) {
        if (!is.null(myPars$myAudio_trimmed) &&
            myPars$ls_trimmed > (10 ^ input$maxPoints_osc)) {
            if (myPars$print) print('Downsampling osc')
            myseq = round(seq(1, myPars$ls_trimmed,
                              by = myPars$ls_trimmed / input$maxPoints_osc))
            myPars$myAudio_trimmed = myPars$myAudio_trimmed[myseq]
            myPars$ls_trimmed = length(myseq)
        }
    }

    downsample_spec = function(x, maxPoints) {
        lxy = nrow(x) * ncol(x)
        if (lxy > maxPoints) {
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

    # Actuall plotting of the spec / osc
    output$spectrogram = renderPlot({
        if (myPars$drawSpec == TRUE) {
            if (myPars$print) print('Drawing spectrogram...')
            par(mar = c(ifelse(input$osc == 'none', 2, 0.2), 2, 0.5, 2))  # no need to save user's graphical par-s - revert to orig on exit
            if (is.null(myPars$myAudio_trimmed) | is.null(myPars$spec)) {
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
                if (input$osc == 'none') {
                    axis(side = 1)
                    title(xlab = 'Time, ms')
                }

                # add a rectangle showing the currently annotated region
                if (!is.null(myPars$currentAnn)) {
                    rect(
                        xleft = myPars$currentAnn$from,
                        xright = myPars$currentAnn$to,
                        ybottom = input$spec_ylim[1],
                        ytop = input$spec_ylim[2],
                        col = rgb(.2, .2, .2, alpha = .25)
                    )
                }

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
        output$oscillogram = renderPlot({
            if (!is.null(myPars$myAudio_trimmed) & input$osc != 'none') {
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
                axis(side = 1)
                if (input$osc == 'dB') {
                    axis(side = 4, at = seq(0, input$dynamicRange, by = 10))
                    mtext("dB", side = 2, line = 3)
                }
                abline(h = 0, lty = 2)
            }
        }, height = input$osc_height)
    })

    observe({
        output$ann_plot = renderPlot({
            if (nrow(myPars$ann) > 0) {
                par(mar = c(0, 0, 0, 0))
                plot(myPars$time_trimmed,
                     xlim = myPars$spec_xlim,
                     ylim = c(0, 1),
                     type = 'n',
                     bty = 'n',
                     xaxt = 'n', yaxt = 'n',
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
                         adj = c(.5, 1), cex = 1.5)
                }
                par(mar = c(ifelse(input$osc == 'none', 2, 0.2), 2, 0.5, 2))
            }
        })
    })

    observe({
        # analyze the file (executes every time a slider with arg value is changed)
        if (!is.null(myPars$myAudio)) {
            if (myPars$print) print('Calling analyze()...')
            withProgress(message = 'Analyzing the sound...', value = 0.5, {
                myPars$step = input$windowLength * (1 - input$overlap / 100)
                if (TRUE) {
                    # just the mean value
                    temp_anal = phonTools::findformants(
                        sound = myPars$myAudio,  # change to selection
                        fs = myPars$samplingRate,
                        verify = FALSE
                    )
                    myPars$formants = temp_anal$formant
                    myPars$bandwidth = temp_anal$bandwidth
                } else {
                    # frame-by-frame formant tracks
                    temp_anal = analyze(
                        myPars$myAudio_path,
                        windowLength = input$windowLength,
                        step = myPars$step,
                        wn = input$wn,
                        zp = input$zp,
                        dynamicRange = input$dynamicRange,
                        silence = input$silence,
                        entropyThres = input$entropyThres,
                        nFormants = 6,     # disable formant tracking
                        SPL_measured = 0,  # disable loudness analysis
                        pitchMethods = NA,
                        # we don't want analyze to waste time on pathfinding
                        # b/c we do it separately in obs_pitch()
                        interpolWin = 0,
                        pathfinding = 'none',
                        snakeStep = 0,
                        snakePlot = FALSE,
                        smooth = 0,
                        summary = FALSE,
                        plot = FALSE
                    )
                }
            })
        }
    })


    ## Clicking events
    observeEvent(input$spectrogram_click, {

    })

    observeEvent(input$spectrogram_dblclick, {
        if (!is.null(input$spectrogram_brush)) {
            myPars$currentAnn = data.frame(
                from = input$spectrogram_brush$xmin,
                to = input$spectrogram_brush$xmax)
            # clear the selection
            session$resetBrush("spectrogram_brush")
            newAnnot()
        }
    })

    observeEvent(input$ann_click, {
        # select the annotation whose middle is closest to the click
    })

    dataModal = function() {
        modalDialog(
            textInput("annotation", "Label the region if needed",
                      placeholder = '...some info...'
            ),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("ok", "OK")
            )
        )
    }

    observeEvent(input$ok, {
        myPars$currentAnn$label = input$annotation
        myPars$ann = rbind(myPars$ann, myPars$currentAnn)
        removeModal()
    })

    newAnnot = function() {
        myPars$currentAnn
        showModal(dataModal())
        # shinyjs::runjs(window.prompt("sometext","defaultText");)
        # window.prompt("sometext","defaultText");
    }

    ## Buttons for operations with selection
    playSel = function() {
        if (!is.null(myPars$myAudio_path)) {
            if (!is.null(input$spectrogram_brush) & length(myPars$brush_sel_x) > 0) {
                from = myPars$brush_sel_x[1] / length(myPars$pitch) * myPars$dur / 1000
                to = tail(myPars$brush_sel_x, 1) / length(myPars$pitch) * myPars$dur / 1000
            } else {
                from = myPars$spec_xlim[1] / 1000
                to = myPars$spec_xlim[2] / 1000
            }
            playme(myPars$myAudio_path, from = from, to = to)
        }
    }
    observeEvent(input$selection_play, playSel())

    observeEvent(input$userPressedSmth, {
        button_code = floor(input$userPressedSmth)
        if (button_code == 32) {                      # SPACEBAR (play)
            playSel()
        } else if (button_code == 37) {               # ARROW LEFT (scroll left)
            shiftFrame('left')
        } else if (button_code == 39) {               # ARROW RIGHT (scroll right)
            shiftFrame('right')
        } else if (button_code == 38) {               # ARROW UP (horizontal zoom-in)
            changeZoom(myPars$zoomFactor)
        } else if (button_code == 83) {               # S (horizontal zoom to selection)
            zoomToSel()
        } else if (button_code == 40) {               # ARROW DOWN (horizontal zoom-out)
            changeZoom(1 / myPars$zoomFactor)
        } else if (button_code %in% c(61, 107)) {     # + (vertical zoom-in)
            changeZoom_freq(1 / myPars$zoomFactor_freq)
        } else if (button_code %in% c(173, 109)) {    # - (vertical zoom-out)
            changeZoom_freq(myPars$zoomFactor_freq)
        } else if (button_code == 13) {               # ENTER (next file)
            nextFile()
        } else if (button_code == 8) {                # BACKSPACE (previous file)
            lastFile()
        }
    })


    # HOVER
    hover_label = reactive({
        hover_temp = input$spectrogram_hover
        if (!is.null(hover_temp) & !is.null(myPars$myAudio_path)) {
            cursor_hz = hover_temp$y * 1000
            cursor_notes = soundgen::notesDict$note[round(HzToSemitones(cursor_hz)) + 1]
            label = paste0('Cursor: ',
                           round(hover_temp$y * 1000), 'Hz (',
                           cursor_notes, ')')
        } else {
            label = ''
        }
        return(label)
    })
    output$pitchAtCursor = renderUI(HTML(hover_label()))

    # BRUSH
    brush = observeEvent(input$spectrogram_brush, {
    })

    # ZOOM
    changeZoom_freq = function(coef) {
        # midpoint = mean(input$spec_ylim)
        # halfRan = diff(input$spec_ylim) / 2 / coef
        # newLow = max(0, midpoint - halfRan)
        # newHigh = min(myPars$samplingRate / 2, midpoint + halfRan)
        updateSliderInput(session, 'spec_ylim', value = c(0, input$spec_ylim[2] * coef))
    }
    observeEvent(input$zoomIn_freq, changeZoom_freq(1 / myPars$zoomFactor_freq))
    observeEvent(input$zoomOut_freq, changeZoom_freq(myPars$zoomFactor_freq))

    changeZoom = function(coef) {
        midpoint = mean(myPars$spec_xlim)
        halfRan = diff(myPars$spec_xlim) / 2 / coef
        newLeft = max(0, midpoint - halfRan)
        newRight = min(myPars$dur, midpoint + halfRan)
        myPars$spec_xlim = c(newLeft, newRight)
    }
    observeEvent(input$zoomIn, changeZoom(myPars$zoomFactor))
    observeEvent(input$zoomOut, changeZoom(1 / myPars$zoomFactor))
    zoomToSel = function() {
        if (!is.null(input$spectrogram_brush)) {
            myPars$spec_xlim = round(c(input$spectrogram_brush$xmin, input$spectrogram_brush$xmax))
        }
    }
    observeEvent(input$zoomToSel, {
        zoomToSel()
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

    # SAVE OUTPUT
    done = function() {
        # meaning we have finished editing pitch contour for a sound - prepares
        # the output
        if (myPars$print) print('Running done()...')
        session$resetBrush("spectrogram_brush")  # doesn't reset automatically
        if (!is.null(myPars$myAudio_path) && !is.null(myPars$result)) {
            new = data.frame(
                file = basename(myPars$myAudio_filename),
                time = paste(round(myPars$X), collapse = ', '),
                pitch = paste(round(myPars$pitch), collapse = ', '),
                stringsAsFactors = FALSE
            )
            result_new = soundgen:::updateAnalyze(
                result = myPars$result,
                pitch_true = myPars$pitch,
                spectrogram = myPars$spec_from_anal,
                harmHeight_pars = list(
                    harmThres = defaults_analyze['harmThres', 'default'],
                    harmTol = defaults_analyze['harmTol', 'default'],
                    harmPerSel = defaults_analyze['harmPerSel', 'default']),
                smooth = input$smooth,
                smoothing_ww = myPars$smoothing_ww,
                smoothingThres = myPars$smoothing_ww
            )
            summary_new = soundgen:::summarizeAnalyze(
                result_new,
                summaryFun = isolate(myPars$summaryFun),)
            new = cbind(new$file,
                        summary_new,
                        new[, c('time', 'pitch')])
            colnames(new)[1] = 'file'  # otherwise misnamed
            new$file = as.character(new$file)  # otherwise becomes a factor
            if (is.null(myPars$out)) {
                myPars$out = new
            } else {
                idx = which(myPars$out$file == new$file)
                if (length(idx) == 1) {
                    myPars$out[idx, ] = new
                } else {
                    myPars$out = rbind(myPars$out, new)
                }
            }
        }
        if (!is.null(myPars$out))
            write.csv(myPars$out, 'www/temp.csv', row.names = FALSE)

        # add manual corrections to the history list
        if (!is.null(myPars$myAudio_filename)) {
            myPars$history[[myPars$myAudio_filename]]$manual = myPars$manual
            myPars$history[[myPars$myAudio_filename]]$manualUnv = myPars$manualUnv
        }
    }

    nextFile = function() {
        if (!is.null(myPars$myAudio_path)) {
            done()
            if (myPars$n < myPars$nFiles) {
                myPars$n = myPars$n + 1
                reset()
                readAudio(myPars$n)
            }
        }

    }
    observeEvent(input$nextFile, nextFile())

    lastFile = function() {
        if (!is.null(myPars$myAudio_path)) {
            done()
            if (myPars$n > 1) {
                myPars$n = myPars$n - 1
                reset()
                readAudio(myPars$n)
                # todo: re-load the manual pitch contour for the previous file -
                # remember myPars$manual and myPars$manualUnv
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
        id <<- showNotification(
            ui = paste0("Manual pitch editor: soundgen ", packageVersion('soundgen'), ". Left-click to add/correct a pitch anchor, double-click to remove/unvoice the frame. More info: ?pitch_app and http://cogsci.se/soundgen.html"),
            duration = 10,
            closeButton = TRUE,
            type = 'default'
        )
    })

    ## TOOLTIPS - have to be here instead of UI b/c otherwise problems with regulating delay
    # (see https://stackoverflow.com/questions/47477237/delaying-and-expiring-a-shinybsbstooltip)
    # STFT
    shinyBS::addTooltip(session, id='reset_to_def', title = 'Reset all settings to default values', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='windowLength', title = 'Length of STFT window, ms. Larger values improve frequency resolution at the expense of time resolution', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='overlap', title = 'Overlap between analysis frames, %', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='dynamicRange', title = 'Dynamic range of spectrogram, dB', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='zp', title = 'Zero padding of STFT window (improves frequency resolution): 8 means 2^8 = 256, etc.', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='wn', title = 'Type of STFT window', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

    # voicing
    shinyBS::addTooltip(session, id='silence', title = 'Frames with RMS below silence threshold are not analyzed', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='entropyThres', title = 'Frames with Weiner entropy above entropy threshold are ignored when searching for pitch candidates', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='nCands', title = 'Maximum number of pitch candidates to use per method', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='minVoicedCands', title = 'Minimum number of pitch candidates that have to be defined to consider a frame voiced', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

    # priors
    shinyBS::addTooltip(session, id='pitchFloor', title = 'No candidates below this absolute threshold', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='pitchCeiling', title = 'No candidates above this absolute threshold', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='priorMean', title = 'Candidates close to this value are prioritized (how close is determined by priorSD)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='priorSD', title = 'Determines the width of expected pitch range (standard deviation of gamma distribution around priorMean)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

    # trackers
    shinyBS::addTooltip(session, id='domThres', title = 'Dominant frequency is defined as the lowest bin in a spectrum smoothed and normalized to range from 0 to 1 that it at least "domThres" high (1 = absolute maximum, ie peak frequency)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='domSmooth', title = 'Width of smoothing interval for finding the lowest dominant frequency band (low values = no smoothing)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='autocorThres', title = 'Voicing threshold for autocorrelation algorithm', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='autocorSmooth', title = 'Width of smoothing interval (in bins) for finding peaks in the autocorrelation function', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='autocorUpsample', title = 'Upsamples acf to this resolution (Hz) to improve accuracy in high frequencies', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='autocorBestPeak', title = 'Amplitude of the lowest best candidate relative to the absolute max of the acf', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='cepThres', title = 'Voicing threshold for cepstral algorithm', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='cepSmooth', title = 'Width of smoothing interval for finding peaks in the cepstrum', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='cepZp', title = 'Length of cepstral window after zero padding: 8 means 2^8 = 256, etc.', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specThres', title = 'Voicing threshold for Ba-Na algorithm', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specPeak', title = 'Minimum amplitude of harmonics considered pitch candidates', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specHNRslope', title = '0 = same threshold regardless of HNR; positive = lower threshold in noisy sounds', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specSmooth', title = 'Width of window for detecting harmonics in the spectrum, Hz', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specMerge', title = 'Pitch candidates within specMerge semitones are merged with boosted certainty', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specSinglePeakCert', title = 'If pitch is calculated based on a single harmonic ratio (as opposed to several ratios converging on the same candidate), its certainty is taken to be specSinglePeakCert', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='hpsNum', title = 'How many times to downsample and then multiply the spectra', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='hpsThres', title = 'How high a spectral peak has to be to be considered a pitch candidate, ~0 to 1', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='hpsNorm', title = 'Rather arbitrary normalization of certainty in hps candidates intended to make them more comparable to other pitch tracking methods (0 = no boost in certainty, 2 = default quadratic)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='hpsPenalty', title = 'HPS performs worse at low frequencies (relative to windowLength), so low-frequency pitch candidates are penalized (0 = no penalization, ~10-20 = a lot)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

    # pathfinder
    shinyBS::addTooltip(session, id='summaryFun', title = "The function(s) used to summarize output", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='summaryFun_text', title = "If specified, overrides the options above. For short column names, define and name your function in R prior to starting pitch_app", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='automPathUpdate', title = "Update the optimal pitch contour automatically every time an anchor changes? Turn off to avoid delays when editing a long audio", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='pathfinding', title = "Method of finding the optimal path through pitch candidates: 'none' = best candidate per frame, 'fast' = simple heuristic, 'slow' = annealing (initial analysis only)", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='certWeight', title = 'Specifies how much we prioritize the certainty of pitch candidates vs. pitch jumps', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='shortestSyl', title = 'Shorter voiced segments (ms) will be treated as voiceless or merged with longer segments', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='shortestPause', title = "The smallest gap between voiced syllables (ms) that means they shouldn't be merged", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='smooth', title = 'Amount of median smoothing', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

    # smoothing
    shinyBS::addTooltip(session, id='interpolWin', title = "If no pitch candidates are found within ±interpolTol of the median 'best guess' over ±interpolWin, this median is added as an interpolated candidate", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='interpolTol', title = "Tolerated deviance from 'best guess' before adding an interpolated candidate: proportion of best guess frequency", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='interpolCert', title = "Certainty assigned to interpolated pitch candidates", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

    # spectrogram
    shinyBS::addTooltip(session, id='spec_ylim', title = "Range of displayed frequencies, kHz", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='maxPoints_spec', title = 'The number of points to plot in the spectrogram (smaller = faster, but low resolution)', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='spec_cex', title = "Magnification coefficient controlling the size of points showing pitch candidates", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specContrast', title = 'Regulates the contrast of the spectrogram', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specBrightness', title = 'Regulates the brightness of the spectrogram', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))

    # oscillogram
    shinyBS::addTooltip(session, id='osc', title = 'The type of oscillogram to show', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='osc_height', title = 'The height of oscillogram, pixels', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='maxPoints_osc', title = 'The number of points to plot in the oscillogram (smaller = faster, but low resolution)', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))

    # action buttons
    shinyBS:::addTooltip(session, id='lastFile', title='Save and return to the previous file (BACKSPACE)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS:::addTooltip(session, id='nextFile', title='Save and proceed to the next file (ENTER)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS:::addTooltip(session, id='selection_play', title='Play selection (SPACEBAR)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='saveRes', title = 'Download results (see ?pitch_app for recovering unsaved data after a crash)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

    # navigation / zoom
    shinyBS::addTooltip(session, id='zoomIn_freq', title = 'Zoom in frequency (+)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='zoomOut_freq', title = 'Zoom out frequency (-)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='scrollLeft', title = 'Scroll left (arrow LEFT)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='zoomOut', title = 'Zoom out time (arrow DOWN)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='zoomToSel', title = 'Zoom to selection (S)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='zoomIn', title = 'Zoom in time (arrow UP)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='scrollRight', title = 'Scroll right (arrow RIGHT)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
}
