# pitch_app()
#
# To do: maybe analyze one bit at a time like in formant_app
#
# # tip: to read the output, do smth like:
# a = read.csv('~/Downloads/output.csv', stringsAsFactors = FALSE)
# as.numeric(unlist(strsplit(a$pitch, ',')))

# shinyBS needs to be included as a dependency (instead of just "import"):
# see https://stackoverflow.com/questions/52649138/including-shinybs-in-a-package

server = function(input, output, session) {
    # make overlaid plots resizable (js fix)
    shinyjs::js$inheritSize(parentDiv = 'specDiv')

    myPars = reactiveValues(
        zoomFactor = 2,     # zoom buttons change time zoom by this factor
        zoomFactor_freq = 1.5,  # same for frequency
        print = TRUE,       # if TRUE, some functions print a meassage to the console when called
        out = NULL,         # for storing the output
        drawSpec = TRUE,
        shinyTip_show = 1000,      # delay until showing a tip (ms)
        shinyTip_hide = 0,         # delay until hiding a tip (ms)
        slider_ms = 50,            # how often to update play slider
        cursor = 0,
        initDur = 1500,            # initial duration to plot (ms)
        play = list(on = FALSE)
    )


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
        if (myPars$print) print('Resetting...')
        myPars$pitch = NULL       # pitch contour
        myPars$pitchCands = NULL  # matrix of pitch candidates
        myPars$bp = NULL          # selected points (under brush)
        myPars$manual = data.frame(frame = NA, freq = NA)[-1, ]  # manually added pitch anchors
        myPars$manualUnv = numeric()                             # manually unvoiced frames
        myPars$drawSpec = FALSE   # prevent the spectrogram from being redrawn needlessly
        # (only draw it after extracting it)
        myPars$spec = NULL
        myPars$selection = NULL
        myPars$cursor = 0
        myPars$spectrogram_brush = NULL
    }

    resetSliders = function() {
        if (myPars$print) print('Resetting sliders...')
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
        reset()  # also triggers done()

        # work only with audio files
        idx_audio = apply(matrix(input$loadAudio$type), 1, function(x) {
            grepl('audio', x, fixed = TRUE)
        })
        myPars$fileList = input$loadAudio[idx_audio, ]
        myPars$n = 1   # file number in queue
        myPars$nFiles = nrow(myPars$fileList)  # number of uploaded files in queue
        # set up a list for storing manual anchors for each of uploaded files
        myPars$history = vector('list', length = myPars$nFiles)
        names(myPars$history) = myPars$fileList$name
        for (i in 1:length(myPars$history)) {
            myPars$history[[i]] = list(manual = NULL, manualUnv = NULL)
        }

        choices = as.list(myPars$fileList$name)
        names(choices) = myPars$fileList$name
        updateSelectInput(session, 'fileList',
                          choices = as.list(myPars$fileList$name))
        # readAudio(1)  # read the first sound in queue
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
        if (myPars$print) print('Reading audio...')
        temp = myPars$fileList[i, ]
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
        if (input$normalizeInput) {
            myPars$myAudio = myPars$myAudio / max(abs(myPars$myAudio)) * myPars$maxAmpl
        }
        updateSliderInput(session, 'spec_ylim', max = myPars$samplingRate / 2 / 1000)  # check!!!
        myPars$dur = round(length(myPars$temp_audio@left) / myPars$temp_audio@samp.rate * 1000)
        myPars$time = seq(1, myPars$dur, length.out = myPars$ls)
        myPars$spec_xlim = c(0, min(myPars$initDur, myPars$dur))
        moveSlider()

        # update info - file number ... out of ...
        updateSelectInput(session, 'fileList',
                          label = NULL,
                          selected = myPars$fileList$name[myPars$n])
        file_lab = paste0('File ', myPars$n, ' of ', myPars$nFiles)
        output$fileN = renderUI(HTML(file_lab))

        # if we've already worked with this file in current session,
        # re-load the manual anchors
        hist = myPars$history[[myPars$myAudio_filename]]
        if (!is.null(hist$manual)) myPars$manual = hist$manual
        if (!is.null(hist$manualUnv)) myPars$manualUnv = hist$manualUnv
    }

    extractSpectrogram = observe({
        # Instead of re-loading the file every time, save the spectrogram matrix
        # and re-draw manually with soundgen:::filled.contour.mod
        if (!is.null(myPars$myAudio) & is.null(myPars$spec)) {
            if (myPars$print) print('Extracting spectrogram...')
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


    # Updating spec / osc stuff to speed up plotting
    observe({
        if (!is.null(myPars$myAudio)) {
            if (myPars$print) print('Scaling audio...')
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
                min(myPars$ls, (myPars$spec_xlim[2] / 1.05 * myPars$samplingRate / 1000))
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
        if (!is.null(myPars$spec) && myPars$drawSpec == TRUE) {
            if (myPars$print) print('Drawing spectrogram...')
            par(mar = c(0.2, 2, 0.5, 2))
            # no need to save user's graphical par-s - revert to orig on exit
            if (is.null(myPars$myAudio_trimmed) | is.null(myPars$spec)) {
                plot(1:10, type = 'n', bty = 'n', axes = FALSE, xlab = '', ylab = '')
                text(
                    x = 5, y = 5,
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
                    labels = myPars$spectrogram_hover$time,
                    adj = .5))
            }

            # Add a rectangle showing the selected region
            if (!is.null(myPars$spectrogram_brush) & input$spectro_clickAct == 'select') {
                rect(
                    xleft = myPars$spectrogram_brush$xmin,
                    xright = myPars$spectrogram_brush$xmax,
                    ybottom = input$spec_ylim[1],
                    ytop = input$spec_ylim[2],
                    col = rgb(.2, .2, .2, alpha = .15),
                    border = NA
                )
            }

            # show prior
            ran_x_5 = diff(range(myPars$spec_xlim)) * .05   # 5% of plot width
            points(myPars$spec_xlim[1] + myPars$prior$prob * ran_x_5,
                   myPars$prior$freq / 1000, type = 'l', lty = 2)
            text(x = myPars$spec_xlim[1] + ran_x_5,
                 y = input$priorMean / 1000,
                 pos = 2, labels = 'Prior', offset = 0.25)
            text(x = myPars$spec_xlim[1],
                 y = input$pitchFloor / 1000,
                 pos = 4, labels = 'floor', offset = 0)
            text(x = myPars$spec_xlim[1],
                 y = input$pitchCeiling / 1000,
                 pos = 4, labels = 'ceiling', offset = 0)

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
            soundgen:::addPitchCands(
                pitchCands = temp_freq,
                pitchCert = temp_cert,
                pitchSource = temp_source,
                pitch = myPars$pitch,
                addToExistingPlot = TRUE,
                showLegend = TRUE,
                ylim = c(input$spec_ylim[1], input$spec_ylim[2]),
                pitchPlot = list(cex = input$spec_cex)
            )
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
                time_location = axTicks(1)
                time_labels = convert_sec_to_hms(time_location / 1000)
                axis(side = 1, at = time_location, labels = time_labels)
                if (input$osc == 'dB') {
                    axis(side = 4, at = seq(0, input$dynamicRange, by = 10))
                    mtext("dB", side = 2, line = 3)
                }
                abline(h = 0, lty = 2)
            }
        }, height = input$osc_height)
    })

    observe({
        if (input$summaryFun_text != '') {
            myPars$summaryFun = input$summaryFun_text
        } else {
            myPars$summaryFun = input$summaryFun
        }
        # print(myPars$summaryFun)
    })

    observe({
        # analyze the file (executes every time a slider with arg value is changed)
        if (!is.null(myPars$myAudio)) {
            if (myPars$print) print('Calling analyze()...')

            withProgress(message = 'Analyzing the sound...', value = 0.5, {
                myPars$step = input$windowLength * (1 - input$overlap / 100)
                temp_anal = analyze(
                    myPars$myAudio,
                    samplingRate = myPars$samplingRate,
                    scale = myPars$maxAmpl,
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
                    nCands = input$nCands,
                    minVoicedCands = input$minVoicedCands,
                    pitchDom = list(
                        domThres = input$domThres,
                        domSmooth = input$domSmooth
                    ),
                    pitchAutocor = list(
                        autocorThres = input$autocorThres,
                        # autocorSmooth = input$autocorSmooth,
                        autocorUpsample = input$autocorUpsample,
                        autocorBestPeak = input$autocorBestPeak
                    ),
                    pitchCep = list(
                        cepThres = input$cepThres,
                        cepSmooth = input$cepSmooth,
                        cepZp = input$cepZp
                    ),
                    pitchSpec = list(
                        specThres = input$specThres,
                        specPeak = input$specPeak,
                        specSinglePeakCert = input$specSinglePeakCert,
                        specHNRslope = input$specHNRslope,
                        specSmooth = input$specSmooth,
                        specMerge = input$specMerge
                    ),
                    pitchHps = list(
                        hpsThres = input$hpsThres,
                        hpsNum = input$hpsNum,
                        hpsNorm = input$hpsNorm,
                        hpsPenalty = input$hpsPenalty
                    ),
                    # we don't want analyze to waste time on pathfinding
                    # b/c we do it separately in obs_pitch()
                    interpolWin = 0,
                    pathfinding = 'none',
                    snakeStep = 0,
                    snakePlot = FALSE,
                    smooth = 0,
                    summary = 'extended',
                    plot = FALSE
                )
                myPars$summary = temp_anal$summary
                myPars$result = temp_anal$result
                myPars$pitchCands = temp_anal$pitchCands
                myPars$spec_from_anal = temp_anal$spectrogram
                myPars$X = as.numeric(colnames(myPars$spec_from_anal))
                # add: update defaults that depend on samplingRate, eg cepSmooth

                # if rerunning analyze() for the same audio, preserve the old manual values
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
            })
        }
    })

    obs_pitch = function() {
        if (length(myPars$pitchCands$freq) > 0) {
            if (myPars$print) print('Looking for pitch contour with obs_pitch()')
            myPars$voicedSegments = soundgen:::findVoicedSegments(
                myPars$pitchCands$freq,
                manualV = myPars$manual$frame,
                manualUnv = myPars$manualUnv,
                shortestSyl = input$shortestSyl,
                shortestPause = input$shortestPause,
                minVoicedCands = input$minVoicedCands,
                pitchMethods = input$pitchMethods,
                step = myPars$step,
                samplingRate = myPars$samplingRate
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
                    myPars$pitch[myseq] = soundgen:::pathfinder(
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
                myPars$smoothing_ww = round(input$smooth * points_per_sec / 10, 0)
                # the larger smooth, the heavier the smoothing (lower tolerance
                # threshold before values are replaced by median over smoothing window).
                # smooth of 1 gives smoothingThres of 4 semitones
                myPars$smoothingThres = 4 / input$smooth
                #print(myPars$pitchCands$source)
                myPars$pitch = soundgen:::medianSmoother(
                    as.data.frame(myPars$pitch),
                    smoothing_ww = myPars$smoothing_ww,
                    smoothingThres = myPars$smoothingThres,
                    inviolable = myPars$manual$frame
                )[, 1]
            }
        }
    }
    observeEvent(
        # par-s that we don't need to use in analyze(), only in obs_pitch()
        # (b/c they do not affect the pitch candidates)
        c(input$shortestSyl, input$shortestPause,
          input$interpolWin, input$interpolTol, input$interpolCert,
          input$pathfinding, input$certWeight, input$smooth),
        obs_pitch()
    )

    ## Clicking events
    observeEvent(input$spectrogram_click, {
        myPars$spectrogram_brush = NULL
        if (length(myPars$pitchCands$freq) > 0 & input$spectro_clickAct == 'addCand') {
            closest_frame = which.min(abs(
                as.numeric(colnames(myPars$pitchCands$freq)) - input$spectrogram_click$x))
            # create a manual pitch estimate for the closest frame with the clicked value
            new_freq = round(input$spectrogram_click$y * 1000, 3)
            # don't accept values beyond [pitchFloor, pitchCeiling]
            if (new_freq >= input$pitchFloor &
                new_freq <= input$pitchCeiling) {
                if (closest_frame %in% myPars$manual$frame) {
                    myPars$manual$freq[myPars$manual$frame == closest_frame] = new_freq
                } else {
                    myPars$manual = rbind(
                        myPars$manual,
                        data.frame(frame = closest_frame, freq = new_freq))
                }
                # just to keep things tidy
                myPars$manual = myPars$manual[order(myPars$manual$frame), ]
                # if this frame was manually flagged as unvoiced, remove this flag
                idx_rem = which(myPars$manualUnv == closest_frame)
                if (length(idx_rem) > 0) myPars$manualUnv = myPars$manualUnv[-idx_rem]
                if(input$automPathUpdate) {
                    obs_pitch()
                } else {
                    myPars$pitch[closest_frame] = new_freq
                }
            }
        } else if (input$spectro_clickAct == 'select') {
            myPars$cursor = input$spectrogram_click$x
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
            if(input$automPathUpdate) {
                obs_pitch()
            } else {
                myPars$pitch[closest_frame] = NA
            }
        }
    })

    ## Buttons for operations with selection
    startPlay = function() {
        if (!is.null(myPars$myAudio)) {
            if (!is.null(myPars$spectrogram_brush)) {
                myPars$play$from = myPars$spectrogram_brush$xmin / 1000
                myPars$play$to = myPars$spectrogram_brush$xmax / 1000
            } else if (!is.null(myPars$currentAnn)) {
                myPars$play$from = myPars$ann$from[myPars$currentAnn] / 1000
                myPars$play$to = myPars$ann$to[myPars$currentAnn] / 1000
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

            # play selection with javascript
            shinyjs::js$playme_js(  # need an external js script for this
                audio_id = 'myAudio',  # defined in UI
                from = myPars$play$from,
                to = myPars$play$to)
            # or play with R:
            # playme(myPars$myAudio_path, from = myPars$play$from, to = myPars$play$to)
        }
    }
    observeEvent(c(input$selection_play), startPlay())  # add , myPars$myAudio for autoplay

    stopPlay = function() {
        # browser()  # for debugging
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

    observeEvent(input$userPressedSmth, {
        button_code = floor(input$userPressedSmth)
        if (button_code == 32) {                      # SPACEBAR (play / stop)
            if (myPars$play$on) stopPlay() else startPlay()
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
        } else if (button_code == 85) {               # U (unvoice)
            unvoiceSel()
        } else if (button_code == 86) {               # V (voice)
            voiceSel()
        } else if (button_code == 82) {               # R (raise by an octave)
            octaveUp()
        } else if (button_code == 76) {               # L (lower by an octave)
            octaveDown()
        } else if (button_code == 80) {               # P (set prior)
            setPrior()
        }
    })

    # UNVOICE
    observeEvent(input$selection_unvoice, {
        unvoiceSel()
    })
    unvoiceSel = function() {
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
    }

    # VOICE
    voiceSel = function() {
        if (myPars$print) print('Voicing selection...')
        if (!is.null(myPars$bp) &
            length(myPars$brush_sel_x) > 0 &
            length(myPars$manualUnv) > 0) {
            idx_rem = which(myPars$manualUnv %in% myPars$brush_sel_x)
            if (length(idx_rem) > 0) myPars$manualUnv = myPars$manualUnv[-idx_rem]
            obs_pitch()
        }
    }
    observeEvent(input$selection_voice, {
        voiceSel()
    })

    # SHIFT BY AN OCTAVE
    octaveUp = function() {
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
            if (input$automPathUpdate) {
                obs_pitch()
            } else {
                myPars$pitch[myPars$brush_sel_xy] = myPars$pitch[myPars$brush_sel_xy] * 2
            }
        }
    }
    observeEvent(input$selection_octaveUp, {
        octaveUp()
    })

    octaveDown = function() {
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
            if (input$automPathUpdate) {
                obs_pitch()
            } else {
                myPars$pitch[myPars$brush_sel_xy] = myPars$pitch[myPars$brush_sel_xy] / 2
            }
        }
    }
    observeEvent(input$selection_octaveDown, {
        octaveDown()
    })

    # PRIOR
    observe({
        myPars$prior = getPrior(
            priorMean = input$priorMean,
            priorSD = input$priorSD,
            pitchFloor = input$pitchFloor,
            pitchCeiling = input$pitchCeiling,
            len = 100,
            plot = FALSE
        )
    })
    setPrior = function() {
        if (myPars$print) print('Setting prior...')
        if (!is.null(input$spectrogram_brush)) {
            pr = c(input$spectrogram_brush$ymin, input$spectrogram_brush$ymax) * 1000
            # don't push down below pitchFloor
            pr[pr < input$pitchFloor] = input$pitchFloor
            # but update pitchCeiling if prior is higher
            if (any(pr > input$pitchCeiling)) {
                updateSliderInput(session, 'pitchCeiling', value = round(max(pr)))
            }
            meanPr = round(mean(pr))
            sdPr = round((HzToSemitones(pr[2]) - HzToSemitones(mean(pr))) / 2, 1)
            updateSliderInput(session, 'priorMean', value = meanPr)
            updateSliderInput(session, 'priorSD', value = sdPr)
        }
    }
    observeEvent(input$selection_setPrior, {
        setPrior()
    })

    observeEvent(input$button_pathUpdate, {
        obs_pitch()
    })

    # HOVER
    observeEvent(input$spectrogram_hover, {
        if (!is.null(input$spectrogram_hover) & !is.null(myPars$spec)) {
            myPars$spectrogram_hover = input$spectrogram_hover
            cursor_hz = myPars$spectrogram_hover$y * 1000
            cursor_notes = soundgen::notesDict$note[round(HzToSemitones(cursor_hz)) + 1]
            myPars$spectrogram_hover$freq = paste0(
                round(myPars$spectrogram_hover$y * 1000), ' Hz (',
                cursor_notes, ')')
            myPars$spectrogram_hover$time = paste0(
                round(myPars$spectrogram_hover$x), ' ms'
            )
        }
    })
    shinyjs::onevent('mouseout', id = 'specOver', {
        # NB: more flexible and less mafan than juggling with the observer of
        # input$spectrogram_hover
        myPars$spectrogram_hover = NULL
        shinyjs::js$clearBrush(s = '_brush')
    } )

    # BRUSH
    brush = observeEvent(input$spectrogram_brush, {
        if (myPars$print) print('Running brush()...')
        if (!is.null(input$spectrogram_brush)) {
            myPars$spectrogram_brush = input$spectrogram_brush
        }
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

    changeZoom = function(coef, toCursor = FALSE) {
        if (myPars$print) print('Zooming')
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
        moveSlider()
    }
    observeEvent(input$zoomIn, changeZoom(myPars$zoomFactor, toCursor = TRUE))
    observeEvent(input$zoomOut, changeZoom(1 / myPars$zoomFactor))
    zoomToSel = function() {
        if (!is.null(myPars$spectrogram_brush)) {
            myPars$spec_xlim = round(c(myPars$spectrogram_brush$xmin,
                                       myPars$spectrogram_brush$xmax))
            moveSlider()
        }
    }
    observeEvent(input$zoomToSel, {
        zoomToSel()
    })

    shiftFrame = function(direction) {
        if (myPars$print) print('Shifting frame')
        ran = diff(myPars$spec_xlim)
        if (direction == 'left') {
            newLeft = max(0, myPars$spec_xlim[1] - ran)
            newRight = newLeft + ran
        } else if (direction == 'right') {
            newRight = min(myPars$dur, myPars$spec_xlim[2] + ran)
            newLeft = newRight - ran
        }
        myPars$spec_xlim = c(newLeft, newRight)
        # update cursor when shifting frame, but not when zooming
        myPars$cursor = myPars$spec_xlim[1]
        moveSlider()
    }
    observeEvent(input$scrollLeft, shiftFrame('left'))
    observeEvent(input$scrollRight, shiftFrame('right'))

    moveSlider = function() {
        if (myPars$print) print('Moving slider')
        width = round(diff(myPars$spec_xlim) / myPars$dur * 100, 2)
        left = round(myPars$spec_xlim[1] / myPars$dur * 100, 2)
        shinyjs::js$scrollBar(  # need an external js script for this
            id = 'scrollBar',  # defined in UI
            width = paste0(width, '%'),
            left = paste0(left, '%'))
    }

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
            shiftFrame('left')
        } else if (direction == 'r') {
            shiftFrame('right')
        }
    }, ignoreNULL = TRUE)


    # SAVE OUTPUT
    done = function() {
        # meaning we have finished editing pitch contour for a sound - prepares
        # the output
        if (myPars$print) print('Running done()...')
        if (!is.null(myPars$myAudio_path) &&
            !is.null(myPars$result) && !
            is.null(myPars$pitch)) {
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

    observeEvent(input$fileList, {
        done()
        myPars$n = which(myPars$fileList$name == input$fileList)
        reset()
        readAudio(myPars$n)
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
        showNotification(
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
    shinyBS::addTooltip(session, id='spec_maxPoints', title = 'The number of points to plot in the spectrogram (smaller = faster, but low resolution)', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='spec_cex', title = "Magnification coefficient controlling the size of points showing pitch candidates", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specContrast', title = 'Regulates the contrast of the spectrogram', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specBrightness', title = 'Regulates the brightness of the spectrogram', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))

    # oscillogram
    shinyBS::addTooltip(session, id='osc', title = 'The type of oscillogram to show', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='osc_height', title = 'The height of oscillogram, pixels', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='osc_maxPoints', title = 'The number of points to plot in the oscillogram (smaller = faster, but low resolution)', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))

    # action buttons
    shinyBS:::addTooltip(session, id='lastFile', title='Save and return to the previous file (BACKSPACE)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS:::addTooltip(session, id='nextFile', title='Save and proceed to the next file (ENTER)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS:::addTooltip(session, id='selection_play', title='Play selection (SPACEBAR)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='selection_unvoice', title = 'Treat selection as unvoiced (U)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='selection_voice', title = 'Undo treating selection as unvoiced (V)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='selection_octaveUp', title = 'Raise pitch for selection by an octave (R)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='selection_octaveDown', title = 'Lower pitch for selection by an octave (L)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='selection_setPrior', title = 'Set a prior on expected pitch values corresponding to the selected frequency range (P)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='button_pathUpdate', title = 'Update the path through pitch candidates (only needed if Out/Path/Update path automatically is turned off)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
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
