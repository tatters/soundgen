# # tip: to read the output, do smth like:
# a = read.csv('~/Downloads/output.csv', stringsAsFactors = FALSE)
# as.numeric(unlist(strsplit(a$pitch, ',')))

# shinyBS needs to be included as a dependency (instead of just "import"):
# see https://stackoverflow.com/questions/52649138/including-shinybs-in-a-package

server = function(input, output, session) {
    myPars = reactiveValues()
    myPars$zoomFactor = 2     # zoom buttons change time zoom by this factor
    myPars$zoomFactor_freq = 1.5  # same for frequency
    myPars$print = FALSE       # if TRUE, some functions print a meassage to the console when called
    myPars$out = NULL         # for storing the output
    myPars$drawSpec = TRUE
    myPars$shinyTip_show = 1000      # delay until showing a tip (ms)
    myPars$shinyTip_hide = 0         # delay until hiding a tip (ms)
    myPars$myAudio = NULL

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
        myPars$samplingRate = myPars$temp_audio@samp.rate
        updateSliderInput(session, 'spec_ylim', max = myPars$samplingRate / 2 / 1000)  # check!!!
        myPars$dur = round(length(myPars$temp_audio@left) / myPars$temp_audio@samp.rate * 1000)
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

    output$spectrogram = renderPlot({
        if (myPars$drawSpec == TRUE) {
            if (myPars$print) print('Drawing spectrogram...')
            par(mar = c(ifelse(input$osc == 'none', 2, 0.2), 2, 0.5, 2))  # no need to save user's graphical par-s - revert to orig on exit
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
                soundgen:::filled.contour.mod(
                    x = as.numeric(colnames(myPars$spec)),
                    y = as.numeric(rownames(myPars$spec)),
                    z = t(myPars$spec),
                    levels = seq(0, 1, length = 30),
                    color.palette = color.palette,
                    xlim = myPars$spec_xlim,
                    xaxt = 'n',
                    xaxs = 'i', xlab = '',
                    ylab = 'Frequency, kHz',
                    main = '',
                    ylim = c(input$spec_ylim[1], input$spec_ylim[2])
                )
                if (input$osc == 'none') {
                    axis(side = 1)
                    title(xlab = 'Time, ms')
                }

                # show prior
                points(myPars$prior$prob * myPars$dur * .05,  # 5% of plot width
                       myPars$prior$freq / 1000, type = 'l', lty = 2)
                text(x = myPars$dur * .05,
                     y = input$priorMean / 1000,
                     pos = 2, labels = 'Prior')

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
                    candPlot = list(cex = input$spec_cex)
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
        output$oscillogram = renderPlot({
            if (!is.null(myPars$myAudio_path) & input$osc != 'none') {
                par(mar = c(2, 2, 0, 2))
                maxAmpl = 2 ^ (myPars$temp_audio@bit - 1) # max(abs(myPars$myAudio))
                # to speed up plotting the osc of very long files
                # convert osc_res [0,1] to smth that varies from 1000'ish to length(myPars$myAudio)
                l = length(myPars$myAudio)
                log_len_range = log(c(1000, l))
                maxLen = exp(log_len_range[1] + input$osc_res * diff(log_len_range))
                # demo:
                # osc_res = seq(0, 1, length.out = 100)
                # log_len_range = log(c(1000, length(myPars$myAudio)))
                # maxLen = exp(log_len_range[1] + osc_res * diff(log_len_range))
                # plot(osc_res, maxLen, type = 'p')
                idx = seq(1, l, length.out = min(l, maxLen))
                if (input$osc == 'dB') {
                    sound = osc_dB(myPars$myAudio,
                                   dynamicRange = input$dynamicRange,
                                   maxAmpl = maxAmpl,
                                   plot = FALSE,
                                   returnWave = TRUE)[idx]
                    ylim_osc = c(-2 * input$dynamicRange, 0)
                } else {
                    sound = myPars$myAudio[idx]
                    ylim_osc = c(-maxAmpl, maxAmpl)
                }
                plot(idx / l * myPars$dur,
                     sound,
                     type = 'l',
                     xlim = myPars$spec_xlim,
                     ylim = ylim_osc,
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
        if (input$summaryFun_text != '') {
            myPars$summaryFun = input$summaryFun_text
        } else {
            myPars$summaryFun = input$summaryFun
        }
        # print(myPars$summaryFun)
    })

    obs_anal = observe({
        # analyze the file (executes every time a slider with arg value is changed)
        if (!is.null(input$loadAudio$datapath)) {
            if (myPars$print) print('Calling analyze()...')
            withProgress(message = 'Analyzing the sound...', value = 0.5, {
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
                windowLength_points = floor(input$windowLength / 1000 * myPars$samplingRate / 2) * 2
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
        if (myPars$print) print('Looking for pitch contour with obs_pitch()')
        if (length(myPars$pitchCands$freq) > 0) {
            myPars$voicedSegments = soundgen:::findVoicedSegments(
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
                smoothing_ww = round(input$smooth * points_per_sec / 10, 0)
                # the larger smooth, the heavier the smoothing (lower tolerance
                # threshold before values are replaced by median over smoothing window).
                # smooth of 1 gives smoothingThres of 4 semitones
                smoothingThres = 4 / input$smooth
                #print(myPars$pitchCands$source)
                myPars$pitch = soundgen:::medianSmoother(
                    as.data.frame(myPars$pitch),
                    smoothing_ww = smoothing_ww,
                    smoothingThres = smoothingThres,
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
            if(input$automPathUpdate) {
                obs_pitch()
            } else {
                myPars$pitch[closest_frame] = new_freq
            }
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
        # meaning we have finished editing pitch contour for a sound - prepares the output
        if (myPars$print) print('Running done()...')
        session$resetBrush("spectrogram_brush")  # doesn't reset automatically for some reason
        if (!is.null(myPars$myAudio_path)) {
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
                    harmPerSel = defaults_analyze['harmPerSel', 'default'])
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
        write.csv(myPars$out, 'www/temp.csv', row.names = FALSE)

        # add manual corrections to the history list
        myPars$history[[myPars$myAudio_filename]]$manual = myPars$manual
        myPars$history[[myPars$myAudio_filename]]$manualUnv = myPars$manualUnv
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
    shinyBS::addTooltip(session, id='domThres', title = 'Minimum amplitude of dominant frequency', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='domSmooth', title = 'Width of smoothing interval for finding the lowest dominant frequency band', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='autocorThres', title = 'Voicing threshold for autocorrelation algorithm', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='autocorSmooth', title = 'Width of smoothing interval (in bins) for finding peaks in the autocorrelation function', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='cepThres', title = 'Voicing threshold for cepstral algorithm', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='cepSmooth', title = 'Width of smoothing interval for finding peaks in the cepstrum', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='cepZp', title = 'Length of cepstral window after zero padding: 8 means 2^8 = 256, etc.', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specThres', title = 'Voicing threshold for Ba-Na algorithm', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specPeak', title = 'Minimum amplitude of harmonics considered pitch candidates', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specHNRslope', title = '0 = same threshold regardless of HNR; positive = lower threshold in noisy sounds', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specSmooth', title = 'Width of window for detecting harmonics in the spectrum, Hz', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specMerge', title = 'Pitch candidates within specMerge semitones are merged with boosted certainty', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specSinglePeakCert', title = 'If pitch is calculated based on a single harmonic ratio (as opposed to several ratios converging on the same candidate), its certainty is taken to be specSinglePeakCert', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))

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
    shinyBS::addTooltip(session, id='spec_cex', title = "Magnification coefficient controlling the size of points showing pitch candidates", placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specContrast', title = 'Regulates the contrast of the spectrogram', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='specBrightness', title = 'Regulates the brightness of the spectrogram', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))

    # oscillogram
    shinyBS::addTooltip(session, id='osc', title = 'The type of oscillogram to show', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='osc_height', title = 'The height of oscillogram, pixels', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))
    shinyBS::addTooltip(session, id='osc_res', title = '0 = very low (fast), 1 = maximum (slow)', placement="below", trigger="hover", options = list(delay = list(show=1000, hide=0)))

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
