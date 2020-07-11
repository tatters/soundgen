# formant_app()
#
# To do: finalize layout; load audio upon session start; maybe arbitrary number of annotation tiers; feed selected part of spectrogram instead of raw audio from myPars$selection to get smooth spectrum; load annotations from output.csv in the same folder;

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
    myPars$drawSpec = TRUE
    myPars$shinyTip_show = 1000      # delay until showing a tip (ms)
    myPars$shinyTip_hide = 0         # delay until hiding a tip (ms)
    myPars$initDur = 1000         # initial duration to analyze (ms)
    myPars$out_fTracks = list()      # a list for storing formant tracks per file
    myPars$out_spects = list()       # a list for storing spectrograms
    myPars$selectedF = 'f1'          # preselect F1 for correction
    myPars$slider_ms = 50            # how often to update play slider
    myPars$cursor = 0
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
        myPars$out = read.csv('www/temp.csv')
        removeModal()
    })

    files = list.files('www/', pattern = '.wav')
    files = files[files != 'temp.wav']
    for (f in files){
        file.remove(paste0('www/', f))
    }

    reset = function() {
        myPars$ann = NULL         # a dataframe of annotations for the current file
        myPars$currentAnn = NULL  # the idx of currently selected annotation
        myPars$bp = NULL          # selected points (under brush)
        myPars$spec = NULL
        myPars$formantTracks = NULL
        myPars$formants = NULL
        myPars$analyzedUpTo = NULL
        myPars$selection = NULL
        myPars$cursor = 0
    }

    resetSliders = function() {
        sliders_to_reset = names(input)[which(names(input) %in% rownames(def_form))]
        for (v in sliders_to_reset) {
            new_value = def_form[v, 'default']
            try(updateSliderInput(session, v, value = new_value))
            try(updateNumericInput(session, v, value = new_value))
            updateSelectInput(session, 'wn', selected = 'gaussian')
            updateSliderInput(session, 'spec_ylim', value=c(0, def_form['spec_ylim','default']))
            updateRadioButtons(session, 'spec_colorTheme', selected='bw')
            updateSelectInput(session, 'osc', selected = 'linear')
            updateTextInput(session, 'coeffs', value = '')
        }
    }
    observeEvent(input$reset_to_def, resetSliders())

    loadAudio = function() {
        if (myPars$print) print('Loading audio...')
        done()  # save previous work, if any
        myPars$n = 1   # file number in queue
        myPars$nFiles = nrow(input$loadAudio)  # number of uploaded files in queue
        myPars$fileList = paste(input$loadAudio$name, collapse = ', ')
        myPars$drawSpec = FALSE  # hold on plotting the spectrogram until after running analyze()
        reset()
        readAudio(1)  # read the first sound in queue
    }
    observeEvent(input$loadAudio, loadAudio())

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
        if (input$normalizeInput) {
            myPars$myAudio = myPars$myAudio / max(abs(myPars$myAudio)) * myPars$maxAmpl
        }
        updateSliderInput(session, 'spec_ylim', max = myPars$samplingRate / 2 / 1000)  # check!!!
        myPars$dur = round(length(myPars$temp_audio@left) / myPars$temp_audio@samp.rate * 1000)
        myPars$time = seq(1, myPars$dur, length.out = myPars$ls)
        myPars$spec_xlim = c(0, min(myPars$initDur, myPars$dur))
        myPars$regionToAnalyze = myPars$spec_xlim

        # update info - file number ... out of ...
        file_lab = paste0('File ', myPars$n, ' of ', myPars$nFiles) # , ': ', myPars$myAudio_filename)
        output$fileN = renderUI(HTML(file_lab))

        # if we've already worked with this file in current session,
        # re-load the annotations and formant tracks
        idx = which(myPars$out$file == myPars$myAudio_filename)
        if (length(idx) > 0) myPars$ann = myPars$out[idx, ]
        if (!is.null(myPars$out_fTracks[[myPars$myAudio_filename]])) {
            myPars$formantTracks = myPars$out_fTracks[[myPars$myAudio_filename]]
            myPars$spec = myPars$out_specs[[myPars$myAudio_filename]]
            myPars$drawSpec = TRUE  # don't need to wait for analyze() to run
        }
        drawAnn()
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
                       style="display: none; transform: scale(0.75); transform-origin: 0 0;")
        )
    })


    ## OUTPUTs
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
            ))
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
    updateF = observeEvent(input$nFormants, {
        lapply(1:input$nFormants, function(f) {
            fn = paste0('f', f, '_text')
            observeEvent(input[[fn]], {
                v = suppressWarnings(as.numeric(input[[fn]]))
                if (is.na(v)) {
                    myPars$ann[myPars$currentAnn, paste0('f', f)] = myPars$formants[myPars$currentAnn]
                    updateTextInput(
                        session, fn,
                        value = as.character(myPars$formants[myPars$currentAnn]))
                } else {
                    myPars$ann[myPars$currentAnn, paste0('f', f)] = v
                }
            })
            # add onclick event with shinyjs to select the formant to edit
            this_div = paste0('fDiv_', f)
            other_divs = paste0('fDiv_', (1:input$nFormants)[-f])
            shinyjs::onclick(id = this_div, {
                myPars$selectedF = paste0('f', f)
                shinyjs::addCssClass(id = this_div, class = 'selected')
                for (d in other_divs) {
                    shinyjs::removeCssClass(id = d, class = 'selected')
                }
            })
        })
    })

    # Updating spec / osc stuff to speed up plotting
    observeEvent(myPars$myAudio, {
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
            par(mar = c(ifelse(input$osc == 'none', 2, 0.2), 2, 0.5, 2))  # no need to save user's graphical par-s - revert to orig on exit
            if (is.null(myPars$spec)) {
                plot(1:10, type = 'n', bty = 'n', axes = FALSE, xlab = '', ylab = '')
                text(x = 5, y = 5,
                     labels = 'Upload wav/mp3 file(s) to begin...')
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
            par(mar = c(ifelse(input$osc == 'none', 2, 0.2), 2, 0.5, 2), bg = NA)
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

            # Add a rectangle showing the current annotation
            if (!is.null(myPars$currentAnn)) {
                rect(
                    xleft = myPars$ann$from[myPars$currentAnn],
                    xright = myPars$ann$to[myPars$currentAnn],
                    ybottom = input$spec_ylim[1],
                    ytop = input$spec_ylim[2],
                    col = rgb(.2, .2, .2, alpha = .25),
                    border = NA
                )
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
                for (f in 2:ncol(myPars$formantTracks)) {
                    points(myPars$formantTracks$time,
                           myPars$formantTracks[, f] / 1000,
                           col = 'red', pch = 16)
                }
            }
        }
    })

    output$specSlider = renderPlot({
        if (!is.null(myPars$spec)) {
            par(mar = c(ifelse(input$osc == 'none', 2, 0.2), 2, 0.5, 2), bg = NA)
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

    drawAnn = function() {
        output$ann_plot = renderPlot({
            isolate({
                if (!is.null(myPars$ann)) {
                    if (nrow(myPars$ann) > 0) {
                        if (myPars$print) print('Drawing annotations...')
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
                        par(mar = c(ifelse(input$osc == 'none', 2, 0.2), 2, 0.5, 2))
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

    output$spectrum = renderPlot({
        if (!is.null(myPars$spectrum)) {
            if (myPars$print) print('Drawing spectrum...')
            par(mar = c(2, 2, 0.5, 0))
            plot(myPars$spectrum$freq,
                 myPars$spectrum$ampl,
                 xlab = '', ylab = '',
                 xlim = c(0, myPars$spectrum$freq_range[2]),
                 ylim = myPars$spectrum$ampl_range,
                 xaxs = "i",
                 type = 'l')
            # # fill in the AUC if needed
            # polygon(c(myPars$spectrum$freq, xlim[2], xlim[1]),
            #         c(myPars$spectrum$ampl, ylim[1], ylim[1]),
            #         border = NA,
            #         col = rgb(.5, .5, .5, .15))
            spectrum_peaks()

            # ? in the upper right corner for help
            text(x = myPars$spectrum$freq_range[2] - .05 * diff(myPars$spectrum$freq_range),
                 y = myPars$spectrum$ampl_range[2] - .05 * diff(myPars$spectrum$ampl_range),
                 labels = "?", cex = 5,
                 col = rgb(.5, .5, .5, .1))

            # add a vertical line and freq label on hover
            if (!is.null(myPars$spectrum_hover)) {
                abline(v = myPars$spectrum_hover$x, lty = 2)
                text(x = myPars$spectrum_hover$x,
                     y = myPars$spectrum$ampl_range[1],
                     labels = myPars$spectrum_hover$cursor,
                     adj = c(0, 0))
                text(x = myPars$spectrum_hover$pf,
                     y = myPars$spectrum_hover$pa,
                     labels = myPars$spectrum_hover$peak,
                     adj = c(0, 0))
                points(x = myPars$spectrum_hover$pf,
                       y = myPars$spectrum_hover$pa, pch = 16)
            }
        }
    })

    observe({
        if (!is.null(myPars$spec_trimmed)) {
            if (myPars$print) print('Extracting spectrum of selection...')
            if (!is.null(myPars$selection)) {
                # take the spectrum of selection (annotated region) from raw audio
                myPars$spectrum = as.list(getSmoothSpectrum(
                    myPars$selection,
                    samplingRate = myPars$samplingRate,
                    len = input$spectrum_len,
                    loessSpan = 10 ^ input$spectrum_smooth
                ))
                # myPars$spectrum = list(
                #     freq = as.numeric(rownames(myPars$spec)),
                #     ampl = rowMeans(myPars$spec_trimmed)
                # )
            } else {
                # just use the visible part of the spectrogram
                myPars$spectrum = list(
                    freq = as.numeric(rownames(myPars$spec_trimmed)),
                    ampl = as.numeric(rowMeans(myPars$spec_trimmed))
                )
            }

            isolate({
                myPars$spectrum$freq_range = range(myPars$spectrum$freq)
                myPars$spectrum$ampl_range = range(myPars$spectrum$ampl)
            })
        }
    })

    observeEvent(myPars$currentAnn, {
        if (!is.null(myPars$currentAnn)) {
            if (myPars$print) print('Updating selection...')
            sel_points = as.numeric(round(myPars$ann[myPars$currentAnn, c('from', 'to')] /
                                              1000 * myPars$samplingRate))
            idx_points = sel_points[1]:sel_points[2]
            myPars$selection = myPars$myAudio[idx_points]
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

    # Analysis
    # observe({
    #     myPars$step = input$windowLength * (1 - input$overlap / 100)
    # })
    # observe({
    #     myPars$step_lpc = input$windowLength_lpc * (1 - input$overlap_lpc / 100)
    # })

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
                overlap = input$overlap_lpc,
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
                nFormants = input$nFormants,
                summary = FALSE,
                plot = FALSE
            )
            myPars$temp_anal = myPars$temp_anal[, c('time', myPars$f_col_names)]
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
                    myPars$formantTracks = rbind(
                        myPars$formantTracks[, c('time', myPars$f_col_names)],
                        myPars$temp_anal
                    )
                    myPars$formantTracks = myPars$formantTracks[order(myPars$formantTracks$time), ]
                }
            })
        }
    })

    # if any of LPC settings change, we re-analyze the entire file
    observeEvent(
        c(input$nFormants, input$silence, input$coeffs, input$minformant,
          input$maxbw, input$windowLength_lpc, input$overlap_lpc,
          input$wn_lpc, input$zp_lpc, input$dynamicRange_lpc), {
              myPars$analyzedUpTo = 0
          },
        ignoreInit = TRUE
    )

    observe({
        # analyze annotated selection
        if (!is.null(myPars$currentAnn) & !is.null(myPars$formantTracks)) {
            if (myPars$print) print('Averaging formants in selection...')
            isolate({
                # don't want dependency on myPars$ann
                idx = which(myPars$formantTracks$time >= myPars$ann$from[myPars$currentAnn] &
                                myPars$formantTracks$time <= myPars$ann$to[myPars$currentAnn])
            })
            myPars$formants = round(colMeans(myPars$formantTracks[idx, 2:ncol(myPars$formantTracks)], na.rm = TRUE))
            # myPars$bandwidth ?

            # fill in the formant boxes
            for (f in 1:input$nFormants) {
                updateTextInput(session, inputId = paste0('f', f, '_text'),
                                value = as.character(myPars$formants[f]))
            }
        }
    })

    observe({
        output$ann_table = renderTable(myPars$ann[, -1], digits = 0)
    })

    observeEvent(input$nFormants, {
        myPars$ff = paste0('f', 1:input$nFormants)
        myPars$f_col_names = paste0(myPars$ff, '_freq')
        # keep track of the maximum number of formant ever analyzed
        # to make sure the output table has enough columns
        myPars$maxF = max(myPars$maxF, input$nFormants)
        if (!is.null(myPars$formantTracks)) {
            missingCols = myPars$f_col_names[which(!myPars$f_col_names %in% colnames(myPars$formantTracks))]
            if (length(missingCols > 0)) myPars$formantTracks[, missingCols] = NA
        }
        if (!is.null(myPars$ann)) {
            missingCols = myPars$ff[which(!myPars$ff %in% colnames(myPars$ann))]
            myPars$ann[, missingCols] = NA
        }
    })


    ## Clicking events
    observeEvent(input$spectrogram_click, {
        myPars$spectrogram_brush = NULL
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
            }
        }
    })

    observeEvent(input$spectrogram_dblclick, {
        if (!is.null(myPars$spectrogram_brush)) {
            # create a new annotation
            new = data.frame(
                # idx = ifelse(is.null(myPars$ann), 1, nrow(myPars$ann) + 1),
                file = myPars$myAudio_filename,
                from = round(myPars$spectrogram_brush$xmin),
                to = round(myPars$spectrogram_brush$xmax),
                label = '',
                stringsAsFactors = FALSE)
            new[, paste0('f', 1:input$nFormants)] = NA

            # depending on the history of changing input$nFormants,
            # there may be more formants in myPars$ann than in the current sel
            if (input$nFormants < myPars$maxF) {
                new[, paste0('f', ((input$nFormants + 1):myPars$maxF))] = NA
            }

            # append to myPars$ann
            if (is.null(myPars$ann)) {
                myPars$ann = new
            } else {
                myPars$ann = rbind(myPars$ann, new)
            }

            # select the newly added annotation
            myPars$currentAnn = nrow(myPars$ann)

            # clear the selection
            session$resetBrush("spectrogram_brush")
            showModal(dataModal_new())
        }
    })

    observeEvent(input$spectrum_click, {
        # update both myPars$ann and the corresponding formant button
        if (!is.null(myPars$currentAnn)) {
            myPars$ann[myPars$currentAnn, myPars$selectedF] = round(input$spectrum_click$x * 1000)
            updateTextInput(
                session, paste0(myPars$selectedF, '_text'),
                value = as.character(myPars$ann[myPars$currentAnn, myPars$selectedF]))
        }
    })

    observeEvent(input$ann_click, {
        # select the annotation whose middle (label) is closest to the click
        if (!is.null(myPars$ann)) {
            ds = abs(input$ann_click$x - (myPars$ann$from + myPars$ann$to) / 2)
            myPars$currentAnn = which.min(ds)
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
        myPars$ann$label[myPars$currentAnn] = input$annotation
        myPars$ann[myPars$currentAnn, myPars$ff] = myPars$formants[myPars$f_col_names]
        ord = order(myPars$ann$from)
        myPars$ann = myPars$ann[ord, ]
        myPars$currentAnn = which(ord == nrow(myPars$ann))
        removeModal()
        drawAnn()
    })

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
    })

    ## Buttons for operations with selection
    playSel = function() {
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
            myPars$play$timeOn = Sys.time()
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
    observeEvent(c(input$selection_play), playSel())  # add , myPars$myAudio for autoplay
    observeEvent(input$selection_stop, {
        myPars$play$on = FALSE
        myPars$cursor = myPars$cursor_temp
        shinyjs::js$stopAudio_js(audio_id = 'myAudio')
    })

    observe({
        if (!is.null(myPars$play$on) && myPars$play$on) {
            time = Sys.time()
            if (!is.null(myPars$slider_ms)) invalidateLater(myPars$slider_ms)
            if (time > myPars$play$timeOff) {
                myPars$play$on = FALSE
                myPars$cursor = myPars$cursor_temp  # reset to original cursor
            } else {
                myPars$cursor = as.numeric(
                    myPars$play$from + time - myPars$play$timeOn
                ) * 1000
            }
        }
    })

    deleteSel = function() {
        if (!is.null(myPars$currentAnn)) {
            myPars$ann = myPars$ann[-myPars$currentAnn, ]
            myPars$selection = NULL
            myPars$currentAnn = NULL
        }
    }
    observeEvent(input$selection_delete, deleteSel())

    observeEvent(input$userPressedSmth, {
        button_code = floor(input$userPressedSmth)
        if (button_code == 32) {                      # SPACEBAR (play)
            playSel()
        } else if (button_code == 46) {               # DELETE (delete current annotation)
            deleteSel()
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
    } )

    # HOVER: SPECTRUM
    observeEvent(input$spectrum_hover, {
        if (!is.null(myPars$spectrum) & !is.null(input$spectrum_hover)) {
            myPars$spectrum_hover = data.frame(x = input$spectrum_hover$x,
                                               y = input$spectrum_hover$y)

            help = myPars$spectrum_hover$x > .9 * myPars$spectrum$freq_range[2] &
                myPars$spectrum_hover$y > (myPars$spectrum$ampl_range[1] +
                                               .9 * diff(myPars$spectrum$ampl_range))
            if (help) {
                showNotification(
                    ui = paste('Click to update the selected formant in current',
                               'annotation (uses cursor position, not closest peak)'),
                    duration = 10,
                    closeButton = TRUE,
                    type = 'default'
                )
                # showModal(modalDialog(
                #     title = paste('Click to update the selected formant in current',
                #                'annotation (uses cursor position, not closest peak)'),
                #     size = 's',
                #     easyClose = TRUE
                # ))
            }

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


    # BRUSH
    brush = observeEvent(input$spectrogram_brush, {
        if (!is.null(input$spectrogram_brush)) {
            myPars$spectrogram_brush = input$spectrogram_brush
        }
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
        if (!is.null(myPars$spectrogram_brush)) {
            myPars$spec_xlim = round(c(myPars$spectrogram_brush$xmin, myPars$spectrogram_brush$xmax))
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
        # meaning we are done with a sound - prepares the output
        if (myPars$print) print('Running done()...')
        session$resetBrush("spectrogram_brush")  # doesn't reset automatically
        if (!is.null(myPars$myAudio_path)) {
            if (is.null(myPars$out)) {
                myPars$out = myPars$ann
            } else {
                # remove previous records for this file, if any
                idx = which(myPars$out$file == myPars$ann$file[1])
                if (length(idx) > 1)
                    myPars$out = myPars$out[-idx, ]

                # make sure out and ann have the same formant columns
                # (in case nFormants changed)
                mco = which(!colnames(myPars$ann) %in% colnames(myPars$out))
                if (length(mco > 0))
                    myPars$out[, colnames(myPars$ann)[mco]] = NA
                mca = which(!colnames(myPars$out) %in% colnames(myPars$ann))
                if (length(mca > 0))
                    myPars$out[, colnames(myPars$out)[mca]] = NA

                # append annotations from the current audio
                myPars$out = rbind(myPars$out, myPars$ann)
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
            ui = paste0("App for measuring formants in small annotated segments: soundgen ", packageVersion('soundgen'), ". Select an area of spectrogram and double-click to add an annotation, left-click to correct a formant measure. More info: ?formant_app and http://cogsci.se/soundgen.html"),
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

    # trackers

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
    shinyBS:::addTooltip(session, id='selection_delete', title='Remove annotation (DELETE)', placement="right", trigger="hover", options = list(delay = list(show=1000, hide=0)))
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
