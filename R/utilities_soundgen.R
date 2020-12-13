### UTILITIES FOR SOUND GENERATION ###

#' Report time
#'
#' Provides a nicely formatted "estimated time left" in loops plus a summary upon completion.
#' @param i current iteration
#' @param nIter total number of iterations
#' @param time_start time when the loop started running
#' @param jobs vector of length \code{nIter} specifying the relative difficulty
#'   of each iteration. If not NULL, estimated time left takes into account
#'   whether the jobs ahead will take more or less time than the jobs already
#'   completed
#' @param reportEvery report progress every n iterations
#' @export
#' @examples
#' time_start = proc.time()
#' for (i in 1:20) {
#'   Sys.sleep(i ^ 2 / 10000)
#'   reportTime(i = i, nIter = 20, time_start = time_start,
#'   jobs = (1:20) ^ 2, reportEvery = 5)
#' }
#' \dontrun{
#' # Unknown number of iterations:
#' time_start = proc.time()
#' for (i in 1:20) {
#'   Sys.sleep(i ^ 2 / 10000)
#'   reportTime(i = i, time_start = time_start,
#'   jobs = (1:20) ^ 2, reportEvery = 5)
#' }
#'
#' # when analyzing a bunch of audio files, their size is a good estimate
#' # of how long each will take to process
#' time_start = proc.time()
#' filenames = list.files('~/Downloads/temp', pattern = "*.wav|.mp3",
#'   full.names = TRUE)
#' filesizes = file.info(filenames)$size
#' for (i in 1:length(filenames)) {
#'   # ...do what you have to do with each file...
#'   reportTime(i = i, nIter = length(filenames),
#'              time_start = time_start, jobs = filesizes)
#' }
#' }
reportTime = function(i,
                      time_start,
                      nIter = NULL,
                      jobs = NULL,
                      reportEvery = 1) {
  time_diff = as.numeric((proc.time() - time_start)[3])
  if (is.null(nIter)) {
    # number of iter unknown, so we just report time elapsed
    if (i %% reportEvery == 0) {
      print(paste0('Completed ', i, ' iterations in ',
                   convert_sec_to_hms(time_diff)))
    }
  } else {
    # we know how many iter, so we also report time left
    if (i == nIter) {
      time_total = convert_sec_to_hms(time_diff)
      print(paste0('Completed ', i, ' iterations in ', time_total, '.'))
    } else {
      if (i %% reportEvery == 0) {
        if (is.null(jobs)) {
          # simply count iterations
          time_left = time_diff / i * (nIter - i)
        } else {
          # take into account the expected time for each iteration
          speed = time_diff / sum(jobs[1:i])
          time_left = speed * sum(jobs[min((i + 1), nIter):nIter])
        }
        time_left_hms = convert_sec_to_hms(time_left)
        print(paste0('Done ', i, ' / ', nIter, '; Estimated time left: ', time_left_hms))
      }
    }
  }
}


#' Print time
#'
#' Internal soundgen function.
#'
#' Converts time in seconds to time in y m d h min s for pretty printing.
#' @param time_s time (s)
#' @param digits number of digits to preserve for s (1-60 s)
#' @return Returns a character string like "1 h 20 min 3 s"
#' @keywords internal
#' @examples
#' time_s = c(.0001, .01, .33, .8, 2.135, 5.4, 12, 250, 3721, 10000,
#'            150000, 365 * 24 * 3600 + 35 * 24 * 3600 + 3721)
#' soundgen:::convert_sec_to_hms(time_s)
#' soundgen:::convert_sec_to_hms(time_s, 1)
convert_sec_to_hms = function(time_s, digits = 0) {
  if (any(time_s > 1)) {
    # years = time_s %/% 31536000
    # years_string = ifelse(years > 0, paste(years, 'y '), '')
    #
    # months = time_s %/% 2592000 - years * 12
    # months_string = ifelse(months > 0, paste(months, 'm '), '')

    days = time_s %/% 86400
    days_string = ifelse(days > 0, paste(days, 'd '), '')

    hours = time_s %/% 3600 - days * 24
    hours_string = ifelse(hours > 0, paste(hours, 'h '), '')

    minutes = time_s %/% 60 - days * 1440 - hours * 60
    minutes_string = ifelse(minutes > 0, paste(minutes, 'min '), '')

    seconds = time_s - days * 86400 - hours * 3600 - minutes * 60
    seconds_floor = floor(seconds)
    idx_s = time_s > 1 & time_s < 60
    seconds_round = ifelse(idx_s, round(seconds, digits = digits), seconds_floor)
    seconds_string = ifelse(seconds_round > 0, paste(seconds_round, 's '), '')

    idx_ms = time_s < 1
    ms_string = ifelse(idx_ms, paste(round((seconds - seconds_floor) * 1000), 'ms'), '')
    ms_string[ms_string == '0 ms'] = '0'

    output = paste0(days_string, hours_string,
                    minutes_string, seconds_string, ms_string)
  } else {
    output = paste(round(time_s * 1000), 'ms')
  }
  output = trimws(output)
  return(output)
}


#' HTML for clickable plots
#'
#' Internal soundgen function
#'
#' Writes an html file for displaying clickable plots in a browser.
#' @param myfolder full path to target folder, without a '/' at the end
#' @param myfiles a list of full names of files (with paths and extensions)
#' @param width default flex-basis, ie approximate default width of each image
#' @keywords internal
#' @examples
#' \dontrun{
#' htmlPlots(myfolder = '~/Downloads/temp',
#'           myfiles = c('~/Downloads/temp/myfile1.wav',
#'                       '~/Downloads/temp/myfile2.wav'))
#' }
htmlPlots = function(myfolder, myfiles, width = "900px") {
  # a list of basenames without extension
  basenames = basename(myfiles)
  basenames_stripped = as.character(sapply(
    basenames,
    function(x) substr(x, 1, nchar(x) - 4)
  ))
  basenames_concat = paste0(basenames, collapse = "', '")

  # create an html file to display nice, clickable spectrograms
  out_html = file(paste0(myfolder,'/00_clickable_plots.html'))
  writeLines(
    c("<!DOCTYPE html>",
      "<html>",
      "<head>",
      "<title>Click to play</title>",
      "<meta charset='UTF-8'>",
      "<meta name='viewport' content='width=device-width, initial-scale=1.0'>",
      "<style>",
      "#flexbox {",
      "  display: flex;",
      "  flex-flow: row wrap;",
      "  justify-content: space-around;",
      "  align-items: stretch;",
      "}",
      "#flexbox > div {",
      paste0("  flex: 1 1 ", width, ";"),
      "  margin: 20px 5px;",
      "  border: 1px gray solid;",
      "  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 15px 0 rgba(0, 0, 0, 0.19);",
      "}",
      "#flexbox > div:hover{",
      "  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.4), 0 6px 15px 0 rgba(0, 0, 0, 0.3);",
      "}",
      "#flexbox img {",
      "  width: 100%;",
      "}",
      "</style>",
      "</head>",
      "<body>",
      "<div id='flexbox'> </div>",
      "<script>",
      paste0("var mylist = ['", basenames_concat, "'];"),
      "var sounds = [];",
      "var flex = document.getElementById('flexbox');",
      "for (var i = 0; i < mylist.length; i++) {",
      "  sounds[i] = mylist[i].slice(0, -4);  // remove '.wav' / '.mp3'",
      "  let newDiv = document.createElement('div');",
      "  newDiv.innerHTML = '<img src=\"' + sounds[i] + '.png\">';",
      "  flex.appendChild(newDiv);",
      "  var mysound = mylist[i];",
      "  newDiv.onclick = (function(mysound) {",
      "    return function() {",
      "      var audioElement = document.createElement('audio');",
      "      audioElement.setAttribute('src', mysound);",
      "      audioElement.play();",
      "    };",
      "  })(mysound);",
      "}",
      "</script>",
      "</body>",
      "</html>",
      "),"),
    out_html)
  close(out_html)
}


#' Find zero crossing
#'
#' Internal soundgen function.
#'
#' \code{findZeroCrossing} looks for the last negative point before a zero
#' crossing as close as possible to the specified location. Since this is
#' primarily intended for joining waveforms without a click, this function only
#' looks at upward segments of a waveform (see example).
#'
#' @param ampl a vector of amplitudes oscillating around zero, such as a sound
#'   waveform
#' @param location the index indicating the desired location of a zero crossing
#' @return Returns the index of the last negative value before zero crossing
#'   closest to specified location.
#' @keywords internal
#' @examples
#' ampl = sin(1:100/2)
#' plot(ampl, type = 'b')
#' lines(1:100, rep(0,100), lty = 2)
#' zc = vector()
#' for (i in 1:length(ampl)){
#'   zc[i] = soundgen:::findZeroCrossing (ampl, i)
#'   # find zc closest to each of 100 points
#' }
#' for (z in unique(zc)){
#'   points(z, ampl[z], col = 'red', pch = 17)
#'   # only on upward segments
#' }
#' zc # see which zc is closest to each point
findZeroCrossing = function(ampl, location) {
  len = length(ampl)
  if (len < 1 | location < 1 | location > len)
    return (NA)
  if (len == 1 & location == 1)
    return(location)
  zc_left = zc_right = NA

  # left of location
  if (location > 1) {
    i = location
    while (i > 1) {
      if (ampl[i] > 0 & ampl[i - 1] < 0) {
        zc_left = i - 1
        break
      }
      i = i - 1
    }
  }

  # right of location
  if (location < len)
    i = location
  while (i < (len - 1)) {
    if (ampl[i + 1] > 0 & ampl[i] < 0) {
      zc_right = i
      break
    }
    i = i + 1
  }

  if (is.na(zc_left) & is.na(zc_right)) return (NA)
  zc_nearest = which.min(c(abs(zc_left - location), abs(zc_right - location)))
  if (zc_nearest == 1) {
    return (zc_left)
  } else if (zc_nearest == 2) {
    return (zc_right)
  } else {
    return (NA) # zc not found
  }
}


#' Upsample pitch contour
#'
#' Internal soundgen function.
#'
#' Upsamples a pitch contour to samplingRate through linear interpolation
#' between successive glottal cycles.
#' @param pitch_per_gc a vector of fundamental frequencies per glottal cycle
#' @param samplingRate target sampling rate after upsampling, in Hz
#' @return Returns a list of two vectors: pitch_upsampled (the upsampled version
#'   of the input) and gc_upsampled (new indices of glottal cycles on an
#'   upsampled scale)
#' @keywords internal
#' @examples
#' soundgen:::upsample(pitch_per_gc = c(100, 150, 130), samplingRate = 16000)
upsample = function(pitch_per_gc, samplingRate = 16000) {
  l = length(pitch_per_gc)
  gccrossLenPoints = round(samplingRate / pitch_per_gc)
  c = cumsum(gccrossLenPoints)
  gc_upsampled = c(1, c)

  if (l == 1) {
    pitch_upsampled = rep(pitch_per_gc, gccrossLenPoints)
  } else if (l == 2) {
    pitch_upsampled = seq(pitch_per_gc[1], pitch_per_gc[2], length.out = sum(gccrossLenPoints))
  } else {
    # find time stamps (in gc) corresponding to centers of each pitch value
    t = rep(1, l)
    t[1] = 1  # start at 1
    t[l] = sum(gccrossLenPoints)  # end at total number of gc
    for (i in 2:(l - 1)) {
      t[i] = c[i - 1] + round(gccrossLenPoints[i] / 2)
    }
    pitch_upsampled = spline(x = t,
                             y = pitch_per_gc,
                             n = tail(c, 1))$y
  }
  # plot(pitch_upsampled, type = 'l')
  return (list(pitch = pitch_upsampled, gc = gc_upsampled))
}


#' Divide f0 contour into glottal cycles
#'
#' Internal soundgen function.
#'
#' Returns a vector of indices giving the borders between "glottal cycles",
#' assuming that we know the true f0 at each time point (as we do in synthesized
#' sounds). The first index is always 1.
#' @param pitch a vector of fundamental frequency values
#' @param samplingRate sampling rate at which f0 values are provided
#' @keywords internal
#' @examples
#' # 100 ms of audio with f0 steadily increasing from 150 to 200 Hz
#' soundgen:::getGlottalCycles(seq(150, 200, length.out = 350),
#'   samplingRate = 3500)
getGlottalCycles = function (pitch, samplingRate) {
  if (length(pitch) < 2) return(1)
  glottalCycles = numeric()
  i = 1 # the first border is the first time point
  while (i < length(pitch)) {
    glottalCycles = c(glottalCycles, i)
    # take steps proportionate to the current F0
    i = i + max(2, floor(samplingRate / pitch[i]))
  }
  return(glottalCycles)
}


#' Syllable structure of a bout
#'
#' Internal soundgen function.
#'
#' Stochastic generation of syllable structure of a bout. Calls
#' \code{\link{rnorm_truncated}} to vary the duration of each new syllable and of
#' pauses between syllables. Total bout duration will also vary, unless
#' temperature is zero. However, the output will always contain exactly
#' \code{nSyl} syllables.
#' @param nSyl the desired number of syllables
#' @param sylLen the desired mean syllable duration, in ms (vectorized)
#' @param pauseLen the desired mean pause between syllables, in ms (vectorized)
#' @param sylDur_min,sylDur_max the lower and upper bounds on possible syllable
#'   duration, in ms
#' @param pauseDur_min,pauseDur_max the lower and upper bounds on possible pause
#'   duration, in ms
#' @param temperature a non-negative float regulating the stochasticity of
#'   syllable segmentation; 0 = no stochasticity; 1 = sd of proposals is equal
#'   to sylLen (very strong stochasticity)
#' @param plot produce a plot of syllable structure?
#' @inheritParams soundgen
#' @return Returns a matrix with a list of start-end points for syllables
#' @keywords internal
#' @examples
#' soundgen:::divideIntoSyllables (nSyl = 1, sylLen = 180)
#' soundgen:::divideIntoSyllables (nSyl = 5, sylLen = 180,
#'   pauseLen = 55, temperature = 0.2, plot = TRUE)
#' soundgen:::divideIntoSyllables (nSyl = 5, sylLen = 180,
#'   pauseLen = 55, temperature = 0)
#' soundgen:::divideIntoSyllables (nSyl = 3, sylLen = 100,
#'   pauseLen = 25, temperature = 0.5)
#'
#' # sylLen and pauseLen are vectorized:
#' soundgen:::divideIntoSyllables (nSyl = 15, sylLen = 100:200,
#'   pauseLen = c(80, 25, 80), temperature = 0.05, plot = TRUE)
divideIntoSyllables = function (nSyl,
                                sylLen,
                                pauseLen,
                                sylDur_min = 20,
                                sylDur_max = 10000,
                                pauseDur_min = 20,
                                pauseDur_max = 1000,
                                temperature = 0.025,
                                invalidArgAction = c('adjust', 'abort', 'ignore')[1],
                                plot = FALSE) {
  if (nSyl == 1) {
    # no variation for a single syllable
    out = data.frame(start = 0, end = sylLen)
  } else {
    # up- or downsample durations to nSyl
    if (length(sylLen) > 1 & length(sylLen) != nSyl) {
      sylLen = getSmoothContour(anchors = sylLen, len = nSyl)
    }
    if (length(pauseLen) > 1 & length(pauseLen) != (nSyl - 1)) {
      pauseLen = getSmoothContour(anchors = pauseLen, len = nSyl - 1)
    }

    # generate random lengths of syllabels and pauses under constraints
    syls = rnorm_truncated(
      n = nSyl,
      mean = sylLen,
      low = sylDur_min,
      high = sylDur_max,
      sd = sylLen * temperature,
      invalidArgAction = invalidArgAction
    )
    pauses = rnorm_truncated(
      n = nSyl - 1,
      mean = pauseLen,
      low = pauseDur_min,
      high = pauseDur_max,
      sd = pauseLen * temperature,
      invalidArgAction = invalidArgAction
    )

    out = data.frame(start = rep(0, nSyl), end = rep(0, nSyl))
    for (i in 1:nSyl) {
      if (i == 1) {
        out$start[i] = 0
      } else {
        out$start[i] = out$end[i - 1] + pauses[i - 1]  # start time of syllable, in ms
      }
      out$end[i] = out$start[i] + syls[i] # end time of syllable, in ms
    }
  }
  out$dur = out$end - out$start

  if (plot) {
    # for the UI
    t = 1:max(out)
    plot(t, rep(1, length(t)), type = 'n', xlab = 'Time, ms', ylab = '',
         bty = 'n', yaxt = 'n', ylim = c(0.8, 1.2))
    for (i in 1:nrow(out)) {
      rect(xleft = out[i, 1], xright = out[i, 2], ybottom = .9, ytop = 1.1,
           col = 'blue')
      text(x = mean(c(out[i, 2], out[i, 1])), y = 1,
           col = 'yellow', cex = 5, labels = i)
    }
  }
  return(out)
}


#' Randomly modify anchors
#'
#' Internal soundgen function.
#'
#' A helper function for introducing random variation into any anchors (for
#' pitch / breathing / amplitude / ...). At higher temperatures can also add or
#' delete an anchor. NB: make sure the lower and upper bounds are reasonable
#' given the scale of df$value!
#' @param df dataframe of anchors, for ex. \code{data.frame(time = c(0, .1, .8,
#'   1), value = c(100, 230, 180, 90))}
#' @param temperature,temp_coef regulate the amount of stochasticity
#'   ("wiggling"). Since \code{temperature} is used in several functions,
#'   \code{temp_coef} gives more flexibility by controlling how much temperature
#'   affects this particular aspect, namely random variation in anchors. These
#'   two are multiplied, so \code{temp_coef} of 0.5 halves the effect of
#'   temperature.
#' @param low,high bounds on possible variation. Both \code{low} and \code{high}
#'   should be vectors of length 2: the first element specifies the boundary for
#'   \code{df$time} and the second for \code{df$value}. Ex.: low = c(0,1) - low
#'   bound on "time"=0, low bound on "value"=1
#' @param wiggleAllRows should the first and last time anchors be wiggled? (TRUE
#'   for breathing, FALSE for other anchors)
#' @param sd_values (optional) the exact value of sd used by rnorm_truncated in
#'   columns 2 and beyond
#' @param roundToInteger if TRUE, rounds the values (not time points)
#' @inheritParams soundgen
#' @return Modified original dataframe.
#' @keywords internal
#' @examples
#' soundgen:::wiggleAnchors(df = data.frame(
#'   time = c(0, .1, .8, 1), value = c(100, 230, 180, 90)),
#'   temperature = .2, temp_coef = .1, low = c(0, 50), high = c(1, 1000),
#'   wiggleAllRows = FALSE) # pitch
#' soundgen:::wiggleAnchors(df = data.frame(time = 0, value = 240),
#'   temperature = .2, temp_coef = .1, low = c(0, 50), high = c(1, 1000),
#'   wiggleAllRows = FALSE) # pitch, single anchor
#' soundgen:::wiggleAnchors(df = data.frame(
#'   time = c(-100, 100, 600, 900), value = c(-120, -80, 0, -120)),
#'   temperature = .4, temp_coef = .5, low = c(-Inf, -120), high = c(+Inf, 30),
#'   wiggleAllRows = TRUE) # noise

#' # formants
#' formants = list(f1 = list(time = 0, freq = 860, amp = 30, width = 120),
#'                 f2 = list(time = c(0,1), freq = 1280,
#'                 amp = c(10,40), width = 120))
#' for (f in 1:length(formants)) {
#'   formants[[f]] = soundgen:::wiggleAnchors(
#'     df = formants[[f]],
#'     temperature = .4, temp_coef = .5,
#'     low = c(0, 50, 0, 1),
#'     high = c(1, 8000, 120, 2000),
#'     wiggleAllRows = FALSE
#'   )
#' }
#' print(formants)
#'
#' # manually provided sd (temp only affects prob of adding/dropping anchors)
#' soundgen:::wiggleAnchors(df = data.frame(
#'   time = c(0, .1, .8, 1), value = c(100, 230, 180, 90)),
#'   wiggleAllRows = FALSE, sd_values = 5)
wiggleAnchors = function(df,
                         temperature = .05,
                         temp_coef = 1,
                         low = c(0, -Inf),
                         high = c(1, Inf),
                         wiggleAllRows = FALSE,
                         sd_values = NULL,
                         roundToInteger = FALSE,
                         invalidArgAction = c('adjust', 'abort', 'ignore')[1]) {
  if (temperature == 0 | temp_coef == 0) return(df)
  if (any(is.na(df))) return(NA)
  if (class(df)[1] != 'data.frame') df = as.data.frame(df)

  if (ncol(df) != length(low) |
      ncol(df) != length(high) |
      length(low) != length(high)) {
    warning('Vectors "low" and "high" should be the same length as ncol(df)')
  }

  # should we add a new anchor or remove one?
  action = sample(c('nothing', 'remove', 'add'),
                  size = 1,
                  prob = c(1 - temperature, temperature / 2, temperature / 2))
  if (action == 'add') {  # add an anchor
    if (nrow(df) == 1) {
      # the first anchor is the original, the second random
      idx = 2:ncol(df)
      newAnchor = try(rnorm_truncated(
        n = ncol(df) - 1,
        mean = as.numeric(df[1, idx]),
        sd = ifelse(is.numeric(sd_values),
                    sd_values,
                    as.numeric(df[1, idx] * temperature * temp_coef)),
        low = low[idx],
        high = high[idx],
        roundToInteger = roundToInteger,
        invalidArgAction = invalidArgAction))
      if (class(newAnchor)[1] == 'try-error') {
        stop(paste('Failed to add an anchor to df:', paste(df, collapse = ', ')))
      } else {
        df = rbind(df, c(1, newAnchor))
        df[1, 1] = 0  # make time c(0, 1)
      }
    } else {
      # insert between any two existing anchors
      a1 = sample(1:nrow(df), size = 1)
      direction = sample(c(-1, 1), size = 1)
      a2 = ifelse(a1 + direction < 1 | a1 + direction > nrow(df),
                  a1 - direction,
                  a1 + direction)
      i1 = min(a1, a2)
      i2 = max(a1, a2)  # insert between rows i1 and i2
      newAnchor = colMeans(df[i1:i2, ])
      df = rbind(df[1:i1, ],
                 newAnchor,
                 df[i2:nrow(df), ])
    }
  } else if (action == 'remove') {
    if (wiggleAllRows) {
      # we can remove any anchor
      idx = sample(1:nrow(df), 1)
      df = df[-idx, ]
    } else {
      # we don't touch the first and last anchors
      if (nrow(df) > 2) {
        # NB: sample() may return 1 if nrow(df) = 2, hence sampleModif()
        idx = sampleModif(x = (2:(nrow(df) - 1)), size = 1)
        df = df[-idx, ]
      }
    }
  }
  rownames(df) = 1:nrow(df)  # in case we added / removed an anchor

  # wiggle anchors
  if (wiggleAllRows) {
    orig = NULL
  } else {
    # save the original time values and put them back in later (usually 0 and 1)
    orig = c(df[1, 1], df[nrow(df), 1])
  }
  if (nrow(df) == 1) {
    ranges = as.numeric(df)
  } else {
    ranges = as.numeric(apply(df, 2, function(x) abs(diff(range(x)))))
    # if no variation in values, defaults to value
    z = which(ranges == 0)
    ranges[z] = abs(as.numeric(df[1, z]))
  }
  for (i in 1:ncol(df)) {
    w = try(rnorm_truncated(
      n = nrow(df),
      mean = as.numeric(df[, i]),
      sd = ifelse(i > 1 & !is.null(sd_values),
                  sd_values,
                  as.numeric(ranges[i] * temperature * temp_coef)),
      low = low[i],
      high = high[i],
      roundToInteger = roundToInteger,
      invalidArgAction = invalidArgAction
    ))
    if (class(w)[1] == 'try-error') {
      warning(paste('Failed to wiggle column', i, 'of df:',
                    paste(df, collapse = ', ')))
    } else {
      df[, i] = w
    }
  }
  if (is.numeric(orig)) {
    df[c(1, nrow(df)), 1] = orig
  }

  # make sure the anchors are still in the right time order
  df = df[order(df$time), ]

  return(df)
}


#' Scale noise anchors
#'
#' Internal soundgen function.
#'
#' Scales a dataframe containing noise anchors so as to preserve the timing of
#' positive anchors relative to the new syllable duration. Negative time anchors
#' are not changed: the pre-aspiration length is constant, regardless of the
#' actual syllable duration. Time anchors from 0 to sylLen are proportional to
#' the actual syllable duration re the average expected duration (which the user
#' sees in the UI when choosing time anchors). Time anchors beyond sylLen are
#' scaled to preserve post-aspiration duration.
#' @param noiseTime vector of time points at which noise anchors are defined
#' @param sylLen_old syllable length relative to which the timing of noise anchors is
#' specified
#' @param sylLen_new the new syllable length
#' @keywords internal
#' @examples
#' noiseTime = c(-20, 50, 120)
#' soundgen:::scaleNoiseAnchors(noiseTime, sylLen_old = 100, sylLen_new = 200)
#' soundgen:::scaleNoiseAnchors(noiseTime, sylLen_old = 100, sylLen_new = 50)
#' soundgen:::scaleNoiseAnchors(noiseTime, sylLen_old = 200, sylLen_new = 300)
scaleNoiseAnchors = function(noiseTime, sylLen_old, sylLen_new) {
  idx_mid = which(noiseTime > 0 &             # not before syl
                    noiseTime < sylLen_old)   # not after syl
  idx_after = which(noiseTime >= sylLen_old)  # after syl
  noiseTime[idx_mid] = noiseTime[idx_mid] * sylLen_new / sylLen_old
  noiseTime[idx_after] = noiseTime[idx_after] - sylLen_old + sylLen_new
  return(noiseTime)
}


#' Wiggle glottal cycles
#'
#' Internal soundgen function
#'
#' Helper function for preparing a vector of multiplication factors for adding
#' jitter and shimmer per glottal cycle. Generates random anchors for each
#' jitter/shimmer period and draws a smooth contour between them by spline
#' interpolation.
#' @param dep a vector of any length specifying the strengh of applied effect as
#'   2 ^ rnorm(..., 0, dep))
#' @param len a vector of any length specifying the period of applied effect in
#'   ms
#' @param nGC number of glottal cycles
#' @param pitch_per_gc vector of length nGC specifying pitch per glottal cycle,
#'   Hz
#' @param rw vector of length nGC specifying a random walk around 1 to multiply
#'   the effect with
#' @param effect_on vector of length nGC specifying glottal cycles to which the
#'   effect should be applied (0 = off, 1 = on)
#' @keywords internal
#' @examples
#' plot(soundgen:::wiggleGC(dep = 5 / 12, len = c(3, 50), nGC = 100,
#'               pitch_per_gc = rnorm(100, 150, 10),
#'               rw = rep(1, 100), effect_on = rep(1, 100)),
#'      type = 'b')
#' plot(soundgen:::wiggleGC(dep = 5 / 12, len = c(3, 50), nGC = 100,
#'               pitch_per_gc = rnorm(100, 150, 10),
#'               rw = rep(1, 100),
#'               effect_on = c(rep(1, 30), rep(0, 20), rep(1, 50))),
#'      type = 'b')
#' plot(soundgen:::wiggleGC(dep = c(1/12, 10/12), len = c(3, 50), nGC = 100,
#'               pitch_per_gc = rnorm(100, 150, 10),
#'               rw = rep(1, 100), effect_on = rep(1, 100)),
#'      type = 'b')
wiggleGC = function(dep, len, nGC, pitch_per_gc, rw, effect_on) {
  # if (length(dep) > 1) dep = getSmoothContour(dep, len = nGC)
  # if (length(len) > 1) len = getSmoothContour(len, len = nGC)
  ratio = pitch_per_gc * len / 1000 # the number of gc that make
  #   up one period of effect (vector of length nGC)
  idx = 1
  i = 1
  while (i < nGC) {
    i = tail(idx, 1) + ratio[i]
    idx = c(idx, i)
  }
  idx = round(idx)
  idx = idx[idx <= nGC]
  idx = unique(idx)  # pitch for these gc will be wiggled
  dep_idx = getSmoothContour(dep, len = length(idx))

  effect = 2 ^ (rnorm(
    n = length(idx),
    mean = 0,
    sd = dep_idx
  ) * rw[idx] * effect_on[idx])
  # plot(effect, type = 'b')

  # upsample to length nGC
  effect_per_gc = spline(effect, n = nGC, x = idx)$y
  # plot(effect_per_gc, type = 'b')
  return(effect_per_gc)
}

#' Validate parameters
#'
#' Internal soundgen function
#'
#' Checks whether the value of a numeric parameter falls within the allowed
#' range. Options: abort, reset to default, throw a warning and continue.
#' @param p parameter name
#' @param gp parameter value
#' @param def matrix or dataframe containing reference values (low, high,
#'   default)
#' @param invalidArgAction what to do if an argument is invalid or outside the
#'   range: 'adjust' = reset to default value, 'abort' = stop execution,
#'   'ignore' = throw a warning and continue (may crash)
#' @keywords internal
validatePars = function(p, gp, def,
                        invalidArgAction = c('adjust', 'abort', 'ignore')[1]) {
  if (any(gp < def[p, 'low']) |
      any(gp > def[p, 'high'])) {
    if (invalidArgAction == 'abort') {
      # exit with a warning
      stop(paste0(
        "\n", p, " should be between ", def[p, 'low'],  " and ",
        def[p, 'high'], "; exiting",
        ". Use invalidArgAction = 'ignore' to override"))
    } else if (invalidArgAction == 'ignore') {
      # throw a warning and continue
      warning(paste0(
        "\n", p, " should be between ", def[p, 'low'],  " and ",
        def[p, 'high'], "; override with caution"))
    } else {
      # reset p to default, with a warning
      gp = def[p, 'default']
      warning(paste0(
        "\n", p, " should be between ", def[p, 'low'],  " and ",
        def[p, 'high'], "; resetting to ", def[p, 'default'],
        ". Use invalidArgAction = 'ignore' to override"))
    }
  }
  return(gp)
}

#' Object to string
#'
#' Internal soundgen function. Converts any object to a string that preserves all internal structure and names.
#' @param x any R object (unquoted)
#' @keywords internal
#' @examples
#' soundgen:::objectToString('adja')
#' soundgen:::objectToString(500)
#' soundgen:::objectToString(c(870, 1250, 1900))
#' soundgen:::objectToString(list(f1 = c(870, 1250), f2 = list(freq = 500, amp = 30)))
#' soundgen:::objectToString(list(
#'   pitch = list(time = c(0, 1), value = c(160, 150)),
#'   noise = list(time = c(-1, 170, 362), value = c(-14, 0, -26)),
#'   mouth = list(time = c(0, 0.07, 1), value = c(0, 0.48, 0.32))))
#' # NB: no matter how long, the object is still returned as an unbroken string
objectToString = function(x) {
  if (is.character(x)) {
    cp = x
  } else {
    # tried and failed: toString, capture.output(call('print', x)), etc.
    cp = deparse(x, width.cutoff = 500, control = c('keepNA', 'niceNames'))
    if (length(cp) > 1) cp = paste(cp, collapse = '')
    # deparse1 comes close, but it require R 4.0 and mishandles strings
  }
  return(cp)
}



#' Reverb & echo
#'
#' Add reverberation and/or echo to a sound. Algorithm for reverb: add
#' time-shifted copies of the signal weighted by a decay function, which is
#' analogous to convoluting the input with a parametric model of some
#' hypothetical impulse response function. In simple terms: we specify how much
#' and when the sound rebounds back (as from a wall) and add these time-shifted
#' copies to the original, optionally with some spectral filtering.
#' @inheritParams spectrogram
#' @param len (optional) the length of input vector
#' @param filter (optional) a spectral filter to apply to the created reverb and
#'   echo (see \code{addFormants} for acceptable formats)
#' @param echoDelay the delay at which the echo appears, ms
#' @param echoLevel the rate at which the echo weakens at each repetition, dB
#' @param reverbDelay the time of maximum reverb density, ms
#' @param reverbSpread standard deviation of reverb spread around time \code{reverbDelay}, ms
#' @param reverbLevel the maximum amplitude of reverb, dB below input
#' @param reverbDensity the number of echos or "voices" added
#' @param reverbType not yet implemented
#' @param dynamicRange the precision with which the reverb and echo are calculated, dB
#' @export
#' @examples
#' s = soundgen()
#' s_rev = reverb(s, 16000)
#' # playme(s_rev)
#'
#' \dontrun{
#' # double echo, no reverb
#' s1 = reverb(s, samplingRate = 16000, reverbLevel = NULL,
#'             echoDelay = c(250, 800), echoLevel = c(-15, -25))
#' # playme(s1)
#' # spectrogram(s1, 16000, osc = TRUE, ylim = c(0, 4))
#'
#' # only reverb (indoors)
#' s2 = reverb(s, samplingRate = 16000, echoDelay = NULL,
#'             reverbDelay = 70, reverbSpread = 130,
#'             reverbLevel = -20, reverbDensity = 20)
#' # playme(s2)
#' # spectrogram(s2, 16000, osc = TRUE, ylim = c(0, 4))
#'
#' # reverb (caves)
#' s3 = reverb(s, samplingRate = 16000, echoDelay = NULL,
#'             reverbDelay = 600, reverbSpread = 1500,
#'             reverbLevel = -10, reverbDensity = 100)
#' # playme(s3)
#' # spectrogram(s3, 16000, osc = TRUE, ylim = c(0, 4))
#'
#' # both echo and reverb with high frequencies emphasized
#' s4 = reverb(s, samplingRate = 16000,
#'             echoDelay = 250, echoLevel = -20,
#'             reverbDelay = 70, reverbSpread = 220,
#'             reverbLevel = -35, reverbDensity = 50,
#'             filter = list(formants = NULL, lipRad = 3))
#' # playme(s4)
#' # spectrogram(s4, 16000, osc = TRUE, ylim = c(0, 4))
#' }
reverb = function(x,
                  samplingRate,
                  len = NULL,
                  echoDelay = 200,
                  echoLevel = -20,
                  reverbDelay = 70,
                  reverbSpread = 130,
                  reverbLevel = -25,
                  reverbDensity = 50,
                  reverbType = 'gaussian',
                  filter = list(),
                  dynamicRange = 80,
                  output = c('audio', 'detailed')[1]) {
  if (is.null(len)) len = length(x)

  ## reverb
  if (is.numeric(reverbDelay) & is.numeric(reverbLevel)) {
    if (reverbType == 'gaussian') {
      # Gaussian over 3 SD
      rvb_len_ms = reverbDelay + 3 * reverbSpread
      nFr_rvb = round(rvb_len_ms * samplingRate / 1000)
      win = dnorm(1:nFr_rvb,
                  mean = reverbDelay * samplingRate / 1000,
                  sd = reverbSpread * samplingRate / 1000)
      win = win / max(win) * dynamicRange - dynamicRange + reverbLevel
      idx_keep = sample(1:nFr_rvb, size = min(nFr_rvb, reverbDensity))
      win = 10 ^ (win / 20)
      win[-idx_keep] = 0
      # plot(win, type = 'l')
    } else {
      stop("Only reverbType = 'gaussian' has been implemented so far")
      # exponential decay: halves (-6 dB) every reverbDelay
      n_halves = min(4, round((dynamicRange + reverbLevel) / 6))
      rvb_len_ms = n_halves * reverbDelay
      nFr_rvb = ceiling(rvb_len_ms * samplingRate / 1000)
      len_halflife = ceiling(reverbDelay * samplingRate / 1000)
      reverbLevel_lin = 10 ^ (reverbLevel / 20)
      win = 2 ^ (-(1:nFr_rvb) / len_halflife) * reverbLevel_lin
      # plot(win, type = 'l')

      # add some noise
      if (FALSE) {
        # rvb_wiggle = getRandomWalk(len = nFr_rvb,
        #   rw_range = reverbSpread * reverbLevel_lin * 2) - reverbSpread * reverbLevel_lin
        lw = rvb_len_ms / 2  # relatively smooth wiggle (1 value per 2 ms)
        # sd_w = seq(reverbSpread, reverbSpread / n_halves, length.out = lw) * win[1]
        rvb_wiggle = rnorm(lw, mean = 0, sd = sd_w)
        rvb_wiggle = approx(rvb_wiggle, n = nFr_rvb)$y
        # plot(rvb_wiggle, type = 'l')
        win = win + rvb_wiggle
        # plot(win, type = 'l')
      }
    }

    # create reverb by adding up time-shifted signal weighted by decay window
    rvb = rep(0, len + nFr_rvb - 1)
    for (i in idx_keep) {
      idx_i = i:(i + len - 1)
      rvb[idx_i] = rvb[idx_i] + x * win[i]
    }
    # rvb = rvb / max(abs(rvb)) * 10 ^ (reverbLevel / 20)  # ideally get win on the correct scale from the start to avoid this step
    # playme(rvb)
    # spectrogram(rvb, 16000, ylim = c(0, 4), osc = TRUE)
    # rvb = rvb_pad[1:len]  # range(rvb)
  } else {
    rvb = 0
  }


  ## echo
  if (is.numeric(echoLevel) && echoLevel > (-dynamicRange) & any(echoDelay > 0)) {
    le = length(echoDelay)
    if (le > 1 & length(echoLevel) == 1) echoLevel = rep(echoLevel, le)
    echo = NULL
    for (e in 1:le) {
      nFr_echo = dynamicRange / (-echoLevel[e])
      step_echo = ceiling(echoDelay[e] * samplingRate / 1000)
      echo_e = rep(0, len + nFr_echo * step_echo - 1)
      for (i in 1:nFr_echo) {
        idx_start = i * step_echo
        idx_i = idx_start:(idx_start + len - 1)
        echo_e[idx_i] = echo_e[idx_i] + x * 10 ^ (echoLevel[e] * i / 20)
      }
      if (is.null(echo)) {
        echo = echo_e
      } else {
        echo = addVectors(echo, echo_e, normalize = FALSE)
      }
    }
    # playme(echo)
  } else {
    echo = 0
  }

  effect = addVectors(rvb, echo, normalize = FALSE)
  if (length(filter) > 1) {
    scale = max(effect)
    rvb = do.call(addFormants, c(list(
      sound = effect, samplingRate = samplingRate,
      normalize = TRUE),
      filter)
    ) * scale
    # playme(effect)
    # spectrogram(effect, 16000, ylim = c(0, 4), osc = TRUE)
  }

  out = addVectors(x, effect)
  # playme(out)
  # spectrogram(out, 16000, ylim = c(0, 4), osc = TRUE)

  if (output == 'audio') {
    result = out
  } else if (output == 'detailed') {
    result = list(
      rvb_win = win,
      rvb = rvb,
      echo = echo,
      effect = effect,
      audio = out
    )
  }
  invisible(result)
}

