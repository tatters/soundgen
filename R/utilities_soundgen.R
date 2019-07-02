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
reportTime = function(i, nIter, time_start, jobs = NULL, reportEvery = 1) {
  time_diff = as.numeric((proc.time() - time_start)[3])
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


#' Print time
#'
#' Internal soundgen function.
#'
#' Converts time in seconds to time in hh:mm:ss for pretty printing.
#' @param time_s time (s)
#' @return Returns a character string like "1 h 20 min 3 s"
#' @keywords internal
#' @examples
#' time_start = proc.time()
#' Sys.sleep(1)
#' time_diff = as.numeric((proc.time() - time_start)[3])
#' soundgen:::convert_sec_to_hms(time_diff)
convert_sec_to_hms = function(time_s) {
  hours = time_s %/% 3600
  minutes = time_s %/% 60 - hours * 60
  seconds = round(time_s %% 60, 0)

  output = ''
  if (hours > 0) output = paste0(output, hours, ' h ')
  if (minutes > 0) output = paste0(output, minutes, ' min ')
  output = paste0(output, seconds, ' s')

  # remove the last space, if any
  if (substr(output, nchar(output), nchar(output)) == ' ') {
    output = substr(output, 1, nchar(output)-1)
  }
  return(output)
}


#' HTML for clickable plots
#'
#' Internal soundgen function
#'
#' Writes an html file for displaying clickable plots in a browser.
#' @param myfolder full path to target folder, without a '/' at the end
#' @param myfiles a list of full names of files (with paths and extensions)
#' @keywords internal
#' @examples
#' \dontrun{
#' htmlPlots(myfolder = '~/Downloads/temp',
#'           myfiles = c('~/Downloads/temp/myfile1.wav',
#'                       '~/Downloads/temp/myfile2.wav'))
#' }
htmlPlots = function(myfolder, myfiles) {
  # a list of basenames without extension
  basenames = basename(myfiles)
  basenames_stripped = as.character(sapply(
    basenames,
    function(x) substr(x, 1, nchar(x) - 4)
  ))
  n = paste0(basenames, collapse = "', '")
  n = paste0("var mylist = ['", n, "'];")

  # create an html file to display nice, clickable spectrograms
  out_html = file(paste0(myfolder,'/00_clickable_plots.html'))
  writeLines(
    c("<!DOCTYPE html>",
      "<html>",
      "<head>",
      "<title>Labels</title>",
      "<meta charset='UTF-8'>",
      "<style>",
      "table { width:100%; float:center; }",
      "table, th, td { border: 1px solid black; border-collapse: collapse; }",
      "th, td { padding: 5px; text-align: center; }",
      "table#t01 tr:nth-child(even) { background-color: #eee; }",
      "table#t01 tr:nth-child(odd) { background-color:#fff; }",
      "table#t01 th	{ background-color: black; color: white; }",
      "</style>",
      "<script>",
      n,
      "</script>",
      "</head>",
      "<body>",
      "<div id='instruction' style='font-size:200%; padding:5px; text-align:center'>Plots of all files in a folder</div>",
      "<div id='container'> </div>",
      "<script>",
      "var sound = [];",
      "var table = document.createElement('table'), tr, td, row, cell;",
      "for (row = 0; row < mylist.length; row++) {",
      "  sound[row] = mylist[row].substring(0, mylist[row].length - 4);",
      "  tr = document.createElement('tr');",
      "  td = document.createElement('td');",
      "  tr.appendChild(td);",
      "  td.innerHTML = '<img src=\"' + sound[row] + '.png\">';",
      "  var mysound = mylist[row];",
      "  td.onclick = (function(mysound) {",
      "    return function() {",
      "      var audioElement = document.createElement('audio');",
      "      audioElement.setAttribute('src', mysound);",
      "      audioElement.play();",
      "    };",
      "  })(mysound);",
      "  table.appendChild(tr);",
      "}",
      "document.getElementById('container').appendChild(table);",
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
      sd = sylLen * temperature
    )
    pauses = rnorm_truncated(
      n = nSyl - 1,
      mean = pauseLen,
      low = pauseDur_min,
      high = pauseDur_max,
      sd = pauseLen * temperature
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
                         invalidArgAction = c('adjust', 'abort', 'ignore')[1]) {
  if (temperature == 0 | temp_coef == 0) return(df)
  if (any(is.na(df))) return(NA)
  if (class(df) != 'data.frame') df = as.data.frame(df)

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
        invalidArgAction = invalidArgAction))
      if (class(newAnchor) == 'try-error') {
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
      roundToInteger = FALSE,
      invalidArgAction = invalidArgAction
    ))
    if (class(w) == 'try-error') {
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

