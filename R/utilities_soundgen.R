### UTILITIES FOR SOUND GENERATION ###

#' Report time
#'
#' Internal soundgen function.
#'
#' Based on the current iteration, total number of iterations, and time when the
#' loop started running, prints estimated time left or a summary upon
#' completion.
#' @param i current iteration
#' @param nIter total number of iterations
#' @param time_start time when the loop started running
#' @param jobs vector of length \code{nIter} specifying the relative difficulty
#'   of each iteration. If not NULL, estimated time left takes into account
#'   whether the jobs ahead will take more or less time than the jobs already
#'   completed
#' @param reportEvery report progress every n iterations
#' @keywords internal
#' @examples
#' time_start = proc.time()
#' for (i in 1:20) {
#'   Sys.sleep(i ^ 2 / 10000)
#'   soundgen:::reportTime(i = i, nIter = 20, time_start = time_start,
#'   jobs = (1:20) ^ 2, reportEvery = 5)
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


#' Play audio
#'
#' Plays an audio file or a numeric vector. This is a simple wrapper for the
#' functionality provided by \code{\link[tuneR]{play}}
#' @param sound a vector of numbers on any scale or a path to a .wav file
#' @param samplingRate sampling rate (only needed if sound is a vector)
#' @export
#' @examples
#' # playme('~/myfile.wav')
#' f0_Hz = 440
#' sound = sin(2 * pi * f0_Hz * (1:16000) / 16000)
#' # playme(sound, 16000)
#' # in case of errors, look into tuneR::play()
playme = function(sound, samplingRate = 16000) {
  # input: a vector of numbers on any scale or a path to a .wav file
  if (class(sound) == 'character') {
    soundWave = tuneR::readWave(sound)
  } else if (class(sound) == 'numeric' | class(sound) == 'integer') {
    soundWave = tuneR::Wave(
      left = sound,
      samp.rate = samplingRate,
      bit = 16,
      pcm = TRUE
    )
    soundWave = tuneR::normalize(soundWave, unit = '32') # / 2
  }

  os = Sys.info()[['sysname']]
  if (os == 'Linux' | os == 'linux') {
    p = tuneR::play(soundWave, 'play')
  } else {  # windows | darwin
    p = tuneR::play(soundWave)
  }
  if (p > 0) {  # error in sh
    warning("Error in tuneR::play. Try setting the default audio player. See http://music.informatics.indiana.edu/courses/I546/tuneR_play.pdf")
  }
  # can't get rid of printed output! sink(), capture.output, invisible don't work!!!
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
  )
  )
  n = paste0(basenames_stripped, collapse = "', '")
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
      "var image = [];",
      "var table = document.createElement('table'), tr, td, row, cell;",
      "for (row = 0; row < mylist.length; row++) {",
      "  sound[row] = mylist[row] + '.wav';",
      "  image[row] = mylist[row] + '.jpg';",
      "  tr = document.createElement('tr');",
      "  td = document.createElement('td');",
      "  tr.appendChild(td);",
      "  td.innerHTML = '<img src=\"' + image[row] + '\">';",
      "  var mysound = sound[row];",
      "  td.onclick = (function(mysound) {",
      "    return function() {",
      "      var audioElement = document.createElement('audio');",
      "      audioElement.setAttribute('src', mysound);",
      "      audioElement.play();",
      "    };",
      "  })(sound[row]);",
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
      if (ampl[i] > 0 && ampl[i - 1] < 0) {
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
    if (ampl[i + 1] > 0 && ampl[i] < 0) {
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


#' Join two waveforms by cross-fading
#'
#' \code{crossFade} joins two input vectors (waveforms) by cross-fading. It
#' truncates both input vectors, so that \code{ampl1} ends with a zero crossing
#' and \code{ampl2} starts with a zero crossing, both on an upward portion of
#' the soundwave. Then it cross-fades both vectors linearly with an overlap of
#' crossLen or crossLenPoints. If the input vectors are too short for the
#' specified length of cross-faded region, the two vectors are concatenated at
#' zero crossings instead of cross-fading. Soundgen uses \code{crossFade} for
#' gluing together epochs with different regimes of pitch effects (see the
#' vignette on sound generation), but it can also be useful for joining two
#' separately generated sounds without audible artifacts.
#' @param ampl1,ampl2 two numeric vectors (waveforms) to be joined
#' @param crossLenPoints (optional) the length of overlap in points
#' @param crossLen the length of overlap in ms (overrides crossLenPoints)
#' @param samplingRate the sampling rate of input vectors, Hz (needed only if
#'   crossLen is given in ms rather than points)
#' @inheritParams fade
#' @export
#' @return Returns a numeric vector.
#' @examples
#' sound1 = sin(1:100 / 9)
#' sound2 = sin(7:107 / 3)
#' plot(c(sound1, sound2), type = 'b')
#' # an ugly discontinuity at 100 that will make an audible click
#'
#' sound = crossFade(sound1, sound2, crossLenPoints = 5)
#' plot(sound, type = 'b') # a nice, smooth transition
#' length(sound) # but note that cross-fading costs us ~60 points
#' #  because of trimming to zero crossings and then overlapping
#'
#' \dontrun{
#' # Actual sounds, alternative shapes of fade-in/out
#' sound3 = soundgen(formants = 'a', pitchAnchors = 200,
#'                   addSilence = 0, attackLen = c(50, 0))
#' sound4 = soundgen(formants = 'u', pitchAnchors = 200,
#'                   addSilence = 0, attackLen = c(0, 50))
#'
#' # simple concatenation (with a click)
#' playme(c(sound3, sound4), 16000)
#'
#' # concatentation from zc to zc (no click, but a rough transition)
#' playme(crossFade(sound3, sound4, crossLen = 0), 16000)
#'
#' # linear crossFade over 35 ms - brief, but smooth
#' playme(crossFade(sound3, sound4, crossLen = 35, samplingRate = 16000), 16000)
#'
#' # s-shaped cross-fade over 300 ms (shortens the sound by ~300 ms)
#' playme(crossFade(sound3, sound4, samplingRate = 16000,
#'                  crossLen = 300, shape = 'cos'), 16000)
#' }
crossFade = function(ampl1,
                     ampl2,
                     crossLenPoints = 240,
                     crossLen = NULL,
                     samplingRate = NULL,
                     shape = c('lin', 'exp', 'log', 'cos', 'logistic')[1],
                     steepness = 1) {
  # cut to the nearest zero crossings
  zc1 = findZeroCrossing(ampl1, location = length(ampl1))
  if (!is.na(zc1)) {
    # up to the last point before the last zero-crossing in sound 1 on the
    # upward curve + one extra zero (to have a nice, smooth transition line:
    # last negative in s1 - zero - first positive in s2)
    ampl1 = c(ampl1[1:zc1], 0)
  }
  zc2 = findZeroCrossing(ampl2, location = 1)
  if (!is.na(zc2)) {
    ampl2 = ampl2[(zc2 + 1):length(ampl2)]
    # from the first positive point on the upward curve. Note the +1 - next
    # point after the first zero crossing in s2
  }

  # check whether there is enough data to cross-fade. Note that ampl1 or ampl2
  # may even become shorter than crossLenPoints after we shortened them to the
  # nearest zero crossing
  if (is.null(crossLenPoints) |
      (!is.null(crossLen) & !is.null(samplingRate))) {
    crossLenPoints = floor(crossLen * samplingRate / 1000)
  }
  crossLenPoints = min(crossLenPoints, length(ampl1) - 1, length(ampl2) - 1)

  # concatenate or cross-fade
  if (crossLenPoints < 2) {
    # for segments that are too short,
    # just concatenate from zero crossing to zero crossing
    ampl = c(ampl1, ampl2)
  } else {
    # for segments that are long enough, cross-fade properly
    # multipl = seq(0, 1, length.out = crossLenPoints)
    multipl = fade(rep(1, crossLenPoints),
                   fadeIn = crossLenPoints,
                   fadeOut = 0,
                   shape = shape,
                   steepness = steepness)
    idx1 = length(ampl1) - crossLenPoints
    cross = rev(multipl) * ampl1[(idx1 + 1):length(ampl1)] +
      multipl * ampl2[1:crossLenPoints]
    ampl = c(ampl1[1:idx1],
             cross,
             ampl2[(crossLenPoints + 1):length(ampl2)])
  }
  return(ampl)
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
#' \code{\link{rnorm_bounded}} to vary the duration of each new syllable and of
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
    syls = rnorm_bounded(
      n = nSyl,
      mean = sylLen,
      low = sylDur_min,
      high = sylDur_max,
      sd = sylLen * temperature
    )
    pauses = rnorm_bounded(
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

#' sampleModif
#'
#' Internal soundgen function
#'
#' Same as \code{\link[base]{sample}}, but without defaulting to x = 1:x if
#' length(x) = 1. See
#' https://stackoverflow.com/questions/7547758/using-sample-with-sample-space-size-1
#' @param x vector
#' @param ... other arguments passed to \code{sample}
#' @keywords internal
#' @examples
#' soundgen:::sampleModif(x = 3, n = 1)
#' # never returns 1 or 2: cf. sample(x = 3, n = 1)
sampleModif = function(x, ...) x[sample.int(length(x), ...)]

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
#' @param sd_values (optional) the exact value of sd used by rnorm_bounded in
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
      newAnchor = try(rnorm_bounded(
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
      a2 = ifelse(a1 + direction < 1 || a1 + direction > nrow(df),
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
    w = try(rnorm_bounded(
      n = nrow(df),
      mean = as.numeric(df[, i]),
      sd = ifelse(i > 1 && !is.null(sd_values),
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


#' Get amplitude envelope
#'
#' Internal soundgen function
#'
#' Returns the smoothed amplitude envelope of a waveform on the original scale.
#' @inheritParams flatEnv
#' @param method 'peak' for peak amplitude per window, 'rms' for root mean
#'   square amplitude, 'mean' for mean (for DC offset removal)
#' @keywords internal
#' @examples
#' a = rnorm(500) * seq(1, 0, length.out = 500)
#' plot(soundgen:::getEnv(a, 20))
getEnv = function(sound,
                  windowLength_points,
                  method = c('rms', 'peak', 'mean')[1]) {
  len = length(sound)
  if (method == 'peak') sound_abs = abs(sound)  # avoid repeated calculations

  if (windowLength_points >= len / 2) {
    # short sound relative to window - just take beginning and end (2 points)
    s = c(1, len)
  } else {
    s = c(1,
          seq(from = floor(windowLength_points / 2),
              to = length(sound) - floor(windowLength_points / 2),
              by = windowLength_points),
          length(sound))
  }
  # s is a sequence of starting indices for windows over which we average
  envShort = rep(NA, length(s) - 1)
  for (i in 1:(length(s) - 1)) {
    seg = s[i] : s[i+1]
    if (method == 'peak') {
      # get moving peak amplitude
      envShort[i] = max(sound_abs[seg])
    } else if (method == 'rms') {
      # get moving RMS amplitude
      envShort[i] = sqrt(mean(sound[seg] ^ 2))
    } else if (method == 'mean') {
      envShort[i] = mean(sound[seg])
    }
  }

  # upsample and smooth
  env = getSmoothContour(
    anchors = data.frame(time = seq(0, 1, length.out = length(envShort)),
                         value = envShort),
    len = length(sound)
  )
  return(env)
}


#' Flat envelope
#'
#' Flattens the amplitude envelope of a waveform. This is achieved by dividing
#' the waveform by some function of its smoothed rolling amplitude (peak or root
#' mean square).
#' @param sound input vector oscillating about zero
#' @param windowLength the length of smoothing window, ms
#' @param samplingRate the sampling rate, Hz. Only needed if the length of
#'   smoothing window is specified in ms rather than points
#' @param method 'hil' for Hilbert envelope, 'rms' for root mean square
#'   amplitude, 'peak' for peak amplitude per window
#' @param windowLength_points the length of smoothing window, points. If
#'   specified, overrides both \code{windowLength} and \code{samplingRate}
#' @param killDC if TRUE, dynamically removes DC offset or similar deviations of
#'   average waveform from zero
#' @param throwaway parts of sound quieter than \code{throwaway} dB will not be
#'   amplified
#' @param plot if TRUE, plots the original sound, smoothed envelope, and
#'   flattened sound
#' @export
#' @examples
#' a = rnorm(500) * seq(1, 0, length.out = 500)
#' b = flatEnv(a, plot = TRUE, killDC = TRUE,
#'             windowLength_points = 5)         # too short
#' c = flatEnv(a, plot = TRUE, killDC = TRUE,
#'             windowLength_points = 250)       # too long
#' d = flatEnv(a, plot = TRUE, killDC = TRUE,
#'             windowLength_points = 50)        # about right
#'
#' \dontrun{
#' s = soundgen(sylLen = 1000, amplAnchors = c(0, -40, 0), plot = TRUE, osc = TRUE)
#' # playme(s)
#' s_flat = flatEnv(s, plot = TRUE, windowLength = 50)
#' # playme(s_flat)
#' }
flatEnv = function(sound,
                   windowLength = 200,
                   samplingRate = 16000,
                   method = c('hil', 'rms', 'peak')[1],
                   windowLength_points = NULL,
                   killDC = FALSE,
                   throwaway = -80,
                   plot = FALSE) {
  if (!is.numeric(windowLength_points)) {
    if (is.numeric(windowLength)) {
      if (is.numeric(samplingRate)) {
        windowLength_points = windowLength / 1000 * samplingRate
      } else {
        stop(paste('Please, specify either windowLength (ms) plus samplingRate (Hz)',
                   'or the length of smoothing window in points (windowLength_points)'))
      }
    }
  }

  m = max(abs(sound))       # original scale (eg -1 to +1 gives m = 1)
  soundNorm = sound / m    # normalize
  throwaway_lin = 10 ^ (throwaway / 20)  # from dB to linear
  # get smoothed amplitude envelope
  if (method == 'hil') {
    env = seewave::env(
      soundNorm,
      f = samplingRate,
      envt = 'hil',
      ssmooth = windowLength_points,
      fftw = FALSE,
      plot = FALSE
    )
    env = as.numeric(env)
  } else {
    env = getEnv(sound = soundNorm,
                 windowLength_points = windowLength_points,
                 method = method)
  }
  env = env / max(abs(env))

  # don't amplify very quiet sections
  env_cut = env
  env_cut[env_cut < throwaway_lin] = 1
  # flatten amplitude envelope
  soundFlat = soundNorm / env_cut
  # re-normalize to original scale
  soundFlat = soundFlat / max(abs(soundFlat)) * m
  # remove DC offset
  if (killDC) {
    sound_norm = killDC(sound = soundFlat,
                        windowLength_points = windowLength_points,
                        plot = FALSE)
  }

  if (plot) {
    op = par('mfrow')
    par(mfrow = c(1, 2))
    plot(sound, type = 'l', main = 'Original')
    points(env * m, type = 'l', lty = 1, col = 'blue')
    plot(soundFlat, type = 'l', main = 'Flattened')
    par(mfrow = op)
  }
  return(soundFlat)
}


#' Kill envelope
#'
#' Removes DC offset or similar disbalance in a waveform dynamically, by
#' subtracting a smoothed ~moving average. Simplified compared to a true moving
#' average, but very fast (a few ms per second of 44100 audio).
#' @inheritParams flatEnv
#' @param plot if TRUE, plots the original sound, smoothed moving average, and
#'   modified sound
#' @keywords internal
#' @examples
#' # remove static DC offset
#' a = rnorm(500) + .3
#' b = soundgen:::killDC(a, windowLength_points = 500, plot = TRUE)
#'
#' # remove trend
#' a = rnorm(500) + seq(0, 1, length.out = 500)
#' b = soundgen:::killDC(a, windowLength_points = 100, plot = TRUE)
#'
#' # can also be used as a high-pass filter
#' a = rnorm(500) + sin(1:500 / 50)
#' b = soundgen:::killDC(a, windowLength_points = 25, plot = TRUE)
killDC = function(sound,
                  windowLength = 200,
                  samplingRate = 16000,
                  windowLength_points = NULL,
                  plot = FALSE) {
  if (!is.numeric(windowLength_points)) {
    if (is.numeric(windowLength)) {
      if (is.numeric(samplingRate)) {
        windowLength_points = windowLength / 1000 * samplingRate
      } else {
        stop(paste('Please, specify either windowLength (ms) plus samplingRate (Hz)',
                   'or the length of smoothing window in points (windowLength_points)'))
      }
    }
  }

  env = getEnv(sound = sound,
               windowLength_points = windowLength_points,
               method = 'mean')
  soundNorm = sound - env

  if (plot) {
    op = par('mfrow')
    par(mfrow = c(1, 2))
    plot(sound, type = 'l', main = 'Original')
    points(env, type = 'l', lty = 1, col = 'blue')
    points(rep(0, length(sound)), type = 'l', lty = 2)
    plot(soundNorm, type = 'l', main = 'Env removed')
    points(rep(0, length(sound)), type = 'l', col = 'blue')
    par(mfrow = op)
  }
  return(soundNorm)
}


#' Fade
#'
#' Applies fade-in and/or fade-out of variable length, shape, and steepness. The
#' resulting effect softens the attack and release of a waveform.
#' @param x zero-centered (!) numeric vector such as a waveform
#' @param fadeIn,fadeOut length of segments for fading in and out, interpreted
#'   as points if \code{samplingRate = NULL} and as ms otherwise (0 = no fade)
#' @param samplingRate sampling rate of the input vector, Hz
#' @param shape controls the type of fade function: 'lin' = linear, 'exp' =
#'   exponential, 'log' = logarithmic, 'cos' = cosine, 'logistic' = logistic
#'   S-curve
#' @param steepness scaling factor regulating the steepness of fading curves if
#'   the shape is 'exp', 'log', or 'logistic' (0 = linear, >1 = steeper than
#'   default)
#' @param plot if TRUE, produces an oscillogram of the waveform after fading
#' @return Returns a numeric vector of the same length as input
#' @export
#' @examples
#' #' # Fading a real sound: say we want fast attack and slow release
#' s = soundgen(attack = 0, windowLength = 10,
#'              sylLen = 500, addSilence = 0)
#' # playme(s)
#' # plot(s, type = 'l')
#' s1 = fade(s, fadeIn = 10, fadeOut = 350,
#'           samplingRate = 16000, shape = 'cos')
#' # playme(s1)
#' # plot(s1, type = 'l')
#'
#'
#' # Illustration of fade shapes
#' x = runif(5000, min = -1, max = 1)  # make sure to zero-center input!!!
#' # plot(x, type = 'l')
#' y = fade(x, fadeIn = 1000, fadeOut = 0, plot = TRUE)
#' y = fade(x,
#'          fadeIn = 1000,
#'          fadeOut = 1500,
#'          shape = 'exp',
#'          plot = TRUE)
#' y = fade(x,
#'          fadeIn = 1500,
#'          fadeOut = 500,
#'          shape = 'log',
#'          plot = TRUE)
#' y = fade(x,
#'          fadeIn = 1500,
#'          fadeOut = 500,
#'          shape = 'log',
#'          steepness = 8,
#'          plot = TRUE)
#' y = fade(x,
#'          fadeIn = 1000,
#'          fadeOut = 1500,
#'          shape = 'cos',
#'          plot = TRUE)
#' y = fade(x,
#'          fadeIn = 1500,
#'          fadeOut = 500,
#'          shape = 'logistic',
#'          steepness = 4,
#'          plot = TRUE)
fade = function(x,
                fadeIn = 1000,
                fadeOut = 1000,
                samplingRate = NULL,
                shape = c('lin', 'exp', 'log', 'cos', 'logistic')[1],
                steepness = 1,
                plot = FALSE) {
  if ((!is.numeric(fadeIn) || fadeIn < 1) &
      (!is.numeric(fadeOut) || fadeOut < 1)) {
    return(x)
  }

  if (steepness < 0) {
    steepness = 1
    warning('steepness must be non-negative; resetting to 1')
  } else if (steepness == 0) {
    shape = 'lin'
  }

  if (is.numeric(samplingRate)) {
    fadeIn = round(fadeIn * samplingRate / 1000)
    fadeOut = round(fadeOut * samplingRate / 1000)
  }

  # round fading window just in case of non-integers, shorten if needed
  fadeIn = min(length(x), round(fadeIn))
  fadeOut = min(length(x), round(fadeOut))

  time_in = seq(0, 1, length.out = fadeIn)
  time_out = seq(1, 0, length.out = fadeOut)
  if (shape == 'lin') {
    fi = time_in
    fo = time_out
  } else if (shape == 'exp') {
    fi = zeroOne(exp(time_in * steepness / 3))
    fo = zeroOne(exp(time_out * steepness / 3))
  } else if (shape == 'log') {
    fi = 1 - rev(zeroOne(exp(time_in * steepness / 3)))
    fo = 1 - rev(zeroOne(exp(time_out * steepness / 3)))
  } else if (shape == 'cos') {
    fi = (1 - cos(time_in * pi)) / 2
    fo = (1 - cos(time_out * pi)) / 2
  } else if (shape == 'logistic') {
    fi = zeroOne(1 - 1 / (1 + exp(steepness * (time_in - .5))))
    fo = zeroOne(1 - 1 / (1 + exp(steepness * (time_out - .5))))
  }
  # plot(fi, type = 'l')
  # plot(fo, type = 'l')

  if (fadeIn > 0) {
    x[1:fadeIn] = x[1:fadeIn] * fi
  }
  if (fadeOut > 0) {
    x[(length(x) - fadeOut + 1):length(x)] =
      x[(length(x) - fadeOut + 1):length(x)] * fo
  }

  if (plot) {
    plot(x, type = 'l', xlab = '', ylab = '')
    abline(v = fadeIn, col = 'blue')
    abline(v = length(x) - fadeOut, col = 'blue')
  }
  return(x)
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
#' @param noiseTime vector of time points at which noiseAnchors are defined
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


#' Oscillogram dB
#'
#' Plots the oscillogram (waveform) of a sound on a logarithmic scale, in dB.
#' Analogous to "Waveform (dB)" view in Audacity.
#'
#' Algorithm: centers and normalizes the sound, then takes a logarithm of the
#' positive part and a flipped negative part.
#' @return Returns the input waveform on a dB scale: a vector with
#'   range from `-range` to `range`.
#' @param x path to a .wav file or a CENTERED (mean ~= 0) vector of amplitudes
#'   with specified samplingRate
#' @param dynamicRange dynamic range of the oscillogram, dB
#' @param maxAmpl the maximum theoretically possible value indicating on which
#'   scale the sound is coded: 1 if the range is -1 to +1, 2^15 for 16-bit wav
#'   files, etc
#' @param samplingRate sampling rate of \code{x} (only needed if \code{x} is a
#'   numeric vector, rather than a .wav file)
#' @param returnWave if TRUE, returns a log-transformed waveform as a numeric vector
#' @param plot if TRUE, plots the oscillogram
#' @param xlab,ylab axis labels
#' @param bty box type (see `?par`)
#' @param midline if TRUE, draws a line at 0 dB
#' @param ... Other graphical parameters passed on to `plot()`
#' @export
#' @examples
#' sound = sin(1:2000/10) *
#'         getSmoothContour(anchors = c(1, .01, .5), len = 2000)
#'
#' # Oscillogram on a linear scale
#' plot(sound, type = 'l')
#' # or, for fancy plotting options: seewave::oscillo(sound, f = 1000)
#'
#' # Oscillogram on a dB scale
#' osc_dB(sound)
#'
#' # Time in ms if samplingRate is specified
#' osc_dB(sound, samplingRate = 5000)
#'
#' # Assuming that the waveform can range up to 50 instead of 1
#' osc_dB(sound, maxAmpl = 50)
#'
#' # Embellish and customize the plot
#' o = osc_dB(sound, samplingRate = 1000, midline = FALSE,
#'            main = 'My waveform', col = 'blue')
#' abline(h = 0, col = 'orange', lty = 3)
osc_dB = function(x,
                  dynamicRange = 80,
                  maxAmpl = NULL,
                  samplingRate = NULL,
                  returnWave = FALSE,
                  plot = TRUE,
                  xlab = NULL,
                  ylab = 'dB',
                  bty = 'n',
                  midline = TRUE,
                  ...) {
  # import a sound
  if (class(x) == 'character') {
    sound_wav = tuneR::readWave(x)
    samplingRate = sound_wav@samp.rate
    sound = sound_wav@left
    if (is.null(maxAmpl)) maxAmpl = 2^(sound_wav@bit - 1)
  } else if (length(x) > 1 && class(x) %in% c('numeric', 'integer')) {
    sound = x
  }

  # get original range
  if (!is.null(maxAmpl)) {
    mult = diff(range(sound)) / 2 / maxAmpl
  } else {
    mult = 1  # assume max loudness
  }

  # normalize to range from -1 to +1, unless it is quieter than maxAmpl
  s1 = sound / max(abs(sound)) * mult

  # indices of values above/below midline
  floor = 10^(-dynamicRange / 20)  # treat smaller values as 0 (beyond dynamic range)
  zero = which(abs(s1) < floor)
  pos = which(s1 > floor)
  neg = which(s1 < -floor)

  # log-transform
  sound[pos] = 20 * log10(s1[pos]) + dynamicRange
  sound[neg] = -20 * log10(-s1[neg]) - dynamicRange
  sound[zero] = 0

  # plot
  if (plot) {
    if (!is.null(samplingRate)) {
      time = 1:length(sound) / samplingRate * 1000
      if (is.null(xlab)) xlab = 'Time, ms'
    } else {
      time = 1:length(sound)
      if (is.null(xlab)) xlab = 'Time, points'
    }
    # plot(time, sound, type = 'l', xlab = xlab, ylab = ylab, ...)
    plot(time, sound, type = 'l', xlab = xlab, ylab = ylab,
         bty = bty, yaxt = 'n', ylim = c(-dynamicRange, dynamicRange), ...)
    axis(side = 2, at = seq(0, dynamicRange, by = 10))
    if (midline) abline(h = 0, lty = 2, col = 'gray70')
  }

  if (returnWave) return(sound)
}
