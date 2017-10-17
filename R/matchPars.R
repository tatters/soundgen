## FIND SOUNDGEN SETTINGS TO REPRODUCE AN EXISTING SOUND ##

#' Match soundgen pars (experimental)
#'
#' Attempts to find settings for \code{\link{soundgen}} that will reproduce an
#' existing sound. The principle is to mutate control parameters, trying to
#' improve fit to target. The currently implemented optimization algorithm is
#' simple hill climbing. Disclaimer: this function is experimental and may or
#' may not work for particular tasks. It is intended as a supplement to - not
#' replacement of - manual optimization. See the vignette on sound generation
#' for more information.
#'
#' @return Returns a list of length 2: \code{$history} contains the tried
#'   parameter values together with their fit to target (\code{$history$sim}),
#'   and \code{$pars} contains a list of the final - hopefully the best -
#'   parameter settings.
#' @param target the sound we want to reproduce using soundgen: path to a .wav
#'   file or numeric vector
#' @param samplingRate sampling rate of \code{target} (only needed if target is
#'   a numeric vector, rather than a .wav file)
#' @inheritParams spectrogram
#' @param pars arguments to \code{\link{soundgen}} that we are attempting to
#'   optimize
#' @param init a list of initial values for the optimized parameters \code{pars}
#'   and the values of other arguments to soundgen that are fixed at non-default
#'   values (if any)
#' @param method method of comparing mel-transformed spectra of two sounds:
#'   "cor" = average Pearson's correlation of mel-transformed spectra of
#'   individual FFT frames; "cosine" = same as "cor" but with cosine similarity
#'   instead of Pearson's correlation; "pixel" = absolute difference between
#'   each point in the two spectra; "dtw" = discrete time warp with
#'   \code{\link[dtw]{dtw}}
#' @param probMutation the probability of a parameter mutating per iteration
#' @param stepVariance scale factor for calculating the size of mutations
#' @param maxIter maximum number of mutated sounds produced without improving
#'   the fit to target
#' @param minExpectedDelta minimum improvement in fit to target required to
#'   accept the new sound candidate
#' @param verbose if TRUE, plays back the accepted candidate at each iteration
#'   and reports the outcome
#' @param padWith compared spectra are padded with either silence (\code{padWith
#'   = 0}) or with NA's (\code{padWith = NA}) to have the same number of
#'   columns. When the sounds are of different duration, padding with zeros
#'   rather than NA's improves the fit to target measured by \code{method =
#'   'pixel'} and \code{'dtw'}, but it has no effect on \code{'cor'} and
#'   \code{'cosine'}.
#' @param penalizeLengthDif if TRUE, sounds of different length are considered
#'   to be less similar; if FALSE, only the overlapping parts of two sounds are
#'   compared
#' @param throwaway parts of the spectra quieter than \code{throwaway} dB are
#'   not compared
#' @param maxFreq parts of the spectra above \code{maxFreq} Hz are not compared
#' @export
#' @examples
#' playback = c(TRUE, FALSE)[2]  # set to TRUE to play back the audio from examples
#'
#' target = soundgen(repeatBout = 3, sylLen = 120, pauseLen = 70,
#'   pitchAnchors = data.frame(time = c(0, 1), value = c(300, 200)),
#'   rolloff = -5, play = playback)  # we hope to reproduce this sound
#'
#' # Match pars based on acoustic analysis alone, without any optimization.
#' # This *MAY* match temporal structure, pitch, and stationary formants
#' m1 = matchPars(target = target,
#'                samplingRate = 16000,
#'                maxIter = 0,  # no optimization, only acoustic analysis
#'                verbose = playback)
#' cand1 = do.call(soundgen, c(m1$pars, list(play = playback, temperature = 0)))
#'
#' \dontrun{
#' # Try to improve the match by optimizing rolloff
#' # (this may take a few minutes to run, and the results may vary)
#' m2 = matchPars(target = target,
#'                samplingRate = 16000,
#'                pars = 'rolloff',
#'                maxIter = 100,
#'                verbose = playback)
#' # rolloff should be moving from default (-12) to target (-5):
#' sapply(m2$history, function(x) x$pars$rolloff)
#' cand2 = do.call(soundgen, c(m2$pars, list(play = playback, temperature = 0)))
#' }
matchPars = function(target,
                     samplingRate = NULL,
                     pars = NULL,
                     init = NULL,
                     method = c('cor', 'cosine', 'pixel', 'dtw'),
                     probMutation = .25,
                     stepVariance = 0.1,
                     maxIter = 50,
                     minExpectedDelta = 0.001,
                     windowLength = 40,
                     overlap = 50,
                     step = NULL,
                     verbose = TRUE,
                     padWith = NA,
                     penalizeLengthDif = TRUE,
                     throwaway = -120,
                     maxFreq = NULL) {
  parsToRound = c('repeatBout', 'nSyl', 'rolloffParabHarm')
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  if (is.character(target)) {
    targetWave = tuneR::readWave(target)
    samplingRate = targetWave@samp.rate
  } else if (is.numeric(target)) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    }
    targetWave = tuneR::Wave(target, samp.rate = samplingRate, bit = 16)
  } else if (class(target) == 'Wave') {
    targetWave = target
    samplingRate = targetWave@samp.rate
  }

  targetSpec = getMelSpec(targetWave,
                          windowLength = windowLength,
                          overlap = overlap,
                          step = step,
                          throwaway = throwaway,
                          maxFreq = maxFreq,
                          plot = FALSE)

  ## initialize
  # start with default par values, unless initial values are provided
  parDefault = defaults[pars]
  parDefault[names(parDefault) %in% names(init)] = init[names(parDefault)]
  parDefault[['samplingRate']] = samplingRate

  # analyse the target and update the default pars
  as = segment(target, samplingRate = samplingRate, plot = FALSE)
  aa = analyze(target, samplingRate = samplingRate, plot = FALSE)
  af = phonTools::findformants(targetWave@left, fs = samplingRate, verify = FALSE)

  parDefault$nSyl = max(nrow(as$bursts), 1)
  parDefault$sylLen = round(mean(as$syllables$sylLen))
  medianPause = median(as$syllables$pauseLen, na.rm = TRUE)
  if (is.numeric(medianPause)) {
    parDefault$pauseLen = round(medianPause)
  }

  p = as.numeric(na.omit(aa$pitch))
  p = downsample(p, srNew = 5, srOld = 1 / step * 1000)  # downsample F0 measures to 5 Hz
  if (length(p) > 1) {
    parDefault$pitchAnchors = data.frame(
      time = round(seq(0, 1, length.out = length(p)), 2),
      value = round(p)
  )}
  if (is.list(af) && nrow(af) > 0) {
    for (f in 1:min(3, nrow(af))) {  # add max 3 formants
      parDefault$formants[[paste0('f', f)]] = list(
        freq = round(af$formant[f]),
        width = round(af$bandwidth[f])
      )
    }
  }

  # replace defaults with user-provided values, if any
  if (is.list(init)) {
    for (i in 1:length(init)) {
      if (!names(init)[i] %in% names(defaults)) {
        stop(paste('init parameter not recognized:', init[i]))
      }
      parDefault[[names(init)[i]]] = init[[i]]
    }
  }

  # calculate method of initial par values
  output = list(list(pars = parDefault, sim = NA))
  parLoop = parDefault
  cand = try(do.call(soundgen, parLoop), silent = FALSE)
  if (class(cand) == 'try-error') {
    stop ('Invalid initial pars')
  }
  output[[1]]$sim = compareSounds(
    target = NULL,
    targetSpec = targetSpec,
    cand = cand,
    samplingRate = samplingRate,
    method = method,
    windowLength = windowLength,
    overlap = overlap,
    step = step,
    padWith = padWith,
    penalizeLengthDif = penalizeLengthDif,
    throwaway = throwaway,
    maxFreq = maxFreq,
    summary = TRUE
  )

  # iteratively mutate pars and save par values that improve fit to target ('sim')
  i = 1
  while (i < maxIter) {
    if (!is.numeric(length(pars)) || length(pars) < 1) {
      stop(paste("No parameters for optimization are specified!",
                    "Either list them in 'pars' or set 'maxIter = 0'"))
    }
    # mutate pars
    parMut = parLoop
    parMut = wigglePars(parMut,
                        parsToWiggle = pars,
                        probMutation = probMutation,
                        stepVariance = stepVariance)
    # generate a sound based on mutated pars
    cand = try(do.call(soundgen, parMut), silent = FALSE)
    if (class(cand) == 'try-error') {
      sim_new = -Inf
      delta = -Inf
      warning(paste('soundgen crashed with settings',
                    paste(parMut, collapse = ', ')))
    } else {
      # compare to target
      sim_new = compareSounds(
        target = NULL,
        targetSpec = targetSpec,
        cand = cand,
        samplingRate = samplingRate,
        method = method,
        windowLength = windowLength,
        overlap = overlap,
        step = step,
        padWith = padWith,
        penalizeLengthDif = penalizeLengthDif,
        throwaway = throwaway,
        maxFreq = maxFreq,
        summary = TRUE
      )
      delta = sim_new - output[[length(output)]]$sim  # want to maximize similarity
    }

    condition = (delta > minExpectedDelta)
    if (condition) {
      i = 1  # reset the count of iterations
      output = c(output, list(list(pars = parMut,
                                   sim = sim_new)))
      parLoop = parMut
      if (verbose) {
        print(paste('Best similarity: ', round(output[[length(output)]]$sim, 4)))
        playme(cand, samplingRate)
      }
    } else {
      parMut = parLoop # back to previous step
    }
    i = i + 1
  }

  if (verbose) {
    if (length(output) == 1) {
      print ('Failed to improve fit to target! Try increasing maxIter.')
    } else {
      print ('Improved fit to target')
    }
  }

  return (list(history = output, pars = output[[length(output)]]$pars))
}


#' Compare sounds (experimental)
#'
#' Computes similarity between two sounds based on correlating mel-transformed
#' spectra (auditory spectra). Called by \code{\link{matchPars}}.
#' @inheritParams matchPars
#' @param targetSpec if already calculated, the target auditory spectrum can be
#'   provided to speed things up
#' @param cand the sound to be compared to \code{target}
#' @param summary if TRUE, returns the mean of similarity values calculated by
#'   all methods in \code{method}
#' @export
#' @examples
#' \dontrun{
#' target = soundgen(sylLen = 500, formants = 'a',
#'                   pitchAnchors = data.frame(time = c(0, 0.1, 0.9, 1),
#'                                             value = c(100, 150, 135, 100)),
#'                   temperature = 0)
#' targetSpec = soundgen:::getMelSpec(target, samplingRate = 16000)

#' parsToTry = list(
#'   list(formants = 'i',                                            # wrong
#'        pitchAnchors = data.frame(time = c(0, 1),                  # wrong
#'                                  value = c(200, 300))),
#'   list(formants = 'i',                                            # wrong
#'        pitchAnchors = data.frame(time = c(0, 0.1, 0.9, 1),        # right
#'                                  value = c(100, 150, 135, 100))),
#'   list(formants = 'a',                                            # right
#'        pitchAnchors = data.frame(time = c(0,1),                   # wrong
#'                                  value = c(200, 300))),
#'   list(formants = 'a',
#'        pitchAnchors = data.frame(time = c(0, 0.1, 0.9, 1),        # right
#'                                  value = c(100, 150, 135, 100)))  # right
#' )
#'
#' sounds = list()
#' for (s in 1:length(parsToTry)) {
#'   sounds[[length(sounds) + 1]] =  do.call(soundgen,
#'     c(parsToTry[[s]], list(temperature = 0, sylLen = 500)))
#' }
#'
#' method = c('cor', 'cosine', 'pixel', 'dtw')
#' df = matrix(NA, nrow = length(parsToTry), ncol = length(method))
#' colnames(df) = method
#' df = as.data.frame(df)
#' for (i in 1:nrow(df)) {
#'   df[i, ] = compareSounds(
#'     target = NULL,            # can use target instead of targetSpec...
#'     targetSpec = targetSpec,  # ...but faster to calculate targetSpec once
#'     cand = sounds[[i]],
#'     samplingRate = 16000,
#'     padWith = NA,
#'     penalizeLengthDif = TRUE,
#'     method = method,
#'     summary = FALSE
#'   )
#' }
#' df$av = rowMeans(df, na.rm = TRUE)
#' df  # row 1 = wrong pitch & formants, ..., row 4 = right pitch & formants
#' }
compareSounds = function(target,
                         targetSpec = NULL,
                         cand,
                         samplingRate = NULL,
                         method = c('cor', 'cosine', 'pixel', 'dtw')[1:4],
                         windowLength = 40,
                         overlap = 50,
                         step = NULL,
                         padWith = NA,
                         penalizeLengthDif = TRUE,
                         throwaway = -120,
                         maxFreq = NULL,
                         summary = TRUE) {
  # extract spectrums
  if (is.null(targetSpec)) {
    targetSpec = getMelSpec(target,
                            samplingRate = samplingRate,
                            windowLength = windowLength,
                            overlap = overlap,
                            step = step,
                            throwaway = throwaway,
                            maxFreq = maxFreq)
  }
  candSpec = getMelSpec(cand,
                        samplingRate = samplingRate,
                        windowLength = windowLength,
                        overlap = overlap,
                        step = step,
                        throwaway = throwaway,
                        maxFreq = maxFreq)

  # make sure the number of columns (frames) is equal for comparing the two
  # spectrograms by padding with zeros (silence)
  if (ncol(targetSpec) < ncol(candSpec)) {
    targetSpec = matchColumns(matrix_short = targetSpec,
                              nCol = ncol(candSpec),
                              padWith = padWith)
  } else if (ncol(targetSpec) > ncol(candSpec)) {
    candSpec = matchColumns(matrix_short = candSpec,
                            nCol = ncol(targetSpec),
                            padWith = padWith)
  }

  # correlate column by column
  sim_by_column = matrix(NA, nrow = ncol(targetSpec), ncol = length(method))
  colnames(sim_by_column) = method
  sim_by_column = as.data.frame(sim_by_column)
  for (c in 1:ncol(targetSpec)) {
    if ('cor' %in% method) {
      sim_by_column$cor[c] = suppressWarnings(
        cor(targetSpec[,c], candSpec[,c], use = 'na.or.complete')
      )
    }
    if ('cosine' %in% method) {
      sim_by_column$cosine[c] = crossprod(targetSpec[,c], candSpec[,c]) /
        sqrt(crossprod(targetSpec[,c]) * crossprod(candSpec[,c]))
    }
    if ('pixel' %in% method) {
      sim_by_column$pixel[c] = 1 - mean(abs(targetSpec[,c] - candSpec[,c]))
    }
    if ('dtw' %in% method) {
      d = try(dtw::dtw(targetSpec[,c], candSpec[,c],
                       distance.only = TRUE)$normalizedDistance, silent = TRUE)
      if (class(d) == 'try-error') d = NA
      sim_by_column$dtw[c] = 1 - d
    }
  }

  if (penalizeLengthDif) {
    out = apply(sim_by_column, 2, function(x) {
      # if two sounds are of different length, similarity is reduced
      sum(x, na.rm = TRUE) / length(x)
    })
  } else {
    # length difference has no effect on similarity (e.g., it just tells us
    # how well the shorter sound fits the middle part of the longer sound)
    out = colMeans(sim_by_column, na.rm = TRUE)
  }
  if (summary) {
    out = mean(out[method], na.rm = TRUE)
  }
  return(out)
}

#' Wiggle parameters
#'
#' Internal soundgen function
#'
#' Helper function for \code{\link{matchPars}}. Takes a list of control
#' parameters for \code{\link{soundgen}} and introduces some random variation in
#' their values.
#' @param parList full list of considered parameters
#' @param parsToWiggle a list of the names of pars that might be mutated
#' @inheritParams matchPars
#' @keywords internal
#' @examples
#' soundgen:::wigglePars(
#'   parList = list(
#'     sylLen = 250,
#'     pitchAnchors = data.frame(time = c(0, 1), value = c(200, 300))
#'   ),
#'   parsToWiggle = c('sylLen', 'pitchAnchors'),
#'   probMutation = .75,
#'   stepVariance = .5
#' )
wigglePars = function(parList,
                      parsToWiggle,
                      probMutation,
                      stepVariance) {
  parsToRound = c('repeatBout', 'nSyl', 'rolloffParabHarm')

  # choose pars to mutate
  if (length(parsToWiggle) > 1) {
    idx_mut = rbinom(n = length(parsToWiggle),
                     size = 1,
                     prob = probMutation)
    idx_mut_bin = which(idx_mut == 1)
    parsToMutate = parsToWiggle[idx_mut_bin]
    if (length(parsToMutate) == 0) {
      # need to mutate at least one par
      parsToMutate = sample(parsToWiggle, size = 1)
    }
  } else {
    parsToMutate = parsToWiggle
  }

  # prepare a list of mutated par values to feed to the sound generator
  for (p in parsToMutate) {
    if (is.numeric(parList[[p]])) {  # continuous pars
      l = permittedValues[p, 'low']
      h = permittedValues[p, 'high']
      r = ifelse(parList[[p]] != 0,  # if par = 0, we wiggle based on its range
                 abs(parList[[p]]),  # otherwise based on its current value
                 h - l)
      parList[[p]] = rnorm_bounded(
        n = 1,
        mean = parList[[p]],
        low = l,
        high = h,
        sd = r * stepVariance,
        roundToInteger = (p %in% parsToRound)
      )
    } else if (is.list(parList[[p]])) {  # anchors
      if (p == 'formants' | p == 'formantsNoise') {  # formants
        for (f in 1:length(parList[[p]])) {
          parList[[p]][[f]] = wiggleAnchors(
            df = parList[[p]][[f]],
            temperature = stepVariance,
            temp_coef = 1,
            low = c(0, 50, -120, 1),  # time freq amp width
            high = c(1, 8000, 120, 2000),
            wiggleAllRows = FALSE
          )
        }
      } else {  # pitch / ampl / etc
        wiggleAllRows = FALSE
        if (p == 'pitchAnchors') {
          low = c(0, permittedValues['pitchFloor', 'default'])
          high = c(1, permittedValues['pitchCeiling', 'default'])
        } else if (p == 'pitchAnchorsGlobal') {
          low = c(0, permittedValues['pitchDeltas', 'low'])
          high = c(1, permittedValues['pitchDeltas', 'high'])
        } else if (p == 'amplAnchors' |
                   p == 'amplAnchorsGlobal') {
          low = c(0, 0)
          high = c(1, -permittedValues['throwaway', 'default'])
        } else if (p == 'noiseAnchors') {
          low = c(-Inf, permittedValues['throwaway', 'default'])
          high = c(+Inf, 40)
          wiggleAllRows = TRUE
        }
        parList[[p]] = wiggleAnchors(
          df = parList[[p]],
          temperature = stepVariance,
          temp_coef = 1,
          low = low,  # time, value
          high = high,
          wiggleAllRows = wiggleAllRows
        )
      }
    }
  }
  return (parList)
}


#' Mel-transformed spectrum
#'
#' Internal soundgen function
#'
#' Takes a .wav file or a waveform as numeric vector + samplingRate and returns
#' mel-transformed spectrum (auditory spectrum). Calls
#' \code{\link[tuneR]{melfcc}}. See \code{\link{matchPars}}.
#' @param s input sound (path to a .wav file or numeric vector)
#' @inheritParams matchPars
#' @param plot if TRUE, plots the spectrum
#' @keywords internal
getMelSpec = function(s,
                      samplingRate = NULL,
                      windowLength = 40,
                      overlap = 50,
                      step = NULL,
                      throwaway = -120,
                      maxFreq = NULL,
                      plot = FALSE) {
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  throwaway01 = 2 ^ (throwaway / 10)

  if (is.character(s)) {
    sWave = tuneR::readWave(s)
    samplingRate = sWave@samp.rate
  } else if (is.numeric(s)) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    }
    sWave = tuneR::Wave(s, samp.rate = samplingRate, bit = 16)
  } else if (class(s) == 'Wave') {
    sWave = s
    samplingRate = sWave@samp.rate
  }

  if (is.null(maxFreq)) maxFreq = samplingRate / 2
  spec = t(tuneR::melfcc(
    sWave,
    wintime = windowLength / 1000,
    hoptime = step / 1000,
    maxfreq = maxFreq,
    nbands = 100 * windowLength / 20,
    spec_out = T
  )$aspectrum)
  # strip empty frames
  spec = spec[, colMeans(spec, na.rm = TRUE) > throwaway01, drop = FALSE]
  # log-transform and normalize
  spec = log01(spec)

  if (plot) {
    # show the spectrum of the target
    seewave::filled.contour.modif2(
      x = seq(1, ncol(spec) * step,
              length.out = ncol(spec)),
      y = 1:nrow(spec),
      z = t(spec),
      levels = seq(0, 1, length = 30),
      color.palette = function(x) gray(seq(1, 0, length.out = x))
    )
  }
  return(spec)
}
