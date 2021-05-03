#' Reformat formants
#'
#' Internal soundgen function.
#'
#' Checks that the formants are formatted in a valid way and expands them to a
#' standard list of dataframes with time, frequency, amplitude, and bandwidth of
#' each formant specified explicitly.
#' @param formants character string like "aoiu", numeric vector like "c(500,
#'   1500, 2400)", or a list with an entry for each formant
#' @param output 'all' (default) includes times stamps, freqs, amplitudes, and
#'   bandwidths; 'freqs' includes only frequencies
#' @param keepNonInteger if FALSE, fractional (anti)formants like 'f1.5' are
#'   removed
#' @keywords internal
#' @examples
#' soundgen:::reformatFormants(NA)
#' soundgen:::reformatFormants('aau')
#' soundgen:::reformatFormants(c(500, 1500, 2500))
#' soundgen:::reformatFormants(list(f1 = 500, f2 = c(1500, 1700)))
#' soundgen:::reformatFormants(list(
#'      f1 = list(freq = 800, amp = 30),
#'      f2 = list(freq = c(1500, 1700, 2200), width = c(100, 150, 175))
#' ))
#'
#' f = list(f1 = c(550, 600), f2 = c(1100, NA, 1600), f2.5 = 2500, f3 = 3000)
#' soundgen:::reformatFormants(f)
#' soundgen:::reformatFormants(f, output = 'freqs')
#' soundgen:::reformatFormants(f, output = 'freqs', keepNonInteger = FALSE)
#'
#' soundgen:::reformatFormants(c(500, 1400), output = 'freqs')
#' soundgen:::reformatFormants(list(f1 = 500, f2 = 1400), output = 'freqs')
#' soundgen:::reformatFormants(list(f1 = c(570, 750),
#'   f2 = NA, f3 = c(2400, 2200, NA)), output = 'freqs')
reformatFormants = function(formants,
                            output = c('all', 'freqs')[1],
                            keepNonInteger = TRUE) {
  if (is.null(formants)) {
    formants = NA
  } else if (class(formants)[1] == 'character') {
    # "aui" etc - read off values from presets$M1
    formants = convertStringToFormants(formants)
  } else if (is.numeric(formants)) {
    # expand to full format from e.g. "formants = c(500, 1500, 2500)"
    freqs = formants
    formants = vector('list', length(freqs))
    names(formants) = paste0('f', 1:length(freqs))
    if (output == 'all') {
      for (f in 1:length(freqs)) {
        formants[[f]] = data.frame(time = 0,
                                   freq = freqs[f],
                                   amp = NA,
                                   width = getBandwidth(freqs[f]))
      }
    } else {
      # just the frequencies
      for (f in 1:length(freqs)) {
        formants[[f]] = data.frame(freq = freqs[f])
      }
    }
  }
  if (is.list(formants)) {
    if (is.null(names(formants))) {
      names(formants) = paste0('f', 1:length(formants))
    }
    for (f in 1:length(formants)) {
      formant = as.data.frame(formants[[f, drop = FALSE]])
      if (ncol(formant) == 1) colnames(formant) = 'freq'
      if (is.list(formant)) {
        if ('freq' %in% names(formant)) {
          if (output == 'all') {
            # expand to full format from e.g. f1 = list(freq = 550, amp = 30)
            if (is.null(formant$time)) {
              formant$time = seq(0, 1, length.out = nrow(formant))
            }
            if (is.null(formant$amp)) {
              formant$amp = NA
            }
            if (is.null(formant$width)) {
              formant$width = getBandwidth(formant$freq)
            }
          }
        }
      } else {
        # expand to full format from e.g. f1 = 550
        # (numbers are assumed to represent frequency)
        if (output == 'all') {
          formant = data.frame(
            time = seq(0, 1, length.out = length(formant)),
            freq = formant,
            amp = rep(NA, length(formant)),
            width = getBandwidth(formant)
          )
        } else {
          formant = data.frame(freq = as.numeric(formant))
        }
      }
      if (output == 'all') {
        # make sure columns are in the right order (for esthetics & debugging)
        formants[[f]] = formant[, c('time', 'freq', 'amp', 'width')]
      } else {
        formants[[f]] = formant
      }
    }
  } else if (!is.null(formants)) {
    if (any(!is.na(formants))) {
      stop('If defined, formants must be either a list or a string of characters
         from dictionary presets: a, o, i, e, u, 0 (schwa)')
    } else {
      return(NA)
    }
  }

  if (!keepNonInteger) {
    # remove non-integer (anti)formants, if any
    non_integer_formants = apply(as.matrix(names(formants)),
                                 1,
                                 function(x) {
                                   grepl('.', x, fixed = TRUE)
                                 })
    if (any(non_integer_formants)) formants =  formants[!non_integer_formants]
  }

  return(formants)
}



#' Get bandwidth
#'
#' Internal soundgen function.
#'
#' Calculates formant bandwidth as a function of formant frequencies using a
#' modified version of TMF-63 formula. Namely, above 500 Hz it follows the
#' original formula from Tappert, Martony, and Fant (TMF)-1963, and below 500 Hz
#' it applies a correction to allow for energy losses at low frequencies. See
#' Khodai-Joopari & Clermont (2002), "Comparison of formulae for estimating
#' formant bandwidths". Below 250 Hz the bandwidth is forces to drop again to
#' avoid very large values near zero (just guesswork!)
#' @param f a vector of formant frequencies, Hz
#' @keywords internal
#' @examples
#' f = 1:5000
#' plot(f, soundgen:::getBandwidth(f), type = 'l',
#'   xlab = 'Formant frequency, Hz', ylab = 'Estimated bandwidth, Hz')
getBandwidth = function(f) {
  b = rep(NA, length(f))
  f1 = which(f < 250)
  f2 = which(f >= 250 & f < 500)
  f3 = which(f >= 500)
  # just guesswork below 250 Hz - no data for such low freqs,
  # apart from elephant rumbles etc.
  b[f1] = 26.38541 - .042 * f[f1] + .0011 * f[f1] ^ 2
  # see Khodai-Joopari & Clermont 2002
  b[f2] =  52.08333 * (1 + (f[f2]-500) ^ 2/ 10 ^ 5)
  b[f3] = 50 * (1 + f[f3] ^ 2 / 6 / 10 ^ 6)
  # plot(f, b, type = 'l')
  return(b)
}


#' Prepare a list of formants
#'
#' Internal soundgen function.
#'
#' Takes a string of phonemes entered WITHOUT ANY BREAKS. Recognized phonemes in
#' the human preset dictionary: vowels "a" "o" "i" "e" "u" "0" (schwa);
#' consonants "s" "x" "j".
#' @param phonemeString a string of characters from the dictionary of phoneme
#'   presets, e.g., uaaaaii (short u - longer a - medium-long i)
#' @param speaker name of the preset dictionary to use
#' @return Returns a list of formant values, which can be fed directly into
#'   \code{\link{getSpectralEnvelope}}
#' @keywords internal
#' @examples
#' formants = soundgen:::convertStringToFormants(phonemeString = 'a')
#' formants = soundgen:::convertStringToFormants(
#'   phonemeString = 'au', speaker = 'M1')
#' formants = soundgen:::convertStringToFormants(
#'   phonemeString = 'aeui', speaker = 'F1')
#' formants = soundgen:::convertStringToFormants(
#'   phonemeString = 'aaeuiiiii', speaker = 'Chimpanzee')
convertStringToFormants = function(phonemeString, speaker = 'M1') {
  availablePresets = names(presets[[speaker]]$Formants$vowels)
  if (length(availablePresets) < 1) {
    warning(paste0('No phoneme presets found for speaker ', speaker,
                   '. Defaulting to M1'))
    speaker = 'M1'
    availablePresets = names(presets[[speaker]]$Formants$vowels)
  }
  input_phonemes = strsplit(phonemeString, "")[[1]]
  valid_phonemes = input_phonemes[input_phonemes %in% availablePresets]
  unique_phonemes = unique(valid_phonemes)
  if (length(valid_phonemes) < 1)
    return(NA)

  # for each input vowel, look up the corresponding formant values
  # in the presets dictionary and append to formants
  vowels = list()
  formantNames = character()
  for (v in 1:length(unique_phonemes)) {
    vowels[[v]] = presets[[speaker]]$Formants$vowels[unique_phonemes[v]][[1]]
    formantNames = c(formantNames, names(vowels[[v]]))
  }
  formantNames = sort(unique(formantNames))
  names(vowels) = unique_phonemes

  # make sure we have filled in info on all formants from the entire
  # sequence of vowels for each individual vowel
  for (v in 1:length(vowels)) {
    absentFormants = formantNames[!formantNames %in% names(vowels[[v]])]
    for (f in absentFormants) {
      closestFreq = unlist(sapply(vowels, function(x) x[f]))
      # names_stripped = substr(names(closestFreq),
      #                         nchar(names(closestFreq)) - 3,
      #                         nchar(names(closestFreq)))
      # closestFreq = closestFreq[which(names_stripped == 'freq')]
      vowels[[v]] [[f]] = as.numeric(na.omit(closestFreq))[1]
      # NB: instead of the last [1], ideally we should specify some intelligent
      # way to pick up the closest vowel with this missing formant, not just the
      # first one, but that's only a problem in long sequences of vowels with
      # really different numbers of formants (nasalization)
    }
  }

  # initialize a common list of exact formants
  formants = vector("list", length(formantNames))
  names(formants) = formantNames

  # for each vowel, append its formants to the common list
  for (v in 1:length(valid_phonemes)) {
    vowel = vowels[[valid_phonemes[v]]]
    for (f in 1:length(vowel)) {
      formantName = names(vowel)[f]
      formants[[formantName]] = c(formants[[formantName]], vowel[[f]])
    }
  }

  return(formants)
}


#' Lock to formants
#'
#' Internal soundgen function
#'
#' When f0 or another relatively strong harmonic is close to one of the
#' formants, the pitch contour is modified so as to "lock" it to this formant.
#' The relevant metric is energy gain (ratio of amplitudes before and after the
#' adjustment) penalized by the magnitude of the necessary pitch jump (in
#' semitones) and the amplitude of the locked harmonic relative to f0.
#' @param pitch pitch contour, numeric vector (normally pitch_per_gc)
#' @param specEnv spectral envelope as returned by getSpectralEnvelope
#' @param rolloffMatrix rolloff matrix as returned by getRolloff
#' @param formantSummary matrix of exact formant frequencies (formants in rows,
#'   time in columns)
#' @param lockProb the (approximate) proportion of sound affected by formant
#'   locking
#' @param minLength the minimum number of consecutive pitch values affected
#'   (shorter segments of formant locking are ignored)
#' @param plot if TRUE, plots the original and modified pitch contour
#' @keywords internal
#' @examples
#' n = 50
#' pitch = getSmoothContour(len = n, anchors = c(600, 2000, 1900, 400),
#'   thisIsPitch = TRUE, plot = TRUE)
#' rolloffMatrix = getRolloff(pitch_per_gc = pitch)
#' specEnv = getSpectralEnvelope(nr = 512, nc = length(pitch),
#'   formants = list(f1 = c(800, 1200), f2 = 2000, f3 = c(3500, 3200)),
#'   lipRad = 0, temperature = .00001, plot = TRUE)
#' formantSummary = t(data.frame(f1 = c(800, 1200), f2 = c(2000, 2000), f3 = c(3500, 3200)))
#' pitch2 = soundgen:::lockToFormants(pitch = pitch, specEnv = specEnv,
#'   rolloffMatrix = rolloffMatrix,
#'   formantSummary = formantSummary,
#'   lockProb = .5, minLength = 5, plot = TRUE)
#' pitch3 = soundgen:::lockToFormants(pitch = pitch, specEnv = specEnv,
#'   rolloffMatrix = rolloffMatrix,
#'   formantSummary = formantSummary,
#'   lockProb = list(time = c(0, .7, 1), value = c(0, 1, 0)),
#'   minLength = 5, plot = TRUE)
lockToFormants = function(pitch,
                          specEnv,
                          formantSummary,
                          rolloffMatrix = NULL,
                          lockProb = .1,
                          minLength = 3,
                          plot = FALSE) {
  if (!is.null(rolloffMatrix)) {
    nr = nrow(rolloffMatrix)
  } else {
    nr = 1
    rolloffMatrix = matrix(1)
  }
  n = length(pitch)
  if (is.list(lockProb)) {
    lockProb = getSmoothContour(
      anchors = lockProb,
      len = n,
      valueFloor = permittedValues['formantLocking', 'low'],
      valueCeiling = permittedValues['formantLocking', 'high']
    )
  }
  freqs = as.numeric(rownames(specEnv)) * 1000
  specEnv = interpolMatrix(
    specEnv,
    nr = nrow(specEnv),
    nc = n,
    interpol = 'approx'
  )
  # rownames(specEnv) = freqs
  formants = interpolMatrix(as.matrix(formantSummary), nc = n)

  # Calculate how much we can gain by locking a harmonic to a formant at each time point
  d = data.frame(pitch = pitch)
  d$pitch2 = d$pitch1 = d$pitch
  d[, c('idx_max_gain', 'gain', 'nearest_formant')] = NA
  for (i in 1:n){  # for each time point (normally glottal cycle in pitch_per_gc)
    if (FALSE) {
      plot(freqs, specEnv[, i], type = 'l')
      abline(v = pitch[i], lty = 3)
    }
    # formants = as.data.frame(seewave::fpeaks(cbind(freqs, specEnv[, i]),
    #                                          threshold = 1, plot = FALSE))

    # for each harmonic in rolloffMatrix
    temp = data.frame(harmonic = 1:nr, gain = NA, nearest_formant = NA)
    for (h in 1:nr) {  # for each harmonic
      nearest_formant = which.min(abs(formants[, i] - pitch[i] * h))
      nearest_formant_amp = specEnv[which.min(abs(formants[nearest_formant, i] - freqs)), i]
      idx_cur = which.min(abs(freqs - pitch[i] * h))
      amp_gain = nearest_formant_amp / specEnv[idx_cur, i]
      # amp_gain ~ how much E can be gained if locked to nearest formant
      dist_to_nearest_formant = max(1, 12 * abs(log2(pitch[i] * h) - log2(formants[nearest_formant, i])))
      # in semitones (anything closer than 1 semitone counts as 1 semitone to
      # standardize penalization, otherwise we divide by very small numbers and
      # thus inflate apparent gain)
      temp$gain[h] = amp_gain / dist_to_nearest_formant * rolloffMatrix[h, i]
      temp$nearest_formant[h] = formants[nearest_formant, i]
    }
    d$idx_max_gain[i] = which.max(temp$gain)
    d$gain[i] = temp$gain[d$idx_max_gain[i]]
    d$nearest_formant[i] = temp$nearest_formant[d$idx_max_gain[i]]
    # the prob of locking to the nearest formant is some function of the gain in E
    # penalized by dist_to_nearest_formant
  }

  # find the frames in which formant locking should be applied
  lockThreshold = as.numeric(quantile(d$gain, probs = 1 - lockProb))  # can be a vector
  lock_idx = which(d$gain > lockThreshold)
  d$pitch1[lock_idx] = d$nearest_formant[lock_idx] / d$idx_max_gain[lock_idx]

  # only preserve sufficiently long fragments of formant locking
  d$pitch2 = d$pitch1
  if (is.numeric(minLength) && minLength > 1) {
    d$modif = (d$pitch != d$pitch1)
    r = rle(d$modif)
    r = data.frame(len = r[[1]], val = r[[2]], idx = 1)
    if (r$len[1] < minLength) r$val[1] = FALSE
    for (i in 2:nrow(r)) {
      r$idx[i] = sum(r$len[1:(i - 1)]) + 1
      if (r$len[i] < minLength) {
        r$val[i] = FALSE
      }
    }
    r1 = r[r$val == TRUE, , drop = FALSE]
    d$modif1 = FALSE
    #modif1 = soundgen:::clumper(modif, minLength = 5)
    if (nrow(r1) >= 1) {
      for (j in 1:nrow(r1)) {
        d$modif1[r1$idx[j] : (r1$idx[j] + r1$len[j] - 1)] = TRUE
      }
    }
    # + exclude rapid changes b/w the harmonic to which to lock? Tricky b/c we need to access different formant frequencies again, or find some intelligent way of removing pitch jumps

    # un-modify short fragments, reverting to the original pitch
    no_modif = which(d$modif1 == FALSE)
    d$pitch2 = d$pitch1
    d$pitch2[no_modif] = d$pitch[no_modif]
  }


  # # median smoothing to remove weird pitch jumps
  # d$pitch2 = soundgen:::medianSmoother(data.frame(p = d$pitch1),
  #                                             smoothing_ww = 30,
  #                                             smoothingThres = 1)

  if (plot) {
    y_min = min(d[, c('pitch', 'pitch1', 'pitch2')]) / 1.1
    y_max = max(d[, c('pitch', 'pitch1', 'pitch2')]) * 1.1
    plot(d$pitch, type = 'l', lty = 2, col = 'red', ylim = c(y_min, y_max))
    points(d$pitch1, type = 'l', lty =3, col = 'blue')
    points(d$pitch2, type = 'l')
    # for (f in 1:length(formants)) {
    #   abline(h = formants[[f]]$freq, lty = 3)
    # }
  }

  invisible(d$pitch2)
}
