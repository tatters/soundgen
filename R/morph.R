#' Morph sounds
#'
#' Takes two formulas for synthesizing two target sounds with
#' \code{\link{soundgen}} and produces a number of intermediate forms (morphs),
#' attempting to go from one target sound to the other in a specified number of
#' equal steps.
#' @param formula1,formula2 lists of parameters for calling
#'   \code{\link{soundgen}} that produce the two target sounds between which
#'   morphing will occur. Character strings containing the full call to soundgen
#'   are also accepted (see examples)
#' @param nMorphs the length of morphing sequence, including target sounds
#' @param playMorphs if TRUE, the morphing sequence will be played
#' @param savePath if it is the path to an existing directory, morphs will be
#'   saved there as individual .wav files (defaults to NA)
#' @param samplingRate sampling rate of output, Hz. NB: must be the same as in
#'   \code{formula1} and \code{formula2}!
#' @return A list of two sublists (\code{$formulas} and \code{$sounds}), each
#'   sublist of length nMorphs. For ex., the formula for the second hybrid is
#'   \code{m$formulas[[2]]}, and the waveform is \code{m$sounds[[2]]}
#' @export
#' @examples
#' # write two formulas or copy-paste them from soundgen_app() or presets, for example:
#' m = morph(formula1 = list(repeatBout = 2),
#'           # equivalently: formula1 = 'soundgen(repeatBout = 2)',
#'           formula2 = presets$Misc$Dog_bark,
#'           nMorphs = 5, playMorphs = FALSE)
#'  # use $formulas to access formulas for each morph, $sounds for waveforms
#'  # m$formulas[[4]]
#'  # playme(m$sounds[[3]])
morph = function(formula1,
                 formula2,
                 nMorphs,
                 playMorphs = TRUE,
                 savePath = NA,
                 samplingRate = 16000) {
  # convert formulas to lists
  if (is.character(formula1)) {
    formula1 = paste0('list', substr(formula1, 9, nchar(formula1)))
    formula1 = eval(parse(text = formula1))
  }
  if (!is.list(formula1)) {
    stop('Formula1 must be either a list of pars like "list(sylLen = 500)"
         or a character string like "soundgen(sylLen = 500"')
  }

  if (is.character(formula2)) {
    formula2 = paste0('list', substr(formula2, 9, nchar(formula2)))
    formula2 = eval(parse(text = formula2))
  }
  if (!is.list(formula2)) {
    stop('Formula2 must be either a list of pars like "list(sylLen = 500)"
         or a character string like "soundgen(sylLen = 500"')
  }

  # which pars are different from the defaults of soundgen()?
  defaults = as.character(call('print', args(soundgen)))[2]
  defaults = substr(defaults, 11, (nchar(defaults) - 12))
  defaults = paste0('list(', defaults, ')')
  defaults = eval(parse(text = defaults))
  # NB: defaults = as.list(args(soundgen)) produces list elements of type "call" :((
  # exclude booleans
  defaults = defaults[!names(defaults) %in% c('plot', 'play', 'savePath', 'invalidArgAction')]

  notDefaultIdx_formula1 = notDefaultIdx_formula2 = NA
  if (length(formula1) > 0) {
    notDefaultIdx_formula1 = which(apply(matrix(1:length(formula1)), 1, function(x) {
      identical(formula1[[x]], defaults[[names(formula1)[x]]]) == FALSE
    }))
  }
  if (length(formula2) > 0) {
    notDefaultIdx_formula2 = which(apply(matrix(1:length(formula2)), 1, function(x) {
      identical(formula2[[x]], defaults[[names(formula2)[x]]]) == FALSE
    }))
  }

  # these pars have to be morphed:
  notDefaultNames = unique(c(names(formula1)[notDefaultIdx_formula1],
                             names(formula2)[notDefaultIdx_formula2]))
  notDefaultNames1 = names(formula1) [names(formula1) %in% notDefaultNames]
  notDefaultNames2 = names(formula2) [names(formula2) %in% notDefaultNames]

  # set up two formulas that contain the same number of pars, fill in with
  # specified values or, for values that are not specified, with defaults
  f1 = f2 = defaults[notDefaultNames]
  f1[notDefaultNames1] = formula1[match(notDefaultNames1, names(formula1))]
  f2[notDefaultNames2] = formula2[match(notDefaultNames2, names(formula2))]
  f2 = f2[match(names(f1), names(f2))]

  # fill in formant stuff if it is missing for one sound but defined for the other
  if (!is.list(f1$formants) & is.list(f2$formants)) {
    # class list means it's not NA or NULL
    f1$formants = f2$formants
    for (f in 1:length(f1$formants))
      f1$formants[[f]]$amp = 0
  }
  if (!is.list(f1$formantsNoise) & is.list(f2$formantsNoise)) {
    f1$formantsNoise = f2$formantsNoise
    for (f in 1:length(f1$formantsNoise))
      f1$formantsNoise[[f]]$amp = 0
  }
  if (!is.list(f2$formants) & is.list(f1$formants)) {
    f2$formants = f1$formants
    for (f in 1:length(f2$formants))
      f2$formants[[f]]$amp = 0
  }
  if (!is.list(f2$formantsNoise) & is.list(f1$formantsNoise)) {
    f2$formantsNoise = f1$formantsNoise
    for (f in 1:length(f2$formantsNoise))
      f2$formantsNoise[[f]]$amp = 0
  }

  # log-transform pitch and formant frequencies before morphing
  if ('pitchAnchors' %in% names(f1)) {
    if (is.numeric(f1$pitchAnchors$value)) f1$pitchAnchors$value = log(f1$pitchAnchors$value)
    if (is.numeric(f2$pitchAnchors$value)) f2$pitchAnchors$value = log(f2$pitchAnchors$value)
  }
  if ('formants' %in% names(f1)) {
    for (l in 1:length(f1$formants)) {
      if (is.numeric(f1$formants[[l]]$freq)) {
        f1$formants[[l]]$freq = log(f1$formants[[l]]$freq)
      }
    }
  }
  if ('formants' %in% names(f2)) {
    for (l in 1:length(f2$formants)) {
      if (is.numeric(f2$formants[[l]]$freq)) {
        f2$formants[[l]]$freq = log(f2$formants[[l]]$freq)
      }
    }
  }
  if ('formantsNoise' %in% names(f1)) {
    for (l in 1:length(f1$formantsNoise)) {
      if (is.numeric(f1$formantsNoise[[l]]$freq)) {
        f1$formantsNoise[[l]]$freq = log(f1$formantsNoise[[l]]$freq)
      }
    }
  }
  if ('formantsNoise' %in% names(f2)) {
    for (l in 1:length(f2$formantsNoise)) {
      if (is.numeric(f2$formantsNoise[[l]]$freq)) {
        f2$formantsNoise[[l]]$freq = log(f2$formantsNoise[[l]]$freq)
      }
    }
  }

  # f1 and f2 are now fully prepared: two target formulas,
  # both of the same length, including the same pars, in the same order

  # MORPH THE FORMULAS
  m = f1
  formulas = rep(list(f1), nMorphs)
  sounds = list()
  for (p in 1:length(f1)) {
    # morph each element of the formula list according to its type
    if (class(f1[[p]]) == 'numeric') {
      m[[p]] = seq(f1[[p]], f2[[p]], length.out = nMorphs)
    } else if (names(f1[p]) %in% c('formants', 'formantsNoise')) {
      m[[p]] = morphList(f1[[p]], f2[[p]], nMorphs = nMorphs)
    } else {
      m[[p]] = morphDF(f1[[p]], f2[[p]], nMorphs = nMorphs)
    }
    # a convoluted way of saving the output of morphFormants() in appropriate
    # slots in the output list
    for (h in 1:nMorphs) {
      formulas[[h]] [[p]] = m[[p]] [[h]]
    }
  }
  # END OF MORPHING THE FORMULAS

  # exponentiate pitch and formant frequencies after morphing
  if ('pitchAnchors' %in% names(f1)) {
    for (h in 1:nMorphs) {
      if (is.numeric(formulas[[h]]$pitchAnchors$value)) {
        formulas[[h]]$pitchAnchors$value = exp(formulas[[h]]$pitchAnchors$value)
      }
    }
  }
  if ('formants' %in% names(f1)) {
    for (h in 1:nMorphs) {
      for (l in 1:length(f1$formants)) {
        if (is.numeric(formulas[[h]]$formants[[l]]$freq)) {
          formulas[[h]]$formants[[l]]$freq =
            exp(formulas[[h]]$formants[[l]]$freq)
        }
      }
    }
  }
  if ('formantsNoise' %in% names(f1)) {
    for (h in 1:nMorphs) {
      for (l in 1:length(f1$formantsNoise)) {
        if (is.numeric(formulas[[h]]$formantsNoise[[l]]$freq)) {
          formulas[[h]]$formantsNoise[[l]]$freq =
            exp(formulas[[h]]$formantsNoise[[l]]$freq)
        }
      }
    }
  }

  # generate the morphs based on the formulas
  for (h in 1:nMorphs) {
    sounds[[h]] = do.call(soundgen, formulas[[h]])
    if (playMorphs) playme(sounds[[h]], samplingRate = samplingRate)
    if (!is.na(savePath)){
      filename = paste0(savePath, 'morph_', h, '.wav')
      seewave::savewav(sounds[[h]], f = samplingRate, filename = filename)
    }
  }
  return (list(formulas = formulas, sounds = sounds))
}
