#' Get formant dispersion
#'
#' Internal soundgen function.
#'
#' Estimates formant dispersion based on one or more formant frequencies.
#' @inheritParams estimateVTL
#' @keywords internal
#' @examples
#' soundgen:::getFormantDispersion(c(500, 1400, 2900, 3500),
#'   method = 'meanDispersion')
#' soundgen:::getFormantDispersion(c(500, 1400, 2900, 3500),
#'   method = 'regression')
#' soundgen:::getFormantDispersion(c(500, 1400, NA, 3500))
#' \dontrun{
#' nIter = 100  # nIter = 10000 for better results
#' speedSound = 35400
#' out = data.frame(vtl = runif(nIter, 5, 70),
#'                  nFormants = round(runif(nIter, 1, 10)),
#'                  noise = runif(nIter, 0, .2),
#'                  vtl_est = rep(NA, nIter),
#'                  error = rep(NA, nIter))
#' for (i in 1:nIter) {
#'   a = 1:out$nFormants[i]
#'   formants = sort(speedSound * (2 * a - 1) / (4 * out$vtl[i]) * rnorm(n = length(a),
#'                                                                  mean = 1,
#'                                                                  sd = out$noise[i]))
#'   disp = soundgen:::getFormantDispersion(formants, method = 'fast')
#'   out$vtl_est[i] = speedSound / 2 / disp
#'   out$error[i] = (out$vtl[i] -  out$vtl_est[i]) / out$vtl[i]
#' }
#'
#' library(ggplot2)
#' ggplot(out, aes(x = nFormants, y = error)) +
#'   geom_point(alpha = .1) +
#'   geom_smooth() +
#'   theme_bw()
#' ggplot(out, aes(x = noise, y = error)) +
#'   geom_point(alpha = .1) +
#'   geom_smooth() +
#'   theme_bw()
#' }
getFormantDispersion = function(formants,
                                method = c('meanDispersion', 'regression')[2],
                                speedSound = 35400) {
  if (!is.numeric(formants) | length(formants) < 1) return(NA)
  if (method == 'meanDispersion') {
    l = length(formants)
    if (l > 1) {
      formantDispersion = mean(diff(formants), na.rm = TRUE)
    } else {
      formantDispersion = 2 * formants
    }
  } else if (method == 'regression') {
    # Reby et al. (2005) "Red deer stags use formants..."
    deltaF = (2 * (1:length(formants)) - 1) / 2
    # plot(deltaF, formants)
    mod = suppressWarnings(lm(formants ~ deltaF - 1))
    # NB: no intercept, i.e. forced to pass through 0
    formantDispersion = suppressWarnings(summary(mod)$coef[1])
  }
  return(formantDispersion)
}


#' Estimate vocal tract length
#'
#' Estimates the length of vocal tract based on formant frequencies, assuming
#' that the vocal tract can be modeled as a tube open at both ends.
#'
#' If \code{method = 'meanFormant'}, vocal tract length (VTL) is calculated
#' separately for each formant as \eqn{(2 * formant_number - 1) * speedSound /
#' (4 * formant_frequency)}, and then the resulting VTLs are averaged. If
#' \code{method = 'meanDispersion'}, formant dispersion is calculated as the
#' mean distance between formants, and then VTL is calculated as \eqn{speed of
#' sound / 2 / formant dispersion}. If \code{method = 'regression'}, formant
#' dispersion is estimated using the regression method described in Reby et al.
#' (2005) "Red deer stags use formants as assessment cues during intrasexual
#' agonistic interactions". For a review of these and other VTL-related summary
#' measures of formant frequencies, refer to Pisanski et al. (2014) "Vocal
#' indicators of body size in men and women: a meta-analysis". See also
#' \code{\link{schwa}} for VTL estimation with additional information on formant
#' frequencies.
#'
#' @seealso \code{\link{schwa}}
#'
#' @param formants a character string like "aaui" referring to default presets
#'   for speaker "M1"; a vector of formant frequencies; or a list of formant
#'   times, frequencies, amplitudes, and bandwidths, with a single value of each
#'   for static or multiple values of each for moving formants
#' @param method the method of estimating vocal tract length (see details)
#' @param speedSound speed of sound in warm air, cm/s. Stevens (2000) "Acoustic
#'   phonetics", p. 138
#' @param checkFormat if TRUE, expands shorthand format specifications into the
#'   canonical form of a list with four components: time, frequency, amplitude
#'   and bandwidth for each format (as returned by the internal function
#'   \code{reformatFormants})
#' @return Returns the estimated vocal tract length in cm.
#' @export
#' @examples
#' estimateVTL(NA)
#' estimateVTL(500)
#' estimateVTL(c(600, 1850, 3100))
#' estimateVTL(formants = list(f1 = 600, f2 = 1650, f3 = 2400))
#'
#' # Missing values are OK
#' estimateVTL(c(600, 1850, 3100, NA, 5000))
#'
#' # For moving formants, frequencies are averaged over time,
#' # i.e. this is identical to c(600, 1650, 2400)
#' estimateVTL(formants = list(f1 = c(500, 700), f2 = 1650, f3 = c(2200, 2600)))
#'
#' # Note that VTL estimates based on the commonly reported 'meanDispersion'
#' # depend only on the first and last formant
#' estimateVTL(c(500, 1400, 2800, 4100), method = 'meanDispersion')
#' estimateVTL(c(500, 1100, 2300, 4100), method = 'meanDispersion') # identical
#' # ...but
#' estimateVTL(c(500, 1400, 2800, 4100), method = 'meanFormant')
#' estimateVTL(c(500, 1100, 2300, 4100), method = 'meanFormant') # much longer
#'
#' \dontrun{
#' # Compare the results produced by the three methods
#' nIter = 1000
#' out = data.frame(meanFormant = rep(NA, nIter), meanDispersion = NA, regression = NA)
#' for (i in 1:nIter) {
#'   # generate a random formant configuration
#'   f = runif(1, 300, 900) + (1:6) * rnorm(6, 1000, 200)
#'   out$meanFormant[i]    = estimateVTL(f, method = 'meanFormant')
#'   out$meanDispersion[i] = estimateVTL(f, method = 'meanDispersion')
#'   out$regression[i]     = estimateVTL(f, method = 'regression')
#' }
#' pairs(out)
#' cor(out)
#' # 'meanDispersion' is pretty different, while 'meanFormant' and 'regression'
#' # give broadly comparable results
#' }
estimateVTL = function(formants,
                       method = c('meanFormant', 'meanDispersion', 'regression')[3],
                       speedSound = 35400,
                       checkFormat = TRUE) {
  if (!method %in% c('meanFormant', 'meanDispersion', 'regression')) {
    stop('Invalid method; valid methods are: meanFormant, meanDispersion, regression')
  }
  if (checkFormat) {
    formants = reformatFormants(formants)
  }
  if (is.list(formants)) {
    if (is.numeric(formants[[1]]$freq)) {
      # if we don't know vocalTract, but at least one formant is defined,
      # we guess the length of vocal tract
      formant_freqs = unlist(sapply(formants, function(f) mean(f$freq)))
      non_integer_formants = apply(as.matrix(names(formant_freqs)),
                                   1,
                                   function(x) {
                                     grepl('.', x, fixed = TRUE)
                                   })
      formant_freqs = formant_freqs[!non_integer_formants]
      if (method == 'meanFormant') {
        vtls = (2 * (1:length(formant_freqs)) - 1) * speedSound / 4 / formant_freqs
        vocalTract = mean(vtls, na.rm = TRUE)
      } else if (method %in% c('meanDispersion', 'regression')) {
        formantDispersion = getFormantDispersion(formant_freqs,
                                                 speedSound = speedSound,
                                                 method = method)
        vocalTract = ifelse(
          is.numeric(formantDispersion),
          speedSound / 2 / formantDispersion,
          speedSound / 4 / formants$f1$freq
        )
      }
    }
  } else {
    vocalTract = NA
  }
  return(vocalTract)
}


#' Schwa-related formant conversion
#'
#' This function performs several conceptually related types of conversion of
#' formant frequencies in relation to the neutral schwa sound based on the
#' one-tube model of the vocal tract. Case 1: if we know vocal tract length
#' (VTL) but not formant frequencies, \code{schwa()} estimates formants
#' corresponding to a neutral schwa sound in this vocal tract, assuming that it
#' is perfectly cylindrical. Case 2: if we know the frequencies of a few lower
#' formants, \code{schwa()} estimates the deviation of observed formant
#' frequencies from the neutral values expected in a perfectly cylindrical vocal
#' tract (based on the VTL as specified or as estimated from formant
#' dispersion). Case 3: if we want to geneate a sound with particular relative
#' formant frequencies (e.g. high F1 and low F2 relative to the schwa for this
#' vocal tract), \code{schwa()} calculates the corresponding formant frequencies
#' in Hz. See examples below for an illustration of these three suggested uses.
#'
#' Algorithm: the expected formant dispersion is given by \eqn{speedSound / (2 *
#' vocalTract)}, and F1 is expected at half the value of formant dispersion. See
#' e.g. Stevens (2000) "Acoustic phonetics", p. 139. Basically, we estimate
#' vocal tract length and see if each formant is higher or lower than expected
#' for this vocal tract. For this to work, we have to know either the
#' frequencies of enough formants (not just the first two) or the true length of
#' the vocal tract. See also \code{\link{estimateVTL}} on the algorithm for
#' estimating formant dispersion if VTL is not known (note that \code{schwa}
#' calls \code{\link{estimateVTL}} with the option \code{method = 'regression'}.
#'
#' @seealso \code{\link{estimateVTL}}
#'
#' @return Returns a list with the following components: \describe{
#'   \item{vtl_measured}{VTL as provided by the user, cm}
#'   \item{vocalTract_apparent}{VTL estimated based on formants frequencies
#'   provided by the user, cm}
#'   \item{formantDispersion}{average distance between formants, Hz}
#'   \item{ff_measured}{formant frequencies as
#'   provided by the user, Hz}
#'   \item{ff_schwa}{formant frequencies corresponding
#'   to a neutral schwa sound in this vocal tract, Hz}
#'   \item{ff_theoretical}{formant frequencies corresponding to the
#'   user-provided relative formant frequencies, Hz}
#'   \item{ff_relative}{deviation of formant frequencies from those expected for
#'   a schwa, \% (e.g. if the first ff_relative is -25, it means that F1 is 25\%
#'   lower than expected for a schwa in this vocal tract)}
#'   \item{ff_relative_semitones}{deviation of formant frequencies from those expected for
#'   a schwa, semitones}
#' }
#' @param formants a numeric vector of observed (measured) formant frequencies,
#'   Hz
#' @param vocalTract the length of vocal tract, cm
#' @param formants_relative a numeric vector of target relative formant
#'   frequencies, \% deviation from schwa (see examples)
#' @param nForm the number of formants to estimate (integer)
#' @inheritParams getSpectralEnvelope
#' @export
#' @examples
#' ## CASE 1: known VTL
#' # If vocal tract length is known, we calculate expected formant frequencies
#' schwa(vocalTract = 17.5)
#' schwa(vocalTract = 13, nForm = 5)
#'
#' ## CASE 2: known (observed) formant frequencies
#' # Let's take formant frequencies in three vocalizations
#' #       (/a/, /i/, /roar/) by the same male speaker:
#' formants_a = c(860, 1430, 2900, 4200, 5200)
#' s_a = schwa(formants = formants_a)
#' s_a
#' # We get an estimate of VTL (s_a$vtl_apparent = 15.2 cm),
#' #   same as with estimateVTL(formants_a)
#' # We also get theoretical schwa formants: s_a$ff_schwa
#' # And we get the difference (% and semitones) in observed vs expected
#' #   formant frequencies: s_a[c('ff_relative', 'ff_relative_semitones')]
#' # [a]: F1 much higher than expected, F2 slightly lower
#'
#' formants_i = c(300, 2700, 3400, 4400, 5300, 6400)
#' s_i = schwa(formants = formants_i)
#' s_i
#' # The apparent VTL is slightly smaller (14.5 cm)
#' # [i]: very low F1, very high F2
#'
#' formants_roar = c(550, 1000, 1460, 2280, 3350,
#'                   4300, 4900, 5800, 6900, 7900)
#' s_roar = schwa(formants = formants_roar)
#' s_roar
#' # Note the enormous apparent VTL (22.5 cm!)
#' # (lowered larynx and rounded lips exaggerate the apparent size)
#' # s_roar$ff_relative: high F1 and low F2-F4
#'
#' schwa(formants = formants_roar[1:4])
#' # based on F1-F4, apparent VTL is almost 28 cm!
#' # Since the lowest formants are the most salient,
#' # the apparent size is exaggerated even further
#'
#' # If you know VTL, a few lower formants are enough to get
#' #   a good estimate of the relative formant values:
#' schwa(formants = formants_roar[1:4], vocalTract = 19)
#' # NB: in this case theoretical and relative formants are calculated
#' #  based on user-provided VTL (vtl_measured) rather than vtl_apparent
#'
#' ## CASE 3: from relative to absolute formant frequencies
#' # Say we want to generate a vowel sound with F1 20% below schwa
#' #    and F2 40% above schwa, with VTL = 15 cm
#' s = schwa(formants_relative = c(-20, 40), vocalTract = 15)
#' # s$ff_schwa gives formant frequencies for a schwa, while
#' #   s$ff_theoretical gives formant frequencies for a sound with
#' #   target relative formant values (low F1, high F2)
#' schwa(formants = s$ff_theoretical)
schwa = function(formants = NULL,
                 vocalTract = NULL,
                 formants_relative = NULL,
                 nForm = 8,
                 speedSound = 35400) {
  # check input
  if (is.null(formants) & is.null(vocalTract)) {
    stop('Please pecify formant frequencies and/or vocal tract length')
  }
  if (!is.null(formants)) {
    if (!is.numeric(formants) | any(formants < 0)) {
      stop('formants must be positive numbers (Hz)')
    }
  }
  if (!is.null(formants_relative)) {
    if (!is.numeric(formants_relative)) {
      stop('formants_relative must be positive numbers (Hz)')
    }
  }
  if (!is.null(vocalTract)) {
    if (!is.numeric(vocalTract) | vocalTract < 0) {
      stop('vocalTract must be a positive number (cm)')
    }
  }
  if (!is.null(formants_relative)) {
    if (is.null(vocalTract)) {
      stop('vocalTract must be specified to convert relative formants to Hz')
    }
  }

  if (is.null(formants_relative)) {
    ## we don't know and want to calculate formants_relative
    # calculate formant dispersion and apparent VTL
    if (is.null(formants)) {
      # we don't know formants, we know VTL
      formantDispersion = speedSound / (2 * vocalTract)
      vocalTract_apparent = NULL
    } else {
      # we know formants
      if (is.null(vocalTract)) {
        # we don't know VTL
        formantDispersion = getFormantDispersion(formants,
                                                 speedSound = speedSound,
                                                 method = 'regression')
        vocalTract_apparent = speedSound / (2 * formantDispersion)
      } else {
        # we know VTL
        formantDispersion_apparent = getFormantDispersion(
          formants,
          speedSound = speedSound,
          method = 'regression'
        )
        formantDispersion = speedSound / (2 * vocalTract)
        vocalTract_apparent = speedSound / (2 * formantDispersion_apparent)
      }
    }

    # calculate relative formant frequencies
    if (is.null(formants)) {
      idx = 1:nForm
    } else {
      idx = 1:length(formants)
    }
    ff_schwa = (2 * idx - 1) / 2 * formantDispersion
    ff_relative = (formants / ff_schwa - 1) * 100
    ff_relative_semitones = HzToSemitones(formants) - HzToSemitones(ff_schwa)
    ff_theoretical = NULL
  } else {
    ## we know formants_relative and vocalTract and want to convert ff to Hz

    # make sure formants_relative is at least nForm long
    if (length(formants_relative) < nForm) {
      formants_relative = c(formants_relative,
                            rep(0, nForm - length(formants_relative)))
    }

    # calculate formants in Hz
    formantDispersion = speedSound / (2 * vocalTract)
    idx = 1:length(formants_relative)
    ff_schwa = (2 * idx - 1) / 2 * formantDispersion
    ff_theoretical = ff_schwa * (1 + formants_relative / 100)
    vocalTract_apparent = NULL
    ff_relative = formants_relative
    ff_relative_semitones = HzToSemitones(ff_theoretical) - HzToSemitones(ff_schwa)
  }

  # prepare the output
  out = list(vtl_measured = vocalTract,
             vtl_apparent = vocalTract_apparent,
             formantDispersion = formantDispersion,
             ff_measured = formants,
             ff_schwa = ff_schwa,
             ff_theoretical = ff_theoretical,
             ff_relative = ff_relative,
             ff_relative_semitones = ff_relative_semitones)
  # do not return empty elements
  out = out[lapply(out, length) > 0]
  return(out)
}

