### Functions for analyzing subjective loudness

#' Get loudness folder
#'
#' Deprecated; use \code{\link{getLoudness}} instead
#' @param ... any input parameters
getLoudnessFolder = function(...) {
  message('getLoudnessFolder() is deprecated; please use getLoudness() instead')
}


#' Get loudness
#'
#' Estimates subjective loudness per frame, in sone. Based on EMBSD speech
#' quality measure, particularly the matlab code in Yang (1999) and Timoney et
#' al. (2004). Note that there are many ways to estimate loudness and many other
#' factors, ignored by this model, that could influence subjectively experienced
#' loudness. Please treat the output with a healthy dose of skepticism! Also
#' note that the absolute value of calculated loudness critically depends on the
#' chosen "measured" sound pressure level (SPL). \code{getLoudness} estimates
#' how loud a sound will be experienced if it is played back at an SPL of
#' \code{SPL_measured} dB. The most meaningful way to use the output is to
#' compare the loudness of several sounds analyzed with identical settings or of
#' different segments within the same recording.
#'
#' Algorithm: calibrates the sound to the desired SPL (Timoney et al., 2004),
#' extracts a \code{\link{spectrogram}}, converts to bark scale
#' (\code{\link[tuneR]{audspec}}), spreads the spectrum to account for frequency
#' masking across the critical bands (Yang, 1999), converts dB to phon by using
#' standard equal loudness curves (ISO 226), converts phon to sone (Timoney et
#' al., 2004), sums across all critical bands, and applies a correction
#' coefficient to standardize output. Calibrated so as to return a loudness of 1
#' sone for a 1 kHz pure tone with SPL of 40 dB.
#'
#' @seealso \code{\link{getLoudnessFolder}} \code{\link{getRMS}}
#'   \code{\link{analyze}}
#'
#' @inheritParams spectrogram
#' @inheritParams analyze
#' @param SPL_measured sound pressure level at which the sound is presented, dB
#' @param Pref reference pressure, Pa (currently has no effect on the estimate)
#' @param spreadSpectrum if TRUE, applies a spreading function to account for
#'   frequency masking
#' @param mar margins of the spectrogram
#' @param ... other plotting parameters passed to \code{\link{spectrogram}}
#' @return Returns a list: \describe{ \item{specSone}{spectrum in bark-sone (one
#'   per file): a matrix of loudness values in sone, with frequency on the bark
#'   scale in rows and time (STFT frames) in columns} \item{loudness}{a vector
#'   of loudness in sone per STFT frame (one per file)} \item{summary}{a
#'   dataframe of summary loudness measures (one row per file)} }
#' @references \itemize{
#'   \item ISO 226 as implemented by Jeff Tackett (2005) on
#'   https://www.mathworks.com/matlabcentral/fileexchange/
#'   7028-iso-226-equal-loudness-level-contour-signal \item Timoney, J.,
#'   Lysaght, T., Schoenwiesner, M., & MacManus, L. (2004). Implementing
#'   loudness models in matlab. \item Yang, W. (1999). Enhanced Modified Bark
#'   Spectral Distortion (EMBSD): An Objective Speech Quality Measure Based on
#'   Audible Distortion and Cognitive Model. Temple University. }
#' @export
#' @examples
#' sounds = list(
#'   white_noise = runif(8000, -1, 1),
#'   white_noise2 = runif(8000, -1, 1) / 2,  # ~6 dB quieter
#'   pure_tone_1KHz = sin(2*pi*1000/16000*(1:8000))  # pure tone at 1 kHz
#' )
#' l = getLoudness(
#'     x = sounds, samplingRate = 16000, scale = 1,
#'     windowLength = 20, step = NULL,
#'     overlap = 50, SPL_measured = 40,
#'     Pref = 2e-5, plot = FALSE)
#' l$summary
#' # white noise (sound 1) is twice as loud as pure tone at 1 KHz (sound 3),
#' # and note that the same white noise with lower amplitude has lower loudness
#' # (provided that "scale" is specified)
#' # compare: lapply(sounds, range)
#'
#' \dontrun{
#' s = soundgen()
#' # playme(s)
#' l1 = getLoudness(s, samplingRate = 16000, SPL_measured = 70)
#' l1$summary
#' # The estimated loudness in sone depends on target SPL
#' l2 = getLoudness(s, samplingRate = 16000, SPL_measured = 40)
#' l2$summary
#'
#' # ...but not (much) on windowLength and samplingRate
#' l3 = getLoudness(s, samplingRate = 16000, SPL_measured = 40, windowLength = 50)
#' l3$summary
#'
#' # input can be an audio file...
#' getLoudness('~/Downloads/temp/032_ut_anger_30-m-roar-curse.wav')
#'
#' ...or a folder with multiple audio files
#' getLoudness('~/Downloads/temp2', plot = FALSE)$summary
#' # Compare:
#' analyze('~/Downloads/temp2', pitchMethods = NULL,
#'         plot = FALSE, silence = 0)$summary$loudness_mean
#' # (per STFT frame; should be similar if silence = 0, because
#' # otherwise analyze() discards frames considered silent)
#'
#' # custom summaryFun
#' ran = function(x) diff(range(x))
#' getLoudness('~/Downloads/temp2', plot = FALSE,
#'             summaryFun = c('mean', 'ran'))$summary
#' }
getLoudness = function(x,
                       samplingRate = NULL,
                       scale = NULL,
                       from = NULL,
                       to = NULL,
                       windowLength = 50,
                       step = NULL,
                       overlap = 50,
                       SPL_measured = 70,
                       Pref = 2e-5,
                       spreadSpectrum = TRUE,
                       summaryFun = c('mean', 'median', 'sd'),
                       reportEvery = NULL,
                       plot = TRUE,
                       savePlots = NULL,
                       ylim = NULL,
                       width = 900,
                       height = 500,
                       units = 'px',
                       res = NA,
                       mar = c(5.1, 4.1, 4.1, 4.1),
                       ...) {
  ## Prepare a list of arguments to pass to .getLoudness()
  myPars = c(as.list(environment()), list(...))
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'savePlots', 'reportEvery', 'summaryFun')]

  # analyze
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.getLoudness',
    myPars = myPars,
    reportEvery = reportEvery,
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots)) {
    htmlPlots(
      htmlFile = paste0(pa$input$savePlots, '00_clickablePlots_loudness.html'),
      plotFiles = paste0(pa$input$savePlots, pa$input$filenames_base, "_loudness.png"),
      audioFiles = pa$input$filenames,
      width = paste0(width, units))
  }

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in 1:pa$input$n) {
      if (!pa$input$failed[i]) {
        temp[[i]] = summarizeAnalyze(
          data.frame(loudness = pa$result[[i]]$loudness),
          summaryFun = summaryFun,
          var_noSummary = NULL)
      }
    }
    idx_failed = which(pa$input$failed)
    if (length(idx_failed) > 0) {
      idx_ok = which(!pa$input$failed)
      if (length(idx_ok) > 0) {
        filler = temp[[idx_ok[1]]] [1, ]
        filler[1, ] = NA
      } else {
        stop('Failed to analyze any input')
      }
      for (i in idx_failed) temp[[i]] = filler
    }
    mysum_all = cbind(data.frame(file = pa$input$filenames_base),
                      do.call('rbind', temp))
  } else {
    mysum_all = NULL
  }

  if (pa$input$n == 1) {
    # unlist specSone and loudness
    specSone = pa$result[[1]]$specSone
    loudness = pa$result[[1]]$loudness
  } else {
    specSone = lapply(pa$result, function(x) x[['specSone']])
    loudness = lapply(pa$result, function(x) x[['loudness']])
  }

  invisible(list(
    specSone = specSone,
    loudness = loudness,
    summary = mysum_all
  ))
}


#' Loudness per sound
#'
#' Internal soundgen function
#' @inheritParams getLoudness
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.getLoudness = function(audio,
                        windowLength = 50,
                        step = NULL,
                        overlap = 50,
                        SPL_measured = 70,
                        Pref = 2e-5,
                        spreadSpectrum = TRUE,
                        plot = TRUE,
                        savePlots = NULL,
                        ylim = NULL,
                        width = 900,
                        height = 500,
                        units = 'px',
                        res = NA,
                        mar = c(5.1, 4.1, 4.1, 4.1),
                        ...) {
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  if (audio$samplingRate < 2000) {
    warning(paste('samplingRate of', audio$samplingRate, 'is too low;',
                  'need a Nyquist of at least 8 barks (1 kHz)'))
    return(NA)
  }

  # scale to dB SPL
  # range(audio$sound)
  sound_scaled = scaleSPL(audio$sound,
                          scale = audio$scale,
                          SPL_measured = SPL_measured,
                          Pref = Pref)
  # range(sound_scaled)
  # log10(sqrt(mean(sound_scaled ^ 2))) * 20
  # (should be the same as SPL_measured w/o scale adjustment)

  # get power spectrum
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_base, "_loudness.png"),
        width = width, height = height, units = units, res = res)
  }
  powerSpec = tuneR::powspec(
    sound_scaled, sr = audio$samplingRate,
    wintime = windowLength / 1000, steptime = step / 1000,
    dither = FALSE)
  # range(log10(powerSpec) * 10)

  # normalize power spectrum by the size of STFT frame
  # windowLength_points = floor(windowLength / 1000 * audio$samplingRate / 2) * 2
  powerSpec_scaled = powerSpec / nrow(powerSpec)  # same as * 2 / windowLength_points
  # range(log10(powerSpec_scaled) * 10)
  # image(t(powerSpec_scaled))

  # get auditory spectrum
  audSpec = tuneR::audspec(
    powerSpec_scaled,
    sr = audio$samplingRate,
    fbtype = 'bark')$aspectrum
  # image(t(audSpec))
  # range(log10(audSpec) * 10)
  # plot(audSpec[, 1], type = 'l')
  # plot(log10(audSpec[, 1]) * 10, type = 'l')

  # throw away very high frequencies
  if (audio$samplingRate > 44100) {
    message(paste('Sampling rate above 44100, but discarding frequencies above 27 barks',
                  '(27 KHz) as inaudible to humans'))
    audSpec = audSpec[1:27, ]  # max 27 barks
  }

  # apply spreading function (NB: linear, not dB scale!)
  if (spreadSpectrum) {
    nonZeroCols = which(colSums(audSpec) > 0)
    for (c in nonZeroCols) {
      audSpec[, c] = spreadSpec(audSpec[, c])
    }
    # image(t(audSpec))
    # range(log10(audSpec) * 10)
    # plot(audSpec[, 1], type = 'l')
    # plot(log10(audSpec[, 1]) * 10, type = 'l')
  }

  # convert spectrum to sone
  specSone = matrix(0, nrow = nrow(audSpec), ncol = ncol(audSpec))
  for (i in 1:ncol(specSone)) {
    # spectrum in dB SPL
    y = 10 * log10(audSpec[, i])
    # plot(y, type = 'b')

    # dB SPL to phons (8 barks ~= 1 kHz, reference value for equal loudness curves)
    n_phonCurve = which.min(abs(y[8] - as.numeric(names(phonCurves))))
    # correction curve for frame i
    curve = phonCurves[[n_phonCurve]][1:length(y), ]
    y_phon = y + curve$spl[8] - curve$spl
    # plot(y_phon, type = 'b')

    # ignore frequency bins below hearing threshold
    y_phon[y_phon < curve$hearingThres_dB | y_phon < 0] = 0

    # phons to sone
    specSone[, i] = phon2sone(y_phon)
    # plot(specSone[, i], type = 'b')
  }
  # image(t(specSone))
  loudness = apply(specSone, 2, sum)

  # empirical normalization (see commented-out code below the function)
  loudness = loudness / (5.73 +  6.56 * windowLength ^ .35) /
    (.0357 + .0345 * audio$samplingRate ^ .3113)

  # plotting
  if (plot) {
    if (is.null(ylim)) ylim = c(0, audio$samplingRate / 2 / 1000)
    loudness_norm = loudness / max(loudness) * ylim[2] * 1000
    .spectrogram(
      audio[which(names(audio) != 'savePlots')],
      windowLength = windowLength, step = step,
      output = 'original', normalize = FALSE,
      padWithSilence = FALSE,
      plot = TRUE, mar = mar, ylim = ylim,
      extraContour = list(x = loudness_norm, col = 'blue'),
      ...)
    if (is.character(audio$savePlots)) dev.off()
  }
  invisible(list(specSone = specSone, loudness = loudness))
}


# Where is the ^5/3 in loudness adjustment coming from?
# s = '~/Downloads/temp/145_ut_effort_24.wav'
# s1 = tuneR::readWave(s)
# s2 = as.numeric(s1@left)
# range(s2)
#
# mean(getLoudness(s2, samplingRate = s1@samp.rate, scale = 2^(s1@bit-1), plot = FALSE)$loudness)
# mean(getLoudness(s2 / 10, samplingRate = s1@samp.rate, scale = 2^(s1@bit-1), plot = FALSE)$loudness)
#
# out = data.frame(coef = seq(0, 1, length.out = 100), loud = NA)
# for (i in 1:nrow(out)) {
#   out$loud[i] = mean(getLoudness(s2 * out$coef[i],
#                                  samplingRate = s1@samp.rate,
#                                  scale = 2^(s1@bit-1),
#                                  plot = FALSE)$loudness)
# }
# plot(out, type = 'l')
#
# mod = nls(loud ~ a + b * coef ^ c, out, start = list(a = 0, b = 1, c = .5))
# plot(out, type = 'l')
# points(out$coef, predict(mod, list(coef = out$coef)), type = 'b', col = 'green')
# summary(mod)  # a = 0, b = 12, c = 0.6
# # so loud1/loud2 = coef1^c / coef2^c = (coef1/coef2)^c, where c = 0.6,
# # so coef1/coef2 = (loud1/loud2)^(1/0.6) = (loud1/loud2)^(5/3)

# ## EMPIRICAL CALIBRATION OF LOUDNESS (SONE) RETURNED BY getLoudness()
# # Simple linear scaling can correct for a given windowLength, but
# # how does scaling coef depend on windowLength and samplingRate?
# wl = expand.grid(windowLength = seq(10, 150, by = 10),
#                  samplingRate = c(16000, 24000, 32000, 44000))
# wl$windowLength_points = 2 ^ (ceiling(log2(wl$windowLength * samplingRate / 1000)))
# for (i in 1:nrow(wl)) {
#   sound = sin(2*pi*1000/wl$samplingRate[i]*(1:20000))
#   wl$loudness[i] = getLoudness(x = sound, samplingRate = wl$samplingRate[i], windowLength = wl$windowLength[i], step = NULL, overlap = 0, SPL_measured = 40, Pref = 2e-5, plot = FALSE)$loudness[1]
# }
# # plot(wl$windowLength, wl$loudness)
# library(ggplot2)
# ggplot(wl, aes(x = windowLength, y = loudness, color = as.factor(samplingRate))) +
#   geom_point() +
#   geom_line()
#
# # account for windowLength
# samplingRate = 44100
# sound = sin(2*pi*1000/samplingRate*(1:20000))
# wl = data.frame(windowLength = seq(5, 200, by = 5))
# wl$windowLength_points = 2 ^ (ceiling(log2(wl$windowLength * samplingRate / 1000)))
# for (i in 1:nrow(wl)) {
#   wl$loudness[i] = getLoudness(x = sound, samplingRate = samplingRate, windowLength = wl$windowLength[i], step = NULL, overlap = 0, SPL_measured = 40, Pref = 2e-5, plot = FALSE)$loudness[1]
# }
# plot(wl$windowLength, wl$loudness)
# # wl
# # plot(wl$windowLength_points, wl$loudness)
#
# mod = nls(loudness ~ a + b * windowLength ^ c, wl, start = list(a = 0, b = 1, c = .5))
# plot(wl$windowLength, wl$loudness)
# lines(wl$windowLength, predict(mod, list(windowLength = wl$windowLength)))
# summary(mod)
# # use these regression coefficients to calculate scaling factor
# # as a function of windowLength
#
#
# # account for samplingRate
# windowLength = 30
# wl = data.frame(samplingRate = seq(16000, 44000, by = 1000))
# for (i in 1:nrow(wl)) {
#   sound = sin(2*pi*1000/wl$samplingRate[i]*(1:20000))
#   wl$loudness[i] = getLoudness(x = sound, samplingRate = wl$samplingRate[i], windowLength = windowLength, step = NULL, overlap = 0, SPL_measured = 40, Pref = 2e-5, plot = FALSE)$loudness[1]
# }
# plot(wl$samplingRate, wl$loudness)
# mod = nls(loudness ~ a + b * samplingRate ^ c, wl, start = list(a = 0, b = 1, c = .5))
# plot(wl$samplingRate, wl$loudness)
# lines(wl$samplingRate, predict(mod, list(samplingRate = wl$samplingRate)))
# summary(mod)
#
# # CHECKING THE CALIBRATION
# samplingRate = 24000
# sound = sin(2*pi*1000/samplingRate*(1:20000))
# cal = data.frame(SPL_measured = seq(40, 80, by = 10))
# for (i in 1:nrow(cal)) {
#   cal$loudness[i] = mean(getLoudness(x = sound, samplingRate = samplingRate, windowLength = 20, step = NULL, overlap = 50, SPL_measured = cal$SPL_measured[i], Pref = 2e-5, plot = FALSE)$loudness)
# }
# cal  # loudness should be 1, 2, 4, 8, 16 sone
# cal$loudness / cal$loudness[1]
# plot(cal$SPL_measured, cal$loudness / cal$loudness[1] - c(1, 2, 4, 8, 16))
