#' Auditory spectrogram
#'
#' Produces an auditory spectrogram by extracting a bank of bandpass filters (work in progress)
#'
#' While tuneR::audspec is based on FFT, here we convolve the sound with a bank
#' of filters. The main difference is that we don't window the signal and
#' therefore get full temporal resolution in all frequency bins. For ex., this
#' preserves high-frequency AM that is completely invisible in a spectrogram.
#' @keywords internal
#' @examples
#' # synthesize a sound 500 ms long, with gradually increasing hissing noise
#' sound = soundgen(sylLen = 200, temperature = 0.001,
#'   noise = list(time = c(0, 650), value = c(-40, 0)),
#'   formantsNoise = list(f1 = list(freq = 5000, width = 10000)),
#'   addSilence = 25)
#' # playme(sound, samplingRate = 16000)
#'
#' # auditory spectrogram
#' soundgen:::audSpectrogram(sound, samplingRate = 16000)
#'
#' # compare to an FFT-based spectrogram on log-scale
#' spectrogram(sound, samplingRate = 16000, yScale = 'log')
#'
#' \dontrun{
#' # add bells and whistles
#' soundgen:::audSpectrogram(sound, samplingRate = 16000,
#'   yScale = 'linear',
#'   osc = 'dB',  # plot oscillogram in dB
#'   heights = c(2, 1),  # spectro/osc height ratio
#'   brightness = -.1,  # reduce brightness
#'   colorTheme = 'heat.colors',  # pick color theme
#'   cex.lab = .75, cex.axis = .75,  # text size and other base graphics pars
#'   grid = 5,  # lines per kHz; to customize, add manually with graphics::grid()
#'   ylim = c(0, 5),  # always in kHz
#'   main = 'My spectrogram' # title
#'   # + axis labels, etc
#' )
#'
#' # change dynamic range
#' soundgen:::audSpectrogram(sound, samplingRate = 16000, dynamicRange = 40)
#' soundgen:::audSpectrogram(sound, samplingRate = 16000, dynamicRange = 120)
#'
#' # remove the oscillogram
#' soundgen:::audSpectrogram(sound, samplingRate = 16000, osc = 'none')  # or NULL etc
#' }
audSpectrogram = function(
  x,
  samplingRate = NULL,
  scale = NULL,
  from = NULL,
  to = NULL,
  dynamicRange = 80,
  nFilters = 128,
  minFreq = 20,
  maxFreq = samplingRate / 2,
  minBandwidth = 1,
  reportEvery = NULL,
  plot = TRUE,
  savePlots = NULL,
  osc = c('none', 'linear', 'dB')[2],
  heights = c(3, 1),
  ylim = NULL,
  yScale = c('log', 'llinear')[1],
  contrast = .2,
  brightness = 0,
  maxPoints = c(1e5, 5e5),
  padWithSilence = TRUE,
  colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
  extraContour = NULL,
  xlab = NULL,
  ylab = NULL,
  xaxp = NULL,
  mar = c(5.1, 4.1, 4.1, 2),
  main = NULL,
  grid = NULL,
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  ...
) {
  # match args
  myPars = c(as.list(environment()), list(...))
  # myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to', 'reportEvery', 'savePlots')]

  # call .audSpectrogram
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.audSpectrogram',
    myPars = myPars,
    reportEvery = reportEvery,
    savePlots = savePlots
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots)) {
    htmlPlots(
      htmlFile = paste0(pa$input$savePlots, '00_clickablePlots_audSpectrogram.html'),
      plotFiles = paste0(pa$input$filenames_noExt, "_audSpectrogram.png"),
      audioFiles = if (savePlots == '') pa$input$filenames_base else pa$input$filenames,
      width = paste0(width, units))
  }
  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}


#' Auditory spectrogram per sound
#'
#' Internal soundgen function.
#' @inheritParams audSpectrogram
#' @param audio a list returned by \code{readAudio}
#' @keywords internal
.audSpectrogram = function(
  audio,
  dynamicRange = 80,
  nFilters,
  minFreq = 20,
  maxFreq = audio$samplingRate / 2,
  minBandwidth = 1,
  plot = TRUE,
  osc = c('none', 'linear', 'dB')[2],
  heights = c(3, 1),
  ylim = NULL,
  yScale = c('log', 'llinear')[1],
  contrast = .2,
  brightness = 0,
  maxPoints = c(1e5, 5e5),
  padWithSilence = TRUE,
  colorTheme = c('bw', 'seewave', 'heat.colors', '...')[1],
  extraContour = NULL,
  xlab = NULL,
  ylab = NULL,
  xaxp = NULL,
  mar = c(5.1, 4.1, 4.1, 2),
  main = NULL,
  grid = NULL,
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  ...
) {
  if (is.null(maxFreq) || length(maxFreq) < 1) maxFreq = audio$samplingRate / 2
  filter_width = max(
    minBandwidth,
    (HzToSemitones(maxFreq) - HzToSemitones(minFreq)) / nFilters
  )
  halfFW = filter_width / 2
  nyquist = audio$samplingRate / 2
  cf_semitones = seq(
    HzToSemitones(minFreq),
    HzToSemitones(min(maxFreq, nyquist / 2^(filter_width / 2 / 12))),
    length.out = nFilters
  )
  filters = data.frame(
    cf = semitonesToHz(cf_semitones),
    from = semitonesToHz(cf_semitones - halfFW),
    to = semitonesToHz(cf_semitones + halfFW)
  )
  # summary(filters)
  # filters = filters[filters$to < (samplingRate / 2), ]
  # nFilters = nrow(filters)

  # bandpass filter the signal. seewave::ffilter goes via stdft, so it's not suitable
  # NB: signal needs to be included in dependencies (unless I use smth else)
  fb = sp = vector('list', nFilters)
  for (i in 1:nFilters) {
    btord = signal::FilterOfOrder(
      n = 3,
      Wc = c(filters$from[i], filters$to[i]) / nyquist,
      type = 'pass'
    )
    bt = signal::butter(btord)
    fb[[i]] = signal::filter(filt = bt, x = audio$sound)
    # osc(fb / max(fb))
    fb_env = seewave::env(fb[[i]], f = audio$samplingRate,
                          envt = 'hil',
                          msmooth = c(10, 0),
                          plot = F)
    # plot(fb_env, type = 'l')
    sp[[i]] = matrix(fb_env, nrow = 1)
  }
  audSpec = do.call(rbind, sp)
  audSpec[is.na(audSpec)] = 0
  rownames(audSpec) = filters$cf / 1000
  colnames(audSpec) = seq(0, audio$duration, length.out = ncol(audSpec)) * 1000

  # rescale etc for plotting
  Z1 = t(audSpec)
  # set to zero under dynamic range
  threshold = max(Z1) / 10^(dynamicRange/20)
  Z1[Z1 < threshold] = 0
  # re-normalize
  positives = which(Z1 > 0)
  nonpositives = which(Z1 <= 0)
  Z1[positives] = log(Z1[positives])
  if (length(positives) > 0 & length(nonpositives) > 0) {
    Z1[nonpositives] = min(Z1[positives])
  }
  Z1 = Z1 - min(Z1)

  # contrast & brightness
  contrast_exp = exp(3 * contrast)
  brightness_exp = exp(3 * brightness)
  # visualization: plot(exp(3 * seq(-1, 1, by = .01)), type = 'l')
  if (contrast_exp != 1) {
    Z1 = Z1 ^ contrast_exp
  }
  if (any(Z1 != 0)) Z1 = Z1 / max(Z1)
  if (brightness_exp != 1) {
    Z1 = Z1 / brightness_exp
  }
  if (brightness_exp < 1) {
    Z1[Z1 > 1] = 1 # otherwise values >1 are shown as white instead of black
  }

  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_spectrogram.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    if (is.null(ylim)) ylim = c(minFreq, maxFreq) / 1000
    plotSpec(
      X = as.numeric(colnames(audSpec)),  # time
      Y = as.numeric(rownames(audSpec)),  # freq
      Z = Z1,
      audio = audio, internal = NULL, dynamicRange = dynamicRange,
      osc = osc, heights = heights, ylim = ylim, yScale = yScale,
      contrast = contrast, brightness = brightness,
      maxPoints = maxPoints, colorTheme = colorTheme,
      extraContour = extraContour,
      xlab = xlab, ylab = ylab, xaxp = xaxp,
      mar = mar, main = main, grid = grid,
      width = width, height = height,
      units = units, res = res,
      ...
    )
    if (is.character(audio$savePlots)) {
      dev.off()
    }
  }

  return(list(
    filterbank = fb,
    audSpec = audSpec
  ))
}
