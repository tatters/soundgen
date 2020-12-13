## FINDING SYLLABLES AND VOCAL BURSTS ##

#' Internal soundgen function
#'
#' A helper function called internally by segment() for segmenting a single sound.
#' @inheritParams segment
#' @param inputType to save time, specify what the input is (file or waveform)
#'
#' @keywords internal
segmentSound = function(
  x,
  inputType,
  samplingRate = NULL,
  from = NULL,
  to = NULL,
  shortestSyl = 40,
  shortestPause = 40,
  method = c('env', 'spec', 'mel')[3],
  propNoise = NULL,
  SNR = NULL,
  noiseLevelStabWeight = c(1, .25),
  windowLength = shortestSyl,
  step = NULL,
  overlap = 80,
  reverbPars = list(reverbDelay = 70, reverbSpread = 130,
                    reverbLevel = -35, reverbDensity = 50),
  interburst = NULL,
  peakToTrough = SNR + 3,
  troughLocation = c('left', 'right', 'both', 'either')[4],
  summaryFun = NULL,
  plot = FALSE,
  savePlots = NULL,
  saveAudio = NULL,
  addSilence = 50,
  xlab = '',
  ylab = 'Signal, dB',
  main = NULL,
  showLegend = FALSE,
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  maxPoints = 1e4,
  specPlot = NULL,
  contourPlot = list(lty = 1, lwd = 2, col = 'green'),
  sylPlot = list(lty = 1, lwd = 2, col = 'blue'),
  burstPlot = list(pch = 8, cex = 3, col = 'red'),
  ...) {
  ## import a sound
  if (inputType == 'file') {
    extension = substr(x, nchar(x) - 2, nchar(x))
    if (extension == 'wav' | extension == 'WAV') {
      sound_wav = tuneR::readWave(x)
    } else if (extension == 'mp3' | extension == 'MP3') {
      sound_wav = tuneR::readMP3(x)
    } else {
      stop('Input not recognized: must be a numeric vector or wav/mp3 file')
    }
    samplingRate = sound_wav@samp.rate
    sound = as.numeric(sound_wav@left)
    plotname = tail(unlist(strsplit(x, '/')), n = 1)
    plotname = substring(plotname, 1, nchar(plotname) - 4)
    if (is.null(main)) main = plotname
  }  else if (inputType == 'waveform') {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    } else {
      sound = x
      plotname = 'newPlot'
      if (is.null(main)) main = ''
    }
  }

  # from...to selection
  ls = length(sound)
  if (any(is.numeric(c(from, to)))) {
    if (!is.numeric(from)) {
      from_points = 1
    } else {
      from_points = max(1, round(from * samplingRate))
    }
    if (!is.numeric(to)) {
      to_points = ls
    }  else {
      to_points = min(ls, round(to * samplingRate))
    }
    sound = sound[from_points:to_points]
    timeShift = from_points / samplingRate * 1000
    ls = length(sound)
  } else {
    timeShift = 0
  }

  ## normalize
  sound = sound - mean(sound)  # center around 0
  sound = sound / max(abs(sound))  # range approx. -1 to 1
  len = length(sound)
  windowLength_points = ceiling(windowLength * samplingRate / 1000)
  if (windowLength_points > len / 2) {
    windowLength_points = len / 2
    step = windowLength_points / samplingRate * 1000 * (1 - overlap / 100)
  }
  dur_total = len / samplingRate * 1000
  step_points = round(step / 1000 * samplingRate)
  step = step_points / samplingRate * 1000
  # step_points can only be an integer, introducing small timing errors in long sounds
  # plot(sound, type='l')

  if (method == 'env') {
    ## work with smoothed amplitude envelope
    ampl = seewave::env(
      sound,
      f = samplingRate,
      envt = 'hil',
      msmooth = c(windowLength_points, overlap),
      fftw = FALSE,
      plot = FALSE
    )[, 1]
    ampl = 20 * log10(ampl)
    ampl = ampl - min(ampl)
    nc = length(ampl)
    # plot(ampl, type = 'l')

    # attempt to estimate propNoise from data
    d_ampl = c(0, abs(diff(ampl)))
    # ampl_dampl = ampl * d_ampl  # plot(ampl_dampl, type = 'l')
    # weighted contributions from ampl (overall level) and d_ampl (level stability)
    ampl_dampl = exp(log(ampl) * noiseLevelStabWeight[1] +
                       log(d_ampl) * noiseLevelStabWeight[2])
    nonZero_ampl_dampl = which(ampl_dampl > 0)
    if (is.null(propNoise)) {
      dens_ampl_dampl = density(ampl_dampl[nonZero_ampl_dampl])
      a_zoo = zoo::as.zoo(dens_ampl_dampl$y)
      temp = zoo::rollapply(a_zoo,
                            width = 3,
                            align = 'center',
                            function(x) {
                              which.max(x) == ceiling(length(x) / 2)
                            })
      idx = zoo::index(temp)[zoo::coredata(temp)]
      thres_noise = dens_ampl_dampl$x[idx[1]]
      col_noise = which(ampl_dampl <= thres_noise)
      propNoise = max(.01, round(length(col_noise) / nc, 3))
      message(paste0('propNoise set to ', propNoise, '; reset manually if needed'))
    } else {
      col_noise = which(ampl_dampl <= quantile(ampl_dampl[nonZero_ampl_dampl],
                                               probs = propNoise))
      thres_noise = as.numeric(median(ampl[col_noise]))
    }
    thres_difNoise = as.numeric(quantile(ampl[nonZero_ampl_dampl], probs = propNoise))

    # attempt to estimate SNR from data
    if (is.null(SNR) & propNoise > 0) {
      SNR = round((median(ampl[-col_noise]) - thres_difNoise) / 2, 1)
      message(paste0('SNR set to ', SNR, '; reset manually if needed'))
    }
    if (length(peakToTrough) < 1) peakToTrough = 10 ^ ((SNR + 3) / 20)

    # adaptive thresholds may help to control for reverb
    if (length(reverbPars) > 0 & is.list(reverbPars)) {
      # dynamic thresholdf
      rvb_list = do.call('reverb', c(
        list(x = ampl,
             samplingRate = 1000 / step,
             len = nc,
             output = 'detailed'),
        reverbPars
      ))
      rvb = rvb_list$rvb[1:nc]
      threshold = thres_difNoise + SNR + rvb
      # plot(difNoise, type = 'l')
      # abline(h = thres_difNoise + SNR, lty = 2)
      # points(threshold, type = 'l', col = 'blue')
    } else {
      # static threshold
      threshold = rep(thres_difNoise + SNR, nc)
    }

    # find syllables
    syllables = findSyllables(
      ampl = ampl,
      threshold = threshold,
      shortestSyl = shortestSyl,
      shortestPause = shortestPause,
      step = step,
      windowLength = windowLength
    )
  } else if (method %in% c('mel', 'spec')) {
    ## work with some form of spectrogram
    if (!exists('sound_wav') | any(is.numeric(c(from, to)))) {
      # redo Wave in case of from...to... to avoid analyzing the entire file
      sound_wav = tuneR::Wave(sound, samp.rate = samplingRate, bit = 16)
    }
    myspec = tuneR::melfcc(sound_wav,
                           wintime = windowLength / 1000,
                           hoptime = step / 1000,
                           lifterexp = 0,
                           preemph = 0,
                           numcep = 1,
                           spec_out = TRUE)
    if (method == 'mel') {
      sp = t(myspec$aspectrum)
    } else if (method == 'spec') {
      sp = t(myspec$pspectrum)
    }
    sp = 20 * log10(sp / max(sp) + 1e-4)  # in case of pure silence --> log(0)
    nc = ncol(sp)
    cs = colMeans(sp)
    cs = cs - min(cs)  # make non-negative for ease of further processing
    # image(t(sp))

    # novelty
    # better than ssm-related novelty b/c that one looks inside each syllable
    if (FALSE) {  # could use as a par to segment(useNovelty = c(TRUE, FALSE)[2])
      nFr = max(3, ceiling(windowLength / step))
      win = dnorm(1:nFr, mean = 1, sd = nFr / 2)
      win = rev(win / sum(win))  # normalize to sum to 1
      novelty = vector('numeric', nc)
      novelty[1] = 0
      novelty[2] = cs[2] - cs[1]
      for (i in 3:nc) {
        if (i <= nFr) {
          idx = 1:(i - 1)
          win_i = dnorm(1:(i - 1), mean = 1, sd = nFr / 2)
          win_i = rev(win_i / sum(win_i))
        } else {
          idx = (i - nFr):(i - 1)
          win_i = win
        }
        novelty[i] = cs[i] - sum(cs[idx] * win_i)
      }
      # plot(novelty, type = 'l')
    } else {
      novelty = 0
    }

    # or simple diff, maybe with smoothing afterwards
    # novelty_matrix = t(apply(sp, 1, function(x) diff(x)))
    # NB: not abs(diff) - should be negative at the end of each syl
    # image(t(log(novelty_matrix)))
    # novelty = c(0, colMeans(novelty_matrix))
    # novelty = soundgen::getEnv(novelty,
    #                            windowLength_points = windowLength / step,
    #                            method = 'mean')

    # compare the spectrum of each STFT frame to the spectrum of noise
    # estimate the spectrum of background noise
    d_cs = c(0, abs(diff(cs)))
    # cs_dcs = cs * d_cs  # plot(cs_dcs, type = 'l')
    # weighted contributions from cs (overall level) and d_cs (level stability)
    cs_dcs = exp(log(cs) * noiseLevelStabWeight[1] +
                   log(d_cs) * noiseLevelStabWeight[2])
    nonZero_cs_dcs = which(cs_dcs > 0)
    if (is.null(propNoise)) {
      dens_cs_dcs = density(cs_dcs[nonZero_cs_dcs])
      a_zoo = zoo::as.zoo(dens_cs_dcs$y)
      temp = zoo::rollapply(a_zoo,
                            width = 3,
                            align = 'center',
                            function(x) {
                              which.max(x) == ceiling(length(x) / 2)
                            })
      idx = zoo::index(temp)[zoo::coredata(temp)]
      thres_noise = max(min(cs_dcs), dens_cs_dcs$x[idx[1]])
      col_noise = which(cs_dcs <= thres_noise)
      propNoise = max(.01, round(length(col_noise) / nc, 3))
      message(paste0('propNoise set to ', propNoise, '; reset manually if needed'))
    } else {
      col_noise = which(cs_dcs <= quantile(cs_dcs[nonZero_cs_dcs], probs = propNoise))
      thres_noise = as.numeric(median(cs[col_noise]))
    }
    noise = rowMeans(sp[, col_noise])
    # plot(noise, type = 'l')  # the spectrum of background noise, presumably

    # some kind of bin-by-bin spectral difference from noise
    difNoise = vector('numeric', nc)
    for (c in 1:nc) {
      # cosine
      difNoise[c] = 1 - crossprod(sp[, c], noise) /
        sqrt(crossprod(sp[, c]) * crossprod(noise))
      # difNoise[c] = 1 - cor(sp[, c], noise)  # throws NAs, weird range
      # difNoise[c] = quantile(sp[, c] - noise, .75)
      # max(sp[, c] - noise)   # not robust to noise
      # mean(sp[, c] - noise)  # not sensitive to spectral changes - like using ampl env
    }

    difNoise = difNoise / max(difNoise) * max(cs) + cs - thres_noise
    # plot(difNoise, type = 'l')
    # hist(difNoise)

    thres_difNoise = as.numeric(quantile(difNoise, probs = propNoise))
    # plot(difNoise, type = 'l')
    # abline(h = thres_difNoise + SNR, lty = 3, col = 'blue')

    # attempt to estimate SNR from data
    if (is.null(SNR)) {
      # prop_snr = max(min(.99, propNoise + .25), .5)
      # SNR = quantile(difNoise[difNoise > thres_difNoise], probs = prop_snr) - thres_difNoise
      SNR = round((median(difNoise[-col_noise]) - thres_difNoise) / 2, 1)
      message(paste0('SNR set to ', SNR, '; reset manually if needed'))
      # SNR = min(30, max(cs) / 2 - thres_noise)
      # SNR = max(difNoise[col_noise]) + 10 * sd(difNoise[col_noise])
    }
    if (length(peakToTrough) < 1) peakToTrough = SNR + 3

    # adaptive thresholds may help to control for reverb
    if (length(reverbPars) > 0 & is.list(reverbPars)) {
      # dynamic thresholdf
      rvb_list = do.call('reverb', c(
        list(x = cs,
             samplingRate = 1000 / step,
             len = nc,
             output = 'detailed'),
        reverbPars
      ))
      rvb = rvb_list$rvb[1:nc]
      # plot(cs, type = 'l')
      # points(rvb, type = 'l', col = 'blue')

      # # alternative (a tiny bit slower, produces a more wiggly rvb): calculate
      # # pointwise reverb from preceding samples
      # rvb = vector('numeric', nc)
      # for (i in 1:nc) {
      #   if (i <= lw) {
      #     idx = 1:(i - 1)
      #     win_i = win[(lw - i + 2):lw]
      #   } else {
      #     idx = (i - lw):(i - 1)
      #     win_i = win
      #   }
      #   rvb[i] = sum(cs[idx] * win_i)
      # }

      threshold = thres_difNoise + SNR + rvb  #  * 10 ^ (reverbDep / 20)
      # rvb_adj = rvb / max(cs)
      # rvb[rvb < thres_noise] = thres_noise; (rvb - thres_noise) / (max(cs) - thres_noise)
      # threshold = thres_difNoise + SNR * (1 + rvb_adj)
      # plot(difNoise, type = 'l')
      # abline(h = thres_difNoise + SNR, lty = 2)
      # points(threshold, type = 'l', col = 'blue')

    } else {
      # static threshold
      threshold = rep(thres_difNoise + SNR, nc) # * 10 ^ (SNR / 20)
    }

    # add up the novelty and difNoise curves
    ampl = novelty + difNoise
    # ampl = getEnv(
    #   novelty + difNoise,
    #   windowLength_points = mean(c(shortestSyl, shortestPause)) / step,
    #   method = 'peak'
    # )
    # plot(ampl, type = 'l')
    # points(threshold, type = 'l', col = 'blue')

    # find syllables
    syllables = findSyllables(
      ampl = ampl,
      threshold = threshold,
      shortestSyl = shortestSyl,
      shortestPause = shortestPause,
      step = step,
      windowLength = windowLength
    )
  }
  ## find bursts and get descriptives
  # calculate the window for analyzing bursts based on syllables
  # (if no syllables are detected, just use shortestSyl)
  if (is.null(interburst)) {
    median_scaled = median(syllables$sylLen, na.rm = TRUE)
    if (any(!is.na(syllables$pauseLen))) {
      median_scaled = median_scaled + median(syllables$pauseLen, na.rm = TRUE)
    }
    interburst = ifelse(!is.na(median_scaled) & length(median_scaled) > 0,
                        median_scaled,
                        shortestSyl)
  }

  bursts = findBursts(ampl = ampl,
                      step = step,
                      windowLength = windowLength,
                      interburst = interburst,
                      burstThres = threshold,
                      peakToTrough = peakToTrough,
                      troughLocation = troughLocation,
                      scale = 'dB')
  syllables[, c('start', 'end')] = syllables[, c('start', 'end')] + timeShift
  bursts$time = bursts$time + timeShift

  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    sum_syl = summarizeAnalyze(syllables[, c('sylLen', 'pauseLen')],
                               summaryFun = summaryFun,
                               var_noSummary = NULL)
    sum_bursts = summarizeAnalyze(bursts[, 'interburst', drop = FALSE],
                                  summaryFun = summaryFun,
                                  var_noSummary = NULL)
    result = as.data.frame(c(
      list(nSyl = nrow(syllables)),
      sum_syl,
      list(nBursts = nrow(bursts)),
      sum_bursts
    ))
    result[apply(result, c(1, 2), is.nan)] = NA
  } else {
    result = list(
      syllables = syllables[, c('syllable', 'start', 'end', 'sylLen', 'pauseLen')],
      bursts = bursts)
  }

  ## save all extracted syllables as separate audio files for easy examination
  if (is.character(saveAudio) && !is.na(syllables$sylLen[1])) {
    addSil = rep(0, addSilence * samplingRate / 1000)
    for (i in 1:nrow(syllables)) {
      from = max(1, samplingRate * ((syllables$start[i]) / 1000))  #  - windowLength / 2
      to = min(length(sound), samplingRate * ((syllables$end[i]) / 1000))
      temp = c(addSil, sound[from:to], addSil)
      filename_i = paste0(
        saveAudio, ifelse(is.character(x), basename(x), 'sound'),
        '_', round(syllables$start[i], 0),
        '-', round(syllables$end[i], 0), '.wav')
      seewave::savewav(temp, f = samplingRate, filename = filename_i)
    }
  }

  ## plotting
  if (is.character(savePlots)) plot = TRUE
  if (plot) {
    # defaults
    if (is.null(sylPlot$lty)) sylPlot$lty = 1
    if (is.null(sylPlot$lwd)) sylPlot$lwd = 2
    if (is.null(sylPlot$col)) sylPlot$col = 'blue'
    if (is.null(burstPlot$pch)) burstPlot$pch = 8
    if (is.null(burstPlot$cex)) burstPlot$cex = 3
    if (is.null(burstPlot$col)) burstPlot$col = 'red'

    if (is.character(savePlots)) {
      png(filename = paste0(savePlots, plotname, ".png"),
          width = width, height = height, units = units, res = res)
    }

    op = par(c('mar', 'xaxt', 'yaxt', 'mfrow')) # save user's original pars
    layout(matrix(c(2, 1), nrow = 2, byrow = TRUE), heights = c(2, 1))
    par(mar = c(op$mar[1:2], 0, op$mar[4]), xaxt = 's', yaxt = 's')
    xlim = c(0, len / samplingRate * 1000) + timeShift

    # downsample long sounds to avoid delays when plotting
    if (len > maxPoints) {
      idx_sound = seq(1, len, by = ceiling(len / maxPoints))
    } else {
      idx_sound = 1:len
    }

    # plot osc
    plot(x = idx_sound / samplingRate * 1000 + timeShift,
         y = sound[idx_sound], type = 'l', xlim = xlim,
         axes = FALSE, xaxs = "i", yaxs = "i", bty = 'o',
         xlab = xlab, ylab = '', main = '', ...)
    box()
    time_location = axTicks(1)
    time_labels = convert_sec_to_hms(time_location / 1000, 3)
    axis(side = 1, at = time_location, labels = time_labels, ...)
    abline(h = 0, lty = 2)
    par(mar = c(0, op$mar[2:4]), xaxt = 'n', yaxt = 's')

    # plot envelope
    # if (method != 'env') {
    #   m = min(ampl)
    #   ampl = log(ampl - m + 1e-3)
    #   threshold = log(threshold - m + 1e-3)
    #   # plot(ampl, type = 'l')
    # }
    envelope = data.frame(
      time = (1:length(ampl) - 1) * step + windowLength / 2 + timeShift,
      value = ampl
    )
    # downsample long envelopes
    nr_env = nrow(envelope)
    if (nr_env > maxPoints) {
      idx_env = seq(1, nr_env, by = ceiling(nr_env / maxPoints))
      envelope = envelope[idx_env, ]
      idx_sp = seq(1, nr_env, by = ceiling(nr_env / maxPoints * nrow(sp)))
      sp = sp[, idx_sp]
    } else {
      idx_env = 1:nr_env
    }
    if (method %in% c('spec', 'mel') & !is.null(specPlot)) {
      specPlot$color.palette = ifelse(
        is.null(specPlot$color.palette),
        switchColorTheme('bw'),
        switchColorTheme(specPlot$color.palette)
      )
      do.call('filled.contour.mod', c(list(
        x = seq(envelope$time[1], envelope$time[nrow(envelope)], length.out = ncol(sp)),
        y = seq(min(envelope$value), max(envelope$value) * 1.05, length.out = nrow(sp)),
        z = t(zeroOne(sp)),
        levels = seq(0, 1, length = 30),
        xlim = xlim, xaxs = "i", xlab = '',
        ylab = ylab, main = main, ...),
        specPlot))
      do.call('points', c(list(x = envelope$time[idx_env],
                               y = envelope$value[idx_env],
                               type = 'l'),
                          contourPlot))
    } else {
      do.call('plot', c(list(x = envelope$time[idx_env],
                             y = envelope$value[idx_env],
                             type = 'l',
                             xlim = xlim, xaxs = "i", xlab = '',
                             ylab = ylab, main = main, ...),
                        contourPlot))
    }
    abline(h = thres_difNoise, lty = 3, col = 'black')
    # plot bursts
    do.call('points', c(list(x = bursts$time, y = bursts$ampl),
                        burstPlot))
    if (length(threshold) == 1) threshold = rep(threshold, nc)
    if (any(!is.na(syllables$start))) {
      thres_contour = rep(NA, nc)
      for (i in 1:nrow(syllables)) {
        idx_i = syllables$start_idx[i]:syllables$end_idx[i]
        thres_contour[idx_i] = threshold[idx_i]
        # segments(x0 = syllables$start[s], y0 = threshold[idx_start],
        #          x1 = syllables$end[s], y1 = threshold[idx_end],
        #          lty = sylPlot$lty, lwd = sylPlot$lwd, col = sylPlot$col)
      }
      do.call('points', c(list(x = envelope$time[idx_env],
                               y = thres_contour,
                               type = 'l'),
                          sylPlot))
    }

    if (showLegend) legend('topright',
                           legend = c('Syllable threshold', 'Noise threshold'),
                           lty = c(1, 3), col = c(sylPlot$col, 'gray50')
    )

    # restore original pars
    par('mar' = op$mar, 'xaxt' = op$xaxt, 'yaxt' = op$yaxt, 'mfrow' = op$mfrow)
    if (is.character(savePlots)){
      dev.off()
    }
  }

  return(result)
}


#' Segment a sound
#'
#' Finds syllables and bursts separated by background noise in long recordings.
#' Syllables are defined as continuous segments that seem to be different from
#' noise based on amplitude and/or spectral similarity thresholds. Bursts are
#' defined as local maxima in signal envelope that are high enough both in
#' absolute terms (relative to the global maximum) and with respect to the
#' surrounding region (relative to local mimima). See
#' vignette('acoustic_analysis', package = 'soundgen') for details.
#'
#' Algorithm: first the audio recording is partitioned into signal and noise
#' regions: the quietest and most stable regions are located, and noise
#' threshold is defined from a user-specified proportion of noise in the
#' recording (\code{propNoise}) or, if \code{propNoise = NULL}, from the lowest
#' local maximum in the density function of a weighted product of amplitude and
#' stability (that is, we assume that quiet and stable regions are likely to
#' represent noise). Once we know what the noise looks like - in terms of its
#' typical amplitude and/or spectrum - we derive signal contour as its
#' difference from noise at each time point. If \code{method = 'env'}, this is
#' Hilbert transform minus noise, and if \code{method = 'spec' or 'mel'}, this
#' is the inverse of cosine similarity between the spectrum of each frame and
#' the estimated spectrum of noise weighted by amplitude. By default,
#' signal-to-noise ratio (SNR) is estimated as half-median of above-noise
#' signal, but it is recommended that this parameter is adjusted by hand to suit
#' the purposes of segmentation, as it is the key setting that controls the
#' balance between false negatives (missing faint signals) and false positives
#' (hallucinating signals that are actually noise). Note also that effects of
#' echo or reverberation can be taken into account: syllable detection threshold
#' may be raised following powerful acoustic bursts with the help of the
#' \code{reverbPars} argument. At the final stage, continuous "islands" SNR dB
#' above noise level are detected as syllables, and "peaks" on the islands are
#' detected as bursts. The algorithm is very flexible, but the parameters may be
#' hard to optimize by hand. If you have an annotated sample of the sort of
#' audio you are planning to analyze, with syllables and/or bursts counted
#' manually, you can use it for automatic optimization of control parameters
#' (see \code{\link{optimizePars}}.
#'
#' @seealso \code{\link{analyze}}  \code{\link{ssm}}
#'
#' @inheritParams spectrogram
#' @param x input audio: full path to a folder with wav/mp3 files or a single
#'   wav/mp3, or else a numeric vector representing the actual audio sampled at
#'   \code{samplingRate}
#' @param samplingRate the number of samples per second (only needed if the
#'   input is a vector rather than an audio file)
#' @param shortestSyl minimum acceptable length of syllables, ms
#' @param shortestPause minimum acceptable break between syllables, ms
#'   (syllables separated by shorter pauses are merged)
#' @param method the signal used to search for syllables: 'env' =
#'   Hilbert-transformed amplitude envelope, 'spec' = spectrogram, 'mel' =
#'   mel-transformed spectrogram (see tuneR::melfcc)
#' @param propNoise the proportion of non-zero sound assumed to represent
#'   background noise (note that complete silence is not considered, so padding
#'   with silence won't affect the algorithm)
#' @param SNR expected signal-to-noise ratio (dB above noise), which determines
#'   the threshold for syllable detection. The meaning of "dB" here is
#'   approximate since the "signal" may be different from sound intensity
#' @param noiseLevelStabWeight a vector of length 2 specifying the relative
#'   weights of the overall signal level vs. stability when attempting to
#'   automatically locate the regions that represent noise. Increasing the
#'   weight of stability tends to accentuate the beginning and end of each
#'   syllable.
#' @param reverbPars parameters passed on to \code{\link{reverb}} to attempt to
#'   cancel the effects of reverberation or echo, which otherwise tend to merge
#'   short and loud segments like rapid barks
#' @param interburst minimum time between two consecutive bursts (ms). Defaults
#'   to the average detected \code{(syllable + pause) / 2}
#' @param peakToTrough to qualify as a burst, a local maximum has to be at least
#'   \code{peakToTrough} dB above the left and/or right local trough(s)
#'   (controlled by \code{troughLocation}) over the analysis window (controlled
#'   by \code{interburst}). Defaults to SNR + 3 dB
#' @param troughLocation should local maxima be compared to the trough on the
#'   left and/or right of it? Values: 'left', 'right', 'both', 'either'
#' @param summaryFun functions used to summarize each acoustic characteristic;
#'   see \code{\link{analyze}}
#' @param plot if TRUE, produces a segmentation plot
#' @param savePlots full path to the folder in which to save the plot(s). NULL =
#'   don't save. Note that the html file with clickable plots will only work if
#'   the plots are saved in the same folder as input audio files
#' @param saveAudio full path to the folder in which to save audio files (one
#'   per detected syllable)
#' @param addSilence if syllables are saved as separate audio files, they can be
#'   padded with some silence (ms)
#' @param verbose,reportEvery if TRUE, reports progress every \code{reportEvery}
#'   files and estimated time left
#' @param specPlot a list of graphical parameters for displaying the spectrogram
#'   (if \code{method = 'spec' or 'mel'}); set to NULL to hide the spectrogram
#' @param contourPlot a list of graphical parameters for displaying the signal
#'   contour used to detect syllables (see details)
#' @param sylPlot a list of graphical parameters for displaying the syllables
#' @param burstPlot a list of graphical parameters for displaying the bursts
#' @param xlab,ylab,main main plotting parameters
#' @param width,height,units,res parameters passed to
#'   \code{\link[grDevices]{png}} if the plot is saved
#' @param showLegend if TRUE, shows a legend for thresholds
#' @param maxPoints the maximum number of "pixels" (for quick plotting of long
#'   audio files)
#' @param ... other graphical parameters passed to graphics::plot
#'
#' @return If \code{summaryFun = NULL}, returns returns a list containing full
#'   stats on each syllable and burst (one row per syllable and per burst),
#'   otherwise returns only a dataframe with one row per file - a summary of the
#'   number and spacing of syllables and vocal bursts.
#' @export
#' @examples
#' sound = soundgen(nSyl = 4, sylLen = 100, pauseLen = 70,
#'                  attackLen = 20, amplGlobal = c(0, -20),
#'                  pitch = c(368, 284), temperature = .1)
#' # add noise so SNR decreases from 20 to 0 dB from syl1 to syl4
#' sound = sound + runif(length(sound), -10 ^ (-20 / 20), 10 ^ (-20 / 20))
#' # osc(sound, samplingRate = 16000, dB = TRUE)
#' # spectrogram(sound, samplingRate = 16000, osc = TRUE)
#' # playme(sound, samplingRate = 16000)
#'
#' s = segment(sound, samplingRate = 16000, plot = TRUE)
#'
#' # just a summary (see examples in ?analyze for custom summaryFun)
#' s1 = segment(sound, samplingRate = 16000, summaryFun = c('median', 'sd'))
#' s1
#'
#' # customizing the plot
#' s = segment(sound, samplingRate = 16000, plot = TRUE,
#'             sylPlot = list(lty = 2, col = 'gray20'),
#'             burstPlot = list(pch = 16, col = 'gray80'),
#'             specPlot = list(color.palette = 'heat.colors'),
#'             xlab = 'Some custom label', cex.lab = 1.2,
#'             showLegend = TRUE,
#'             main = 'My awesome plot')
#' \dontrun{
#' # set SNR manually to control detection threshold
#' s = segment(sound, samplingRate = 16000, SNR = 1, plot = TRUE)
#'
#' # Download 260 sounds from the supplements to Anikin & Persson (2017) at
#' # http://cogsci.se/publications.html
#' # unzip them into a folder, say '~/Downloads/temp'
#' myfolder = '~/Downloads/temp260'  # 260 .wav files live here
#' s = segment(myfolder, verbose = TRUE)
#'
#' # Check accuracy: import a manual count of syllables (our "key")
#' key = segmentManual  # a vector of 260 integers
#' trial = as.numeric(s$nBursts)
#' cor(key, trial, use = 'pairwise.complete.obs')
#' boxplot(trial ~ as.integer(key), xlab='key')
#' abline(a=0, b=1, col='red')
#' }
segment = function(
  x,
  samplingRate = NULL,
  from = NULL,
  to = NULL,
  shortestSyl = 40,
  shortestPause = 40,
  method = c('env', 'spec', 'mel')[3],
  propNoise = NULL,
  SNR = NULL,
  noiseLevelStabWeight = c(1, .25),
  windowLength = shortestSyl,
  step = NULL,
  overlap = 80,
  reverbPars = list(reverbDelay = 70, reverbSpread = 130,
                    reverbLevel = -35, reverbDensity = 50),
  interburst = NULL,
  peakToTrough = SNR + 3,
  troughLocation = c('left', 'right', 'both', 'either')[4],
  summaryFun = if (is.character(x) && dir.exists(x)) c('median', 'sd') else NULL,
  plot = FALSE,
  savePlots = NULL,
  saveAudio = NULL,
  addSilence = 50,
  verbose = TRUE,
  reportEvery = 10,
  xlab = '',
  ylab = 'Signal, dB',
  main = NULL,
  showLegend = FALSE,
  width = 900,
  height = 500,
  units = 'px',
  res = NA,
  maxPoints = 1e4,
  specPlot = list(color.palette = 'bw'),
  contourPlot = list(lty = 1, lwd = 2, col = 'green'),
  sylPlot = list(lty = 1, lwd = 2, col = 'blue'),
  burstPlot = list(pch = 8, cex = 3, col = 'red'),
  ...
) {
  time_start = proc.time()  # timing

  ## Check the arguments
  if (windowLength < 10) {
    warning('windowLength < 10 ms is slow and usually not very useful')
  }
  if (!is.null(step)) overlap = 100 * (1 - step / windowLength)
  if (overlap < 0 | overlap > 100) {
    warning('overlap must be >0 and <= 100%; resetting to 70')
    overlap = 70
  }
  if (is.null(step)) step = windowLength * (1 - overlap / 100)

  if (!troughLocation %in% c('left', 'right', 'both', 'either')) {
    warning(paste(
      "Valid values of troughLocation: 'left', 'right', 'both', 'either'.",
      "Defaulting to 'either'")
    )
    troughLocation = 'either'
  }
  if (!method %in% c('env', 'spec', 'mel')) {
    warning(paste(
      "Valid values of method: 'env', 'spec', 'mel'.",
      "Defaulting to 'mel'")
    )
    method = 'mel'
  }

  ## Read the input
  if (is.character(x)) {
    inputType = 'file'
    if (dir.exists(x)) {
      # input is a folder
      filenames = list.files(x, pattern = "*.wav|.mp3|.WAV|.MP3", full.names = TRUE)
      if (length(filenames) < 1)
        stop(paste('No wav/mp3 files found in', x))
    } else if (file.exists(x)) {
      # input is an audio file
      filenames = x
      if (!substr(x, nchar(x) - 3, nchar(x)) %in% c('.wav', '.mp3', '.WAV', '.MP3'))
        stop('Input not recognized - must be a folder, wav/mp3 file, or numeric vector')
    } else {
      stop('Input not recognized - must be a folder, wav/mp3 file, or numeric vector')
    }
    nFiles = length(filenames)
    filenames_base = basename(filenames)
    filesizes = file.info(filenames)$size
  } else if (is.numeric(x)) {
    # input is a numeric vector
    inputType = 'waveform'
    filenames = x
    nFiles = 1
    filenames_base = 'sound'
    if (is.null(samplingRate))
      stop('samplingRate must be provided if input is a numeric vector')
  } else {
    stop('Input not recognized - must be a folder, wav/mp3 file, or numeric vector')
  }

  ## Prepare folders for saving the output
  if (is.character(savePlots)) {
    if (!dir.exists(savePlots)) dir.create(savePlots)
    # make sure the last character of savePlots is "/"
    last_char = substr(savePlots, nchar(savePlots), nchar(savePlots))
    if(last_char != '/') savePlots = paste0(savePlots, '/')
  }
  if (is.character(saveAudio)) {
    if (!dir.exists(saveAudio)) dir.create(saveAudio)
    # make sure the last character of savePlots is "/"
    last_char = substr(saveAudio, nchar(saveAudio), nchar(saveAudio))
    if(last_char != '/') saveAudio = paste0(saveAudio, '/')
  }

  ## Prepare a list of arguments to pass to segmentSound()
  myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'verbose', 'reportEvery',
    'reverbPars', 'sylPlot', 'burstPlot', 'specPlot')]  # otherwise flattens lists
  # exclude ...
  myPars = myPars[1:(length(myPars)-1)]
  # add back arguments that are lists
  myPars$sylPlot = sylPlot
  myPars$burstPlot = burstPlot
  myPars$specPlot = specPlot
  myPars$reverbPars = reverbPars
  myPars$inputType = inputType

  ## Run the analysis
  result = vector('list', nFiles)
  for (i in 1:nFiles) {
    if (inputType == 'file') {
      result[[i]] = do.call('segmentSound', c(list(x = filenames[i]), myPars, ...))
    } else if (inputType == 'waveform') {
      result[[i]] = do.call('segmentSound', c(list(x = filenames), myPars, ...))
    }
    if (verbose) {
      if (i %% reportEvery == 0) {
        reportTime(i = i, nIter = length(filenames),
                   time_start = time_start, jobs = filesizes)
      }
    }
  }

  ## Prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    # summarize
    output = cbind(data.frame(file = filenames_base),
                   do.call(rbind, result))
  } else {
    # return as lists
    if (nFiles > 1) {
      output = result
      names(output) = filenames_base
    } else {
      output = result[[1]]
    }
  }

  if (class(savePlots) == 'character' && nchar(savePlots) > 0) {
    if (!dir.exists(savePlots)) dir.create(savePlots)
    htmlPlots(savePlots, myfiles = filenames, width = paste0(width, units))
  }

  invisible(output)
}


#' Segment folder
#' @param ... any input parameters
segmentFolder = function(...) {
  message('segmentFolder() is deprecated; please use segment() instead')
}
