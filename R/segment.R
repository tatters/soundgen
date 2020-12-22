## FINDING SYLLABLES AND VOCAL BURSTS ##

#' Internal soundgen function
#'
#' A helper function called internally by segment() for segmenting a single sound.
#' @inheritParams segment
#' @param sound waveform as a numeric vector
#' @keywords internal
segmentSound = function(
  sound,
  samplingRate = NULL,
  from = NULL,
  to = NULL,
  shortestSyl = 40,
  shortestPause = 40,
  method = c('env', 'spec', 'mel')[3],
  propNoise = NULL,
  SNR = NULL,
  noiseLevelStabWeight = c(1, .25),
  windowLength = 40,
  step = NULL,
  overlap = 80,
  reverbPars = list(reverbDelay = 70, reverbSpread = 130,
                    reverbLevel = -35, reverbDensity = 50),
  interburst = NULL,
  peakToTrough = SNR + 3,
  troughLocation = c('left', 'right', 'both', 'either')[4],
  maxDur = 30,
  saveAudio = NULL,
  addSilence = 50,
  plot = FALSE,
  plotname = '',
  savePlots = NULL,
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
  # from...to selection
  len = length(sound)
  if (any(is.numeric(c(from, to)))) {
    if (!is.numeric(from)) {
      from_points = 1
    } else {
      from_points = max(1, round(from * samplingRate))
    }
    if (!is.numeric(to)) {
      to_points = len
    }  else {
      to_points = min(len, round(to * samplingRate))
    }
    sound = sound[from_points:to_points]
    timeShift = from_points / samplingRate * 1000
    len = length(sound)
  } else {
    timeShift = 0
  }

  ## normalize
  sound = sound - mean(sound)  # center around 0
  sound = sound / max(abs(sound))  # range approx. -1 to 1
  windowLength_points = ceiling(windowLength * samplingRate / 1000)
  if (windowLength_points > len / 2) {
    windowLength_points = len / 2
    step = windowLength_points / samplingRate * 1000 * (1 - overlap / 100)
    windowLength = windowLength_points / samplingRate * 1000
  }
  dur_total = len / samplingRate * 1000
  step_points = round(step / 1000 * samplingRate)
  step = step_points / samplingRate * 1000
  windowLength = windowLength_points / samplingRate * 1000
  # step_points can only be an integer, introducing small timing errors in long sounds
  # plot(sound, type='l')

  analyze_from = 0
  analyze_to = min(maxDur * 1000, dur_total)
  stopNextTime = analyze_to >= dur_total
  syllables = bursts = NULL
  propNoise_user = propNoise
  SNR_user = SNR
  while(analyze_to <= dur_total) {
    if (plot && analyze_to < dur_total) {
      plot = FALSE
      message(paste(
        'This long sound will be analyzed piecewise and cannot be plotted.',
        'Increase maxDur or use from/to to analyze and plot a part.'))
    }
    from_points = max(1, round(analyze_from * samplingRate / 1000))
    to_points = min(len, round(analyze_to * samplingRate / 1000))
    sound_part = sound[from_points:to_points]

    if (method == 'env') {
      ## work with smoothed amplitude envelope
      ampl = seewave::env(
        sound_part,
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
        # dynamic threshold
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
      syllables_part = findSyllables(
        ampl = ampl,
        threshold = threshold,
        shortestSyl = shortestSyl,
        shortestPause = shortestPause,
        step = step,
        windowLength = windowLength
      )
    } else if (method %in% c('mel', 'spec')) {
      ## work with some form of spectrogram
      if (!exists('sound_wav') | any(is.numeric(c(from, to))) | analyze_to < dur_total) {
        # redo Wave in case of from...to... to avoid analyzing the entire file
        sound_wav = tuneR::Wave(sound_part, samp.rate = samplingRate, bit = 16)
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
      syllables_part = findSyllables(
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
      median_scaled = median(syllables_part$sylLen, na.rm = TRUE)
      if (any(!is.na(syllables_part$pauseLen))) {
        median_scaled = median_scaled + median(syllables_part$pauseLen, na.rm = TRUE)
      }
      interburst = ifelse(!is.na(median_scaled) & length(median_scaled) > 0,
                          median_scaled,
                          shortestSyl)
    }

    bursts_part = findBursts(
      ampl = ampl,
      step = step,
      windowLength = windowLength,
      interburst = interburst,
      burstThres = threshold,
      peakToTrough = peakToTrough,
      troughLocation = troughLocation,
      scale = 'dB'
    )
    syllables_part[, c('start', 'end')] = syllables_part[, c('start', 'end')] +
      timeShift + analyze_from
    bursts_part$time = bursts_part$time + timeShift + analyze_from

    # start next part just before the last detected syllable if it looks like
    # this syllable might be incomplete (ends within 100 ms of the end of sound_part)
    if (!is.na(syllables_part$start[1]) &&
        (analyze_to - (syllables_part$end[nrow(syllables_part)] - timeShift) < 100)) {
      analyze_from = syllables_part$start[nrow(syllables_part)] - timeShift - shortestPause
      # remove syllables and bursts from the overlapping part
      syllables_part = syllables_part[-nrow(syllables_part), ]
      bursts_part = bursts_part[bursts_part$time < analyze_from, ]
    } else {
      analyze_from = analyze_to
    }
    if (is.null(propNoise_user)) propNoise = NULL  # reset in next part
    if (is.null(SNR_user)) SNR = NULL              # reset in next part
    analyze_to = min(analyze_from + maxDur * 1000, dur_total)
    if (analyze_to >= dur_total) stopNextTime = TRUE

    # add to growing syllables/bursts for the entire file
    if (is.null(syllables)) {
      syllables = syllables_part
    } else {
      syllables = rbind(syllables, syllables_part)
    }
    if (is.null(bursts)) {
      bursts = bursts_part
    } else {
      bursts = rbind(bursts, bursts_part)
    }
    if (stopNextTime) break
  }
  # end of WHILE loop for processing long sounds

  # in case of long files, interbursts for the first burst in a new analyzed
  # fragment become NAs - recalculate
  syllables = na.omit(syllables)
  if (nrow(syllables) > 0) {
    syllables$syllable = 1:nrow(syllables)
  } else {
    syllables = data.frame(syllable = NA,
                           start = NA, end = NA,
                           sylLen = NA, pauseLen = NA)
  }
  bursts = bursts[which(!is.na(bursts$time)), ]
  if (nrow(bursts) < 1) {
    bursts = data.frame(time = NA, ampl = NA, interburst = NA)
  } else if (nrow(bursts) > 1) {
    for (i in 2:nrow(bursts)) {
      if (is.na(bursts$interburst[i]))
        bursts$interburst[i] = bursts$time[i] - bursts$time[i - 1]
    }
  }


  ## save all extracted syllables as separate audio files for easy examination
  if (is.character(saveAudio) && !is.na(syllables$sylLen[1])) {
    addSil = rep(0, addSilence * samplingRate / 1000)
    for (i in 1:nrow(syllables)) {
      from = max(1, samplingRate * ((syllables$start[i]) / 1000))  #  - windowLength / 2
      to = min(length(sound), samplingRate * ((syllables$end[i]) / 1000))
      temp = c(addSil, sound[from:to], addSil)
      if (is.character(x)) {
        bx = basename(x)
        name_noExt = substr(bx, 1, nchar(bx) - 4)
      } else {
        name_noExt = 'sound'
      }
      filename_i = paste0(
        saveAudio, name_noExt, '_', round(syllables$start[i], 0),
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

  result = list(
    syllables = syllables[, c('syllable', 'start', 'end', 'sylLen', 'pauseLen')],
    bursts = bursts)
  return(result)
}


#' Segment a sound
#'
#' Finds syllables and bursts separated by background noise in long recordings
#' (up to 1-2 hours of audio per file). Syllables are defined as continuous
#' segments that seem to be different from noise based on amplitude and/or
#' spectral similarity thresholds. Bursts are defined as local maxima in signal
#' envelope that are high enough both in absolute terms (relative to the global
#' maximum) and with respect to the surrounding region (relative to local
#' mimima). See vignette('acoustic_analysis', package = 'soundgen') for details.
#'
#' Algorithm: for each chunk at most \code{maxDur} long, first the audio
#' recording is partitioned into signal and noise regions: the quietest and most
#' stable regions are located, and noise threshold is defined from a
#' user-specified proportion of noise in the recording (\code{propNoise}) or, if
#' \code{propNoise = NULL}, from the lowest local maximum in the density
#' function of a weighted product of amplitude and stability (that is, we assume
#' that quiet and stable regions are likely to represent noise). Once we know
#' what the noise looks like - in terms of its typical amplitude and/or spectrum
#' - we derive signal contour as its difference from noise at each time point.
#' If \code{method = 'env'}, this is Hilbert transform minus noise, and if
#' \code{method = 'spec' or 'mel'}, this is the inverse of cosine similarity
#' between the spectrum of each frame and the estimated spectrum of noise
#' weighted by amplitude. By default, signal-to-noise ratio (SNR) is estimated
#' as half-median of above-noise signal, but it is recommended that this
#' parameter is adjusted by hand to suit the purposes of segmentation, as it is
#' the key setting that controls the balance between false negatives (missing
#' faint signals) and false positives (hallucinating signals that are actually
#' noise). Note also that effects of echo or reverberation can be taken into
#' account: syllable detection threshold may be raised following powerful
#' acoustic bursts with the help of the \code{reverbPars} argument. At the final
#' stage, continuous "islands" SNR dB above noise level are detected as
#' syllables, and "peaks" on the islands are detected as bursts. The algorithm
#' is very flexible, but the parameters may be hard to optimize by hand. If you
#' have an annotated sample of the sort of audio you are planning to analyze,
#' with syllables and/or bursts counted manually, you can use it for automatic
#' optimization of control parameters (see \code{\link{optimizePars}}.
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
#' @param maxDur long files are split into chunks \code{maxDur} s in duration to
#'   avoid running out of RAM; the outputs for all fragments are glued together,
#'   but plotting is switched off. Note that noise profile is estimated in each
#'   chunk separately, so set it low if the background noise is highly variable
#' @param reportEvery report estimated time left every ... iterations (NA = no
#'   reporting, NULL = default frequency)
#' @param plot if TRUE, produces a segmentation plot
#' @param savePlots full path to the folder in which to save the plot(s). NULL =
#'   don't save. Note that the html file with clickable plots will only work if
#'   the plots are saved in the same folder as input audio files
#' @param saveAudio full path to the folder in which to save audio files (one
#'   per detected syllable)
#' @param addSilence if syllables are saved as separate audio files, they can be
#'   padded with some silence (ms)
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
#'                  pitch = c(368, 284), temperature = .001)
#' # add noise so SNR decreases from 20 to 0 dB from syl1 to syl4
#' sound = sound + runif(length(sound), -10 ^ (-20 / 20), 10 ^ (-20 / 20))
#' # osc(sound, samplingRate = 16000, dB = TRUE)
#' # spectrogram(sound, samplingRate = 16000, osc = TRUE)
#' # playme(sound, samplingRate = 16000)
#'
#' s = segment(sound, samplingRate = 16000, plot = TRUE)
#' s
#'
#' # customizing the plot
#' segment(sound, samplingRate = 16000, plot = TRUE,
#'         sylPlot = list(lty = 2, col = 'gray20'),
#'         burstPlot = list(pch = 16, col = 'gray80'),
#'         specPlot = list(color.palette = 'heat.colors'),
#'         xlab = 'Some custom label', cex.lab = 1.2,
#'         showLegend = TRUE,
#'         main = 'My awesome plot')
#' \dontrun{
#' # set SNR manually to control detection threshold
#' s = segment(sound, samplingRate = 16000, SNR = 1, plot = TRUE)
#'
#' # Download 260 sounds from the supplements to Anikin & Persson (2017) at
#' # http://cogsci.se/publications.html
#' # unzip them into a folder, say '~/Downloads/temp'
#' myfolder = '~/Downloads/temp260'  # 260 .wav files live here
#' s = segment(myfolder, propNoise = .05, SNR = 3, verbose = TRUE)
#'
#' # Check accuracy: import a manual count of syllables (our "key")
#' key = segmentManual  # a vector of 260 integers
#' trial = as.numeric(s$summary$nBursts)
#' cor(key, trial, use = 'pairwise.complete.obs')
#' boxplot(trial ~ as.integer(key), xlab='key')
#' abline(a=0, b=1, col='red')
#'
#' # or look at the detected syllables instead of bursts:
#' cor(key, s$summary$nSyl, use = 'pairwise.complete.obs')
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
  windowLength = 40,
  step = NULL,
  overlap = 80,
  reverbPars = list(reverbDelay = 70, reverbSpread = 130,
                    reverbLevel = -35, reverbDensity = 50),
  interburst = NULL,
  peakToTrough = SNR + 3,
  troughLocation = c('left', 'right', 'both', 'either')[4],
  summaryFun = c('median', 'sd'),
  maxDur = 30,
  reportEvery = NULL,
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
  filenames = sound = NULL
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
    sound = x
    nFiles = 1
    filenames_base = 'sound'
    if (is.null(samplingRate))
      stop('samplingRate must be provided if input is a numeric vector')
  } else if (class(x) == 'Wave') {
    # input is a Wave object
    sound = x@left
    nFiles = 1
    filenames_base = 'sound'
    samplingRate = x@samp.rate
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
  if (class(savePlots) == 'character' && nchar(savePlots) > 0) {
    if (!dir.exists(savePlots)) dir.create(savePlots)
  }

  ## Prepare a list of arguments to pass to segmentSound()
  myPars = mget(names(formals()), sys.frame(sys.nframe()))
  # exclude unnecessary args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'verbose', 'reportEvery', 'summaryFun', 'samplingRate',
    'reverbPars', 'sylPlot', 'burstPlot', 'specPlot')]  # otherwise flattens lists
  # exclude ...
  myPars = myPars[1:(length(myPars)-1)]
  # add back arguments that are lists
  myPars$sylPlot = sylPlot
  myPars$burstPlot = burstPlot
  myPars$specPlot = specPlot
  myPars$reverbPars = reverbPars

  ## Run the analysis
  syllables = bursts = mysum = vector('list', nFiles)
  names(syllables) = names(bursts) = filenames_base
  for (i in 1:nFiles) {
    failed = FALSE
    # import file
    if (!is.null(filenames)) {
      fi = filenames[i]
      ext_i = substr(fi, nchar(fi) - 3, nchar(fi))
      if (ext_i %in% c('.wav', '.WAV')) {
        sound_wave = try(tuneR::readWave(filenames[i]))
      } else {
        sound_wave = try(tuneR::readMP3(filenames[i]))
      }
      if (class(sound_wave) == 'try-error') {
        failed = TRUE
      } else {
        sound = as.numeric(sound_wave@left)
        samplingRate = sound_wave@samp.rate
      }
    }

    # segment file
    seg = try(do.call('segmentSound', c(
      list(sound, samplingRate = samplingRate, plotname = filenames_base[i]),
      myPars, ...)
    ))
    if (class(seg) == 'try-error') failed = TRUE
    if (failed) {
      if (nFiles > 1) {
        warning(paste('Failed to process file', filenames[i]))
      } else {
        warning('Failed to process the input')
      }
      seg = list(
        syllables = data.frame(syllable = NA,
                               start_idx = NA, end_idx = NA,
                               start = NA, end = NA,
                               sylLen = NA, pauseLen = NA),
        bursts = data.frame(time = NA, ampl = NA, interburst = NA)
      )
    }
    syllables[[i]] = seg$syllables
    bursts[[i]] = seg$bursts

    # summarize (optional)
    if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
      sum_syl = summarizeAnalyze(
        seg$syllables[, c('sylLen', 'pauseLen')],
        summaryFun = summaryFun,
        var_noSummary = NULL)
      sum_bursts = summarizeAnalyze(
        seg$bursts[, 'interburst', drop = FALSE],
        summaryFun = summaryFun,
        var_noSummary = NULL)
      temp = as.data.frame(c(
        list(nSyl = if (failed) NA else sum(!is.na(seg$syllables$start))),
        sum_syl,
        list(nBursts = if (failed) NA else sum(!is.na(seg$bursts$time))),
        sum_bursts
      ))
      temp[apply(temp, c(1, 2), is.nan)] = NA
      mysum[[i]] = temp
    } else {
      mysum = NULL
    }

    # report time
    if (is.null(reportEvery) || is.finite(reportEvery)) {
      reportTime(i = i, nIter = nFiles, reportEvery = reportEvery,
                 time_start = time_start, jobs = filesizes)
    }
  }

  if (class(savePlots) == 'character' && nchar(savePlots) > 0) {
    if (!dir.exists(savePlots)) dir.create(savePlots)
    htmlPlots(savePlots, myfiles = filenames, width = paste0(width, units))
  }

  ## Prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    # rbind summaries
    mysum_all = cbind(data.frame(file = filenames_base),
                      do.call(rbind, mysum))
  } else {
    mysum_all = NULL
  }
  if (nFiles == 1) {
    # unlist syllables & bursts
    syllables = syllables[[1]]
    bursts = bursts[[1]]
  }
  output = list(
    syllables = syllables,
    bursts = bursts,
    summary = mysum_all
  )

  invisible(output)
}


#' Segment folder
#' @param ... any input parameters
segmentFolder = function(...) {
  message('segmentFolder() is deprecated; please use segment() instead')
}
