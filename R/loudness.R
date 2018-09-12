# WORK IN PROGRESS: functions for estimating subjective loudness based on auditory spectrum. See Timoney et al. (2004) "Implementing loudness models in MATLAB"


#' Scale SPL
#'
#' Internal soundgen function
#'
#' Converts a sound from SPL on a scale [-1, 1] to a desired level of dB SPL.
#' See Timoney et al. (2004) "Implementing loudness models in MATLAB"
#' @param x numeric vector ranging from -1 to +1
#' @param SPLmeas approximate level of original recording, dB (just a guess)
#' @param Pref reference pressure, Pa
#' @keywords internal
#' @examples
#' sound = runif(1000, -1, 1) * getSmoothContour(c(0, 1, 0), len = 1000)
#' # plot(sound, type = 'l')
#' sound_scaled = scaleSPL(sound)
#' # plot(sound_scaled, type = 'l')
scaleSPL = function(x, SPL_measured = 70, Pref = 2e-5) {
  x_refScaled = (x / Pref)  # range(x_refScaled)
  RMS = sqrt(mean(x_refScaled ^ 2))
  SPL_internal = 20 * log10(RMS)  # dB-SPL
  c = 10 ^ ((SPL_measured - SPL_internal) / 20)
  x_scaled = c * x_refScaled
  # plot(x_scaled[5000:6000], type = 'l')
  return(x_scaled)
}


#' iso226
#'
#' Internal soundgen function
#'
#' Calculates equal loudness curves according to the ISO226 standard. Expected
#' range of input values in phon is 0 to 90 (1 phon is 1 dB at 1 kHz). The range
#' of evaluated frequencies is 20 to 12500 Hz, with a total of 29 values (so
#' upsample if more resolution is needed). Translated from the matlab
#' implementation by Jeff Tackett (03/01/05) available from
#' https://www.mathworks.com/matlabcentral/fileexchange/
#' 7028-iso-226-equal-loudness-level-contour-signal
#' @return A dataframe containing evaluated frequencies and SPL values
#' @param phon the phon value in dB SPL represented by the loudness curve
#' @keywords internal
#' @examples
#' i = iso226(40)
#' plot(i$curve29, type = 'l')
#' plot(i$curveBark$freq_Hz, i$curveBark$spl, type = 'l')
iso226 = function(phon) {
  #  Table from ISO 226
  f = c(20, 25, 31.5, 40, 50, 63, 80, 100, 125, 160,
        200, 250, 315, 400, 500, 630, 800, 1000, 1250, 1600,
        2000, 2500, 3150, 4000, 5000, 6300, 8000, 10000, 12500)

  af = c(0.532, 0.506, 0.480, 0.455, 0.432, 0.409, 0.387, 0.367, 0.349, 0.330,
         0.315, 0.301, 0.288, 0.276, 0.267, 0.259, 0.253, 0.250, 0.246, 0.244,
         0.243, 0.243, 0.243, 0.242, 0.242, 0.245, 0.254, 0.271, 0.301)

  Lu = c(-31.6, -27.2, -23.0, -19.1, -15.9, -13.0, -10.3, -8.1, -6.2, -4.5,
         -3.1, -2.0, -1.1, -0.4, 0.0, 0.3, 0.5, 0.0, -2.7, -4.1,
         -1.0, 1.7, 2.5, 1.2, -2.1, -7.1, -11.2, -10.7, -3.1)

  Tf = c(78.5, 68.7, 59.5, 51.1, 44.0, 37.5, 31.5, 26.5, 22.1, 17.9,
         14.4, 11.4, 8.6, 6.2, 4.4, 3.0, 2.2, 2.4, 3.5, 1.7,
         -1.3, -4.2, -6.0, -5.4, -1.5, 6.0, 12.6, 13.9, 12.3)

  # Warn if phon is outside the covered range
  if (phon < 0 | phon > 90) {
    warning('Valid range 0 to 90; extrapolating beyond may be incorrect')
  }

  # Deriving sound pressure level from loudness level (iso226 sect 4.1)
  Af = 4.47e-3 * (10 ^ (0.025 * phon) - 1.15) +
    (0.4 * 10 ^ (((Tf + Lu) / 10) - 9)) ^ af
  Lp = ((10 / af) * log10(Af)) - Lu + 94

  # Calculate on the bark scale
  barkFreqs_hz = 600 * sinh(1:24 / 6)
  s = spline(y = Lp, x = f, n = 1000)
  ups = data.frame(freq = s$x, spl = s$y)  # upsampled curve
  b = data.frame(
    freq_bark = 1:24,
    freq_Hz = barkFreqs_hz
  )
  for (i in 1:nrow(b)) {
    b$spl[i] = ups$spl[which.min(abs(ups$freq - barkFreqs_hz[i]))]
  }
  return(list(
    curve29 = data.frame(freq = f, spl = Lp),
    curveBark = b
  ))
}

phon2sone = function(phon) {
  sone = phon
  idx1 = which(phon < 40)
  idx2 = which(phon >= 40)
  sone[idx1] = (phon[idx1] / 40) ^ 2.642
  sone[idx2] = 2 ^ ((phon[idx2] - 40) / 10)
  return(sone)
}
# idx_phon = seq(0, 140, 10)
# idx_sone = phon2sone(idx_phon)
# plot(idx_phon, idx_sone, type = 'b')

phonLevels = seq(0, 90, 5)
phonCurves = vector('list', length(phonLevels))
names(phonCurves) = phonLevels
for (p in 1:length(phonLevels)) {
  phonCurves[[p]] = iso226(phonLevels[p])$curveBark
}
# NB: add phonCurves to data saved with package!

getLoudness = function(x,
                       samplingRate = NULL,
                       windowLength = 20,
                       step = NULL,
                       overlap = 75,
                       plot = TRUE,
                       mar = c(5.1, 4.1, 4.1, 2.1),
                       ...) {
  # import sound
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  if (class(x) == 'character') {
    sound_wav = tuneR::readWave(x)
    samplingRate = sound_wav@samp.rate
    sound = sound_wav@left
  } else if (class(x) == 'numeric' & length(x) > 1) {
    if (is.null(samplingRate)) {
      stop ('Please specify samplingRate, eg 44100')
    } else {
      sound = x
    }
  }

  # get auditory spectrum
  # powerSpec = tuneR::powspec(sound, sr = samplingRate, wintime = windowLength / 1000, step = step / 1000)
  spctrgm = spectrogram(sound, samplingRate = samplingRate, windowLength = windowLength, step = step, output = 'original', plot = plot, mar = mar, ...)
  powerSpec = spctrgm ^ 2
  # image(t(powerSpec))
  audSpec = tuneR::audspec(powerSpec, sr = samplingRate, fbtype = 'bark')$aspectrum
  # image(t(audSpec))

  # convert spectrum to sones
  specSones = matrix(0, nrow = nrow(audSpec), ncol = ncol(audSpec))
  for (i in 1:ncol(specSones)) {
    # power spectrum in dB SPL
    y = 10 * log10(audSpec[, i])
    # plot(y, type = 'b')

    # dB SPL to phons
    n_phonCurve = which.min(abs(y[9] - as.numeric(names(phonCurves))))  # 9 barks = 1000 Hz
    curve = phonCurves[[n_phonCurve]][1:length(y), ]  # correction curve for frame i
    y_phon = y + curve$spl[9] - curve$spl
    # plot(y_phon, type = 'b')

    # ignore frequency bins below hearing threshold
    y_phon[y_phon < 0] = 0  # change to freq-dependent hearing threshold instead of 0!

    # phons to sones
    specSones[, i] = phon2sone(y_phon)
    # plot(specSones[, i], type = 'b')
  }
  # image(t(specSones))
  loudness = apply(specSones, 2, sum)

  # plotting
  if (plot) {
    # spectrogram(sound, samplingRate = 16000, osc = TRUE)
    op = par(c('mar', 'new')) # save user's original pars
    par(new = TRUE)
    plot(x = seq(1, length(sound) / samplingRate * 1000, length.out = length(loudness)),
         y = loudness,
         type = "b", axes = FALSE, bty = "n", xlab = "", ylab = "")
    axis(side = 4, at = pretty(range(loudness)))
    mtext("Loudness, sone", side = 4, line = 3)
    par(new = FALSE, mar = mar)
    par('mar' = op$mar, 'new' = op$new)  # restore original pars
  }
  return(specSones)
}
# sound = soundgen()
# spectrogram(sound, 16000)
# spectrogram(sound, 16000, osc = TRUE)
# spectrogram(sound, 16000, osc = TRUE, mar = c(5, 4, 1, 7))
# l = getLoudness(sound, samplingRate = 16000, windowLength = 20, overlap = 75, plot = TRUE, mar = c(5.1, 4.1, 4.1, 4.1))


# dB2phon = function(Ci) {  # see Wonhos_Dissertation.pdf, function dbtophon()
#   T = 10 * log10(Ci[4:18]) # COMPUTE BARK 4 TO 18 ONLY IN dB
#   for (i in 1:15) {
#     j = 1
#     while T(i) >= eqlcon(j,i)
#     j = j + 1;
#     if j == 16
#     fprintf(1,'ERROR\n')
#   }
#
#   if j == 1
#   P_XX(i) = phons(1);
#   else
#     t1 = (T(i) - eqlcon(j-1,i))/(eqlcon(j,i) - eqlcon(j-1,i));
#   P_XX(i) = phons(j-1) + t1*(phons(j) - phons(j-1));
# }


#
# sound = soundgen()
# s = spectrogram(sound, samplingRate = 16000, output = 'original', windowLength = 40, step = 10)
# image(s ^ 2)
# p = tuneR::powspec(sound, sr = 16000, wintime = .04, step = .01)
# image(p)
#
# a = tuneR::audspec(p, sr = 16000, fbtype = 'bark')$aspectrum
# image(a)
# View(a)
# range(a)
# range(20*log10(as.numeric(a)))
#
# a = analyze('~/Downloads/117.wav')
# mean(a$specCentroid, na.rm = T)
