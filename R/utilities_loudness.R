#' Get loudness per frame
#'
#' Internal soundgen function
#'
#' Takes as input the absolute (not power) spectrum of a single STFT frame and
#' estimates its loudness in sone. See \code{\link{getLoudness}} for details.
#' @inheritParams getLoudness
#' @keywords internal
#' @examples
#' sound = sin(2*pi*1000/16000*(1:1000))
#' sound_scaled = soundgen:::scaleSPL(sound, SPL_measured = 40)
#' spec = spectrogram(
#'   sound_scaled, samplingRate = 16000, windowLength = 10,
#'   normalize = FALSE, output = 'original')[, 10]
#' # plot(spec, type = 'l')
#' soundgen:::getLoudnessPerFrame(spec, samplingRate = 16000)  # 1 sone
getLoudnessPerFrame = function(spec,
                               samplingRate,
                               spreadSpectrum = TRUE) {
  powerSpec_scaled = matrix(spec ^ 2 / length(spec), ncol = 1)
  audSpec = tuneR::audspec(powerSpec_scaled, sr = samplingRate,
                           fbtype = 'bark')$aspectrum  # plot(audSpec, type = 'l')
  if (spreadSpectrum) audSpec = spreadSpec(audSpec)
  audSpec_dB = 10 * log10(audSpec)
  n_phonCurve = which.min(abs(
    audSpec_dB[8] - as.numeric(names(phonCurves))))  # 8 barks = 1 kHz
  curve = phonCurves[[n_phonCurve]][1:length(audSpec_dB), ]
  audSpec_phon = audSpec_dB + curve$spl[8] - curve$spl
  audSpec_phon[audSpec_phon < curve$hearingThres_dB | audSpec_phon < 0] = 0
  # plot(audSpec_phon, type = 'l')
  audSpec_sone = phon2sone(audSpec_phon)
  # plot(audSpec_sone, type = 'l')
  loudness = sum(audSpec_sone)
  windowLength = length(spec) * 2 / samplingRate * 1000
  loudness_corrected = loudness / (5.73 +  6.56 * windowLength ^ .35) /
    (.0357 + .0345 * samplingRate ^ .3113)
  return(loudness_corrected)
}


#' Scale SPL
#'
#' Internal soundgen function
#'
#' Converts a sound from SPL on any scale to a desired level of dB SPL.
#' See Timoney et al. (2004) "Implementing loudness models in MATLAB"
#' @param x numeric vector ranging from -1 to +1
#' @inheritParams getLoudness
#' @keywords internal
#' @examples
#' sound = runif(100) * getSmoothContour(c(0, 1, 0), len = 100)
#' # plot(sound, type = 'l')
#' sound_scaled = soundgen:::scaleSPL(sound)
#' # plot(sound_scaled, type = 'l')
scaleSPL = function(x, SPL_measured = 70, Pref = 2e-5) {
  x_refScaled = (x - mean(x)) / Pref  # range(x_refScaled)
  RMS = sqrt(mean(x_refScaled ^ 2))
  SPL_internal = 20 * log10(RMS)  # dB-SPL
  c = 10 ^ ((SPL_measured - SPL_internal) / 20)
  x_scaled = c * x_refScaled  # range(x_scaled)
  # plot(x_scaled[5000:6000], type = 'l')
  # check that the new RMS is SPL_measured:
  # 20 * log10(sqrt(mean(x_scaled^2))) should be ~SPL_measured
  return(x_scaled)
}


#' iso226
#'
#' Internal soundgen function
#'
#' Calculates equal loudness curves according to the ISO226 standard. Expected
#' range of input values in phon is 0 to 90 (1 phon is 1 dB at 1 kHz). The range
#' of evaluated frequencies is 20 to 12500 Hz, with a total of 29 values (so
#' upsample if more resolution is needed, but not that beyond 22 barks it's just
#' assumed to be flat). Translated from the matlab implementation by Jeff Tackett
#' (03/01/05) available from
#' "https://www.mathworks.com/matlabcentral/fileexchange/
#' 7028-iso-226-equal-loudness-level-contour-signal"
#' @return A dataframe containing evaluated frequencies and SPL values
#' @param phon the phon value in dB SPL represented by the loudness curve
#' @keywords internal
#' @examples
#' i = soundgen:::iso226(40)
#' plot(i$curve29, type = 'l')
#' plot(i$curveBark$freq_Hz, i$curveBark$spl, type = 'l')
iso226 = function(phon, nBarks = 22) {
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
  barkFreqs_hz = 600 * sinh(1:nBarks / 6)
  s = spline(y = Lp, x = f, n = 1000)
  ups = data.frame(freq = s$x, spl = s$y)  # upsampled curve
  b = data.frame(
    freq_bark = 1:nBarks,
    freq_Hz = barkFreqs_hz
  )
  freq_KHz = b$freq_Hz / 1000
  b$hearingThres_dB = 3.64 / (freq_KHz ^ 0.8) -
    6.5 * exp(-0.6 * (freq_KHz - 3.3) ^ 2) +
    0.001 * freq_KHz ^ 4
  for (i in 1:nrow(b)) {
    b$spl[i] = ups$spl[which.min(abs(ups$freq - barkFreqs_hz[i]))]
  }

  return(list(
    curve29 = data.frame(freq = f, spl = Lp),
    curveBark = b
  ))
}


#' Convert phon to sone
#'
#' Internal soundgen function
#'
#' Source: Timoney, J., Lysaght, T., Schoenwiesner, M., & MacManus, L. (2004).
#' Implementing loudness models in matlab.
#' @param phon loudness level, phon (vectorized)
#' @keywords internal
#' @examples
#' phon = 0:120
#' sone = soundgen:::phon2sone(phon)
#' plot(phon, sone, type = 'b')
#' plot(phon, log2(sone), type = 'b')
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


#' Spread spectrum
#'
#' Internal soundgen function
#'
#' Spreads spectrum to account for frequency masking across critical bands. See
#' Wonho (1999) "Enhanced modified bark spectral distortion (EMBSD)"
#' @param barkSpec a numeric vector of length equal to the number of critical
#'   bands (eg 1 to 24 barks), giving the power in each band on a linear scale
#'   (not dB)
#' @keywords internal
#' @examples
#' barkSpec = c(rep(0, 10), 20000, 2e5, 2e4, 1e4, 2000, rep(0, 8))
#' plot(soundgen:::spreadSpec(barkSpec), type = 'b', col = 'red')
#' points(barkSpec, type = 'b', col = 'blue')
spreadSpec = function(barkSpec) {
  n = length(barkSpec)
  barkSpec_spread = vector('numeric', n)
  for (i in 1:n) {
    barkSpec_spread[i] = sum(spreadSpecCoef[i, 1:n] * barkSpec)
  }
  return(barkSpec_spread)
}
