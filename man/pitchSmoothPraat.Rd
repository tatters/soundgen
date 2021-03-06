% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filters.R
\name{pitchSmoothPraat}
\alias{pitchSmoothPraat}
\title{Pitch smoothing as in Praat}
\usage{
pitchSmoothPraat(pitch, bandwidth, samplingRate, plot = FALSE)
}
\arguments{
\item{pitch}{numeric vector of pitch values (NA = unvoiced)}

\item{bandwidth}{the bandwidth of low-pass filter, Hz (high = less smoothing,
close to zero = more smoothing)}

\item{samplingRate}{the number of pitch values per second}

\item{plot}{if TRUE, plots the original and smoothed pitch contours}
}
\description{
Smoothes an intonation (pitch) contour with a low-pass filter, as in Praat
(http://www.fon.hum.uva.nl/praat/). Algorithm: interpolates missing values
(unvoiced frames), performs FFT to obtain the spectrum, multiplies by a
Gaussian filter, performs an inverse FFT, and fills the missing values back
in. The \code{bandwidth} parameter is about half the cutoff frequency (ie
some frequencies will still be present up to ~2 * bandwidth)
}
\examples{
pitch = c(NA, NA, 405, 441, 459, 459, 460, 462, 462, 458, 458, 445, 458, 451,
444, 444, 430, 416, 409, 403, 403, 389, 375, NA, NA, NA, NA, NA, NA, NA, NA,
NA, 183, 677, 677, 846, 883, 886, 924, 938, 883, 946, 846, 911, 826, 826,
788, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 307,
307, 368, 377, 383, 383, 383, 380, 377, 377, 377, 374, 374, 375, 375, 375,
375, 368, 371, 374, 375, 361, 375, 389, 375, 375, 375, 375, 375, 314, 169,
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 238, 285, 361, 374, 375, 375,
375, 375, 375, 389, 403, 389, 389, 375, 375, 389, 375, 348, 361, 375, 348,
348, 361, 348, 342, 361, 361, 361, 365, 365, 361, 966, 966, 966, 959, 959,
946, 1021, 1021, 1026, 1086, 1131, 1131, 1146, 1130, 1172, 1240, 1172, 1117,
1103, 1026, 1026, 966, 919, 946, 882, 832, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA)
pitchSmoothPraat(pitch, bandwidth = 10, samplingRate = 40, plot = TRUE)
pitchSmoothPraat(pitch, bandwidth = 2, samplingRate = 40, plot = TRUE)
}
\seealso{
\code{\link{analyze}}
}
