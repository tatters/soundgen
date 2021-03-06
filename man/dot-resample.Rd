% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resample.R
\name{.resample}
\alias{.resample}
\title{Resample per sound}
\usage{
.resample(
  audio,
  mult = NULL,
  samplingRate_new = NULL,
  lowPass = TRUE,
  na.rm = FALSE,
  plot = FALSE,
  width = 900,
  height = 500,
  units = "px",
  res = NA,
  ...
)
}
\arguments{
\item{audio}{a list returned by \code{readAudio}}

\item{mult}{multiplier of sampling rate: new sampling rate = old sampling
rate x mult, so 1 = no effect, >1 = upsample, <1 = downsample}

\item{samplingRate_new}{an alternative to \code{mult} provided that the old
\code{samplingRate is know} (NB: \code{mult} takes precedence)}

\item{lowPass}{if TRUE, applies a low-pass filter prior to decimation to
avoid aliasing}

\item{na.rm}{if TRUE, NAs are interpolated, otherwise they are preserved in
the output}

\item{plot}{should a spectrogram be plotted? TRUE / FALSE}

\item{width}{graphical parameters for saving plots passed to
\code{\link[grDevices]{png}}}

\item{height}{graphical parameters for saving plots passed to
\code{\link[grDevices]{png}}}

\item{units}{graphical parameters for saving plots passed to
\code{\link[grDevices]{png}}}

\item{res}{graphical parameters for saving plots passed to
\code{\link[grDevices]{png}}}

\item{...}{other graphical parameters}
}
\description{
Internal soundgen function
}
\keyword{internal}
