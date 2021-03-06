% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectralDescr.R
\name{harmEnergy}
\alias{harmEnergy}
\title{Energy in harmonics}
\usage{
harmEnergy(pitch, s, freqs = NULL, coef = 1.25)
}
\arguments{
\item{pitch}{pitch estimates, Hz (vector)}

\item{s}{spectrogram (ncol = length(pitch))}

\item{freqs}{as.numeric(rownames(s)) * 1000}

\item{coef}{calculate above pitch * coef}
}
\description{
Internal soundgun function
}
\details{
Calculates the % of energy in harmonics based on the provided pitch estimate
}
\keyword{internal}
