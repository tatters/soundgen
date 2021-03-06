% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities_pitch_postprocessing.R
\name{pathfinding_fast}
\alias{pathfinding_fast}
\title{Path through pitch candidates: fast}
\usage{
pathfinding_fast(
  pitchCands,
  pitchCert,
  pitchSource,
  manual,
  pitchCenterGravity,
  certWeight
)
}
\arguments{
\item{pitchCands}{a matrix of multiple pitch candidates per fft frame. Each
column is one fft frame, each row is one candidate (the last row is always
"manual")}

\item{pitchCert}{a matrix of the same dimensionality as pitchCands specifying
our certainty in pitch candidates}

\item{manual}{dataframe giving manual pitch candidates, which the path
MUST go through}

\item{pitchCenterGravity}{numeric vector giving the mean of all pitch
candidates per fft frame weighted by our certainty in each of these
candidates}

\item{certWeight}{(0 to 1) in pitch postprocessing, specifies how much we
prioritize the certainty of pitch candidates vs. pitch jumps / the internal
tension of the resulting pitch curve}
}
\description{
Internal soundgen function.
}
\details{
Uses a quick-and-simple heuristic to find a reasonable path though pitch
candidates. The idea is to start at the median of the center of gravity of
pitch candidates over the first/last few frames and then go over the path
twice (forward and backward), minimizing the cost of transitions at each step
in terms of pitch jumps and distance from high-certainty candidates. The best
of these two paths is accepted.
}
\keyword{internal}
