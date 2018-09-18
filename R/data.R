# devtools::use_data(noiseThresholdsDict, BaNaRatios, internal = TRUE, overwrite = TRUE)

#' Shiny app defaults
#'
#' A list of default values for Shiny app soundgen_app() - mostly the same as
#' the defaults for soundgen(). NB: if defaults change, this has to be
#' updated!!!
"defaults"

#' Manual counts of syllables in 260 sounds
#'
#' A vector of the number of syllables in the corpus of 260 human non-linguistic emotional vocalizations from Anikin & Persson (2017). The corpus can be downloaded from http://cogsci.se/personal/results/01_anikin-persson_2016_naturalistics-non-linguistic-vocalizations/01_anikin-persson_2016_naturalistic-non-linguistic-vocalizations.html
"segmentManual"


#' Manual pitch estimation in 260 sounds
#'
#' A vector of manually verified pitch values per sound in the corpus of 590 human non-linguistic emotional vocalizations from Anikin & Persson (2017). The corpus can be downloaded from http://cogsci.se/personal/results/01_anikin-persson_2016_naturalistics-non-linguistic-vocalizations/01_anikin-persson_2016_naturalistic-non-linguistic-vocalizations.html
"pitchManual"


#' Conversion table from Hz to musical notation
#'
#' A dataframe of 192 rows and 2 columns: "note" and "freq" (Hz). Range: C-5
#' (0.51 Hz) to B10 (31608.53 Hz)
"notesDict"


#' Equal loudness curves for converting dB SPL to phon
#'
#' A list of dataframes, from 0 to 90 phon (see the name of each dataframe).
#' Translated from the matlab implementation by Jeff Tackett (03/01/05)
#' available from https://www.mathworks.com/matlabcentral/fileexchange/
#' 7028-iso-226-equal-loudness-level-contour-signal
"phonCurves"


#' Matrix of scaling coefficients for spreading bark spectrum
#'
#' See Wonho (1999) "Enhanced modified bark spectral distortion (EMBSD)".
#' Original: Schroeder, M. R., Atal, B. S., & Hall, J. L. (1979). Optimizing
#' digital speech coders by exploiting masking properties of the human ear. The
#' Journal of the Acoustical Society of America, 66(6), 1647-1652. Called by
#' \code{\link{spreadSpec}}
"spreadSpecCoef"
