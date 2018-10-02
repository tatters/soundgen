# devtools::use_data(noiseThresholdsDict, BaNaRatios, spreadSpecCoef, phonCurves, internal = TRUE, overwrite = TRUE)

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
