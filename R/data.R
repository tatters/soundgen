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


#' Conversion table from Hz to semitones above C0 to musical notation
#'
#' A dataframe of 132 rows and 2 columns: "note" and "freq" (Hz)
#'
#' @examples
#' # To recompile:
#' notes = c('C', 'C\U266F', 'D', 'D\U266F', 'E', 'F', 'F\U266F', 'G', 'G\U266F', 'A', 'B\U266D', 'B')
#' nOct = 11
#' notes_all = paste0(notes, rep(0:(nOct - 1), each = 12))
#' # 440 / 32 = 13.75  # A-1, and C0 is 3 semitones higher: 16.3516 Hz exactly.
#' c0 = 13.75 * 2 ^ (3 / 12)
#' notes_freq = round (c0 * 2^(0:(12 * nOct - 1) / 12), 1)
"notesDict"

