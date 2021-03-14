# usethis::use_data(noiseThresholdsDict, BaNaRatios, spreadSpecCoef, phonCurves, internal = TRUE, overwrite = TRUE)

#' Shiny app defaults
#'
#' A list of default values for Shiny app soundgen_app() - mostly the same as
#' the defaults for soundgen(). NB: if defaults change, this has to be
#' updated!!!
"defaults"

#' Manual counts of syllables in 260 sounds
#'
#' A vector of the number of syllables in the corpus of 260 human non-linguistic
#' emotional vocalizations from Anikin & Persson (2017). The corpus can be
#' downloaded from http://cogsci.se/publications.html
"segmentManual"


#' Manual pitch estimation in 260 sounds
#'
#' A vector of manually verified pitch values per sound in the corpus of 590
#' human non-linguistic emotional vocalizations from Anikin & Persson (2017).
#' The corpus can be downloaded from http://cogsci.se/publications.html
"pitchManual"

#' Manually corrected pitch contours in 260 sounds
#'
#' A dataframe of 260 rows and two columns: "file" for filename in the corpus
#' (Anikin & Persson, 2017) and "pitch" for pitch values per frame. The corpus
#' can be downloaded from http://cogsci.se/publications.html
"pitchContour"

#' Conversion table from Hz to musical notation
#'
#' A dataframe of 192 rows and 2 columns: "note" and "freq" (Hz). Range: C-5
#' (0.51 Hz) to B10 (31608.53 Hz)
"notesDict"

#' Manually measured frequencies of the first four formants in 21 vowels from
#' "Seeing Speech" (http://www.seeingspeech.ac.uk)
#'
#' A dataframe of 21 rows (one per vowel) and 9 columns: "ipa" = vowel, "F1...4"
#' = measured formant frequencies (Hz) in the initial stable region of each
#' vowel, "F1Rel...F4Rel" = formant frequencies relative to the neutral
#' equidistant formant frequencies in the schwa (semitones)
#' @examples
#' plot(ipa$F1, ipa$F2, type = 'n')
#' text(ipa$F1, ipa$F2, label = ipa$ipa)
"ipa"
