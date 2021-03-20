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


#' Formants in American vowels
#'
#' Relative frequencies of F1 and F2 (semitones above or below schwa based on
#' estimated VTL) in American English, from Hillenbrand (1995), who measured
#' F1-F4 in ~1.5K recordings (139 speakers, 12 vowels from each). Audio and
#' formant measurements are freely available online:
#' https://homepages.wmich.edu/~hillenbr/voweldata.html. The dataset below is
#' the result of modeling Hillenbrand's data with brms: mvbind(F1rel, F2rel) ~
#' vowel. It shows the most credible location of each vowel centroid in the
#' F1Rel-F2Rel space.
#'
#' A dataframe of 12 observations and 3 columns: "vowel" = vowel (American
#' English), "F1Rel" and "F2Rel" = formant frequencies in semitones relative to
#' their neutral, equidistant positions in a perfectly cylindrical vocal tract.
#' See \code{\link{schwa}} - this is what schwa() returns as
#' $ff_relative_semitones
#'
#' @references Hillenbrand, J., Getty, L. A., Clark, M. J., & Wheeler, K.
#'   (1995). Acoustic characteristics of American English vowels. The Journal of
#'   the Acoustical society of America, 97(5), 3099-3111.
#'
#' @examples
#' plot(hillenbrand$F1Rel, hillenbrand$F2Rel, type = 'n')
#' text(hillenbrand$F1Rel, hillenbrand$F2Rel, labels = hillenbrand$vowel)
"hillenbrand"
