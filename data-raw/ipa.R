# Manually measured frequencies of the first four formants in 21 vowels from "Seeing Speech"
# http://www.seeingspeech.ac.uk
# ipa = read.csv('/home/allgoodguys/Documents/Research/experiments/acousticSpace/formants/ipa/ipa_vwl21_F1.F4.csv')[, c('ipa', 'F1', 'F2', 'F3', 'F4')]
# for (i in 1:nrow(ipa)) {
#   ipa[i, c('F1Rel', 'F2Rel', 'F3Rel', 'F4Rel')] = round(schwa(
#     formants = as.numeric(ipa[i, c('F1', 'F2', 'F3', 'F4')])
#   )$ff_relative_semitones, 2)
# }
# from non-ASCII to unicode, otherwise CRAN complains (actually, some versions on CRAN still complain)
# see https://stackoverflow.com/questions/11452796/how-to-use-a-non-ascii-symbol-e-g-%C2%A3-in-an-r-package-function
# ipa$ipa[3] = intToUtf8(0x026a)  # or could try "\u026a"
# ipa$ipa[5] = intToUtf8(0x00f8)
# ipa$ipa[6] = intToUtf8(0x025b)
# ipa$ipa[7] = intToUtf8(0x0153)
# ipa$ipa[9] = intToUtf8(0x0276)
# ipa$ipa[10] = intToUtf8(0x0268)
# ipa$ipa[11] = intToUtf8(0x0289)
# ipa$ipa[12] = intToUtf8(0x0259)
# ipa$ipa[13] = intToUtf8(0x026f)
# ipa$ipa[15] = intToUtf8(0x028a)
# ipa$ipa[16] = intToUtf8(0x0264)
# ipa$ipa[18] = intToUtf8(0x028c)
# ipa$ipa[19] = intToUtf8(0x0254)
# ipa$ipa[20] = intToUtf8(0x0251)
# ipa$ipa[21] = intToUtf8(0x0252)

# For data.R (if used)
# #' Manually measured frequencies of the first four formants in 21 vowels from
# #' "Seeing Speech" (http://www.seeingspeech.ac.uk)
# #'
# #' A dataframe of 21 rows (one per vowel) and 9 columns: "ipa" = vowel, "F1...4"
# #' = measured formant frequencies (Hz) in the initial stable region of each
# #' vowel, "F1Rel...F4Rel" = formant frequencies relative to the neutral
# #' equidistant formant frequencies in the schwa (semitones)
# #' @examples
# #' plot(ipa$F1, ipa$F2, type = 'n')
# #' text(ipa$F1, ipa$F2, label = ipa$ipa)
# "ipa"
# usethis::use_data(ipa, overwrite = TRUE)


## Relative frequencies of F1 and F2 (semitones above or below schwa based on estimated VTL) from Hillenbrand (). Audio and formant measurements freely available online: The dataset below is the result of modeling with brms: mvbind(F1Rel, F2Rel) ~ vowel. It shows the most likely location of each vowel in the F1rel-F2rel space.
# hillenbrand = read.csv('/home/allgoodguys/Documents/Research/methods/sound-synthesis/vowels_hillenbrand/hillenbrand_vowelCentroids.csv')[, c('vowel', 'F1_fit', 'F2_fit')]
# colnames(hillenbrand) = c('vowel', 'F1Rel', 'F2Rel')
usethis::use_data(hillenbrand, overwrite = TRUE)
