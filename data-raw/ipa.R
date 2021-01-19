# Manually measured frequencies of the first four formants in 21 vowels from "Seeing Speech"
# http://www.seeingspeech.ac.uk
# ipa = read.csv('/home/allgoodguys/Documents/Research/experiments/acousticSpace/formants/ipa/ipa_vwl21_F1.F4.csv')[, c('ipa', 'F1', 'F2', 'F3', 'F4')]
# for (i in 1:nrow(ipa)) {
#   ipa[i, c('F1Rel', 'F2Rel', 'F3Rel', 'F4Rel')] = round(schwa(
#     formants = as.numeric(ipa[i, c('F1', 'F2', 'F3', 'F4')])
#   )$ff_relative_semitones, 2)
# }
usethis::use_data(ipa, overwrite = TRUE)

