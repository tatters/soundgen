# A vector of manually estimated (based on checking pitch contours) pitch per file in the corpus of 260 human non-linguistic emotional vocalizations from Anikin & Persson (2017). The corpus can be downloaded from http://cogsci.se/publications/anikin-persson_2017_nonlinguistic-vocs/260sounds_wav.zip
# df = read.csv('/home/allgoodguys/Documents/Studying/Lund_PhD/sounds_corpora/ut_oct2019.csv', stringsAsFactors = FALSE)
# df = df[df$in260 == TRUE & !is.na(df$in260), ]
# pitchContour = df[, c('file', 'pitch')]
# pitchContour$file = substr(pitchContour$file, 5, nchar(pitchContour$file))
# pitchManual = vector('numeric', length = nrow(pitchContour))
# for (i in 1:nrow(pitchContour)) {
#   pm = as.numeric(unlist(strsplit(pitchContour$pitch[i], ',')))
#   pitchManual[i] = mean(pm, na.rm = TRUE)
# }
# names(pitchManual) = pitchContour$file
usethis::use_data(pitchManual, overwrite = TRUE)
usethis::use_data(pitchContour, overwrite = TRUE)
