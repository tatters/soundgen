# Equal loudness curves for converting dB to phon
# Translated from the matlab implementation by Jeff Tackett (03/01/05)
# available from https://www.mathworks.com/matlabcentral/fileexchange/
# 7028-iso-226-equal-loudness-level-contour-signal
max_Barks = 27  # max 27 barks (~27000 Hz)
phonLevels = c(seq(0, 20, 1), seq(22, 40, 2), seq(45, 90, 5))
phonCurves = vector('list', length(phonLevels))
names(phonCurves) = phonLevels
for (p in 1:length(phonLevels)) {
  phonCurves[[p]] = iso226(phonLevels[p], nBarks = max_Barks)$curveBark
}
# plot(phonCurves[[1]][, c('freq_Hz', 'hearingThres_dB')])
# devtools::use_data(phonCurves, overwrite = TRUE, internal = TRUE)
