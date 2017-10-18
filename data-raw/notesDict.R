## conversion from Hz to musical notes (for UI only). See http://www.phy.mtu.edu/~suits/notefreqs.html for reference table. The commented-out section shows how the dictionary was originally compiled
notes = c('C', 'C\U266F', 'D', 'D\U266F', 'E', 'F', 'F\U266F', 'G', 'G\U266F', 'A', 'B\U266D', 'B')
nOct = 11
notes_all = paste0(notes, rep(-5:(nOct - 1), each = 12))
# paste0(notes_all, collapse=', ')

# 440 / 32 = 13.75 - this is A-1, and C0 is 3 semitones higher
c0 = 13.75 * 2 ^ (3 / 12) # 16.3516 Hz exactly
notes_freq = round(c0 * 2 ^ (-60:(12 * nOct - 1) / 12), 2)
# paste0(notes_freq, collapse=', ')

notesDict = data.frame (
  note = notes_all,
  freq = notes_freq,
  stringsAsFactors=F)
# devtools::use_data(notesDict, overwrite = TRUE)
