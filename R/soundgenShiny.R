# Note: might wrap shiny::runApp in suppress.warnings()

#' Interactive sound synthesizer
#'
#' Starts a shiny app that provides an interactive wrapper to
#' \code{\link{soundgen}}. Supported browsers: Firefox / Chrome. Note that the
#' browser has to be able to playback WAV audio files, otherwise there will be
#' no sound.
#' @export
soundgen_app = function() {
  appDir = system.file("shiny", "soundgen_main", package = "soundgen")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}


#' Interactive pitch tracker
#'
#' Starts a shiny app for manually editing pitch contours. Think of it as
#' running \code{\link{analyze}} with manual pitch control. All pitch-dependent
#' descriptives (percentage of voiced frames, energy in harmonics, amplVoiced,
#' etc.) are calculated from the manually corrected pitch contour. Supported
#' browsers: Firefox / Chrome. Note that the browser has to be able to play back
#' WAV audio files, otherwise there will be no sound. The settings in the panels
#' on the left correspond to arguments to \code{\link{analyze}} - see `?analyze`
#' and the vignette on acoustic analysis for help and examples. Loudness and
#' formants are not analyzed to avoid delays; run \code{\link{analyzeFolder}}
#' separately with no pitch tracking (`pitchMethods = NULL`) and merge the two
#' datasets. Same for syllable segmentation: run \code{\link{segmentFolder}}
#' separately since it doesn't depend on accurate pitch tracking.
#'
#' @return The app produces a .csv file with one row per audio file. Apart from
#'   the usual descriptives from analyze(), there are two additional columns:
#'   "time" with time stamps (the midpoint of each STFT frame, ms) and "pitch"
#'   with the manually corrected pitch values for each frame (Hz). To process
#'   pitch contours further in R, do something like:
#'
#' \preformatted{
#' a = read.csv('~/Downloads/output.csv', stringsAsFactors = FALSE)
#' pitch = as.numeric(unlist(strsplit(a$pitch, ',')))
#' mean(pitch, na.rm = TRUE); sd(pitch, na.rm = TRUE)
#' }
#'
#' \bold{Suggested workflow}
#'
#' Start by clicking "Load audio" to upload one or several audio files
#' (wav/mp3). Long files will be very slow, so please cut your audio into
#' manageable chunks (ideally <10 s). Adjust the settings as needed, edit the
#' pitch contour in the first file to your satisfaction, then click "Next" to
#' proceed to the next file, etc. Remember that setting a reasonable prior is
#' often faster than adjusting the contour one anchor at a time. When done,
#' click "Save results". If working with many files, you might want to save the
#' results occasionally in case the app crashes (although you should still be
#' able to recover your data if it does - see below).
#'
#' \bold{How to edit pitch contours}
#'
#' Left-click to add a new anchor, double-click to remove it or unvoice the
#' frame. Each time you make a change, the entire pitch contour is re-fit, so
#' making a change in one frame can affect the path through candidates in
#' adjacent frames. You can control this behavior by changing the settings in
#' Out/Path and Out/Smoothing. If correctly configured, the app corrects the
#' contour with only a few manual values - you shouldn't need to manually edit
#' every single frame. For longer files, you can zoom in/out and navigate within
#' the file. You can also select a region to voice/unvoice or shift it as a
#' whole or to set a prior based on selected frequency range.
#'
#' \bold{Audio playback}
#'
#' The "Play" button / spacebar plays the currently plotted region, but it uses
#' R for playback, which may or may not work - see \code{\link{playme}} for
#' troubleshooting. As a fallback option, the html audio tag at the top plays
#' the entire file.
#'
#' \bold{Recovering lost data}
#'
#' Every time you click "next" or "last" to move in between files in the queue,
#' the output you've got so far is saved in a backup file called "temp.csv". If
#' the app crashes or is closed without saving the results, this backup file
#' preserves your data. To recover it, access this file manually on disk or
#' simply restart pitch_app() - a dialog box will pop up and ask whether you
#' wank to append the old data to the new one. Path to backup file:
#' "[R_installation_folder]/soundgen/shiny/pitch_app/www/temp.csv", for example,
#' "/home/allgoodguys/R/x86_64-pc-linux-gnu-library/3.6/soundgen/shiny/pitch_app/www/temp.csv"
#' @export
#' @examples
#' \dontrun{
#' # Recommended workflow for analyzing a lot of short audio files
#' path_to_audio = '~/Downloads/temp'  # our audio lives here
#'
#' # STEP 1: extract manually corrected pitch contours
#' pitch_app()  # runs in a browser
#' df1 = read.csv('~/Downloads/output.csv')  # saved output from pitch_app()
#'
#' # STEP 2: run analyzeFolder() with manually corrected pitch contours to
#' obtain accurate descriptives like the proportion of energy in harmonics above
#' f0, etc. This also gives you formants and loudness estimates (disabled in
#' pitch_app to speed things up)
#' df2 = analyzeFolder(path_to_audio,
#'   pitchMethods = NULL,  # don't need to re-analyze pitch
#'   nFormants = 5,        # now we can measure formants as well
#'   pitchManual = df1     # df1 contains our manually corrected contours
#' )
#'
#' # STEP 3: add other acoustic descriptors, for ex.
#' df3 = segmentFolder(path_to_audio)
#' df4 = modulationSpectrumFolder(path_to_audio)
#'
#' # STEP 4: merge df2, df3, df4, ... in R or a spreadsheet editor to have all
#' acoustic descriptives together
#' }
pitch_app = function() {
    appDir = system.file("shiny", "pitch_app", package = "soundgen")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}



#' Interactive formant tracker
#'
#' Starts a shiny app for manually correcting formant measurements. Supported browsers: Firefox / Chrome. Note that the
#' browser has to be able to playback WAV audio files, otherwise there will be
#' no sound.
#' @export
formant_app = function() {
  appDir = system.file("shiny", "formant_app", package = "soundgen")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
