# Note: might wrap shiny::runApp in suppress.warnings()

#' Interactive sound synthesizer
#'
#' Starts a shiny app, which provides an interactive wrapper to
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


#' Interactive pitch editor
#'
#' Starts a shiny app for manually editing pitch contours extracted by
#' \code{\link{analyze}}. Supported browsers: Firefox / Chrome. Note that the
#' browser has to be able to play back WAV audio files, otherwise there will be
#' no sound. The settings in the panels on the left correspond to arguments to
#' \code{\link{analyze}} - see `?analyze` and the vignette on acoustic analysis
#' for help and examples.
#'
#' @return The app produces a .csv file with four columns: file name,
#' duration (ms), time stamps (the midpoint of each STFT frame, ms), and
#' manually corrected pitch values for each frame (Hz). To process pitch
#' contours further in R, do something like:\cr
#' \code{
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
#' proceed to the next file, etc. When done, click "Save results". If working
#' with many files, you might want to save the results regularly in case the app
#' crashes (once it has crashed, you cannot recover anything).
#'
#' \bold{How to edit pitch contours}
#'
#' Left-click to add a new anchor, double-click to remove it or unvoice the
#' frame. Each time you make a change, the entire pitch contour is re-fit, so
#' making a change in one frame can affect the path through candidates in
#' adjacent frames. You can control this behavior by changing the settings in
#' Out/Path and Out/Smoothing. If correctly configured, the app corrects the
#' contour with only a few manual values - you shouldn't need to manually edit
#' every single frame. For longer files, use the zoom buttons "<+->" to zoom
#' in/out and navigate within the file. You can also select a region and
#' voice/unvoice or shift it as a whole (see the buttons under the spectrogram).
#'
#' \bold{Audio playback}
#'
#' The "Play" button plays the currently plotted region, but it uses R for
#' playback, which may or may not work - see \code{\link{playme}} for
#' troubleshooting. As a fallback option, the html audio tag at the top plays
#' the entire file.
#' @export
pitch_app = function() {
    appDir = system.file("shiny", "pitch_app", package = "soundgen")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
