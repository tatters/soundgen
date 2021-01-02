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
#' Starts a shiny app for manually editing pitch contours. IMPORTANT: please use
#' Firefox! A bug in Chrome interferes with correct audio playback; Safari may
#' or may not work. The settings in the panels on the left correspond to
#' arguments to \code{\link{analyze}} - see `?analyze` and the vignette on
#' acoustic analysis for help and examples. You can verify the pitch contours
#' first, and then feed them back into \code{analyze} (see examples).
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
#' Start by setting the basic analysis settings such as pitchFloor,
#' pitchCeiling, silence, etc. Then click "Load audio" to upload one or several
#' audio files (wav/mp3). Long files will be very slow, so please cut your audio
#' into manageable chunks (ideally <10 s). If Shiny complains that maximum
#' upload size is exceeded, you can increase it, say to 30 MB, with
#' `options(shiny.maxRequestSize = 30 * 1024^2)`. Once the audio has been
#' uploaded to the browser, fine-tune the analysis settings as needed, edit the
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
#'
#' @seealso \code{\link{formant_app}}
#'
#' @export
#' @examples
#' \dontrun{
#' # Recommended workflow for analyzing a lot of short audio files
#' path_to_audio = '~/Downloads/temp'  # our audio lives here
#'
#' # STEP 0: set up Firefox as default browser either system-wide or just in R.
#' # For ex., on Linux, run:
#' options('browser' = '/usr/bin/firefox')  # path to the executable
#'
#' # STEP 1: extract manually corrected pitch contours
#' pitch_app()  # runs in Firefox
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
#' Starts a shiny app for manually correcting formant measurements. IMPORTANT:
#' please use Firefox! A bug in Chrome interferes with correct audio playback;
#' Safari may or may not work. For more tips, see \code{\link{pitch_app}} and
#' http://cogsci.se/soundgen.html.
#'
#' Suggested workflow: load one or several audio files (wav/mp3), preferably not
#' longer than a minute or so. Select a region of interest in the spectrogram -
#' for example, a sustained vowel with clear and relatively steady formants.
#' Double-click within the selection to create a new annotation (you may add a
#' text label if needed). If you are satisfied with the automatically calculated
#' formant frequencies, proceed to the next region of interest. If not, there
#' are 4 ways to adjust them: (1) type in the correct number in one of the
#' formant boxes in the top right corner; (2) click a spectrogram within
#' selection (pick the formant number to adjust by clicking the formant boxes);
#' (3) single-click the spectrum to use the cursor's position, or (4)
#' double-click the spectrum to use the nearest spectral peak. When done with a
#' file, move on to the next one in the queue. Use the orange button to download
#' the results. To continue work, upload the output file from the previous
#' session together with the audio files (you can rename it, but keep the .csv
#' extension). Use hotkeys (eg spacebar to play/stop) and avoid working with
#' very large files.
#'
#' @seealso \code{\link{pitch_app}}
#'
#' @export
#' @examples
#' \dontrun{
#' # Set up Firefox as default browser either system-wide or just in R.
#' # For ex., on Linux, run:
#' options('browser' = '/usr/bin/firefox')  # path to the executable
#' formant_app()  # runs in Firefox
#' }
formant_app = function() {
  appDir = system.file("shiny", "formant_app", package = "soundgen")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
