# Note: might wrap shiny::runApp in suppress.warnings()

#' Soundgen shiny app
#'
#' Starts a shiny app, which provides an interactive wrapper to
#' \code{\link{soundgen}}
#' @export
soundgen_app = function() {
  appDir = system.file("shiny", "soundgen_main", package = "soundgen")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}


#' Soundgen pitch app
#'
#' Starts a shiny app for manually editing pitch contours extracted by
#' \code{\link{analyze}}
#' @export
pitch_app = function() {
    appDir = system.file("shiny", "pitch_app", package = "soundgen")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `soundgen`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
