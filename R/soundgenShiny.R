# Note: wrap shiny::runApp in suppress.warnings() before publishing

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
