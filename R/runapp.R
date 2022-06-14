#' @export
runExample <- function() {
  appDir <- system.file("myapp", package = "extractoR")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
