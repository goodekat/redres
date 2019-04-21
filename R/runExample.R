#' @export
runExample <- function() {
  appDir <- system.file("shiny", package = "redres")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `redres`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
