#' @export
runIMC <- function() {

  appDir <- system.file("shiny-bits", "ShinyRUNIMC", package = "RUNIMC")
  if (appDir == "") {
    stop("Could not find app. Try re-installing `RUNIMC`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
