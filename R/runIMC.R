#' @export
runIMC <- function() {

  appDir <- system.file("shiny-bits", "ShinyRUNIMC", package = "RUNIMC")
  if (appDir == "") {
    stop("Could not find app. Try re-installing `RUNIMC`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}


#' @export
runIMC_V2 <- function(x=NULL,help=T) {


  appDir <- system.file("shiny-bits", "ShinyRUNIMC_V2", package = "RUNIMC")
  if (appDir == "") {
    stop("Could not find app. Try re-installing `RUNIMC`.", call. = FALSE)
  }

  if (is.null(x)) stop(RUNIMC:::mError("specify a study"),call. = F)

 shiny::shinyOptions(studyName=x$name,
                     rasterPath=file.path(x$rootFolder,x$name,'rasterStacks'),
                     analysisName=x$currentAnalysis$name,
                     trainingPolygonPath=file.path(x$currentAnalysis$folder,'training','polygons'),
                     help = help)

 shiny::runApp(appDir, display.mode = "normal")

}


