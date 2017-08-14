#' Opens the SOM Shiny app.
#'
#' Starts the interaktive SOM Shiny app.
#' 
#' @export
TBSOMsgui <- function() {
  appDir <- system.file("TBSOMsgui", package = "TBSOM")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `TBSOM`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}