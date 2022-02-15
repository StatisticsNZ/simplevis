#' @title Shiny demo

#' @description Run a shiny demo app with option to download code.
#'
#' @param mobile TRUE or FALSE of whether the app and code should also work on mobile devices. Defaults to FALSE.
#'
#' @export
shiny_demo <- function(mobile = FALSE) {
  
  if(any(c("shiny", "shinycssloaders", "DT") %in% utils::installed.packages() == FALSE)) {
    stop("Please ensure shiny, shinycssloaders and DT packages are installed")  
  }
  
  suppressWarnings({
    
    demodir <- system.file("shiny", package = "simplevis")
    
    if (mobile == FALSE) demo <- 1
    if (mobile == TRUE) demo <- 2
    
    shiny::runApp(sprintf("%s/%s", demodir, demo))
  })
}
