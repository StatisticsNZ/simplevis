#' @title shiny_template

#' @description Run a shiny template with option to download.
#'
#' @param template template name. Available templates are 1 with graph and table tabs, and 2 also with a map tab. Defaults to "template1".
#' @param ... passed to \code{shiny::runApp}
#'
#' @export
shiny_template <- function(template = 1, ...) {
  suppressWarnings({
    
    templatedir <- system.file("shiny", package = "simplevis")
    
    templates <- dir(templatedir)
    
    if (!template %in% templates)
      stop(sprintf(
        "No such template.  Available templates are 1 and 2"
      ))
    
    shiny::runApp(sprintf("%s/%s", templatedir, template), ...)
  })
}
