# run_app_fun

# run_template

#' Run shiny template with option to download.
#'
#' @param template template name. Available templates are "template1" for a graph and table, and "template2" and "template3" also providing maps. Defaults to "template1".
#' @param ... passed to \code{shiny::runApp}
#'
#' @export
run_template <- function(template = "template1", ...) {
  suppressWarnings({
    
    templatedir <- system.file("shiny", package = "simplevis")
    
    templates <- dir(templatedir)
    
    if (!template %in% templates)
      stop(sprintf(
        "No such template.  Available templates, and corresponding examples, are:\n%s",
        paste(templates, collapse = "\n")
      ))
    
    shiny::runApp(sprintf("%s/%s", templatedir, template), ...)
  })
}
