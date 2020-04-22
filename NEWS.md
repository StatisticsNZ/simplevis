# simplevis 1.1.0 

* Updated scales for all ggplot wrapper functions to have better limits and breaks
* Added remove_plotly_buttons function
* Added theme(plot.title.position = "plot") to app templates
* Added updated css to app templates
* Renamed pal_trend3, pal_trend5, and pal_set1 to have pal_point prefixes
* Added extra categorical colour palettes
* Updated the default wrapping for titles 
* Updated vertical bar functions to support numeric or date variables on the x axis
* Updated line graphs to move text to geom_point so that hover is on points
* Updated bar code to fix free_y and free_x facet_scales, which were around the wrong way
* Updated bar code to allow for graphs with scale_zero = FALSE & position equals "stack"
* Update plot margins in graphs.
* Update plot label wrapping in graphs.
* Update caption position and wrapping length

# simplevis 1.0.0

Initial release to CRAN.