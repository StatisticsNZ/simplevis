# simplevis 1.5.0

* Added default zero lines to plots for where there are negative values.
* Updated plot scales to deal with negative values better.
* Added x_scale_labels and y_scale_labels for arguments to all plots.
* Removed x_scale_date_format from plots.
* Removed wrap_y_labels argument from plots.
* Update line functions with lines = FALSE argument.
* Default the na_grey argument to FALSE.
* Improved the tooltip for NA values in vbar and hbar plots.
* Deleted remove_na argument from leaflet_sf.
* Updated vbar x_scale code to start and stop at the minimum and maximum x bar.
* Added na_grey argument to vbar and hbar non-coloured functions.
* Updated y scale in bar and line non-facetted plots for improved handling of all zero values.
* Updated templates to help users learn an easy workflow method.

# simplevis 1.4.0

* Updated vbar x scale to accomodate edge bars.
* Added argument to select a label variable in leaflet sf.

# simplevis 1.3.0

* Updated colour in pal_point_trend3 and pal_point_trend5.
* Added x_scale_labels and y_scale_labels argument to all numeric x and y scales in ggplot functions.
* Updated app templates.

# simplevis 1.2.0

* Replaced dplyr and tidyr superceded and retired functions with maturing functions.
* Added support for hover_var's being adding manually as ggplotly tooltips.
* Added support for categoical x variables on vbar ggplots.
* Dropped nz_region shape.
* Added plotly_reverse_legend and plotly_order_legend functions.
* Added width argument to hbar and vbar functions.
* Updated css plot minimum height.
* Added a req statement in the observe function of template2 to ensure the basemap is output before points are tried to be plotted.
* Adjusted ggplot wrapper functions to default to nice sizes for mobile or desktop.
* Removed run_example functions.
* Removed template3 from run_template functions.
* Adjusted vbar expand on the x scale to be zero.
* Updated all maps to default to quantile col_method with quartiles.

# simplevis 1.1.0 

* Renamed pal_trend3, pal_trend5, and pal_set1 to have pal_point prefixes.
* Updated scales for all ggplot wrapper functions to have better limits and breaks.
* Added plotly_remove_buttons function to remove buttons other than camera.
* Updated app templates to add theme(plot.title.position = "plot") to mobile plots.
* Updated css to app templates.
* Updated the default wrapping for titles.
* Updated vertical bar functions to support numeric or date variables on the x axis.
* Updated line graphs to move text to geom_point so that hover is on points.
* Updated bar code to fix free_y and free_x facet_scales, which were around the wrong way.
* Updated bar code to allow for graphs with scale_zero = FALSE & position equals "stack".
* Updated plot margins in graphs.
* Updated plot label wrapping in graphs.
* Updated caption position and wrapping length.

# simplevis 1.0.0

Initial release to CRAN.