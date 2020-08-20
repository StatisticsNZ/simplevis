# simplevis 2.0.0

* Update shiny templates.
* Drop region, TA and sea-draining catchments from nz basemap stack.
* Removed code to automate isMobile, as it was not working.
* Removed na_tip as was not working.

# simplevis 1.8.0

* Renamed leaflet_basemap_stack functions to remove the word stack.
* Removed automatic addition of tooltip text into functions.
* Added support for the user to add a tooltip variable into plot functions.
* Created add_tip function to easily create a tooltip text column within a dataset.
* In scatter plots, changed quantile_cuts and bin_cuts arguments to col_cuts.
* Renamed plotly functions.
* Added a size argument to lines in line plots.
* Added a group var to boxplot functions.
* Removed requirement for categorical x var for boxplot functions.
* Added a width argument to boxplot functions.
* Renamed na_grey_hover_value. 
* Fixed bug in leaflet functions with colouring by bin col_method.
* Fixed bug in ggplot_vbar to support making a plot when all values are zero.

# simplevis 1.7.0

* Renamed all plot x_scale_ prefixed arguments with x_ and likewise for y_scale_ and col_scale_.
* Renamed remove_na argument in scatter and sf plots as col_na_remove.
* Renamed rev_pal as pal_rev in scatter, sf and stars functions.
* Renamed bin_cuts and quantile_cuts as col_cuts.
* Renamed quantile_by_facet as col_quantile_by_facet.
* Added col_na_remove argument to leaflet_sf_col.
* Fixed bug with legend key in line plots not displaying the line and point in the key.
* Removed dplyr 1.0.0 dependency by replacing across function in stars plots with _at functions.
* Moved all aesthetics from ggplot() function to geoms.

# simplevis 1.6.0

* Fixed bug with line hover_var.
* Update template apps, and vignette.
* Made isMobile default to NULL, which selects input$isMobile in apps and FALSE otherwise.
* Removed shiny logical argument from leaflet functions.
* Added argument to modify pretty n algorathim for numeric breaks.
* Fixed bug where hbar and hbar_col were not handling all zero values appropriately.
* Fixed bug where line and line_col were not handling all zero values appropriately.
* Fixed bug where plot functions ability were not able to deal with NA values.
* Fixed bug where vbar and vbar_col were not handling all zero values appropriately.

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