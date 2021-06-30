# simplevis 4.3.0

# simplevis 4.2.9

* Made legend fill background white for all functions. 
* Updated vignette and arrticles to discuss density functions, and make vars more explicit.
* Added minor gridline arguments.

# simplevis 4.2.8

* Added gg_density_col_facet() function.
* Made all gg_density* functions only support position = "identity".

# simplevis 4.2.7

* Added gg_density_facet() function.

# simplevis 4.2.6

* Added arguments to modify density stat within gg_density().
* Added gg_density_col() function.

# simplevis 4.2.5

* Created first density function.

# simplevis 4.2.4

* Updated bar help to note that only stack and dodge positions are supported.

# simplevis 4.2.3

* For point, added position and alpha arguments.

# simplevis 4.2.2 

* Updated bar position code to provide more flexibility.

# simplevis 4.2.1

* Consolidated and renamed themes.

# simplevis 4.2.0

* Bumped version for CRAN release.

# simplevis 4.1.7

* For point*, fixed bug to support use of the same col_var as x or y.

# simplevis 4.1.6

* Changed gg_sf* family alpha default to 0.9.

# simplevis 4.1.5

* Fixed point* family bug to support log10 x axis with x_zero equals TRUE.

# simplevis 4.1.4

* Updated pal_na to add a col_n argument for consistency with other pal functions.
* Add article on titles.

# simplevis 4.1.3

* Added pal_na function.
* Updated colour article.

# simplevis 4.1.2

* Changed label default conversion to use stringr::str_to_sentence.

# simplevis 4.1.1

* Fixed gg_point_col bug to make the zero lines work.
* Updated articles and vignettes.

# simplevis 4.1.0

* Add categorical colour palette.
* Adjusted NA colour.
* Increase title and subtitle wrap defaults to 100.
* Updated hbar col reversing and legend order.

# simplevis 4.0.0

* Bump version for CRAN major release.

# simplevis 3.9.8

* Updated app templates.

# simplevis 3.9.7

* Updated defaults for facetted pretty breaks.

# simplevis 3.9.6

* Added code to ensure logical variables work.

# simplevis 3.9.5

* Updated app templates.

# simplevis 3.9.4

* Changed vbar family to hbar.

# simplevis 3.9.3

* Removed hack example.

# simplevis 3.9.2

* Removed col_rev from gg_boxplot family.

# simplevis 3.9.1

* Added hack example.

# simplevis 3.9.0

* Removed facet_scales arg from gg_sf family.

# simplevis 3.8.9

* Added facet_scales arg into gg_sf family.

# simplevis 3.8.8

* Subtle changes to the aim of the project.

# simplevis 3.8.7

* Updated vignette.

# simplevis 3.8.6

* Updated help for facet_labels.

# simplevis 3.8.5

* Modified group by statement for col stack bars, so that zeros are not dropped.
* Updated bar x_var and y_var help for the specific.

# simplevis 3.8.4

* Updated help.

# simplevis 3.8.3

* Updated examples.

# simplevis 3.8.2

* Fixed col_labels default bug.

# simplevis 3.8.1

* Fixed col_labels default bug.

# simplevis 3.8.0

* Updated col_labels default. 

# simplevis 3.7.9

* Updated help. 

# simplevis 3.7.8

* Added facet_labels argument. 

# simplevis 3.7.7

* Updated leaflet_sf_col col_label_dp defaults.
* Replaced internal function with rlang::set_names(~snakecase::to_sentence_case(.))

# simplevis 3.7.6

* Added new col_labels code for gg_sf family.

# simplevis 3.7.5

* Added new col_labels code for gg_point family.

# simplevis 3.7.4

* Added default sentence case x and y labels for categorical variables.

# simplevis 3.7.3

* Fixed bugs with default titles.

# simplevis 3.7.2

* Changed more Helvetica font defaults to "".

# simplevis 3.7.1

* Removed discrete horizontal scale mobile wrapping.

# simplevis 3.7.0

* Updated code to use snakecase::to_sentence_case where possible.

# simplevis 3.6.9

* Removed hbar reversing of breaks, as not required.

# simplevis 3.6.8

* Updated boxplot x_var scale.
* Removed flipping of scales for boxplot on mobile.

# simplevis 3.6.7 

* Changed themes to default to "".

# simplevis 3.6.6

* Fixed bar scales.

# simplevis 3.6.5

* Reverted bar expand default to c(0, 0).

# simplevis 3.6.4

* Added datetime support to hbar.

# simplevis 3.6.3

* Added support for datetime for point, boxplot and vbar.
* Removed x_trans from line as unnecessary.

# simplevis 3.6.2

* Added support for datetime to the other gg_line functions.

# simplevis 3.6.1

* Added support for datetime to gg_line().

# simplevis 3.6.0

* Added in arguments to filter out NA values.

# simplevis 3.5.8

* For bar, removed the ability to reverse x and y scales for numeric variables. 

# simplevis 3.5.7

* Fixed bar reorder direction.

# simplevis 3.5.6

* Updated boxplot code for how it works with stat = "identity".

# simplevis 3.5.5

* Corrected the hbar and vbar x_var and y_var help.

# simplevis 3.5.4

* Changed position default to "dodge" from "stack".

# simplevis 3.5.3

* Moved palmerpenguins to suggests.

# simplevis 3.5.2

* Removed col_na argument.
* Updated all statement to remove NAs.

# simplevis 3.5.1

* Tweaked hbar title wrapping defaults.

# simplevis 3.5.0

* Breaking change: renamed all ggplot_ prefixes with gg_.

# simplevis 3.4.7

* Update x_rev and col_rev to support logical variables.

# simplevis 3.4.6

* Updated titles logic.

# simplevis 3.4.5

* Fixed bug with hbar x_title and y_title around the wrong way.

# simplevis 3.4.4

* Updated support for x logical variables.

# simplevis 3.4.2 

* Updated col_title wrapping for mobile.

# simplevis 3.4.1

* Updated default colours for 3 values.

# simplevis 3.4.0

* Fixed hbar mobile scales.

# simplevis 3.3.8

* Rewrote hbar_col_facet based on vbar code to expand variable types and arguments available.

# simplevis 3.3.7

* Rewrote hbar_facet based on vbar code to expand variable types and arguments available.

# simplevis 3.3.6

* Rewrote hbar_col based on vbar code to expand variable types and arguments available.

# simplevis 3.3.5

* For hbar, fixed ordering of date or numeric on y scale.
* For vbar and hbar, added x_reorder and y_reorder argument.

# simplevis 3.3.1 

* Rewrote hbar based on vbar code to expand variable types and arguments available.

# simplevis 3.3.0

* Fixed vbar x_limits defaults. 

# simplevis 3.2.4

* Fixed x_rev for if character in vbar, point and line.
* Corrected vbar_col_facet labels bug.
* Stop people from trying to stacjk

# simplevis 3.2.3

* Fixed x_rev in point and line.

# simplevis 3.2.2

* Updated breaks functions for better speed.

# simplevis 3.2.1

* For ggplot functions, updated col_labels.

# simplevis 3.2.0

* For ggplot functions, updated help for x and y_labels arguments.

# simplevis 3.1.9

* For vbar, added support for all variables on the x scale.
* For vbar, added an x_rev argument. 

# simplevis 3.1.8

* Removed x_trans from vbar and boxplot.
* Removed col_quantile_by_facet argument from point.

# simplevis 3.1.7

* Update all boxplot, point, and line x scales to be consistent.
* Added x_rev argument to all boxplot, point, and line x scales.

# simplevis 3.1.6

* Update boxplot x scales to be more flexible.

# simplevis 3.1.5

* Update vbar x scales to be more flexible.

# simplevis 3.1.4

* Breaking change: removed group_var argument, and updated grouping code.

# simplevis 3.1.3

* New feature: added support for x categorical variables in point.
* New feature: added support for x categorical and date variables in boxplot.

# simplevis 3.1.2

* New feature: added support for x date variables in vbar.
* New feature: added support for x categorical variables in line.

# simplevis 3.1.1 

* Bug fix: Updated col_rev code to fix colouring of factors where rev = TRUE.

# simplevis 3.1.0

* Minor change: Removed rnaturalearth from suggests.
* Bug fix: Corrected is_null bug in leaflet_sf.
* Bug fix: Corrected hbar y_var reversing bug.

# simplevis 3.0.2

* Breaking change: changed mutate_text vars_vctr argument to text_vars_vctr.

# simplevis 3.0.1

* Breaking change: Changed col_labels_nrow and col_labels_ncol to col_legend_ncol and col_legend_nrow.
* Breaking change: Added plotly_col_legend function, and removed plotly_legend_rev and plotly_legend_order.
* Breaking change: leaflet popup_var has been removed, and replaced with a popup_vars_vctr argument.

# simplevis 3.0.0

* Bumped version for CRAN release.

# simplevis 2.8.2

* Corrected vbar default reversing of col_var.
* Removed vbar x_rev argument, as unsure of logic rules.

# simplevis 2.8.1

* Corrected vbar default reversing of x_var.

# simplevis 2.8.0

* Update templates.

# simplevis 2.7.9

* Added ggplot_boxplot_col_facet function. 

# simplevis 2.7.8

* Make handling of all zero values pretty for all plots other than point.

# simplevis 2.7.7

* Breaking change: changed the isMobile argument to mobile for snakecase consistency across the package.
* Fixed where trans was equal to log or log10, and zero was selected
* Added ggplot_boxplot_col

# simplevis 2.7.6

* Updated scale_x_date to remove oob.

# simplevis 2.7.5

* Updated website vignette and articles.

# simplevis 2.7.4

* Breaking change: Removed x_na_inf and y_na_inf arguments.

# simplevis 2.7.3

* Export breaks functions.
* Corrected bug fix with x_na_inf and y_na_inf.

# simplevis 2.7.2

* Added col_na argument to all functions lacking it.
* Added x_rev and col_rev to vbar functions.

# simplevis 2.7.1

* Modularised all x_zero adjustments and automatic x_zero line components, and likewise the y_ ones.

# simplevis 2.7.0

* Modularised all x and y numeric breaks.
* New feature: added balance, trans and zero arguments to all numeric arguments.
* New feature: Changed x_zero and y_zero defaults for non-bar graph numeric scales to FALSE.

# simplevis 2.6.9

* Underlying code change of hbar to not use `coord_flip`.
* Rebuilt y numeric breaks.

# simplevis 2.6.8

* Rebuilt default font size code.

# simplevis 2.6.7

* New feature: Added to all themes legend.direction = "vertical" to make titles always be above legends.
* Documentation: added shiny for mobile article.

# simplevis 2.6.6

* New feature: Changed leaflet default alpha to 0.9.
* Documentation: added scales article.

# simplevis 2.6.5

* New feature: Changed boxplot for outlines to be always coloured black, and alpha defaulted to 1. 
* Breaking change: Changed point_size and line_size to size_point and size_line.

# simplevis 2.6.4

* New feature: Changed leaflet_sf default point_size to 2.

# simplevis 2.6.3

* New feature: added text_var arguments to ggplot sf functions to fully support plotly interactive maps. 

# simplevis 2.6.2

* Bug fix: Fix mutate_text, so that it does not add Not available to non-NA charcter values with NA in them. 
* New feature: Make ggplot sf functions not adjust alpha of outlines.
* New feature: Remove key_glyph, as it is not implemented in ggplotly.  

# simplevis 2.6.1

* Updated vignette and articles.

# simplevis 2.6.0

* Added template zip files.

# simplevis 2.5.9

* Breaking change: changed point_size to default to 1 for all functions.
* Breaking change: removed line_alpha from leaflet functions, as not required.
* Breaking change: removed col_drop from leaflet function.
* Breaking change: removed col_quantile_by_facet function from ggplot_sf_col_facet.

# simplevis 2.5.8

* Breaking change: In leaflet_sf functions, changed opacity to fill_alpha.
* Breaking change: In leaflet_sf functions, changed weight to line_size.
* Breaking change: In leaflet_sf functions, changed radius to point_size.
* Breaking change: In ggplot_line functions, removed points and lines argument. 
* Breaking change: In ggplot_sf functions, changed size argument to point_size and line_size for consistency.

# simplevis 2.5.7

* New feature: For boxplot, added line_size argument and alpha. 
* New feature: Default colour changed for where 2 values. 
* Breaking change: Changed x_na_bar/y_na_bar to x_na_inf/y_na_inf. 

# simplevis 2.5.6

* Breaking change: changed output of mutate_text to name the new column text.
* Breaking change: removed all stars functions, as these need more development work.

# simplevis 2.5.5 

* Breaking change: Rename tip_var to text_var to align with ggplot2.
* Breaking change: Rename add_tip to mutate_text to align with ggplot2 and dplyr.
* Breaking change: Rename boundary with borders to align with ggplot2.
* Breaking change: Rename ggplot_box with ggplot_boxplot to align with ggplot2.

# simplevis 2.5.4

* Bug fix: Make ggplot_vbar and vbar_facet scales pretty.
* New feature: Change default point_size to 1.5.

# simplevis 2.5.3

* New feature: Add line_size argument to hbar and vbar functions.

# simplevis 2.5.2

* New feature: Make ggplot_vbar function x scales pretty, where max or min equals the x limit.

# simplevis 2.5.1 

* New feature: Change default of ggplots to legend on right with 1 column.
* New feature: Add facet_ncol argument.
* New feature: Add col_labels_ncol argument.

# simplevis 2.5.0

* Breaking change: Rename col_label_digits argument to col_labels_dp.
* Breaking change: Rename leaflet_sf label_var to tip_var to avoid confusion.
* Breaking change: Rename size arguments in ggplot_point and ggplot_line functions.
* New feature: Fixed vbar legend elements being reversed. 

# simplevis 2.4.9

* Breaking change: In leaflet_sf, rename col_na_remove for consistency.
* Breaking change: In leaflet_sf functions, rename popup as popup_var and improve help.
* Breaking change: Rename col_digits argument to col_label_digits.

# simplevis 2.4.8

* Breaking change: In line functions, rename size argument to size_point for clarity.
* New feature: In line functions, add size_line argument.
* Breaking change: Rename all legend_ arguments to col_ arguments for internal consistency.
* Breaking change: Rename wrap_col_title to col_title_wrap for internal consistency.

# simplevis 2.4.7

* New feature: Defaulted colours to viridis for all functions.
* New feature: Improved ggplot_point and ggplot_sf colouring code.
* New feature: Added col_na arguments to ggplot_point and ggplot_sf functions to show na col_var values or not.  
* New feature: Added alpha argument for the fill of hbar and vbar.

# simplevis 2.4.6

* Breaking change: Removed size argument in line functions.
* Breaking change: Renamed point_size as size in line functions.

# simplevis 2.4.5

* New feature: Add alpha argument into hbar and vbar.
* Breaking change: Changed quantile_by_facet argument to col_quantile_by_facet for internal consistency.

# simplevis 2.4.4

* Breaking change: Remove leaflet_basemap_nz.

# simplevis 2.4.3

* New feature: Updated leaflet_basemap to include an argument for bounds.

# simplevis 2.4.2 

* Breaking change: Removed rnaturalearth wrapper functions, but referenced the package in help, examples and articles instead.

# simplevis 2.4.1

* New feature: Add the ability to set bounds in leaflet basemap for country boundaries from the rnaturalearth package.
* New feature: Add wrapper functions to easily extract sf boundaries and bounds from the rnaturalearth package.

# simplevis 2.4.0

* New feature: Reduce the size of sf example objects.
* New feature: Add vignette for making maps of sf objects.
* New feature: Removed support for boundary arguments in ggplot_stars, as it was not working.

# simplevis 2.3.9

* Updated help for boundary_behind arguments to specify correct default.
* For ggplot_sf and stars functions, added boundary_size argument in.
* Added nz_region as a helpful example boundary.

# simplevis 2.3.8

* Renamed example objects to focus on their object class.

# simplevis 2.3.7

* For line, renamed rev_pal to pal_rev for consistency. 
* For vbar, added in the pal_rev argument for consistency.

# simplevis 2.3.6

* For hbar & vbar, renamed arguments to x_na_bar and y_na_bar.

# simplevis 2.3.5

* Add na_bar argument to vbar_col.

# simplevis 2.3.4

* For hbar_col, fix na_bar so that is works on negative data.

# simplevis 2.2.3

* For hbar and vbar, fix na_bar so that it works for negative and positive data with ggplotly(plot, tooltip = "text").

# simplevis 2.3.2

* For line graphs on mobile, make x axis labels just the minimum and maximum (and zero if applicable). 

# simplevis 2.3.1

* Added y_balance arguments to vbar, line and point functions.

# simplevis 2.3.0

* Defaulted zero reference lines to be on by default if there are positive and negative values in the data. 

# simplevis 2.2.9

* Peplace all superceded scoped functions with across.

# simplevis 2.2.8

* Renamed ggplot_scatter to ggplot_point, and likewise for theme_scatter.  

# simplevis 2.2.7

* Renamed coastline argument to boundary.

# simplevis 2.2.6

* Moved pals to er.helpers package.

# simplevis 2.2.5 

* Changed wrap text arguments to prefix with what is being wrapped.

# simplevis 2.2.4

* Renamed x_na_bar and y_na_bar arguments to na_bar.

# simplevis 2.2.3

* Corrected hbar error messages for faceted plots.

# simplevis 2.2.2

* Adjusted mobile hbar x label justification.

# simplevis 2.2.1

* Ungrouped data in sf functions.

# simplevis 2.2.0

* Removed support for mobile in `_facet` and `_col_facet` functions.

# simplevis 2.1.9

* Updated functions for a more reliable mobile experience.

# simplevis 2.1.8

* Updated app templates css and default table rows.

# simplevis 2.1.7

* For hbar, add mobile wrapping for x labels.

# simplevis 2.1.6

* For hbar, update the mobile x breaks, so always only min and max, and zero if relevant.

# simplevis 2.1.5

* For hbar, made plots x aim for 1 interval of breaks for mobile, and left-align labels.

# simplevis 2.1.4

* For hbar, added x_balance argument to hbar functions.

# simplevis 2.1.3

* Changed name of compare2 pals to alpha2 pals.
* Made ggplot line functions default to expanding by zero on x.

# simplevis 2.1.2

* Changed hbar and vbar na_grey arguments to be x_na_bar or y__na_bar.
* Added y_na_na_bar argument to ggplot_hbar_col.

# simplevis 2.1.1

* Removed col_drop and col_remove_na arguments.

# simplevis 2.1.0

* Added two new palettes for graphs that compare a current year against a previous year.

# simplevis 2.09

* Added left-align to hover values with the plotly_camera function.

# simplevis 2.08

* Added expand arguments to all other plots.

# simplevis 2.07

* Changed zero_lines to default off.
* Removed automatic zero lines, so now they must be manually turned off and on.
* Added expand arguments to scatterplots.

# simplevis 2.06

* Corrected the direction of bar legend labels if someone manually adds them in.
* Update the leaflet sf popup so that it does not load if there is only geometry.

# simplevis 2.05

* Reverted default col_title = "" rather than NULL, as this works better with ggplotly.
* Adjusted add_tip to default to putting in all variables, and work with sf objects.

# simplevis 2.04

* Added support for using logical variables to colour in hbar.
* Added pal_rev argument in hbar.

# simplevis 2.03

* Removed feature id and row number from popup.
* Updated add_tip so that it can also work with sf objects.

# simplevis 2.0.2

* Removed geometry from defaulting into the leaflet popup.

# simplevis 2.0.1

* Fixed bug with scatter not working with NAs appropriately.

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