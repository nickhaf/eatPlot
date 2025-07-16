# eatPlot (development version)

# eatPlot 1.1.2
* [process_bundesland()] now puts simple - signs between two-word Bundesländer instead of ndashes. 

# eatPlot 1.1.1
* Bugfix in `prep_tableplot()`: groups that didn't have any comparison were removed in the prepping process, this is changed now. 
* Implemented stacked barplots. 
* `columns_nudge_y` in [plotsettings_tablebarplot()] now works rowwise as well. 
* Improved rendering of greek letters with linebreak, using lists instead of ´atop()´.
* Implemented more modular options for plotting lines for different years over different facets in `plot_lineplot()`. 
* Removed `standardize_pattern_spacing()`, as it did not work as expected. 
Try around with the pattern spacing instead, and take the template plots as orientation. 


# eatPlot 1.1.0
* Internally changed the data preperation workflow. 
* Prepared the plot default lists for BT24. 

# eatPlot 1.0.1
* Renamed the `grouping_var` argument in `plotsettings_lineplot()` to `subgroup_var`. 

# eatPlot 1.0.0 "Major Update"
* Breaking Changes: The package was completely restructured to encompass changes that were made to the output format of `eatRep`. 
* Mainly, the data preparation was changed internally. 
On user end, changes will be most visible in changed output format of the prepared data, and changed column names of the prepared data. 
* Some function arguments were changed for more consistency. 
* `prep_plot()` was split up into `prep_plot_lineplot()` and `prep_plot_tablebarplot()`, with different arguments and output formats.

## eatPlot 0.0.0.9029 "Better Braces"
* Updated the algorithm for calculating the brace position and indention, so the longer trend always gets the longer brace. The braces get indented automatically, depending on their position in the plot. 

## eatPlot 0.0.0.9028 "x axis label size"
* Implemented the argument `axis_x_label_size` in `plotsettings_tablebarplot()` for setting the fontsize of x axis labels in barplots. 

## eatPlot 0.0.0.9027 "bar nudge"
* Using the argument `bar_nudge_y` in `plotsettings_tablebarplot()` it is now possible to nudge the bars in y direction.

## eatPlot 0.0.0.9026 "complex group comparisons"
* Comparisons of the type group_1.vs.group_2.VS.wholeGroup-group_1.vs.group_2 always compare this group comparison in states vs wholeGroup. 
Therefore, the naming of the respective columns could be adapted, and the output complexity reduced. 

## eatPlot 0.0.0.9025 "white spaces"
* Implemented the possiblity to plot white spaces next to plots, so they can be sperated by a white space. Use `space_right` in `plotsettings_tablebarplot()` to set the width, and controll the background_stripes borders with `background_stripes_border`. 

## eatPlot 0.0.0.9024 "better headers"
* Implemented `column_spanners_2` argument in [plotsettings_tablebarplot()]. This provides a second dimension of column spanners.
* Implemented `headers_row_height`, `column_spanners_row_height` and `column_spanners_2_row_height` arguments in [plotsettings_tablebarplot()]. This makes it possible to set the width of the row the headers and column_spanners are written in.
* Implemented `headers_nudge_y`, `column_spanners_nudge_y` and `columns_spanners_2_nudge_y` arguments in [plotsettings_tablebarplot()] to nudge the individual headers and column_spanners along the y-axis. 
* Removed the `headers_background_widht_y` argument in [plotsettings_tablebarplot()]. 
* Implemented `headers_font_size` argument in [plotsettings_tablebarplot()]. 

## eatPlot 0.0.0.9023
* Fixed `NA`-handling in [filter_rows()]. 

## eatPlot 0.0.0.9022
* Implemented vs.Vs.vs. comparisons, so crossDiff_of_groupDiffs can be plotted now. 

## eatPlot 0.0.0.9021
* Moved background_lines argument from [plot_lineplot()] to [plotsettings_lineplot()].

## eatPlot 0.0.0.9020
* Removed most of the white space around the final plots. For lineplots, some is still needed for drawing a box around. 
* Adjusted distance between single lineplot-tiles.
* It is now possible to set the y-axis of lineplots manually by using the argument `axis_y_lims` and `axis_y_tick_distance` in [plotsettings_lineplot()]. 
* Renamed `y_axis` in [plotsettings_lineplot()] to `axis_y`.
* Tweaked the scales of lineplots, so the white spaces between highest point and year-banner are reduced. 

## eatPlot 0.0.0.9019
* New function [gsub_plot_dat()] for replacing character strings in the prepared data. Mainly intended for replacing "wholeGroup" with "Deutschland", but other use cases are possible as well.
* It is now possible to draw a box around tiles in [plot_lineplot()] by defining the wanted tiles in the `seperate_plot_var_box`-Argument. 
* Improved the y-axis plotting in [plot_lineplot()].
* Changed the underlying architechture of the lineplot-scales. 

## eatPlot 0.0.0.9018
* Trend lines with differing year ranges can now be plotted with equal length (in split and non-split plots). Use the `equal_trend_line_length`-argument in [plotsettings_lineplot()] with care, as the distances between the timepoints might be distorted by this approach (however, it is necessary for the BT22).

## eatPlot 0.0.0.9017
* Changed defaults for y position of column headers. 

## eatPlot 0.0.0.9016
* Column header lines are now more consistent in their width.

## eatPlot 0.0.0.9015
* Limits of the y-axis in lineplots can no be set manually using the argument `axis_y_lims` in [plotsettings_lineplot()].
* New [cmyk()] function for setting colour values using cmyk colour coding.
* Set default cmyk-colours for chapter 3.3 (Frz).
* New defaul-settings list for mean-barplots without trend. 

## eatPlot 0.0.0.9014
* Added the function [standardize_column_widths] which can be used to calculate the relative column width for plots that should be combined later. This way, columns that should have the same width can be easily set to the same width. 
* The calcualtion of the `columns_table_sig_high_letter_nudge_x` argument in [plotsettings_tablebarplot()] was adjusted slightly so it behaves more consistent. 
* Added an argument for specifying the background line colour in lineplots. 
* Changed the default colours in the default plotting lists, so they go ahead with the cmyk colours needed by the publisher.
* Barplots with patterns now have a small black frame.

## eatPlot 0.0.0.9013
* Changed color encoding to CMYK by default in `save_plot()`.
* Because of this, dashes have to be represented by UTF8 Encoding. This will be done automatically in the plot-functions. As a result, the dashes will not be represented correctly if you look at the plot directly in R, but will be correct in the PDF.

## eatPlot 0.0.0.9012
* In `plot_lineplot()` kann die `seperate_plot_var` jetzt als factor übergeben werden, damit die Reihenfolge der Linienplots der Reihenfolge der `factorlevels` entspricht.

## eatPlot 0.0.0.9011
* The bar header argument was merged with the columns_headers argument in `plot_tablebarplot`, which is now named `headers`. The bar header now behaves like the other column headers, and can be aligned by the `headers_alignment` - Argument in `plotsettings_tablebarplot()`. 

## eatPlot 0.0.0.9010
* The argument `columns_table_se` can now be used for specifying which columns should be plotted as (SE) (with brackets).
* There now is an example for Chapter 4 plots without trend.
* It is now possible to define different types of background lines in `plotsettings_tablebarplot()`, using the argument `bar_background_lines`.
* It is now possible to let background lines only span the rows specified in the `bar_background_lines_spanners` argument of the `plotsettings_tablebarplot()` function. 
* Added border line on top of bar plots.
* Added templates for chapter 3 - french and chapter 4 - noTrend analyses.
* Use `ggsave()` in `save_plot()`, so the correct dashes are displayed in the pdf. 

## eatPlot 0.0.0.9009
* `grouping_vars` - argument in `prep_plot()` now takes up to two grouping_vars. 
* Renamed arguments in `prep_plot()`: `grouping_var` and `grouping_var_groups` to `grouping_vars` and `grouping_vars_groups`.
* Comparison columns are now named differently. 
* New comparisons are prepared: : c("crossDiff", "groupDiff", "crossDiffofgroupDiff", "trendDiffgroup", "trendDiffcross"). Nested comparisons of the type 'group_a.vs.group_b.VS.group_b.vs.group_c' are currently not included. 
* Removed default setting for `label_sig_high` argument in `plot_lineplot()`. 
* Added the argument `title_superscript` in `plot_lineplot()` for specifying superscripts for the plot titles. 
* Added the argument `comparisons` in `plot_lineplot()` for specifying wanted comparisons. 
* Standardized the column names in `plot_data$plot_tablebarplot`, so `_noTrend` always is written before `Comp`.

## eatPlot 0.0.0.9008
* Added argument `plotsettings_tablebarplot(bar_background_lines = TRUE)` for striped border lines in barplots.
* Added alignent option `2` for tablebars. This will right align columns in the middle of the column, usefull for numeric columns. 
* Columns with linebreaks are adjusted automatically now, so they don't flow over the upper line borders any more. 


## eatPlot 0.0.0.9007
* Tables/Barplots now take the sorting of the data.frame input. 

## eatPlot 0.0.0.9006
* Columns can now be aligned by the decimal point (only possible for alignment = 1).

## eatPlot 0.0.0.9005
* The default setting for patterned bars is now reversed, so that the pattern is shown in non-significant bars. 
* It is now possible to specify which letter should be used for the superscript significance.

## eatPlot 0.0.0.9004
* `plot_lineplot()` no tries to plot the background lines of the whole group trend as confidence intervall, derived from the values for the whole Group and theire standard errors. 
* `prep_plot()` now distinguishes between columns for crossDiff and groupDiff. 
* `prep_plot()` makes less assumptions about the groups you want to plot. This makes it more flexible. With the `grouping_var_groups` argument, you can now filter the groups you want to plot during the data preperation. 
* Multiple bugfixes in `prep_plot()`.


## eatPlot 0.0.0.9003
* `prep_plot()$plot_tablebarplot` now returns a data.frame in wide format. 

## eatPlot 0.0.0.9002
* Added checks in `prep_plot()` to reduce error messages.

## eatPlot 0.0.0.9001
* Fixed Bug in `prep_plot()` that threw an error, if the comparison-column was empty. 
