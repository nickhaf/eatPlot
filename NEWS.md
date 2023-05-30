# eatPlot (development version)

# eatPlot 0.0.0.9012
* In `plot_lineplot()` kann die `seperate_plot_var` jetzt als factor übergeben werden, damit die Reihenfolge der Linienplots der Reihenfolge der `factorlevels` entspricht.

# eatPlot 0.0.0.9011
* The bar header argument was merged with the columns_headers argument in `plot_tablebar`, which is now named `headers`. The bar header now behaves like the other column headers, and can be aligned by the `headers_alignment` - Argument in `plotsettings_tablebarplot()`. 

# eatPlot 0.0.0.9010
* The argument `columns_table_se` can now be used for specifying which columns should be plotted as (SE) (with brackets).
* There now is an example for Chapter 4 plots without trend.
* It is now possible to define different types of background lines in `plotsettings_tablebarplot()`, using the argument `bar_background_lines`.
* It is now possible to let background lines only span the rows specified in the `bar_background_lines_spanners` argument of the `plotsettings_tablebarplot()` function. 
* Added border line on top of bar plots.
* Added templates for chapter 3 - french and chapter 4 - noTrend analyses.
* Use `ggsave()` in `save_plot()`, so the correct dashes are displayed in the pdf. 

# eatPlot 0.0.0.9009
* `grouping_vars` - argument in `prep_plot()` now takes up to two grouping_vars. 
* Renamed arguments in `prep_plot()`: `grouping_var` and `grouping_var_groups` to `grouping_vars` and `grouping_vars_groups`.
* Comparison columns are now named differently. 
* New comparisons are prepared: : c("crossDiff", "groupDiff", "crossDiffofgroupDiff", "trendDiffgroup", "trendDiffcross"). Nested comparisons of the type 'group_a.vs.group_b.VS.group_b.vs.group_c' are currently not included. 
* Removed default setting for `label_sig_high` argument in `plot_lineplot()`. 
* Added the argument `title_superscript` in `plot_lineplot()` for specifying superscripts for the plot titles. 
* Added the argument `comparisons` in `plot_lineplot()` for specifying wanted comparisons. 
* Standardized the column names in `plot_data$plot_tablebar`, so `_noTrend` always is written before `Comp`.

# eatPlot 0.0.0.9008
* Added argument `plotsettings_tablebarplot(bar_background_lines = TRUE)` for striped border lines in barplots.
* Added alignent option `2` for tablebars. This will right align columns in the middle of the column, usefull for numeric columns. 
* Columns with linebreaks are adjusted automatically now, so they don't flow over the upper line borders any more. 


# eatPlot 0.0.0.9007
* Tables/Barplots now take the sorting of the data.frame input. 

# eatPlot 0.0.0.9006
* Columns can now be aligned by the decimal point (only possible for alignment = 1).

# eatPlot 0.0.0.9005
* The default setting for patterned bars is now reversed, so that the pattern is shown in non-significant bars. 
* It is now possible to specify which letter should be used for the superscript significance.

# eatPlot 0.0.0.9004
* `plot_lineplot()` no tries to plot the background lines of the whole group trend as confidence intervall, derived from the values for the whole Group and theire standard errors. 
* `prep_plot()` now distinguishes between columns for crossDiff and groupDiff. 
* `prep_plot()` makes less assumptions about the groups you want to plot. This makes it more flexible. With the `grouping_var_groups` argument, you can now filter the groups you want to plot during the data preperation. 
* Multiple bugfixes in `prep_plot()`.


# eatPlot 0.0.0.9003
* `prep_plot()$plot_tablebar` now returns a data.frame in wide format. 

# eatPlot 0.0.0.9002
* Added checks in `prep_plot()` to reduce error messages.

# eatPlot 0.0.0.9001
* Fixed Bug in `prep_plot()` that threw an error, if the comparison-column was empty. 
