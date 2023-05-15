# eatPlot (development version)

# eatPlot 0.0.0.9008
* Added argument `plotsettings_tablebarplot(bar_border_lines = TRUE)` for striped border lines in barplots.
* Added alignent option `2` for tablebars. This will right align columns in the middle of the column, usefull for numeric columns. 


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
