# eatPlot (development version)

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
