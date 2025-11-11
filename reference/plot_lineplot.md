# Plot a lineplot.

Plot a lineplot.

## Usage

``` r
plot_lineplot(
  eatPlot_dat,
  facet_var = "state_var",
  point_est = "est_mean_comp_none",
  point_sig = "sig_mean_comp_crossDiff_totalFacet_sameSubgroup",
  line_sig = "sig_mean_comp_trend_sameFacet_sameSubgroup",
  line_se = "se_mean_comp_none",
  brace_label_est = "est_mean_comp_trend_sameFacet_sameSubgroup",
  brace_label_se = "se_mean_comp_trend_sameFacet_sameSubgroup",
  brace_label_sig_superscript = "sig_mean_comp_trend_crossDiff_totalFacet_sameSubgroup",
  brace_label_sig_bold = "sig_mean_comp_trend_sameFacet_sameSubgroup",
  years_lines = list(),
  years_braces = list(),
  background_facet = NULL,
  background_subgroup = NULL,
  box_facet = NULL,
  title_superscripts = NULL,
  plot_settings = plotsettings_lineplot()
)
```

## Arguments

- eatPlot_dat:

  Input is a list prepared by
  [`prep_lineplot()`](https://nickhaf.github.io/eatPlot/reference/prep_lineplot.md).

- facet_var:

  Character string of the column name in `eatPlot_dat` containing the
  variable that should be split over multiple facets. Defaults to
  `NULL`.

- point_est:

  Character string of the column name in `eatPlot_dat` containing the
  y-values for the plotted points. Defaults to `NULL`.

- point_sig:

  Character string of the column name containing significance values for
  `point_values`. Defaults to `NULL`.

- line_sig:

  Character string of the column name containing significance values for
  `line_values`. Defaults to `"sig_Trend_noComp"`, which will show the
  significance of the difference between two time points.

- line_se:

  Character vector of the column name containing the standard errors for
  the plotted lines. Defaults to `NULL`, in which case they will be
  deducted from the line values.

- brace_label_est:

  Character string of the column name containing the brace labels.

- brace_label_se:

  Character string of the column name containing the standard errors for
  `label_est`. Will be put in bracktes behind `label_est`.

- brace_label_sig_superscript:

  Character string of the column name containing significance values for
  `label_est`. Significant values will be marked by a raised 'a'.
  Normally, should be the comparison of the trend vs. the trend in whole
  Germany, which can be found in the trendDiff_cross parameter. Defaults
  to `NULL`, as this parameter is not always provided.

- brace_label_sig_bold:

  Character string of the column name containing significance values for
  `label_est`. Significant values will be marked as bold. Defaults to
  `"sig_Trend_noComp"`.

- years_lines:

  List of numeric vectors containing the start and end year, between
  which a trend line should be plotted. Per default, lines are drawn
  from every year to the next consecutive year.

- years_braces:

  List of numeric vectors containing the start and end year, between
  which a brace should be plotted. Per default, braces are drawn from
  the last year to every other year included in the data.

- background_facet:

  Character string in the `facet_var` column that is assigned to the
  total group. It will not plotted as extra facet, but as background
  line. Defaults to `"Deutschland"`.

- background_subgroup:

  Character string in the `subgroup_var` column that is assigned to the
  total group. It will not plotted as extra facet, but in the background
  line. Defaults to `NULL`, which should be kept if you only want to
  plot one group (without a total group).

- box_facet:

  Character vector, containing strings from the
  `seperate_plot_var`-column, that should get a box drawn around them.

- title_superscripts:

  Named list for superscripts at the plot_titles. The name of the list
  element has to be equal to the title, the value of the list element
  has to be the superscript. Defaults to `NULL`.

- plot_settings:

  Named list constructed with
  [`plotsettings_lineplot()`](https://nickhaf.github.io/eatPlot/reference/plotsettings_lineplot.md).
  Defaults to a list with all settings set to `0`. There are several
  predefined lists with optimized settings for different plots. See
  [`plotsettings_lineplot()`](https://nickhaf.github.io/eatPlot/reference/plotsettings_lineplot.md)
  for an overview.

## Value

`ggplot2` object.

## Examples

``` r
# tbd
```
