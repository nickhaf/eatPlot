# Plot a table and/or a barchart.

Plot a table and/or a barchart.

## Usage

``` r
plot_tablebarplot(
  dat,
  y_axis = NULL,
  bar_est = NULL,
  bar_label = NULL,
  bar_sig = NULL,
  bar_label_sig = NULL,
  bar_fill = NULL,
  columns_table = NULL,
  columns_table_sig_bold = NULL,
  columns_table_sig_superscript = NULL,
  columns_table_se = NULL,
  headers = NULL,
  column_spanners = NULL,
  column_spanners_2 = NULL,
  columns_round = 0,
  plot_settings = plotsettings_tablebarplot()
)
```

## Arguments

- dat:

  Data prepared with
  [`prep_tablebarplot()`](https://nickhaf.github.io/eatPlot/reference/prep_tablebarplot.md).

- y_axis:

  Character string of the columnname used as y-axis. Has to contain
  unique values.

- bar_est:

  Character string for the column that contains the values for the bar
  chart. If `NULL`, no bar chart will be plotted.

- bar_label:

  Character string for the column that should be used for bar labels on
  top of the bars. If `NULL`, no labels are printed. Defaults to `NULL`.

- bar_sig:

  Character string for the column that should be used for marking the
  bars as significant.

- bar_label_sig:

  Character string for the column that should be used for marking the
  bar labels as significant.

- bar_fill:

  Character string for the column that groups the bar filling colours
  into different groups.

- columns_table:

  List of character strings of the columns that should be plotted as
  table columns in the plot.

- columns_table_sig_bold:

  List of character strings of the columns that contain the
  significances for plotting significant values as bold.

- columns_table_sig_superscript:

  List of character strings of the columns that contain the
  significances for plotting significant values with a raised a.

- columns_table_se:

  List of character strings of the columns that contain standard errors,
  which will be plotted in brackets and rounded to `1`.

- headers:

  Character vector containing the headers of the ploted table columns,
  including the bar table.

- column_spanners:

  Named list. The name of each element will be the column header. The
  list element itself has to be a numeric vector indicating which
  columns the column spanner should span.

- column_spanners_2:

  Named list. A second dimension of column spanners. The name of each
  element will be the column header. The list element itself has to be a
  numeric vector indicating which columns the column spanner should
  span.

- columns_round:

  Numeric vector for rounding the column values. Insert `NULL` or `0`
  for no rounding/character columns.

- plot_settings:

  Named list constructed with
  [`plotsettings_tablebarplot()`](https://nickhaf.github.io/eatPlot/reference/plotsettings_tablebarplot.md).
  Defaults to a list with all settings set to `0`. There are several
  predefined lists with optimized settings for different plots. See
  [`plotsettings_tablebarplot()`](https://nickhaf.github.io/eatPlot/reference/plotsettings_tablebarplot.md)
  for an overview.

## Value

`ggplot2` object.

## Examples

``` r
# tbd
```
