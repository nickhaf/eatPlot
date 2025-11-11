# Filter specified rows from plot_dat objects.

Filter specified rows from plot_dat objects.

## Usage

``` r
filter_plot_dat(
  plot_dat,
  filter_statement,
  list_elements = c("plot_lines", "plot_points", "plot_background_lines", "plot_braces")
)
```

## Arguments

- plot_dat:

  A list of data.frames prepared by
  [`prep_lineplot()`](https://nickhaf.github.io/eatPlot/reference/prep_lineplot.md).

- filter_statement:

  Character string containing a logical expression for filtering
  specific rows in all data.frames in the list when possible. Write
  `dat$column_name` to specify the wanted columns.

- list_elements:

  Character vector with the names of the list objects you want to filter
  in. Defaults to
  `c("plot_lines", "plot_points", "plot_background_lines", "plot_braces")`.

## Value

The plot_dat list with rows filtered as specified.

## Examples

``` r
# tbd
```
