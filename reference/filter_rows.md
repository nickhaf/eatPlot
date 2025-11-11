# Extract or remove rows from prepared data.

Extract or remove rows from prepared data.

## Usage

``` r
filter_rows(
  plot_dat,
  column_name,
  subsetter,
  list_elements = c("plot_points", "plot_lines", "plot_braces"),
  remove = FALSE,
  remove_na = FALSE
)
```

## Arguments

- plot_dat:

  List of data.frames, output of
  [prep_lineplot](https://nickhaf.github.io/eatPlot/reference/prep_lineplot.md).

- column_name:

  Column that the subsetter will be searched in.

- subsetter:

  Character string of the state you want to extract or remove.

- list_elements:

  Character vector with the names of the list objects you want to filter
  in. Defaults to
  `c("plot_lines", "plot_points", "plot_background_lines", "plot_braces")`.

- remove:

  Logical. If `TRUE`, the state defined in `state` will be removed.
  Defaults to `FALSE`.

- remove_na:

  Logical. If `TRUE`, rows with `NAs` in `column_name` will be removed.

## Value

List of data.frames.

## Examples

``` r
# tbd
```
