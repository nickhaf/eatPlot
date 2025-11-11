# [`gsub()`](https://rdrr.io/r/base/grep.html) for lists and data.frames.

[`gsub()`](https://rdrr.io/r/base/grep.html) for lists and data.frames.

## Usage

``` r
gsub_plot_dat(
  plot_dat,
  search_for = "wholeGroup",
  replace_with = "Deutschland"
)
```

## Arguments

- plot_dat:

  List or data.frame, mainly usage intention for plot_dat - objects from
  [`prep_lineplot()`](https://nickhaf.github.io/eatPlot/reference/prep_lineplot.md).

- search_for:

  Character string that should be replaced.

- replace_with:

  Character string that will be the replacement.

## Value

Depending on input data.frame or list with the subbed character strings.

## Examples

``` r
plot_dat <- list(
  dat_1 = data.frame(
    col_1 = c("wholeGroup", "notwholeGroup"),
    col_2 = c("wholeGroup", NA),
    col_3 = c(1, 2)
  ),
  dat_2 = data.frame(
    col_1 = c("wholeGroup", "notwholeGroup"),
    col_2 = c("wholeGroup", NA),
    col_3 = c(TRUE, FALSE)
  )
)
gsub_plot_dat(plot_dat)
#> $dat_1
#>            col_1       col_2 col_3
#> 1    Deutschland Deutschland     1
#> 2 notDeutschland        <NA>     2
#> 
#> $dat_2
#>            col_1       col_2 col_3
#> 1    Deutschland Deutschland  TRUE
#> 2 notDeutschland        <NA> FALSE
#> 
```
