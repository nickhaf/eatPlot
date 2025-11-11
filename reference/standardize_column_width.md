# Calculate the column widths for plots that should be later combined with [`combine_plots()`](https://nickhaf.github.io/eatPlot/reference/combine_plots.md).

If multiple tables/barplots should be combined, the column widths might
be distorted, as the plots need to be scaled on the x-axes of the
barplots. Therefore, `standardize_column_width()` can be used to
calculate the column_widths that are entered in
[`plotsettings_tablebarplot()`](https://nickhaf.github.io/eatPlot/reference/plotsettings_tablebarplot.md)
when defining each plot.

## Usage

``` r
standardize_column_width(column_widths, plot_ranges = c(0, 0))
```

## Arguments

- column_widths:

  List of numeric vectors with proportional column widths for the final,
  combined plot. In the end, all proportions have to sum up to 1.
  However, if you want to set the width of barplots automatically, you
  can provide an `NA` for the column containing the barplot. In this
  case, it's width will be calculated automatically, so the proportions
  stay the same.

- plot_ranges:

  Numeric vector containing the ranges of the x-axis for alle barplots.
  Defaults to `c(0, 0)`.

## Value

Returns a list with numeric vectors containing the relative column
widths that have to be set in the single plots.

## Examples

``` r
## The first column of the left plot will cover 10 % of the plot width, the second 20 % and so on:
standardize_column_width(
  column_widths = list(
    p1 = c(0.1, 0.2),
    p2 = c(0.5, 0.2)
  ),
  plot_ranges = c(10, 30)
)
#> $p1
#> [1] 0.3333333 0.6666667
#> 
#> $p2
#> [1] 0.7142857 0.2857143
#> 

## NAs will be interpreted as barplots, in wich case the width of the barplots will be
## calclulated automatically, so the x-axes are on the same scale.
standardize_column_width(
  column_widths = list(
    p1 = c(0.1, NA),
    p2 = c(0.5, NA)
  ),
  plot_ranges = c(10, 30)
)
#> $p1
#> [1] 0.5 0.5
#> 
#> $p2
#> [1] 0.625 0.375
#> 
```
