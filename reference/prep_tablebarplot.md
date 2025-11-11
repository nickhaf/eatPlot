# Prepare tableplot data.

Prepare tableplot data.

## Usage

``` r
prep_tablebarplot(
  eatRep_dat,
  subgroup_var = NULL,
  parameter = "mean",
  facet_var = "TR_BUNDESLAND",
  total_facet = "total",
  sig_niveau = 0.05,
  total_subgroup = "total",
  parameter_to_colname = TRUE
)
```

## Arguments

- eatRep_dat:

  Object returned by `eatRep`.

- subgroup_var:

  Character string of the column in `eatPlot_dat$group` or
  `eatPlot_dat$plain` containing the subgroup mapping. Each supgroup
  will receive it's own line. IF there are subgroups within the data,
  this needs to be set, otherwise the data preparation might fail.
  Defaults to `NULL`.

- parameter:

  Character string. Contains the value in column `parameter` that is
  used for plotting the lines. Defaults to `"mean"`.

- facet_var:

  Character string of the variable containing information on groups some
  of the comparisons are made against. This is needed to decosntruct
  comparisons like `crossDiff` into `crossDiff` and `crossDiffTotal` (so
  a crossDiff comparison against the total group). Name might be a bit
  confusing, but is the same as in `prep_lineplot`. Defaults to
  `TR_BUNDESLAND`.

- total_facet:

  Character string of the name of the total groups containing all other
  groups of the facet var. Defaults to `"total"`.

- sig_niveau:

  Numeric indicating the border below which p-values will be considered
  significant. Defaults to `0.05`.

- total_subgroup:

  Character string indicating the subgroup containing all other groups
  of the subgroup_var. Defaults to `"total"`.

- parameter_to_colname:

  Logical that indicates whether the parameter should be also pivoted
  into wide format, or the parameter column should be kept in the long
  format. Mainly set to `FALSE` for stacked barplots. Defaults to
  `TRUE`.

## Value

Data prepared for plotting the BT-lineplots.

## Examples

``` r
# tbd
```
