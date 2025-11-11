# Prepare lineplot data.

Prepare lineplot data.

## Usage

``` r
prep_lineplot(
  eatRep_dat,
  subgroup_var = NULL,
  total_subgroup = "total",
  facet_var = "TR_BUNDESLAND",
  total_facet = "total",
  parameter = "mean",
  sig_niveau = 0.05
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

- total_subgroup:

  Character string indicating the subgroup containing all other groups
  of the subgroup_var. Defaults to `"total"`.

- facet_var:

  Character string of the name of the variable that should be used for
  faceting the plot. Defaults to `"TR_BUNDESLAND"`.

- total_facet:

  Character string of the name of the total groups containing all other
  groups of the facet var. Defaults to `"total"`.

- parameter:

  Character string. Contains the value in column `parameter` that is
  used for plotting the lines. Defaults to `"mean"`.

- sig_niveau:

  Numeric indicating the border below which p-values will be considered
  significant. Defaults to `0.05`.

## Value

Data prepared for plotting the BT-lineplots.

## Examples

``` r
# tbd
```
