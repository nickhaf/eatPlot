# Coalesce together multiple columns into one.

Columns that contain unique values per row can be combined into one.
Main usage if multiple different subgroup variables should be plotted in
the same column in a plot.

## Usage

``` r
merge_cols(dat, type, comparison, facet = "sameFacet")
```

## Arguments

- dat:

  A data frame prepared by
  [prep_tablebarplot](https://nickhaf.github.io/eatPlot/reference/prep_tablebarplot.md)
  containing the columns to be merged.

- type:

  A string indicating which column type should be merged (e.g., "est",
  "se", "sig" ...)

- comparison:

  A string indicating which comparison should be merged (e.g.,
  "groupDiff").

- facet:

  A string indicating which facet should be merged ("sameFacet",
  "totalFacet").

## Value

A vector consisting of the merged columns.

## Examples

``` r
# tbd
```
