# Process the data in the Bundesland column.

This changes `ue` to `ü` and adds a `-` between bundesländer consisting
of two words.

## Usage

``` r
process_bundesland(vec, linebreak = FALSE, total_group = "total")
```

## Arguments

- vec:

  Character vector that contains Bunesländer that should be processed.

- linebreak:

  Logical. If `TRUE`, a linebreak is added after the '-'. Defaults to
  `FALSE`.

- total_group:

  Character string that should be replaced by `Deutschland`. Defaults to
  `"total"`.

## Value

The eatPlot_data data.frame with the bundesländer column processed.

## Examples

``` r
# tbd
```
