# Multiply Values in Columns by 100 to build percentages.

Multiply Values in Columns by 100 to build percentages.

## Usage

``` r
construct_percent(df, columns)
```

## Arguments

- df:

  Input data.frame.

- columns:

  Character vector with the column names of the columns you want to
  multiply by 100.

## Value

The input data.frame with the new columns multiplied by 100. New columns
have the suffix "\_percent" in their name.

## Examples

``` r
df <- data.frame(col_1 = c(1, 2), col_2 = c(3, 4), col_3 = c("a", "b"))
construct_percent(df, columns = c("col_1", "col_2"))
#>   col_1 col_2 col_3 col_1_percent col_2_percent
#> 1     1     3     a           100           300
#> 2     2     4     b           200           400
```
