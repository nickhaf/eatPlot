# Insert a row above a given index, preserving column types

Insert a row above a given index, preserving column types

## Usage

``` r
insert_row(dat, above, cols = character(), values = list())
```

## Arguments

- dat:

  A data.frame.

- above:

  Integer scalar: insert *before* this row (1..nrow(dat)+1).

- cols:

  Character vector of column names to fill in the new row.

- values:

  List/atomic vector of values matching `cols` (same length).

## Value

The input data.frame with one additional row.
