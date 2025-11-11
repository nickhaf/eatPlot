# Round a numeric vector so it sums exactly to 100

Floors values and then distributes the remaining units to the entries
with the largest fractional parts (the “largest remainders” method) so
that the final vector sums to exactly 100.

## Usage

``` r
round_preserve_100(x)
```

## Arguments

- x:

  A numeric vector whose elements approximately sum to 100 (within c(99,
  101)). Values should be finite and typically non-negative.

## Value

A numeric vector of the same length as `x`, with integer values that sum
to exactly 100.

## Examples

``` r
round_preserve_100(c(33.4, 33.3, 33.3))
#> [1] 34 33 33
round_preserve_100(c(12.9, 12.1, 25.5, 49.5))
#> [1] 13 12 26 49

# Edge case: already sums to 100
round_preserve_100(c(10, 20, 30, 40))
#> [1] 10 20 30 40

# Will error if far from 100:
if (FALSE) { # \dontrun{
round_preserve_100(c(10, 10, 10))  # sum = 30
} # }
```
