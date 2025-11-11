# Wrapper for [`grDevices::pdf()`](https://rdrr.io/r/grDevices/pdf.html).

Wrapper for [`grDevices::pdf()`](https://rdrr.io/r/grDevices/pdf.html).

## Usage

``` r
save_plot(
  p,
  filename,
  width = 160,
  height = 185,
  colormodel = "cmyk",
  scaling = 1,
  ...
)
```

## Arguments

- p:

  Plot that should be saved.

- filename:

  Filepath and -name for the new plot file.

- width:

  Width of the new plot file in mm. Defaults to `160`.

- height:

  Height of the new plot file in mm. Defaults to `185`

- colormodel:

  Colormodel for saving the plot. See
  [`grDevices::pdf()`](https://rdrr.io/r/grDevices/pdf.html) for more
  information. Defaults to "cmyk".

- scaling:

  Scaling parameter for changing height in widht relationally. Defaults
  to `1`.

- ...:

  Further arguments passed to
  [`grDevices::pdf()`](https://rdrr.io/r/grDevices/pdf.html).

## Value

A PDF file.

## Examples

``` r
# tbd
```
