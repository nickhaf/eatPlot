# Construct labels

This function creates a new column or label, merging estimates and
standard errors with significant estimates represented in bold or as
superscript (via `brace_label_est`), and standard errors in brackets, if
needed (via `brace_label_se`). NAs are converted to empty strings. Main
usage is for plotting tables and brace labels.

## Usage

``` r
construct_label(
  dat,
  column_est = NULL,
  column_se = NULL,
  column_sig_bold = NULL,
  column_sig_superscript = NULL,
  sig_superscript_letter = NULL,
  round_est = 0,
  round_se = 1,
  plot_settings = plotsettings_tablebarplot()
)
```

## Arguments

- dat:

  Data frame with the columns that should be merged into labels.

- column_est:

  Character string for the column with the estimates.

- column_se:

  Character string for the column with the standard errors.

- column_sig_bold:

  Character string for the column with the significant estimates that
  should determine the bold printing.

- column_sig_superscript:

  Character string for the column with the significant estimates that
  should determine the superscript.

- sig_superscript_letter:

  Character string for the letter that should be used for the
  superscript.

- round_est:

  Rounding of brace_label_est.

- round_se:

  Rounding of brace_label_se.

- plot_settings:

  Named list constructed with
  [`plotsettings_lineplot()`](https://nickhaf.github.io/eatPlot/reference/plotsettings_lineplot.md).
  Defaults to a list with all settings set to `0`. There are several
  predefined lists with optimized settings for different plots. See
  [`plotsettings_lineplot()`](https://nickhaf.github.io/eatPlot/reference/plotsettings_lineplot.md)
  for an overview.

## Value

The data frame with an added column for the constructed label.

## Examples

``` r
# example data frame
dat <- data.frame(
  names = c("Berlin", "Hamburg", "Hessen", "Niedersachsen", "Saarland"),
  estimate = c(400, 650, 380, 500, 600),
  se = c(0.1, 0.45, 1, 0.27, 0.9),
  p_estimate = c(FALSE, FALSE, TRUE, TRUE, FALSE)
)
```
