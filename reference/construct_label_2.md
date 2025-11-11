# Construct labels

This function creates a new column or label, merging estimates and
standard errors with significant estimates represented in bold or as
superscript (via `label_est`), and standard errors in brackets, if
needed (via `label_se`). NAs are converted to empty strings. Main usage
is for plotting tables and brace labels.

## Usage

``` r
construct_label_2(
  dat,
  new_name = "label",
  label_est = NULL,
  label_se = NULL,
  label_sig_bold = NULL,
  label_sig_superscript = NULL,
  label_sig_superscript_extra_column = FALSE,
  round_est = 0,
  round_se = 1,
  plot_settings = plotsettings_tablebarplot()
)
```

## Arguments

- dat:

  Data frame with the columns that should be merged into labels.

- new_name:

  Character string for the new column that is added to `dat`. Defaults
  to `'label'`.

- label_est:

  Character string for the column with the estimates.

- label_se:

  Character string for the column with the standard errors.

- label_sig_bold:

  Character string for the column with the significant estimates that
  should determine the bold printing.

- label_sig_superscript:

  Character string for the column with the significant estimates that
  should determine the superscript.

- label_sig_superscript_extra_column:

  Logical, if set 'FALSE' the superscript for significant values is
  added directly into the label (necessary for line plots), if set
  'TRUE' the superscript for significant values is written into an extra
  column with the ending '\_sig_superscript' (necessary for tables).

- round_est:

  Rounding of label_est.

- round_se:

  Rounding of label_se.

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
