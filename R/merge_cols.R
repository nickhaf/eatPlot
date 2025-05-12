
#' Coalesce together multiple columns into one.
#'
#' Columns that contain unique values per row can be combined into one. Main usage if multiple different subgroup variables should be plotted in the same column in a plot.
#'
#' @inheritParams plot_lineplot
#' @inheritParams construct_label
#'
#' @param dat Data frame with the columns that should be merged into labels.
#' @param label_est Character string for the column with the estimates.
#' @param label_se Character string for the column with the standard errors.
#' @param label_sig_bold Character string for the column with the significant estimates that should determine the bold printing.
#' @param label_sig_superscript Character string for the column with the significant estimates that should determine the superscript.
#' @param label_sig_superscript_extra_column Logical, if set 'FALSE' the superscript for significant values is added directly into the label (necessary for line plots), if set 'TRUE' the superscript for significant values is written into an extra column with the ending '_sig_superscript' (necessary for tables).
#' @param new_name Character string for the new column that is added to `dat`. Defaults to `'label'`.
#' @param round_est Rounding of label_est.
#' @param round_se Rounding of label_se.
#'
#' @return The data frame with an added column for the constructed label.
#' @export
#'
#' @examples
#' tbd

merge_cols <- function(dat, type, comparison, facet = "sameFacet"){
  cols_to_merge <- grep(paste0("^", type, "_", ".*", facet, ".*", comparison), names(dat), value = TRUE)
  res <- apply(dat[ , cols_to_merge], 1, function(row) {
    row[!is.na(row)][1]
  })

  return(res)
}
