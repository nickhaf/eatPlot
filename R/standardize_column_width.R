#' Calculate the column widths for plots that should be later combined with [combine_plots].
#'
#' @param column_widths List of numeric vectors with proportional column widths regarding the final, combined plot. In the end, all proportions have to sum up to 1. However, if you want to set the width of barplots automatically, you can provide an `NA` for the column containing the barplot. In this case, it's width will be calculated automatically, so the proportions stay the same.
#' @param plot_ranges Numeric vector containing the ranges of the x-axis for alle barplots.
#'
#' @return Returns a list with numeric vectors containing the relative column widths that have to be set in the single plots.
#' @export
#'
#' @examples # tbd
standardize_column_width <- function(column_widths, plot_ranges) {

  names_column_widths <- names(column_widths)
  sum_plot_ranges <- sum(plot_ranges)

  plot_ranges <- sapply(plot_ranges, function(x) {
    x / sum_plot_ranges
  })

  rest <- 1 - sum(unlist(column_widths), na.rm = TRUE)

  plot_width <- rest * plot_ranges

  column_widths <- lapply(1:length(plot_width), function(x) {
    column_widths[[x]][is.na(column_widths[[x]])] <- plot_width[[x]]

    return(column_widths[[x]])
  })


  if (1 - sum(unlist(column_widths)) != 0) {
    stop("The sum of all your column widths has to be equal to 1.")
  }

  plot_widths <- lapply(column_widths, function(x) {
    sum(x, na.rm = TRUE) / 1
  })


  width_list <- lapply(1:length(column_widths), function(x) {
    plot_widths[[x]] * column_widths[[x]]
  })

res_list <- lapply(width_list, function(x) {
    sapply(x, function(y) {
      y / sum(x)
    })
  })


## superscript nudge

#res_list[["columns_table_sig_high_letter_nudge_x"]] <- sapply(plot_width, function(x){x * columns_table_sig_high_letter_nudge_x})

names(res_list) <- c(names_column_widths)#, "columns_table_sig_high_letter_nudge_x")

return(res_list)
}
