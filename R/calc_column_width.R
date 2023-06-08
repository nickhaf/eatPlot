#' Title
#'
#' @param column_widths
#' @param plot_ranges
#'
#' @return
#' @export
#'
#' @examples
calc_column_width <- function(column_widths, plot_ranges, columns_table_sig_high_letter_nudge_x) {
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

  ## Auch für mehr als 3 Plots

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

res_list[["columns_table_sig_high_letter_nudge_x"]] <- sapply(plot_width, function(x){x * columns_table_sig_high_letter_nudge_x})

return(res_list)
}
