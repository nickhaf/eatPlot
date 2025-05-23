#' Calculate the column widths for plots that should be later combined with [combine_plots()].
#'
#' @description
#' If multiple tables/barplots should be combined, the column widths might be distorted, as the plots need to be scaled on the x-axes of the barplots. Therefore, [standardize_column_width()] can be used to calculate the column_widths that are entered in [plotsettings_tablebarplot()] when defining each plot.
#'
#' @param column_widths List of numeric vectors with proportional column widths for the final, combined plot. In the end, all proportions have to sum up to 1. However, if you want to set the width of barplots automatically, you can provide an `NA` for the column containing the barplot. In this case, it's width will be calculated automatically, so the proportions stay the same.
#' @param plot_ranges Numeric vector containing the ranges of the x-axis for alle barplots. Defaults to `c(0, 0)`.
#'
#' @return Returns a list with numeric vectors containing the relative column widths that have to be set in the single plots.
#' @export
#'
#' @examples
#'
#' ## The first column of the left plot will cover 10 % of the plot width, the second 20 % and so on:
#' standardize_column_width(
#'   column_widths = list(
#'     p1 = c(0.1, 0.2),
#'     p2 = c(0.5, 0.2)
#'   ),
#'   plot_ranges = c(10, 30)
#' )
#'
#' ## NAs will be interpreted as barplots, in wich case the width of the barplots will be
#' ## calclulated automatically, so the x-axes are on the same scale.
#' standardize_column_width(
#'   column_widths = list(
#'     p1 = c(0.1, NA),
#'     p2 = c(0.5, NA)
#'   ),
#'   plot_ranges = c(10, 30)
#' )
standardize_column_width <- function(column_widths, plot_ranges = c(0, 0)) {
  if (length(plot_ranges) != length(column_widths)) {
    stop("You need to provide plot ranges for every plot.", call. = FALSE)
  }
  names_column_widths <- names(column_widths)

  column_widths <- calc_barplot_width(column_widths, plot_ranges)

  relative_barplot_widths <- lapply(column_widths, function(x) {
    sum(x, na.rm = TRUE) / 1
  })

  ## Relative column widths weighted on each barplot width:
  relative_column_widths <- lapply(1:length(column_widths), function(x) {
    relative_barplot_widths[[x]] * column_widths[[x]]
  })

  ## Standardize, so all single plots have column widths summing to 1
  standardized_column_widths <- lapply(relative_column_widths, function(x) {
    sapply(x, function(y) {
      y / sum(x)
    })
  })


  ## superscript nudge

  # res_list[["columns_table_sig_superscript_letter_nudge_x"]] <- sapply(plot_width, function(x){x * columns_table_sig_superscript_letter_nudge_x})

  names(standardized_column_widths) <- c(names_column_widths) # , "columns_table_sig_superscript_letter_nudge_x")

  return(standardized_column_widths)
}


# Utils -------------------------------------------------------------------
calc_barplot_width <- function(column_widths, plot_ranges) {
  ## Fill up NAs with the remaining space with the relative barplot widths
  sum_plot_ranges <- sum(abs(plot_ranges))
  relative_plot_ranges <- sapply(plot_ranges, function(x) {
    abs(x / sum_plot_ranges)
  })

  rest <- 1 - sum(unlist(column_widths), na.rm = TRUE)
  plot_width <- rest * relative_plot_ranges

  column_widths <- lapply(1:length(plot_width), function(x) {
    column_widths[[x]][is.na(column_widths[[x]])] <- plot_width[[x]]
    return(column_widths[[x]])
  })

  return(column_widths)
}
