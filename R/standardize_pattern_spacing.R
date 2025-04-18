#' Calculate the pattern spacing, so it is equal within a plot. Currently works only for two combined plots!
#'
#' @description
#' This function takes a pattern spacing, that is for example used for single plots, and calculates the relative pattern_spacings needed for combined plots.
#'
#'
#' @param plot_list List of plots that will be combined by [combine_plots()].
#' @param pattern_spacing Pattern spacing that will be taken as default for a whole plot. Defaults to `0.015`.
#'
#' @return Returns a numeric vector containing the pattern spacings for the plots.
#' @export
#'
#' @examples
#' # 1) Build the plots p1, p2.
#' # 2) standardize_pattern_spacing(list(p1, p2))
#' # 3) Build p1 and p2 new, but with the pattern spacings calculated in step 2.
standardize_pattern_spacing <- function(plot_list, pattern_spacing = 0.015) {
  coordinates <- vapply(plot_list, function(plot) {
    get_x_range(plot)
  }, FUN.VALUE = numeric(1))

  sum_coords <- sum(coordinates)

  plot_widths <- vapply(coordinates, function(coord_range) {
    coord_range / sum_coords
  }, FUN.VALUE = numeric(1))

  # final_spacing <- round((pattern_spacing - (plot_widths * pattern_spacing)), 4)
  spacings <- vapply(plot_widths, function(x) {
    (1 - x) * pattern_spacing
  }, FUN.VALUE = numeric(1))


  # final_spacing <- round(pattern_spacing * (1/plot_widths), 4)
  # spacing_single_plot <- sum(final_spacing)/length(plot_list)
  #
  # res_list <- list(spacing_plot_list = final_spacing,
  #                  spacing_single_plot = spacing_single_plot)

  res_list <- round(c(spacings), 4)

  message("Next Step: Update the bar_pattern_spacing values in your respective plots with the output values.")

  return(res_list)
}
