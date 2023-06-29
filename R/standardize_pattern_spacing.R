#' Calculate the pattern spacing, so it is equal within a plot.
#'
#' @param plot_list List of plots that will be combined by [combine_plots()].
#' @param pattern_spacing Pattern spacing that will be taken as default for a whole plot. Defaults to `0.1`.
#'
#' @return Returns a numeric vector containing the pattern spacings that have to be given into the plot functions.
#' @export
#'
#' @examples
#' # 1) Build the plots p1, p2.
#' # 2) standardize_pattern_spacing(list(p1, p2))
#' # 3) Build p1 and p2 new, but with the pattern spacings calculated in step 2.
standardize_pattern_spacing <- function(plot_list, y_range, pattern_spacing = 0.01) {

  coordinates <- vapply(plot_list, function(plot) {
    get_plot_coords(plot)
  }, FUN.VALUE = numeric(1))

  sum_coords <- sum(coordinates)

  plot_widths <- vapply(coordinates, function(coord_range) {
    coord_range / sum_coords
  }, FUN.VALUE = numeric(1))

# final_spacing <- round((pattern_spacing - (plot_widths * pattern_spacing)), 4)

final_spacing <- round(pattern_spacing * (1/plot_widths), 4)

return(final_spacing)
}
