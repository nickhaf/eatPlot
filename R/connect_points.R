
## Helper function, connects two points with a line.
## Ajdusts linetype according to significance of the within Trend variable.


#' Connect Points
#'
#' @param my_data Data
#' @param grouping_var Variable for grouping
#'
#' @return ggplot
#' @export
#'

connect_points <- function(my_data, grouping_var) {
  list(
    plot_res <- ggplot2::geom_segment(
      data = my_data,
      ggplot2::aes(
        x = year_start,
        xend = year_end,
        y = est_start,
        yend = est_end,
        colour = .data[[grouping_var]],
        linetype = sigTrend_within
      ),
      size = 0.7),
    linetype_iqb
  )

  return(plot_res)
}
