#' Adds a layer with trend lines to a ggplot.
#'
#' @details Wrapper for \link[ggplot2]{geom_segment}.
#' @param data_lines
#'
#' @return ggplot2 object
#' @export
#'
#' @examples #tbd
plot_lines <- function(data_lines){
    ggplot2::geom_segment(
      data = data_lines,
      ggplot2::aes(
        x = .data$year_start,
        xend = .data$year_end,
        y = .data$est_point_start,
        yend = .data$est_point_end,
        colour = .data$grouping_var,
        linetype = .data$sig_trend_within
      )
  )
}
