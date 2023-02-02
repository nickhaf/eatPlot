#' Adds a layer with trend lines to a ggplot.
#'
#' @details Wrapper for [ggplot2::geom_segment()].
#' @param data_trend_point Output `data.frame` from [prep_trend_point()].
#'
#' @return ggplot2 object
#' @export
#'
#' @examples #tbd
plot_lines <- function(data_plot_lines){
    ggplot2::geom_segment(
      data = data_plot_lines,
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
