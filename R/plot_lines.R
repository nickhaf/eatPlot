#' Adds a layer with trend lines to a ggplot.
#'
#' @details Wrapper for [ggplot2::geom_segment()].
#' @param data_plot_lines data.
#' @inheritParams plot_lineplot
#'
#' @return ggplot2 object
#' @export
#'
#' @examples #tbd
plot_lines <- function(data_plot_lines, line_values, line_sig){
    ggplot2::geom_segment(
      data = data_plot_lines,
      ggplot2::aes(
        x = .data$year_start,
        xend = .data$year_end,
        y = .data[[line_values[1]]],
        yend = .data[[line_values[2]]],
        colour = .data$grouping_var,
        linetype = .data[[line_sig]],
        group = .data$trend
      ),
      linewidth = 1.25

  )
}
