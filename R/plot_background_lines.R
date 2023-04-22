#' Plot background lines.
#'
#' @param data_plot_background_lines Data.
#' @inheritParams plot_lineplot
#'
#' @return ggplot2 object.
#' @export
#'
#' @examples #tbd
plot_background_lines <- function(data_plot_background_lines, line_values) {
  ggplot2::geom_segment(
    data = data_plot_background_lines,
    ggplot2::aes(
      x = .data$year_start,
      xend = .data$year_end,
      y = .data[[line_values[1]]],
      yend = .data[[line_values[2]]],
      group = .data$years_Trend
    ),
    linewidth = 1,
    color = grDevices::rgb(147, 205, 221,
      maxColorValue = 255
    )
  )
}
