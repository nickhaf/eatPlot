#' Plot background lines.
#'
#' @param data_plot_background_lines Data.
#'
#' @return ggplot2 object.
#' @export
#'
#' @examples #tbd
plot_background_lines <- function(data_plot_background_lines) {
  ggplot2::geom_segment(
    data = data_plot_background_lines,
    ggplot2::aes(
      x = .data$year_start,
      xend = .data$year_end,
      y = .data$est_point_start,
      yend = .data$est_point_end
    ),
    linewidth = 1.6,
    color = grDevices::rgb(147, 205, 221,
      maxColorValue = 255
    )
  )
}
