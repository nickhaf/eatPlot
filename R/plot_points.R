#' Plot Points.
#'
#' @param data_plot_points Data frame with the point estimates for every year. Prepared by ...
#' @inheritParams plot_lineplot
#'
#' @return ggplot2 object
#' @export
#'
#' @examples # tbd
plot_points <- function(data_plot_points, point_values, point_sig) {
  nudge_y_vec <- calc_y_nudge(data_plot_points[, point_values], n_groups = length(unique(data_plot_points$grouping_var)))


  list(
    ggplot2::geom_point(
      data = data_plot_points,
      ggplot2::aes(
        x = .data$year,
        y = .data[[point_values]],
        colour = .data$grouping_var,
        # group = .data[[grouping_var]],
        shape = .data[[point_sig]]
      ),
      size = 2.3
    ),
    ggplot2::geom_text(
      data = data_plot_points,
      ggplot2::aes(
        x = .data$year,
        y = .data[[point_values]],
        colour = .data$grouping_var,
        label = round(.data[[point_values]], 0)
      ),
      nudge_y = nudge_y_vec,
      size = 3
    )
  )
}
