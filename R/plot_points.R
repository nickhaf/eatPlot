#' Plot Points.
#'
#' @param data_plot_points Data frame with the point estimates for every year. Prepared by ...
#' @inheritParams plot_lineplot
#' @inheritParams plot_single_lineplot
#'
#' @return ggplot2 object
#' @export
#'
#' @examples # tbd
plot_points <- function(data_plot_points, y_range, point_values, point_sig, split_plot = FALSE, nudge_x = 0.05) {
  data_plot_points <- data_plot_points[!is.na(data_plot_points[, point_values]), ]
  data_plot_points_nudge <- calc_y_nudge(data_plot_points, y_range)

  if(split_plot == TRUE){
  data_plot_points_nudge <- calc_x_nudge(data_plot_points_nudge, nudge_x = nudge_x - 0.03)
}else{
  data_plot_points_nudge$x_coords <- data_plot_points_nudge$year
}

  list(
    ggplot2::geom_point(
      data = data_plot_points_nudge,
      ggplot2::aes(
        x = .data$year,
        y = .data[[point_values]],
        colour = .data$grouping_var,
        shape = .data[[point_sig]],
        group = .data$trend
      ),
      size = 1.7
    ),
    ## Hier genau den gleichen Nudge wie für die x-Achse
    ggplot2::geom_text(
      data = data_plot_points_nudge,
      ggplot2::aes(
        x = .data$x_coords,
        y = .data[[point_values]],
        colour = .data$grouping_var,
        label = round(.data[[point_values]], 0),
        group = .data$trend
      ),
      nudge_y = data_plot_points_nudge$nudge_y,
      size = 2
    )
  )
}
