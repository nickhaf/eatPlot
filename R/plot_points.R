#' Plot Points.
#'
#' @param data_plot_points Data frame with the point estimates for every year. Prepared by ...
#' @inheritParams plot_lineplot
#' @inheritParams plot_single_lineplot
#' @inheritParams plotsettings_lineplot
#'
#' @return ggplot2 object
#' @export
#'
#' @examples # tbd
plot_points <- function(data_plot_points,
                        point_values = "est_noTrend_noComp",
                        point_sig = "sig_noTrend_noComp",
                        plot_lims,
                        plot_settings = plotsettings_lineplot()) {
  data_plot_points <- fill_column(data_plot_points, column_name = point_values, filling = NA)
  data_plot_points <- fill_column(data_plot_points, column_name = point_sig, filling = FALSE)

  ## fill_na

  data_plot_points <- data_plot_points[!is.na(data_plot_points$point_values), ]
  data_plot_points_nudge <- calc_y_nudge(data_plot_points,
                                         plot_lims,
                                         plot_settings = plot_settings)

    data_plot_points_nudge <- calc_x_nudge(data_plot_points_nudge,
                                           nudge_x = plot_settings$point_label_nudge_x,
                                           split_plot = plot_settings$split_plot)


  list(
    ggplot2::geom_point(
      data = data_plot_points_nudge,
      ggplot2::aes(
        x = .data$year_axis,
        y = .data$point_values,
        colour = .data$grouping_var,
        shape = .data$point_sig,
        group = .data$years_Trend
      ),
      size = plot_settings$point_size
    ),
    ## Hier genau den gleichen Nudge wie für die x-Achse
    if (plot_settings$point_label_nudge == TRUE) {
      ggrepel::geom_text_repel(
        data = data_plot_points_nudge,
        ggplot2::aes(
          x = .data$x_coords,
          y = .data$point_values,
          colour = .data$grouping_var,
          label = round(.data$point_values, 0),
          group = .data$years_Trend
        ),
        nudge_y = data_plot_points_nudge$nudge_y,
        min.segment.length = 100,
        size = plot_settings$point_label_size,
        direction = "y"
      )
    } else {
      ggplot2::geom_text(
        data = data_plot_points_nudge,
        ggplot2::aes(
          x = .data$x_coords,
          y = .data$point_values,
          colour = .data$grouping_var,
          label = round(.data$point_values, 0),
          group = .data$years_Trend
        ),
        nudge_y = data_plot_points_nudge$nudge_y,
        size = plot_settings$point_label_size
      )
    }
  )
}
