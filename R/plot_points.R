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
                        point_values = "est_point",
                        point_sig = "sig_point",
                        y_range,
                        plot_settings = plotsettings_lineplot()
                        ) {

  data_plot_points <- fill_column(data_plot_points, column_name = point_values, filling = NA)
  data_plot_points <- fill_column(data_plot_points, column_name = point_sig, filling = FALSE)

  data_plot_points <- data_plot_points[!is.na(data_plot_points$point_values), ]
  data_plot_points_nudge <- calc_y_nudge(data_plot_points, y_range, plot_settings = plot_settings)

  if(plot_settings$split_plot == TRUE){
  data_plot_points_nudge <- calc_x_nudge(data_plot_points_nudge, nudge_x = plot_settings$point_label_nudge_x)
}else{
  data_plot_points_nudge$x_coords <- data_plot_points_nudge$year + diff(range(data_plot_points$year)) * plot_settings$point_label_nudge_x
}

  list(
    ggplot2::geom_point(
      data = data_plot_points_nudge,
      ggplot2::aes(
        x = .data$year,
        y = .data$point_values,
        colour = .data$grouping_var,
        shape = .data$point_sig,
        group = .data$trend
      ),
      size = plot_settings$point_size
    ),
    ## Hier genau den gleichen Nudge wie für die x-Achse
    if(plot_settings$point_label_nudge == TRUE){
    ggrepel::geom_text_repel(
      data = data_plot_points_nudge,
      ggplot2::aes(
        x = .data$x_coords,
        y = .data$point_values,
        colour = .data$grouping_var,
        label = round(.data$point_values, 0),
        group = .data$trend
      ),
      nudge_y = data_plot_points_nudge$nudge_y,
      min.segment.length = 100,
      size = plot_settings$point_label_size,
      direction = "y"
    )
    }else{
      ggplot2::geom_text(
        data = data_plot_points_nudge,
        ggplot2::aes(
          x = .data$x_coords,
          y = .data$point_values,
          colour = .data$grouping_var,
          label = round(.data$point_values, 0),
          group = .data$trend
        ),
        nudge_y = data_plot_points_nudge$nudge_y,
        size = plot_settings$point_label_size
      )
    }
  )
}
