#' Plot Points.
#'
#' @keywords internal
#' @noRd
#'
#' @param plot_dat Data frame with the point estimates for every year. Prepared by ...
#' @inheritParams plot_lineplot
#' @inheritParams plot_single_lineplot
#' @inheritParams plotsettings_lineplot
#'
#' @return ggplot2 object
#'
#' @examples # tbd
plot_points <- function(plot_dat) {
  check_columns(plot_dat$plot_dat, c("point_est", "year", "trend", "point_sig"))
  nudge_val <- calc_y_nudge(plot_dat,
    plot_settings = plot_dat$plot_settings
  )
  plot_dat$dat_final <- within(plot_dat$plot_dat, {
    nudge_y <- ifelse(point_est == ave(point_est, year, FUN = min),
      -nudge_val,
      nudge_val
    )
  })

  plot_dat$dat_final <- calc_x_nudge(plot_dat,
                                     nudge_x = plot_dat$plot_settings$point_label_nudge_x,
    plot_settings = plot_dat$plot_settings
  )

  list(
    ggplot2::geom_point(ggplot2::aes(shape = .data$point_sig)),

    ## Hier genau den gleichen Nudge wie für die x-Achse
    # if (plot_settings$point_label_nudge == TRUE) {
    #   ggrepel::geom_text_repel(
    #     data = data_plot_points_nudge,
    #     ggplot2::aes(
    #       x = .data$x_coords,
    #       y = .data$point_values,
    #       colour = .data$grouping_var,
    #       label = round(.data$point_values, 0),
    #       group = .data$years_Trend
    #     ),
    #     nudge_y = data_plot_points_nudge$nudge_y,
    #     min.segment.length = 100,
    #     size = plot_settings$point_label_size,
    #     direction = "y"
    #   )
    # } else {
    ggplot2::geom_text(
      data = plot_dat$dat_final,
      ggplot2::aes(
        x = .data$x_coords,
        label = round(.data$point_est, 0)
      ),
      nudge_y = plot_dat$dat_final$nudge_y,
      size = plot_dat$plot_settings$point_label_size
    )
    # }
  )
}
