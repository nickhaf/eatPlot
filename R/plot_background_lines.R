#' Plot background lines.
#'
#' @keywords internal
#' @noRd

#' @param dat_total Data.
#' @inheritParams plot_lineplot
#'
#' @return ggplot2 object.
#'
#' @examples # tbd
plot_background_lines <- function(dat_total,
                                  plot_settings = plotsettings_lineplot()) {
  check_columns(dat_total, c("point_est", "line_se", "year", "trend"))
  dat_total$y_pos <- dat_total$point_est + 2 * dat_total$line_se
  dat_total$y_neg <- dat_total$point_est - 2 * dat_total$line_se

  ggplot2::geom_ribbon(
    data = dat_total,
    ggplot2::aes(
      x = .data$year,
      ymin = .data$y_neg,
      ymax = .data$y_pos,
      group = .data$trend
    ),
    color = plot_settings$background_line_colour,
    fill = plot_settings$background_line_colour,
    linewidth = 0.5
  )


  # ################ If no SE should be plotted:
  # message("Plotting only the lines for the estimates as no SE column was found. Please check if this was not intended.")
  #
  # # ggplot2::ggplot() +
  # ggplot2::geom_segment(
  #   data = dat,
  #   ggplot2::aes(
  #     x = .data$year_start_axis,
  #     xend = .data$year_end_axis,
  #     y = .data[[line_values[1]]],
  #     yend = .data[[line_values[2]]],
  #     group = .data$years_Trend
  #   ),
  #   linewidth = 1,
  #   color = plot_settings$background_line_colour
  # )
}
