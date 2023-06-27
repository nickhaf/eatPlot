#' Plot the x-Axis.
#'
#' @inheritParams plot_points
#' @inheritParams plot_single_lineplot
#'
#' @return A blue stripe on top of ggplot2 plot which can be used as x-axis.
#' @export
#'
#' @examples # tbd
plot_x_axis <- function(data_plot_points, y_range, plot_settings = plotsettings_lineplot()) {
  coords <- calc_y_value_coords(y_range)
  y_max <- coords[2]

  dat_coords <- data_plot_points[, c("year", "year_axis", "years_Trend")]

  dat_coords$x_labels <- as.character(dat_coords$year)
  dat_coords$y_coords <- y_max - y_max * plot_settings$axis_x_label_nudge_y


  # calc x-axis  ------------------------------------------------------------
  ## x-axis labels should be centered a bit more. So the larger year in the smaller trend and the smaller year in the larger trend need to go into the center more:

  if (plot_settings$split_plot == TRUE) {
    dat_coords <- calc_x_nudge(dat_coords,
      nudge_x = plot_settings$axis_x_label_centralize,
      split_plot = plot_settings$split_plot
    )
  } else {
    ## Don't have to be centralized in this case, looks good without
   dat_coords$x_coords <- dat_coords$year_axis
  }

  res_list <- list(
    ggplot2::annotate(
      geom = "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = y_max - (y_max * plot_settings$axis_x_background_width_y), # Increase, so the x-axis background reaches lower.
      ymax = y_max,
      fill = plot_settings$axis_x_background_colour
    ),
    ggplot2::geom_text(dat_coords,
      mapping = ggplot2::aes(
        x = .data$x_coords,
        y = .data$y_coords,
        label = .data$x_labels,
        group = .data$years_Trend
      ),
      size = plot_settings$axis_x_label_size
    ),
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  )
  return(res_list)
}
