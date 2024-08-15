#' Plot the x-Axis.
#'
#' @inheritParams plot_points
#' @inheritParams plot_single_lineplot
#'
#' @return A blue stripe on top of ggplot2 plot which can be used as x-axis.
#' @export
#'
#' @examples # tbd
plot_x_axis <- function(plot_dat,
                        plot_settings = plotsettings_lineplot()) {


  if(is.null(plot_settings$axis_x_label_nudge_y)){
    plot_settings$axis_x_label_nudge_y <- plot_settings$axis_x_background_width_y / 2
  }

  y_max <- plot_dat$plot_lims$coords[2]

  coord_diff <- diff(range(plot_dat$plot_lims$coords))

  dat_coords <- unique(plot_dat$dat_final[, c("year", "trend")])

  dat_coords$x_labels <- as.character(dat_coords$year)
  dat_coords$y_coords <- y_max - (coord_diff * plot_settings$axis_x_label_nudge_y)


  # calc x-axis  ------------------------------------------------------------
  ## x-axis labels should be centered a bit more. So the larger year in the smaller trend and the smaller year in the larger trend need to go into the center more:

  # if (plot_settings$split_plot == TRUE) {
  #   dat_coords <- calc_x_nudge(dat_coords,
  #     nudge_x = plot_settings$axis_x_label_centralize,
  #     split_plot = plot_settings$split_plot
  #   )
  # } else {
    ## Don't have to be centralized in this case, looks good without
     #}

  res_list <- list(
    ggplot2::annotate(
      geom = "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = y_max - (coord_diff * plot_settings$axis_x_background_width_y), # Increase, so the x-axis background reaches lower.
      ymax = y_max,
      fill = plot_settings$axis_x_background_colour
    ),
    ggplot2::geom_text(dat_coords,
      mapping = ggplot2::aes(
        x = .data$year,
        y = .data$y_coords,
        label = .data$x_labels,
        group = .data$trend
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
