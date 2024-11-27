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
    plot_settings$axis_x_label_nudge_y <- plot_settings$axis_x_background_width_y / 2

  y_max <- plot_dat$plot_lims$coords[2]

  coord_diff <- diff(range(plot_dat$plot_lims$coords))

  dat_coords <- unique(plot_dat$plot_dat[, c("year", "trend")])

  dat_coords$x_labels <- as.character(dat_coords$year)
  dat_coords$y_coords <- y_max - (coord_diff * plot_settings$axis_x_label_nudge_y)

  # calc x-axis  ------------------------------------------------------------
  ## x-axis labels should be centered a bit more. So the larger year in the smaller trend and the smaller year in the larger trend need to go into the center more:

  browser()
  ## so, obviously I don't provide the dat$plot_dat stuff here.
  ## Rework the nudge function, so it gets more versatile.
  ## Just input the range and calc the nudge by that maybe? something like that anyways.
  ## Or put the coords into plot-settings, not into plot_dat?
 ## Get more concious about what the different objects in these lists are for.
  dat_coords <- calc_x_nudge(
      dat_coords,
      nudge_x = plot_settings$axis_x_label_centralize,
      plot_settings = plot_settings
    )

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
        x = .data$x_coords,
        y = .data$y_coords,
        label = .data$x_labels,
        group = .data$trend
      ),
      size = plot_settings$axis_x_label_size,
      colour = "black"
    ),
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  )
  return(res_list)
}
