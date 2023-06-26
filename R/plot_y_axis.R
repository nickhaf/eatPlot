#' Plot a y-axis plot. Can be merged with other plots for a customisable y-axis.
#'
#' @inheritParams plot_lineplot
#'
#' @return [ggplot2] plot that can be used as a y-axis.
#' @export
#'
#' @examples # tbd
plot_y_axis <- function(plot_dat, point_values, plot_settings = plotsettings_tablebarplot()) {
  range_est <- range(plot_dat[["plot_points"]][, point_values], na.rm = TRUE)
  coords <- calc_y_value_coords(range_est)

  df_y <- data.frame(
    years_Trend = "20112016",
    x = min(plot_dat[["plot_points"]]$year_axis),
    y = round(range_est[1] - 10, -1),
    yend = round(range_est[2], -1),
    xmax = max(plot_dat[["plot_points"]]$year_axis)
  )

  y_lims <- calc_plot_lims_y(plot_dat$plot_braces, coords, plot_settings = plot_settings)

  y_axis_ticks <- seq(round(range_est[1] - 10, -1), round(range_est[2], -1), by = 10)

  y_axis_dat <- data.frame(
    x_axis_coords = 0,
    y_axis_coords = y_axis_ticks,
    y_axis_labels = y_axis_ticks
  )


  ## Macht nicht so viel sinn, die Linie wird manuell geplotted, die Ticks nicht.
  list(
    ggplot2::annotate("segment",
      x = 0,
      xend = 0,
      y = round(range_est[1] - 10, -1),
      yend = round(range_est[2], -1)
    ),
    ggplot2::annotate("segment",
      x = 0 - 0.05,
      xend = 0 + 0.05,
      y = y_axis_ticks,
      yend = y_axis_ticks
    ),
    ggplot2::geom_label(
      y_axis_dat,
      ggplot2::aes(
        x = x_axis_coords,
        y = y_axis_ticks - 0.3,
        label = y_axis_labels
      )
    ),
    # ggplot2::scale_x_continuous(
    #   limits = c(
    #     min(plot_dat[["plot_points"]]$year_axis),
    #     min(plot_dat[["plot_points"]]$year_axis) + 1
    #   ),
    #  expand = c(0, 0)
    # ),
    set_y_coords(plot_dat, y_lims = y_lims),
    ## Use same coordinate system as the braces, so the plots can be aligned.
    set_cartesian_coords(y_lims),
    theme_y_axis()
  )
}


# Utils -------------------------------------------------------------------
calc_y_positions <- function(states, n_cols) {
  n_rows <- ceiling(length(states) / n_cols)
  n_cols <- n_cols + 1 # One column added for the y_axis.
  n_tiles <- n_rows * n_cols

  pos <- which(sapply(1:n_tiles, function(x) {
    (x - 1) %% n_cols == 0
  }))
  return(pos)
}
