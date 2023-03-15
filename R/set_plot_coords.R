set_plot_coords <- function(plot_dat) {
  min_year <- min(plot_dat[["plot_points"]]$year, na.rm = TRUE)
  max_year <- max(plot_dat[["plot_points"]]$year, na.rm = TRUE)

  list(
    ggplot2::scale_y_continuous(
      breaks = seq(
        from = round(min(plot_dat[["plot_points"]]$est_point, na.rm = TRUE) - 10, -1),
        to = round(max(plot_dat[["plot_points"]]$est_point, na.rm = TRUE), -1),
        by = 20
      ),
      expand = c(0, 0)
    ),
    ggplot2::scale_x_continuous(
      # limits = c(
      #   min_year - (max_year - min_year) * 0.1,
      #   max_year + (max_year - min_year) * 0
      # ),
      position = "top",
      breaks = unique(plot_dat[["plot_points"]]$year),
      expand = c(0, 0)
    )
  )
}
