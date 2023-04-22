#' Plot a y-axis plot. Can be merged with other plots for a customisable y-axis.
#'
#' @inheritParams plot_lineplot
#'
#' @return [ggplot2] plot that can be used as a y-axis.
#' @export
#'
#' @examples #tbd
plot_y_axis <- function(plot_data) {
  range_est <- range(plot_data[["plot_points"]]$point_values, na.rm = TRUE)
  coords <- calc_coords(range_est)

  df_y <- data.frame(
    years_Trend = "20112016",
    x = min(plot_data[["plot_points"]]$year),
    y = round(range_est[1] - 10, -1),
    yend = round(range_est[2], -1),
    xmax = max(plot_data[["plot_points"]]$year)
  )

  list(
    ggplot2::geom_segment(
      data = df_y,
      ggplot2::aes(
        x = .data$x,
        xend = .data$x,
        y = .data$y,
        yend = .data$yend
      )
    ),
    ggplot2::scale_x_continuous(limits = c(min(plot_data[["plot_points"]]$year),
                                           min(plot_data[["plot_points"]]$year) + 1),
                                expand = c(0, 0)
                                ),
    set_y_coords(plot_data),
    ## Use same coordinate system as the braces, so the plots can be aligned.
    set_cartesian_coords(coords),
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
