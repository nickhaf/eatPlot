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
  y_lim <- calc_plot_lims_y(plot_dat$plot_braces, coords, plot_settings = plot_settings)

  list(
    # Y-Line ------------------------------------------------------------------
    ggplot2::annotate("segment",
                      x = 0,
                      xend = 0,
                      y = round(range_est[1] - 10, -1) - diff(range_est) * 0.00345,
                      yend = round(range_est[2], -1) + diff(range_est) * 0.00345
    ),
    ggplot2::scale_x_continuous(
      limits = c(
        0,1
      ),
      expand = c(0, 0)
    ),
    set_y_coords(plot_dat, range_est, y_lim),
    ## Use same coordinate system as the braces, so the plots can be aligned.
    set_cartesian_coords(y_lim),
    theme_y_axis()
  )}



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
