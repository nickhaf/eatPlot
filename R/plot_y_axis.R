#' Plot a y-axis plot. Can be merged with other plots for a customisable y-axis.
#'
#' @inheritParams plot_lineplot
#'
#' @return [ggplot2] plot that can be used as a y-axis.
#' @export
#'
#' @examples # tbd
plot_y_axis <- function(plot_dat, plot_lims, plot_settings = plotsettings_lineplot()) {

  y_coords <- calc_y_value_space(plot_lims$coords, plot_lims$y_range, plot_settings)

  y_coords[2] <- max(seq_over(
    from = y_coords[1],
    to = y_coords[2],
    by = plot_settings$axis_y_tick_distance
  ))

  list(
    # Y-Line ------------------------------------------------------------------
    ggplot2::annotate("segment",
                      x = 0,
                      xend = 0,
                      y = y_coords[1] - diff(y_coords) * 0.002425, # Without this, the axis tick will be plotted a bit over the y-line.
                      yend = y_coords[2] + diff(y_coords) * 0.002425
    ),
    ggplot2::scale_x_continuous(
      limits = c(
        0,1
      ),
      expand = c(0, 0)
    ),
    set_y_coords(plot_dat, y_coords, plot_lims, plot_settings),
    ## Use same coordinate system as the braces, so the plots can be aligned.
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
