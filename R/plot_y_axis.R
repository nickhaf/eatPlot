#' Plot a y-axis plot. Can be merged with other plots for a customisable y-axis.
#'
#' @keywords internal
#' @noRd
#'
#' @param plot_dat Prepared data.
#' @inheritParams plot_lineplot
#'
#'
#' @return [ggplot2] plot that can be used as a y-axis.
#'
#' @examples # tbd
plot_y_axis <- function(y_axis_min, y_axis_max, y_total_min, y_total_max, tick_distance, plot_settings) {
  list(
    # Y-Line ------------------------------------------------------------------
    ggplot2::annotate("segment",
      x = 0,
      xend = 0,
      y = y_axis_min - ((y_total_max - y_total_min) * 0.001825), # Without this, the axis tick will be plotted a bit over the y-line.
      yend = y_axis_max + ((y_total_max - y_total_min) * 0.001825)
    ),
    ggplot2::scale_x_continuous(
      limits = c(
        0, 1
      ),
      expand = c(0, 0)
    ),
    set_y_scale(
      y_axis_min = y_axis_min,
      y_axis_max = y_axis_max,
      y_total_min = y_total_min,
      y_total_max = y_total_max,
      tick_distance = tick_distance,
      margin_top = plot_settings$margin_top,
      margin_left = plot_settings$margin_left,
      margin_bottom = plot_settings$margin_bottom,
      margin_right = plot_settings$margin_right
    ),
    theme_y_axis(plot_settings)
  )
}



# Utils -------------------------------------------------------------------



calc_y_ticks_min_max <- function(coords, plot_settings) {
  ## Set the nudging parameter lower when calculating the coords in order to decrease the distance from the end of the y_axis to the nearest point.

  y_axis_lims <- c()
  y_axis_lims[1] <- plyr::round_any(coords[1], 10, floor)
  ## Now, go in equal steps until a somehow defined top, so the steps gor from one full value to another full value.
  y_axis_lims[2] <- max(seq_over(
    from = y_axis_lims[1],
    to = coords[2], ## Alternatively to top of y-range. But I think that's not necessary, if I set the nudging par for the coords low enough
    by = plot_settings$axis_y_tick_distance
  ))

  return(y_axis_lims)
}

calc_y_positions <- function(facets, n_cols) {
  n_rows <- ceiling(length(facets) / n_cols)
  n_cols <- n_cols + 1 # One column added for the y_axis.
  n_tiles <- n_rows * n_cols

  pos <- which(sapply(1:n_tiles, function(x) {
    (x - 1) %% n_cols == 0
  }))
  return(pos)
}
