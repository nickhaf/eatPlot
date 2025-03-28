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
plot_y_axis <- function(y_axis_min, y_axis_max, tick_distance, y_axis_lims, plot_settings) {

  list(
    # Y-Line ------------------------------------------------------------------
    ggplot2::annotate("segment",
      x = 0,
      xend = 0,
      y = y_axis_min, #- diff(plot_dat$plot_lims$y_range) * 0.002425, # Without this, the axis tick will be plotted a bit over the y-line.
      yend = y_axis_max #+ diff(plot_dat$plot_lims$y_range) * 0.002425
    ) ,
    ggplot2::scale_x_continuous(
      limits = c(
        0, 1
      ),
      expand = c(0, 0)
    ),
    ggplot2::scale_y_continuous(
      breaks = seq_over(
        from = y_axis_min,
        to = y_axis_max,
        by = tick_distance
      ),
      limits = c(y_axis_min, y_axis_max),
      expand = c(0, 0)
    ),
    # ggplot2::coord_cartesian(ylim = y_axis_lims, clip = "off"),
     theme_y_axis(plot_settings)
  )
}



# Utils -------------------------------------------------------------------



calc_y_lims <- function(coords, plot_settings){

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
