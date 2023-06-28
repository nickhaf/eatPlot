#' Set the x- and y-coordinates for a plot.
#'
#' @inheritParams plot_single_lineplot
#'
#' @return [ggplot2] object.
#' @export
#'
#' @examples # tbd
set_plot_coords <- function(plot_dat, plot_lims = plot_lims, plot_settings = plotsettings_lineplot()) {

  y_coords <- calc_y_value_space(plot_lims$coords,
                                 range_y = plot_lims$y_range,
                                 plot_settings)

  list(
    set_y_coords(plot_dat, y_coords, plot_lims, plot_settings),
    ggplot2::scale_x_continuous(
  #    limits = c(min(plot_dat[["plot_points"]]$year) -1, max(plot_dat[["plot_points"]]$year) + 1),
      breaks = unique(plot_dat[["plot_points"]]$year),
      expand = c(plot_settings$axis_x_background_width_x, 0) #ggplot2::expansion(c(plot_settings$axis_x_background_width_x, plot_settings$axis_x_background_width_x)) ## Increase, so the left and right side of the blue x-axis background gets bigger.
    )
  )
}


#' Calculate the plot-space between first brace and top of the plot.
#'
#' @keywords internal
#' @noRd
#'
#' @param y_range Numeric vector containing the minimum and maximum of the plotted values.
#' @param nudge_param_upper Numeric for increasing/decreasing the distance between highest plotted value and upper x-axis.
#' @param nudge_param_lower Numeric for increasing/decreasing the distance between lowest plotted value and first brace.
#'
#' @return Numeric vector containing the y-range between brace and upper x-axis.
#'
#' @examples calc_y_value_coords(c(0, 30))
calc_y_value_coords <- function(y_range, nudge_param_upper = 0.1, nudge_param_lower = 0.075) { # nudge_param increases the distance between lowest/highest point and braces/x axis
  range_est <- diff(y_range)
  coords <- c(
    ## Lower y limit
    plyr::round_any(y_range[1] - (y_range[1] * nudge_param_lower),
      accuracy = 10,
      f = floor
    ) - range_est * nudge_param_lower,
    ## upper y limit
    plyr::round_any(y_range[2] + (y_range[2] * nudge_param_upper),
      accuracy = 10,
      f = ceiling
    ) + range_est * nudge_param_upper
  )
  return(coords)
}


calc_y_value_space <- function(coords, range_y, plot_settings){
  x_axis_start_y <- coords[2] - (coords[2] * plot_settings$axis_x_background_width_y)
  brace_start_y <- coords[1]
  y_axis_start <- round(brace_start_y, -1)
  y_axis_end <- calc_y_value_coords(range_y, 0, 0)[2]

  y_coords <- c(y_axis_start, y_axis_end)

  return(y_coords)
}


# Utils -------------------------------------------------------------------
set_y_coords <- function(plot_dat, y_coords, plot_lims, plot_settings) {
  ggplot2::scale_y_continuous(
    breaks = seq_over(
      from = y_coords[1],
      to = y_coords[2],
      by = plot_settings$axis_y_tick_distance
    ),
    limits = plot_lims$y_lims_total,
    expand = c(0, 0)
  )
}

set_cartesian_coords <- function(y_lims_total) {
  ggplot2::coord_cartesian(
    # clip = "off", # Clip Coordinate system. Necessary, so the brace can be drawn under the x-axis.
    ylim = y_lims_total
  )
}

#' Sequencing function that also includes the first value of the sequence that is larger than the stopping parameter.
#'
#' @keywords internal
#' @noRd
#'
#' @param from Start value.
#' @param to End value.
#' @param by Step size.
#'
#' @return Sequenced vector.
#'
#' @examples #seq_over(10, 40, 20)
seq_over <- function(from, to, by){

  res_vec <- from
  i <- from
  while(i < to){
    i <- i + by
res_vec[length(res_vec) + 1] <- i
  }
  return(res_vec)
}

