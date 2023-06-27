#' Set the x- and y-coordinates for a plot.
#'
#' @inheritParams plot_single_lineplot
#'
#' @return [ggplot2] object.
#' @export
#'
#' @examples # tbd
set_plot_coords <- function(plot_dat, x_range, y_range, plot_settings = plotsettings_lineplot()) {

  coords <- calc_y_value_coords(y_range)
  y_lim <- calc_plot_lims_y(plot_dat$plot_braces,
                            coords,
                            plot_settings = plot_settings)

  list(
    set_y_coords(plot_dat, coords, y_lim),
    ggplot2::scale_x_continuous(
  #    limits = c(min(plot_dat[["plot_points"]]$year) -1, max(plot_dat[["plot_points"]]$year) + 1),
      breaks = unique(plot_dat[["plot_points"]]$year),
      expand = c(plot_settings$axis_x_background_width_x, 0) #ggplot2::expansion(c(plot_settings$axis_x_background_width_x, plot_settings$axis_x_background_width_x)) ## Increase, so the left and right side of the blue x-axis background gets bigger.
    )
  )
}


#' Calculate the plot-space between first brace and upper x-axis.
#'
#' @keywords internal
#' @noRd
#'
#' @param range_vec Numeric vector containing the minimum and maximum of the plotted values.
#' @param nudge_param_upper Numeric for increasing/decreasing the distance between highest plotted value and upper x-axis.
#' @param nudge_param_lower Numeric for increasing/decreasing the distance between lowest plotted value and first brace.
#'
#' @return Numeric vector containing the y-range between brace and upper x-axis.
#'
#' @examples calc_y_value_coords(c(0, 30))
calc_y_value_coords <- function(range_vec, nudge_param_upper = 0.1, nudge_param_lower = 0.075) { # nudge_param increases the distance between lowest/highest point and braces/x axis
  range_est <- diff(range_vec)
  coords <- c(
    ## Lower y limit
    plyr::round_any(range_vec[1] - (range_vec[1] * nudge_param_lower),
      accuracy = 10,
      f = floor
    ) - range_est * nudge_param_lower,
    ## upper y limit
    plyr::round_any(range_vec[2] + (range_vec[2] * nudge_param_upper),
      accuracy = 10,
      f = ceiling
    ) + range_est * nudge_param_upper
  )
  return(coords)
}


# Utils -------------------------------------------------------------------
set_y_coords <- function(plot_dat, range_est, y_lim) {
  ggplot2::scale_y_continuous(
    breaks = seq(
      from = round(min(range_est, na.rm = TRUE) - 10, -1),
      to = round(max(range_est, na.rm = TRUE), -1),
      by = 20
    ),
    limits = y_lim,
    expand = c(0, 0)
  )
}

set_cartesian_coords <- function(y_lim) {
  ggplot2::coord_cartesian(
    # clip = "off", # Clip Coordinate system. Necessary, so the brace can be drawn under the x-axis.
    ylim = y_lim
  )
}
