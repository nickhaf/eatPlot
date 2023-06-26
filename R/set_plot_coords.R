#' Set the x- and y-coordinates for a plot.
#'
#' @inheritParams plot_lineplot
#'
#' @return [ggplot2] object.
#' @export
#'
#' @examples # tbd
set_plot_coords <- function(plot_dat, point_values, plot_settings = plotsettings_lineplot()) {
  plot_dat$plot_points <- fill_column(plot_dat$plot_points, point_values, filling = NA)


  min_year <- min(plot_dat[["plot_points"]]$year, na.rm = TRUE)
  max_year <- max(plot_dat[["plot_points"]]$year, na.rm = TRUE)

  list(
    set_y_coords(plot_dat, point_values),
    ggplot2::scale_x_continuous(
      # position = "top",
      breaks = unique(plot_dat[["plot_points"]]$year),
      expand = c(plot_settings$axis_x_background_width_x, 0) ## Increase, so the left and right side of the blue x-axis background gets bigger.
    )
  )
}


## Calc coordinate system borders.
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
## Needs to calculate ALL of the yrange, so from the top of the blue border to the last brace label.
set_y_coords <- function(plot_dat, point_values) {
#
#   coords <- calc_y_value_coords() #calc_y_value_coords calculates the difference between the min and max plotted point.
#   coords_total <- calc_brace_coords <- function(dat, coords, output_format = c("wide", "long"), plot_settings = plotsettings_lineplot()) {
#
#
#
#
#
#

  ggplot2::scale_y_continuous(
    breaks = seq(
      from = round(min(plot_dat[["plot_points"]][, point_values], na.rm = TRUE) - 10, -1),
      to = round(max(plot_dat[["plot_points"]][, point_values], na.rm = TRUE), -1),
      by = 20
    ),
    expand = c(0, 0)
  )
}

set_cartesian_coords <- function(coords) {
  ggplot2::coord_cartesian(
    clip = "off", # Clip Coordinate system. Necessary, so the brace can be drawn under the x-axis.
    ylim = coords
  )
}
