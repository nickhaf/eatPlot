set_y_scale <- function(y_axis_min, y_axis_max, y_total_min, y_total_max, tick_distance, margin_top, margin_left, margin_bottom, margin_right) {
  list(
    ggplot2::scale_y_continuous(
      breaks = seq_over(
        from = y_axis_min,
        to = y_axis_max,
        by = tick_distance
      ),
      limits = c(y_total_min, y_total_max),
      expand = c(0, 0)
    ),
    # ggplot2::coord_cartesian(ylim = y_axis_lims, clip = "off"),
    ggplot2::theme(plot.margin = ggplot2::unit(c(
      margin_top,
      margin_right,
      margin_bottom,
      margin_left
    ), "npc"))
  )
}


#' Calculate the plot-space between first brace and top of the plot. Here, the empty space bettween the highest/lowest point and the x-axis-header or the brace label is set.
#'
#' @keywords internal
#' @noRd
#'
#' @param y_value_range Numeric vector containing the minimum and maximum of the plotted values.
#' @param nudge_param_upper Numeric for increasing/decreasing the distance between highest plotted value and upper x-axis.
#' @param nudge_param_lower Numeric for increasing/decreasing the distance between lowest plotted value and first brace.
#'
#' @return Numeric vector containing the y-range between brace and upper x-axis.
#'
#' @examples calc_y_value_coords(c(0, 30))
calc_y_value_space <- function(y_value_range,
                                nudge_param_upper = 0.2,
                                nudge_param_lower = 0.2) { # nudge_param increases the distance between lowest/highest point and braces/x axis
  y_range_diff <- diff(y_value_range)

  value_space <- c(
    ## Lower y limit
    y_value_range[1] - y_range_diff * nudge_param_lower,
    # ## upper y limit
    y_value_range[2] + y_range_diff * nudge_param_upper
  )

  return(value_space)
}

# Utils -------------------------------------------------------------------

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
#' @examples # seq_over(10, 40, 20)
seq_over <- function(from, to, by) {
  res_vec <- from
  i <- from
  while (i < to) {
    i <- i + by
    res_vec[length(res_vec) + 1] <- i
  }
  return(res_vec)
}
