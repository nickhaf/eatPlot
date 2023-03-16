#' Set the x- and y-coordinates for a plot.
#'
#' @inheritParams plot_lineplot
#'
#' @return [ggplot2] object.
#' @export
#'
#' @examples # tbd
set_plot_coords <- function(plot_data) {
  min_year <- min(plot_data[["plot_points"]]$year, na.rm = TRUE)
  max_year <- max(plot_data[["plot_points"]]$year, na.rm = TRUE)

  list(
    set_y_coords(plot_data),
    ggplot2::scale_x_continuous(
      position = "top",
      breaks = unique(plot_data[["plot_points"]]$year),
      expand = c(0.15, 0) ## Increase, so the left and right side of the blue x-axis background gets bigger.
    )
  )
}


## Calc coordinate system borders.
calc_coords <- function(range_vec) {
  range_est <- diff(range_vec)
  coords <- c(
    plyr::round_any(range_vec[1] - range_vec[1] * 0.1,
                    accuracy = 10, f = floor) - range_est * 0.1,
    plyr::round_any(range_vec[2] + range_vec[2] * 0.04,
                    accuracy = 10, f = ceiling) + range_est * 0.1
  )
  return(coords)
}


# Utils -------------------------------------------------------------------
set_y_coords <- function(plot_data){
  ggplot2::scale_y_continuous(
    breaks = seq(
      from = round(min(plot_data[["plot_points"]]$est_point, na.rm = TRUE) - 10, -1),
      to = round(max(plot_data[["plot_points"]]$est_point, na.rm = TRUE), -1),
      by = 20
    ),
    expand = c(0, 0)
  )
}

set_cartesian_coords <- function(coords){
ggplot2::coord_cartesian(
  clip = "off", # Clip Coordinate system. Necessary, so the brace can be drawn under the x-axis.
  ylim = coords
)
}