#' Plot braces below plot.
#'
#' @param data_trend_point Prepared Trend data.
#'
#' @return ggplot2 object.
#' @export
#'
#' @examples ##
plot_braces <- function(data_trend_point, BL) {
  # Get range for y-axis
  range_est <- range(c(data_trend_point$est_point_start, data_trend_point$est_point_end))

  # Here the coordinates for the braces and brace labels are calculated.
  # Change in the functions for fine tuning.
  coords <- calc_coords(range_est)
  brace_coords <- calc_brace_coords(data_trend_point, coords)

  # data$estTrend_within_label <- ifelse(data$sigTrend_within == "bold", paste0("**", round(data$estTrend_within, 0), "**"), round(data$estTrend_within, 0))
  # data$sigTrend_vsGermany <- ifelse(data$pTrend_vsGermany < 0.05, "<sup>a</sup>", "")

  c(
    ## Loop to draw a brace for every year_start
    lapply(unique(data_trend_point$year_start), function(x) {
      coordinates <- brace_coords[brace_coords$year_start == x, ]
      coordiantes <- unique(brace_coords[, c("year_start", "year_end", "brace_upper_y", "brace_lower_y")])

      ggbrace::geom_brace(
        mapping = ggplot2::aes(
          x = c(coordinates$year_start, coordinates$year_end),
          y = c(coordinates$brace_upper_y, coordinates$brace_lower_y)
        ),
        mid = ifelse(coordinates$year_start == min(brace_coords$year_start), 0.25, 0.5),
        inherit.data = F,
        rotate = 180,
        linewidth = 0.8,
        npoints = 200
      )
    }),
    plot_brace_label(brace_coords, BL),
    # Clip Coordinate system. Necessary, so the brace can be drawn outside of the plot
    ggplot2::coord_cartesian(
      clip = "off",
      ylim = coords,
      expand = FALSE)
  )
}





## Utils

## Calc coordinate system borders.
calc_coords <- function(range_vec) {
  coords <- c(
    plyr::round_any(range_vec[1] - range_vec[1] * 0.1,
                    accuracy = 10, f = floor),
    plyr::round_any(range_vec[2] + range_vec[2] * 0.04,
                    accuracy = 10, f = ceiling)
  )
  return(coords)
}


## Calc the coordinates for drawing the braces.
calc_brace_coords <- function(data, coords) {
  # Calculate brace coordinates ---------------------------------------------

  data$brace_upper_y <- ifelse(data$year_start == min(data$year_start),
    coords[1],
    coords[1] - coords[1] * 0.10
  )
  data$brace_lower_y <- ifelse(data$year_start == min(data$year_start),
    coords[1] - coords[1] * 0.10,
    coords[1] - coords[1] * 0.15
  )

  data$label_pos_y <- ifelse(data$grouping_var == 1,
    coords[1] - coords[1] * 0.15,
    coords[1] - coords[1] * 0.17
  )
  data$label_pos_x <- ifelse(data$year_start == min(data$year_start),
    calc_pos_label_x(data$year_start, data$year_end, 0.25),
    calc_pos_label_x(data$year_start, data$year_end, 0.5)
  )

  return(data)
}


plot_brace_label <- function(brace_coords, BL) {
  ggtext::geom_richtext(
    data = brace_coords[brace_coords$TR_BUNDESLAND == BL &
      !(brace_coords$year_start == 2011 &
        brace_coords$year_end == 2016), ],
    mapping = ggplot2::aes(
      x = .data$label_pos_x,
      y = .data$label_pos_y,
      label = paste0(
        .data$est_trend_within,
        # sigTrend_vsGermany,
        " (", round(.data$se_trend_within, 1), ")"
      )
    ),
    size = 3,
    label.padding = grid::unit(rep(0, 4), "pt"),
    fill = NA,
    label.color = NA
  )
}

calc_pos_label_x <- function(year_start, year_end, brace_indent_pos) {
  year_start + (year_end - year_start) * brace_indent_pos
}
