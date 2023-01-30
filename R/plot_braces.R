#' Plot braces below plot.
#'
#' @param data_trend_point Prepared Trend data.
#'
#' @return ggplot2 object.
#' @export
#'
#' @examples ##
plot_braces <- function(data_trend_point){


  # Get range for y-axis
  range_est <- range(c(data_trend_point$est_point_start, data_trend_point$est_point_end))

  coords <- calc_coords(range_est)

  brace_coords <- calc_brace_coords(data_trend_point, coords)

  c(
    # Clip Coordinate system. Necessary, so the brace can be drawn outside of the plot
    ggplot2::coord_cartesian(clip = "off", ylim = c(plyr::round_any(coords[1], accuracy = 10), plyr::round_any(coords[2], accuracy = 10))),

    ## Loop to draw a brace for every year_start
    lapply(unique(data_trend_point$year_start), function(x) {

      coordinates <- brace_coords[brace_coords$year_start == x, ]
      coordiantes <- unique(brace_coords[,c("year_start", "year_end", "brace_upper_y", "brace_lower_y")])

      ggbrace::geom_brace(
        mapping = ggplot2::aes(
          x = c(coordinates$year_start, coordinates$year_end),
          y = c(coordinates$brace_upper_y, coordinates$brace_lower_y)
        ),
        mid = ifelse(coordinates$year_start == min(brace_coords$year_start), 0.25, 0.5),
        inherit.data = F,
        rotate = 180,
        size = 0.8,
        npoints = 200
      )
    })
    )
}





## Utils

## Calc coordinate system borders. Programmatisch lösen.
calc_coords <- function(range_vec){
  coords <- c(plyr::round_any(range_vec[1] - 40, accuracy = 10, f = floor),
              plyr::round_any(range_vec[2] + 20, accuracy = 10, f = ceiling)
              )
  return(coords)
}


## Calc the coordinates for drawing the braces.
calc_brace_coords <- function(data, coords){
  # Calculate brace coordinates
  data$brace_upper_y <- ifelse(data$year_start == min(data$year_start), coords[1], coords[1] - 72) ## Programmatisch lösen
  data$brace_lower_y <- ifelse(data$year_start == min(data$year_start), coords[1] - 70, coords[1] - 102)
  data$label_pos_y <- ifelse(data$grouping_var == 1, range_est[1] - 113, range_est[1] - 133)
  data$label_pos_x <- data$year_start + (data$year_end - max(data$year_start)) / 2
  # data$estTrend_within_label <- ifelse(data$sigTrend_within == "bold", paste0("**", round(data$estTrend_within, 0), "**"), round(data$estTrend_within, 0))
  #   data$sigTrend_vsGermany <- ifelse(data$pTrend_vsGermany < 0.05, "<sup>a</sup>", "")

  return(data)
}


plot_brace_label <- function(brace_coords, BL){
      ggtext::geom_richtext(
        data = brace_coords[brace_coords$TR_BUNDESLAND == BL &
                              !(brace_coords$year_start == 2011 &
                                       brace_coords$year_end == 2016), ],
        mapping = ggplot2::aes(
          x = .data$year_start + (.data$year_end - max(.data$year_start)) / 2,
          y = label_pos_y,
          label = paste0(.data$est_trend_within,
                         #sigTrend_vsGermany,
                         " (", round(.data$se_trend_within, 1), ")")
        ),
        size = 3,
        label.padding = grid::unit(rep(0, 4), "pt"),
        fill = NA,
        label.color = NA
      )
}



