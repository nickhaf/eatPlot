#' Plot the x-Axis.
#'
#' @inheritParams plot_points
#' @inheritParams plot_single_lineplot
#' @param nudge_x Numeric indicating how much the labels should be nudged to the center of the plot. Defaults to `0.05`.
#'
#' @return A blue stripe on top of ggplot2 plot which can be used as x-axis.
#' @export
#'
#' @examples #tbd
plot_x_axis <- function(data_plot_points, y_range, split_plot = FALSE, nudge_x = 0.05){
  coords <- calc_coords(y_range)
  y_max <- coords[2]

  dat_coords <- data_plot_points[, c("year", "trend")]

  dat_coords$x_labels <- as.character(dat_coords$year)
  dat_coords$y_coords <- y_max - y_max * 0.02 ## Increase, so the x-axis labels is printed lower.


# calc x-axis  ------------------------------------------------------------
## x-axis labels should be centered a bit more. So the larger year in the smaller trend and the smaller year in the larger Trend need to go into the center more:

  if(split_plot == TRUE){
  dat_coords <- calc_x_nudge(dat_coords)
  }else{
  dat_coords$x_coords <- dat_coords$year
}

res_list <- list(
  ggplot2::annotate(geom = "rect",
           xmin = -Inf,
           xmax =  Inf,
           ymin = y_max - y_max * 0.04, # Increase, so the x-axis background reaches lower.
           ymax = y_max,
           fill = "lightblue"),
  ggplot2::geom_text(dat_coords,
                     mapping = ggplot2::aes(x = .data$x_coords,
                                            y = .data$y_coords,
                                            label = .data$x_labels,
                                            group = .data$trend),
                     size = 2),
                     ggplot2::theme(axis.line= ggplot2::element_blank(),
                                    axis.text.x=ggplot2::element_blank(),
                                    axis.ticks.x = ggplot2::element_blank()
                                    )
                     )
return(res_list)
}

