#' Plot the x-Axis.
#'
#' @inheritParams plot_points
#' @inheritParams plot_single_lineplot
#'
#' @return A blue stripe on top of ggplot2 plot which can be used as x-axis.
#' @export
#'
#' @examples #tbd
plot_x_axis <- function(data_plot_points, y_range){
  coords <- calc_coords(y_range)
  y_max <- coords[2]


  dat_coords <- data_plot_points[, c("year", "trend")]


  dat_coords$x_labels <- as.character(dat_coords$year)
  dat_coords$y_coords <- y_max - y_max * 0.0175 ## Increase, so the x-axis labels is printed lower.


res_list <- list(
  ggplot2::annotate(geom = "rect",
           xmin = -Inf,
           xmax =  Inf,
           ymin = y_max - y_max * 0.03, # Increase, so the x-axis background reaches lower.
           ymax = y_max,
           fill = "lightblue"),
  ggplot2::geom_text(dat_coords,
                     mapping = ggplot2::aes(x = .data$year,
                                            y = .data$y_coords,
                                            label = .data$x_labels,
                                            group = .data$trend),
                     size = 3),
                     ggplot2::theme(axis.line= ggplot2::element_blank(),
                                    axis.text.x=ggplot2::element_blank(),
                                    axis.ticks.x = ggplot2::element_blank()
                                    )
                     )

return(res_list)
    }