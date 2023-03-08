#' Plot the x-Axis.
#'
#' @inheritParams plot_points
#' @inheritParams plot_single_lineplot
#'
#' @return A blue stripe on top of ggplot2 plot which can be used as x-axis.
#' @export
#'
#' @examples #tbd
plot_axis <- function(data_plot_points, y_range){
  coords <- calc_coords(y_range)
  y_max <- coords[2]


  dat_coords <- data_plot_points[, c("year", "trend")]


  dat_coords$x_labels <- as.character(dat_coords$year)
  dat_coords$y_coords <- y_max - y_max * 0.005


res_list <- list(
  ggplot2::annotate(geom = "rect",
           xmin = -Inf,
           xmax =  Inf,
           ymin = y_max - y_max * 0.01,
           ymax = y_max,
           fill = "lightblue"),
  ggplot2::geom_text(dat_coords,
                     mapping = ggplot2::aes(x = .data$year,
                                            y = .data$y_coords,
                                            label = .data$x_labels,
                                            group = .data$trend)),
                     ggplot2::theme(axis.line= ggplot2::element_blank(),
                                    axis.text.x=ggplot2::element_blank(),
                                    axis.ticks.x = ggplot2::element_blank()
                                    )
                     )


return(res_list)
    }

## Problem: Split lineplot, facet_wrap scheint nicht damit klar zu kommen. Vielleicht den data_plot_points data-frame nutzen, da müsste die trend gruppierung drin sein.
## Zur Not die Jahreszahlen der trend-gruppen rausziehen und dann annotaten

## Oder: unique points nehmen, das sollte eigentlich auch passen, also doch Datengestützt (wahrscheinlich besser?)
