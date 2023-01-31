#' Plot Points.
#'
#' @param data_point_estimates Data frame with the point estimates for every year. Prepared by ...
#'
#' @return ggplot2 object
#' @export
#'
#' @examples #tbd
plot_points <- function(data_point_estimates){
  list(
    ggplot2::geom_point(data = data_point_estimates,
                        ggplot2::aes(x = .data$time,
                                     y = .data$est_point,
                                     colour = .data$grouping_var,
                                     #group = .data[[grouping_var]],
                                     shape = .data$sig_point),
                        size = 2.3),
    ggplot2::geom_text(data = data_point_estimates,
                       ggplot2::aes(x = .data$time,
                                    y = .data$est_point,
                                    colour = .data$grouping_var,
                                    label = round(.data$est_point, 0)),
                       nudge_y = c(-2, 2),
                       size = 3
                       )

  )
}
