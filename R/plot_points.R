#' Plot Points.
#'
#' @param grouping_var Indicating which groups are getting distinct colours.
#'
#' @return ggplot2 object
#' @export
#'
#' @examples #tbd
plot_points <- function(grouping_var){
  list(
    ggplot2::geom_point(
                        ggplot2::aes(x = .data$time,
                                     y = .data$est,
                                     colour = .data[[grouping_var]],
                                     #group = .data[[grouping_var]],
                                     shape = .data$sig),
                        size = 2.3),
    ggplot2::geom_text(
                       ggplot2::aes(x = .data$time,
                                    y = .data$est,
                                    colour = .data[[grouping_var]],
                                    label = round(.data$est, 0)),
                       nudge_y = c(-0.2, 0.2),
                       size = 3
                       ),
    pointshape_iqb
  )
}



