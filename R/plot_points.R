#' Plot Points.
#'
#' @param data_plot_points Data frame with the point estimates for every year. Prepared by ...
#'
#' @return ggplot2 object
#' @export
#'
#' @examples #tbd
plot_points <- function(data_plot_points){

  nudge_y_vec <- calc_y_nudge(data_plot_points$est_point, n_groups = length(unique(data_plot_points$grouping_var)))

  list(
    ggplot2::geom_point(data = data_plot_points,
                        ggplot2::aes(x = .data$year,
                                     y = .data$est_point,
                                     colour = .data$grouping_var,
                                     #group = .data[[grouping_var]],
                                     shape = .data$sig_point),
                        size = 2.3),
    ggplot2::geom_text(data = data_plot_points,
                       ggplot2::aes(x = .data$year,
                                    y = .data$est_point,
                                    colour = .data$grouping_var,
                                    label = round(.data$est_point, 0)),
                       nudge_y = nudge_y_vec,
                       size = 3
                       )

  )
}



calc_y_nudge <- function(vec, n_groups){
range_est <- range(vec, na.rm = TRUE)
nudge_y_val <- (range_est[2]-range_est[1]) * 0.025
nudge_y_vec <- rep(nudge_y_val, n_groups)
nudge_y_vec[1] <- nudge_y_vec[1] * -1
return(nudge_y_vec)
}
