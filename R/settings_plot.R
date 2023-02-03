#' Set theme, colours, pointshapes and linetypes for lineplot.
#'
#' @param data_trend_point Data in Long format
#'
#' @return ggplot2 plot settings
#' @export
#'
#' @examples #tbd
settings_lineplot <- function(data_trend_point){
  list(
    theme_line(),
    ggplot2::scale_x_continuous(position = "top",
                                breaks = unique(c(data_trend_point$year_start, data_trend_point$year_end))
    ),
    sig_linetypes(),
    sig_pointshapes(),
    grouping_colours()
  )
}


# settings_coord_line <- function(){
#   list(
#     ggplot2::coord_cartesian(ylim = c(100, ggplot2::layer_scales(p1)$y$get_limits()[2] + 20), clip = "off")
#     )
#   )
#
# }
#
#
# settings_title_line <- function(title){
#   ggplot2::annotation_custom(
#     grob = grid::rectGrob(gp = grid::gpar(fill = grDevices::rgb(128, 196, 214,
#                                                                 maxColorValue = 255),
#                                           alpha = .4,
#                                           col = 0)),
#     xmin = -Inf, xmax = Inf,
#     ymin = ggplot2::layer_scales(p1)$y$get_limits()[2],
#     ymax = 140),
#   ggplot2::labs(title = paste0(title, "\n", " "))
# }

