#' Plot an IQB barplot.
#'
#' @param data_plot_bar Data for the barplot. Has to be in the right format, ideally prepared with \link{prep_barplot}.
#'
#' @return ggplot object
#' @export
#'
#' @examples # tbd
#' @details text describing parameter inputs in more detail.
##' \itemize{
##'  \item{"data_plot_bar"}{Needs a specific data format.}
##' }
##'
plot_bar <- function(data_plot_bar) {
  ggplot2::ggplot(
    data = data_plot_bar,
    mapping = ggplot2::aes(
      x = .data$est_wholeGroup, y = .data$group,
      fill = .data$fill_wholeGroup,
      pattern = .data$sig_wholeGroup
    )
  ) +
    ggstats::geom_stripped_rows(
      odd = grDevices::rgb(219, 238, 244, maxColorValue = 255),
      even = "#00000000"
    ) +
    ggplot2::geom_vline(
      xintercept = seq(calc_plot_borders(data_plot_bar$est_wholeGroup)[1], calc_plot_borders(data_plot_bar$est_wholeGroup)[2], by = 10),
      linetype = "dashed", colour = "darkgrey"
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = "darkgrey"
    ) +
    ## This chunk only works together with the ggpattern::scale-specifications.
    ggpattern::geom_col_pattern(
      mapping = ggplot2::aes(
        x = .data$est_wholeGroup,
        y = .data$group,
        pattern_fill = .data$grouping_var
      ),
      position = ggplot2::position_dodge(width = 0.8),
      color = "black",
      linewidth = 0.6,
      pattern_colour = "white",
      pattern_angle = -45,
      pattern_density = 0.4, # Streifenbreite
      pattern_spacing = 0.01, # Abstand
      pattern_key_scale_factor = 0.6,
      width = 0.4
    ) +
    ggplot2::scale_x_continuous(breaks = seq(calc_plot_borders(data_plot_bar$est_wholeGroup)[1], calc_plot_borders(data_plot_bar$est_wholeGroup)[2], by = 10)) +
    ggpattern::scale_pattern_manual(values = c(
      "TRUE" = "none",
      "FALSE" = "stripe"
    )) +
    ggpattern::scale_pattern_fill_manual(values = c(
      "ohneAdj" = grDevices::rgb(147, 205, 221, maxColorValue = 255),
      "mitAdj" = grDevices::rgb(33, 89, 104, maxColorValue = 255)
    )) +
    ggplot2::scale_fill_manual(
      values = c(
        "ohneAdj_TRUE" = grDevices::rgb(147, 205, 221, maxColorValue = 255),
        "mitAdj_TRUE" = grDevices::rgb(33, 89, 104, maxColorValue = 255),
        "ohneAdj_FALSE" = "white",
        "mitAdj_FALSE" = "white"
      )
    ) +
    theme_table_bar() +
    NULL
}

#
#
#
# data_plot_bar <- plot_data[["plot_bar"]][plot_data[["plot_bar"]]$group != "wholeGroup",]
#
# ggplot2::ggplot(
#   data = data_plot_bar,
#   mapping = ggplot2::aes(
#     x = .data$est_wholeGroup, y = .data$group,
#     fill = .data$fill_wholeGroup,
#     pattern = .data$sig_wholeGroup
#   )
# ) +
#   ggstats::geom_stripped_rows(
#     odd = grDevices::rgb(219, 238, 244, maxColorValue = 255),
#     even = "#00000000"
#   ) +
#   ggplot2::geom_vline(
#     xintercept = seq(calc_plot_borders(data_plot_bar$est_wholeGroup)[1], calc_plot_borders(data_plot_bar$est_wholeGroup)[2], by = 10),
#     linetype = "dashed", colour = "darkgrey"
#   ) +
#   ggplot2::geom_vline(
#     xintercept = 0,
#     colour = "darkgrey"
#   ) +
#   ## This chunk only works together with the ggpattern::scale-specifications.
#   ggpattern::geom_col_pattern(
#     mapping = ggplot2::aes(
#       x = .data$est_wholeGroup,
#       y = .data$group,
#       pattern_fill = .data$grouping_var
#     ),
#     position = ggplot2::position_dodge(width = 0.8),
#     color = "black",
#     linewidth = 0.6,
#     pattern_colour = "white",
#     pattern_angle = -45,
#     pattern_density = 0.4, # Streifenbreite
#     pattern_spacing = 0.01, # Abstand
#     pattern_key_scale_factor = 0.6,
#     width = 0.4
#   ) +
#   ggplot2::scale_x_continuous(breaks = seq(calc_plot_borders(data_plot_bar$est_wholeGroup)[1], calc_plot_borders(data_plot_bar$est_wholeGroup)[2], by = 10)) +
#   ggpattern::scale_pattern_manual(values = c(
#     "TRUE" = "none",
#     "FALSE" = "stripe"
#   )) +
#   ggpattern::scale_pattern_fill_manual(values = c(
#     "ohneAdj" = grDevices::rgb(147, 205, 221, maxColorValue = 255),
#     "mitAdj" = grDevices::rgb(33, 89, 104, maxColorValue = 255)
#   )) +
#   ggplot2::scale_fill_manual(
#     values = c(
#       "ohneAdj_TRUE" = grDevices::rgb(147, 205, 221, maxColorValue = 255),
#       "mitAdj_TRUE" = grDevices::rgb(33, 89, 104, maxColorValue = 255),
#       "ohneAdj_FALSE" = "white",
#       "mitAdj_FALSE" = "white"
#     )
#   ) +
#   theme_table_bar() +
#   NULL
#
#
