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
plot_bar <- function(data_plot_bar, x_value = "est_wholeGroup", y_value = "TR_BUNDESLAND",
                     bar_pattern = "sig_wholeGroup", bar_fill = "fill_wholeGroup", bar_pattern_fill = "grouping_var",
                     bar_pattern_setting = sig_pattern, bar_fill_setting = adj_fill, bar_pattern_fill_setting = adj_pattern_fill){

  scale_breaks <- seq(calc_plot_borders(data_plot_bar[[x_value]])[1], calc_plot_borders(data_plot_bar[[x_value]])[2], by = 10)

  ggplot2::ggplot(
    data = data_plot_bar,
    mapping = ggplot2::aes(
      x = .data[[x_value]],
      y = .data[[y_value]],
      fill = .data[[bar_fill]],
      pattern = .data[[bar_pattern]]
    )
  ) +
    ggstats::geom_stripped_rows(
      odd = grDevices::rgb(219, 238, 244, maxColorValue = 255),
      even = "#00000000") +
    ggplot2::geom_vline(
      xintercept = scale_breaks,
      linetype = "dashed", colour = "darkgrey"
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = "darkgrey"
    ) +
    ## This chunk only works together with the ggpattern::scale-specifications.
    ggpattern::geom_col_pattern(
      mapping = ggplot2::aes(pattern_fill = .data[[bar_pattern_fill]]),
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
    ggplot2::scale_x_continuous(breaks = scale_breaks) +
    ggpattern::scale_pattern_manual(values = bar_pattern_setting) +
    ggpattern::scale_pattern_fill_manual(values = bar_pattern_fill_setting) +
    ggplot2::scale_fill_manual(values = bar_fill_setting) +
    theme_table_bar() +
    NULL
}
