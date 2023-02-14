#' Plot an IQB barplot.
#'
#' @param no_trend_list Input is a list prepared by [prep_no_trend()]. You can also use the according data.frame named `plot_bar` from this list.
#' @param x_value Character string of the column name containing the estimates that should be plotted on the x-axis. Defaults to `"est_wholeGroup"`, which are the estimates for the comparison of a state against the wholeGroup (Germany).
#' @param y_value Character string of the column name containing the labels that should be plotted on the y-axis. Defaults to `"state_var"`, so the states (Bundesländer) are depicted on the y-axis.
#' @param bar_pattern Character string of the column containing the grouping for the pattern on the bar. Defaults to `sig_wholeGroup`, so the significances of the state vs. wholeGroup (Germany) comparison are represented in the pattern of the bars.
#' @param bar_fill Character string of the column containing the grouping for the filling of the bar. Defaults to `fill_wholeGroup`, so the significances of the state vs. wholeGroup (Germany) comparison, as well as the groups found in "data$grouping_var" are represented in the filling colours of the bars.
#' @param bar_pattern_fill Character string of the column containing the grouping for the filling of the pattern on the bar. Defaults to `fill_wholeGroup`, so the groups found in "data$grouping_var" are represented in the colours of the bar pattern.
#' @param bar_pattern_setting Named vector with the pattern types. Names of the vector must be found in the column specified in `bar_pattern`. Defaults to ...
#' @param bar_fill_setting Named vector with the bar filling colours. Names of the vector must be found in the column specified in `bar_fill`. Defaults to ...
#' @param bar_pattern_fill_setting Named vector with the filling colours for the bar pattern. Names of the vector must be found in the column specified in `bar_pattern_fill`. Defaults to ...
#' @param sig_pattern Character string indicating whether significance levels should be visualized by pattern fill ("pattern") or line type ("frame"). Defaults to "pattern".
#'
#' @return Returns a [ggplot2] barplot.
#' @export
#'
#' @examples #tbd
plot_bar <- function(no_trend_list,
                     x_value = "est_wholeGroup",
                     y_value = "state_var",
                     bar_pattern = "sig_wholeGroup",
                     bar_fill = "fill_wholeGroup",
                     bar_pattern_fill = "grouping_var",
                     bar_pattern_setting = sig_pattern,
                     bar_fill_setting = adj_fill,
                     bar_pattern_fill_setting = adj_pattern_fill,
                     sig_pattern = "pattern") {

  if (inherits(no_trend_list, "list")) {
    data_plot_bar <- no_trend_list[["plot_bar"]]
  } else {
    data_plot_bar <- no_trend_list
  }

  scale_breaks <- seq(
    calc_plot_borders(data_plot_bar[[x_value]])[1],
    calc_plot_borders(data_plot_bar[[x_value]])[2],
    by = 10)

  if (sig_pattern == "pattern") {
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
  } else if (sig_pattern == "frame") {
    ggplot2::ggplot(
      data = data_plot_bar,
      mapping = ggplot2::aes(
        x = .data[[x_value]],
        y = .data[[y_value]],
        fill = .data[[bar_fill]],
        linetype = .data[[bar_pattern]]
      )
    ) +
      ggstats::geom_stripped_rows(
        odd = grDevices::rgb(219, 238, 244, maxColorValue = 255),
        even = "#00000000") +
      ggplot2::geom_col(
        position = ggplot2::position_dodge(width = 0.8),
        color = "black",
        linewidth = 0.6,
        width = 0.4
      ) +
      ggplot2::geom_vline(
        xintercept = scale_breaks,
        linetype = "dashed", colour = "darkgrey"
      ) +
      ggplot2::geom_vline(
        xintercept = 0,
        colour = "darkgrey"
      ) +
      ggplot2::scale_x_continuous(breaks = scale_breaks) +
      ggpattern::scale_pattern_manual(values = bar_pattern_setting) +
      ggpattern::scale_pattern_fill_manual(values = bar_pattern_fill_setting) +
      ggplot2::scale_fill_manual(values = bar_fill_setting) +
      theme_table_bar() +
      NULL
  } else {
    message("`sig_pattern` must be either \"frame\" or \"pattern\"")
  }
}
