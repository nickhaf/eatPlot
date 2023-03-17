#' Plot a single lineplot. Multiple of this lineplots are combined as tiles to the output of [plot_lineplot()].
#'
#' @inheritParams plot_lineplot
#' @param y_range Numeric vector with two elements, indicating the min and max for the y axis.
#'
#' @return ggplot2 Object.
#' @export
#'
#' @examples # tbd
plot_single_lineplot <- function(plot_data,
                                 y_range = NULL,
                                 split_plot,
                                 point_values = "est_point",
                                 point_sig = "sig_point",
                                 line_values = c("est_point_start", "est_point_end"),
                                 line_sig = "sig_trend_comp_within",
                                 label_est = "est_trend_no_comp",
                                 label_se = "se_trend_no_comp",
                                 label_sig_high = "sig_trend_comp_whole",
                                 label_sig_bold = "sig_trend_no_comp") {
  # Assemble a single lineplot (one "tile" in the whole lineplot).
  list(
    settings_lineplot(),
    plot_braces(
      plot_data[["plot_braces"]],
      split_plot = split_plot,
      y_range = y_range,
      label_est = label_est,
      label_se = label_se,
      label_sig_high = label_sig_high,
      label_sig_bold = label_sig_bold
    ),
    plot_background_lines(
      data_plot_background_lines = plot_data[["plot_background_lines"]],
      line_values = line_values
    ),
    plot_lines(
      data_plot_lines = plot_data[["plot_lines"]],
      line_values = line_values,
      line_sig = line_sig
    ),
    plot_points(
      data_plot_points = plot_data[["plot_points"]],
      y_range = y_range,
      point_values = point_values,
      point_sig = point_sig
    ),
    plot_x_axis(plot_data[["plot_points"]], y_range = y_range),
    if (split_plot == TRUE) {
      ggplot2::facet_wrap(~trend, scales = "free_x")
    },
    ggplot2::labs(title = unique(plot_data[["plot_braces"]]$state_var))

  )
}
