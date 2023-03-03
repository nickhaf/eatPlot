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
                                 point_values = "est_point",
                                 point_sig = "sig_point",
                                 line_values = c("est_point_start", "est_point_end"),
                                 line_sig = "sig_trend_comp_within",
                                 label_est = "est_trend_no_comp",
                                 label_se = "se_trend_no_comp",
                                 label_sig_high = "sig_trend_comp_whole",
                                 label_sig_bold = "sig_trend_no_comp") {
  list(
    settings_lineplot(plot_data[["plot_lines"]]),
    plot_braces(plot_data[["plot_braces"]],
      y_range = y_range,
      label_est,
      label_se,
      label_sig_high,
      label_sig_bold
    ),
    plot_background_lines(plot_data[["plot_background_lines"]],
      line_values = line_values
    ),
    plot_points(plot_data[["plot_points"]],
      point_values = point_values,
      point_sig = point_sig
    ),
    plot_lines(plot_data[["plot_lines"]],
      line_values = line_values,
      line_sig = line_sig
    ),
    ggplot2::labs(title = paste0(unique(plot_data[["plot_braces"]]$state_var), "\n", " ")),
    ggplot2::scale_y_continuous(breaks = seq(from = round(y_range[1] - 10, -1), to = round(y_range[2], -1), by = 20))
  )
}


plot_split_lineplot <- function(left_plot_data,
                                right_plot_data,
                                y_range,
                                point_values = "est_point",
                                point_sig = "sig_point",
                                line_values = c("est_point_start", "est_point_end"),
                                line_sig = "sig_trend_comp_within",
                                label_est = "est_trend_no_comp",
                                label_se = "se_trend_no_comp",
                                label_sig_high = "sig_trend_comp_whole",
                                label_sig_bold = "sig_trend_no_comp") {
  res <- patchwork::wrap_plots(
    ggplot2::ggplot() +
      plot_single_lineplot(
        plot_data = left_plot_data,
        y_range = y_range,
        point_values = point_values,
        point_sig = point_sig,
        line_values = line_values,
        line_sig = line_sig,
        label_est = label_est,
        label_se = label_se,
        label_sig_high = label_sig_high,
        label_sig_bold = label_sig_bold
      ) +
      ggplot2::labs(title = ggplot2::element_blank()),
    ggplot2::ggplot() +
      plot_single_lineplot(
        plot_data = right_plot_data,
        y_range = y_range,
        point_values = point_values,
        point_sig = point_sig,
        line_values = line_values,
        line_sig = line_sig,
        label_est = label_est,
        label_se = label_se,
        label_sig_high = label_sig_high,
        label_sig_bold = label_sig_bold
      ) +
      ggplot2::labs(title = ggplot2::element_blank())
  )

  res_2 <- res &
    patchwork::plot_annotation(title = unique(left_plot_data[["plot_braces"]]$state_var)) &
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  return(res_2)
}