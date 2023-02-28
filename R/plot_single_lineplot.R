#'Plot a single lineplot. Multiple of this lineplots are combined as tiles to the output of [plot_lineplot()].
#'
#' @inheritParams plot_lineplot
#'
#' @return ggplot2 Object.
#' @export
#'
#' @examples #tbd
plot_single_lineplot <- function(plot_data,
                          point_values = "est_point",
                          point_sig = "sig_point",
                          line_values = c("est_point_start", "est_point_end"),
                          line_sig = "sig_trend_comp_within",
                          label_est = "est_trend_no_comp",
                          label_se = "se_trend_no_comp",
                          label_sig_high = "sig_trend_comp_whole",
                          label_sig_bold = "sig_trend_no_comp"){

list(
  settings_lineplot(plot_data[["plot_lines"]]),
  plot_braces(plot_data[["plot_braces"]],
              BL = unique(plot_data[["plot_braces"]]$state_var),
              label_est,
              label_se,
              label_sig_high,
              label_sig_bold),
  plot_background_lines(plot_data[["plot_background_lines"]],
                        line_values = line_values),
  plot_points(plot_data[["plot_points"]],
              point_values = point_values,
              point_sig = point_sig),
  plot_lines(plot_data[["plot_lines"]],
             line_values = line_values,
             line_sig = line_sig),
  ggplot2::labs(title = paste0(unique(plot_data[["plot_braces"]]$state_var), "\n", " "))
)

 }
