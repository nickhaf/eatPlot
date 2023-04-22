#' Plot a single lineplot. Multiple of this lineplots are combined as tiles to the output of [plot_lineplot()].
#'
#' @inheritParams plot_lineplot
#' @inheritParams plotsettings_lineplot
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
                                 line_sig = "sig_Trend_CompWithin",
                                 label_est = "est_Trend_noComp",
                                 label_se = "se_Trend_noComp",
                                 label_sig_high = "sig_Trend_CompWhole",
                                 label_sig_bold = "sig_Trend_noComp",
                                 background_lines = TRUE,
                                 plot_settings = plotsettings_lineplot()) {
  # Assemble a single lineplot (one "tile" in the whole lineplot).
  list(
theme_line(plot_settings),
set_scales(plot_settings),
    plot_braces(
      plot_data[["plot_braces"]],
      y_range = y_range,
      label_est = label_est,
      label_se = label_se,
      label_sig_high = label_sig_high,
      label_sig_bold = label_sig_bold,
      plot_settings = plot_settings
    ),
if(background_lines == TRUE){
    plot_background_lines(
      data_plot_background_lines = plot_data[["plot_background_lines"]],
      line_values = line_values
    )
  },
if(!is.null(line_values)){
    plot_lines(
      data_plot_lines = plot_data[["plot_lines"]],
      line_values = line_values,
      line_sig = line_sig,
      plot_settings = plot_settings
    )},
if(!is.null(point_values)){
    plot_points(
      data_plot_points = plot_data[["plot_points"]],
      point_values = point_values,
      point_sig = point_sig,
      y_range = y_range,
      plot_settings = plot_settings
    )},
    plot_x_axis(plot_data[["plot_points"]],
      y_range = y_range,
      plot_settings = plot_settings
    ),
    if (plot_settings$split_plot == TRUE) {
      ggplot2::facet_wrap(~years_Trend, scales = "free_x")
    }
  )
}
