#' Adds a layer with trend lines to a ggplot.
#'
#' @details Wrapper for [ggplot2::geom_segment()].
#' @param data_plot_lines data.
#' @inheritParams plot_lineplot
#'
#' @return ggplot2 object
#' @export
#'
#' @examples #tbd
plot_lines <- function(data_plot_lines, line_values = c("est_point_start", "est_point_end"), line_sig = "sig_trend_comp_within", plot_settings = plotsettings_lineplot()){

  est_y <- line_values[1]
  est_y_end <- line_values[2]
  data_plot_lines <- build_column_2(data_plot_lines, column_name = line_sig, filling = FALSE)
  data_plot_lines <- build_column_2(data_plot_lines, column_name = est_y, filling = NA)
  data_plot_lines <- build_column_2(data_plot_lines, column_name = est_y_end, filling = NA)

    ggplot2::geom_segment(
      data = data_plot_lines,
      ggplot2::aes(
        x = .data$year_start,
        xend = .data$year_end,
        y = .data$est_y,
        yend = .data$est_y_end,
        colour = .data$grouping_var,
        linetype = .data$line_sig,
        group = .data$trend
      ),
      linewidth = plot_settings$line_width

  )
}
