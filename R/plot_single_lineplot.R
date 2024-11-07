#' Plot a single lineplot. Multiple of this lineplots are combined as tiles to the output of [plot_lineplot()].
#'
#' @inheritParams plot_lineplot
#' @inheritParams plotsettings_lineplot
#'
#' @return ggplot2 Object.
#' @export
#'
#' @examples # tbd
plot_single_lineplot <- function(plot_dat,
                                 # point_values = "est_noTrend_noComp",
                                 # point_sig = "sig_noTrend_noComp",
                                 # line_values = c("est_noTrendStart_noComp", "est_noTrendEnd_noComp"),
                                 # line_se = NULL,
                                 # line_sig = "sig_Trend_CompWithin",
                                 # label_se = "se_Trend_noComp",
                                 # label_sig_high = "sig_Trend_CompCrossDiffWhole",
                                 # label_sig_bold = "sig_Trend_noComp",
                                 # background_lines = TRUE,
                                 plot_settings = plotsettings_lineplot()) {
  # Assemble a single lineplot (one "tile" in the whole lineplot).



  ggplot2::ggplot(plot_dat$plot_dat,
    mapping = ggplot2::aes(
      x = year,
      y = est_point,
      group = id
    )
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        colour = .data$mhg,
        linetype = .data$line_sig
      ),
      linewidth = plot_settings$line_width
    ) +
    plot_points(plot_dat, plot_settings = plot_settings) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0.25, 0.11, 0.11, 0.11), units = "npc")) +
    theme_line(plot_settings) +
    draw_braces(plot_dat$brace_dat, plot_settings) +
    draw_brace_label(plot_dat$brace_dat, plot_settings) +
    ggplot2::coord_cartesian(ylim = plot_dat$plot_lims$y_lims_total, clip = "off") +
    set_scales(plot_settings) +
        plot_x_axis(plot_dat,
          plot_settings = plot_settings
        ) +
    NULL



  #   list(
  #     if (background_lines == TRUE) {
  #       plot_background_lines(
  #         dat = plot_dat[["plot_background_lines"]],
  #         line_values = line_values,
  #         line_se = line_se,
  #         plot_settings = plot_settings
  #       )
  #     },
  #     if (plot_settings$split_plot == TRUE) {
  #       if (plot_settings$equal_trend_line_length == TRUE) {
  #         ggplot2::facet_grid(. ~ years_Trend, scales = "free_x")
  #       } else {
  #         ggplot2::facet_grid(. ~ years_Trend, scales = "free_x", space = "free_x")
  #       }
  #     },
  #     set_plot_coords(
  #       plot_dat,
  #       plot_lims = plot_lims,
  #       plot_settings = plot_settings
  #     )
  #   )
}
