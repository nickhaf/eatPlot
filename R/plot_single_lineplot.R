#' Plot a single lineplot. Multiple of this lineplots are combined as tiles to the output of [plot_lineplot()].
#'
#' @keywords internal
#' @noRd
#'
#' @inheritParams plot_lineplot
#' @inheritParams plotsettings_lineplot
#' @param plot_dat Prepared data.
#'
#'
#' @return ggplot2 Object.
#'
#' @examples # tbd
plot_single_lineplot <- function(plot_dat) {
  # Assemble a single lineplot (one "facet" in the whole lineplot).
  list(
    if (plot_dat$plot_settings$background_lines) {
      plot_background_lines(
        dat_total = plot_dat$background_line_dat,
        plot_settings = plot_dat$plot_settings
      )
    },
    ggplot2::geom_line(
      ggplot2::aes(
        linetype = .data$line_sig
      ),
      linewidth = plot_dat$plot_settings$line_width
    ),
    plot_points(plot_dat),
    ggplot2::theme(plot.margin = ggplot2::unit(c(0.25, 0.11, 0.11, 0.11), units = "npc")),
    theme_line(plot_dat$plot_settings),
    if (length(plot_dat$brace_dat$brace_coords) > 0) {
      draw_braces(plot_dat$brace_dat$brace_coords$coord_dat_2, plot_dat$plot_settings)
    },
    if (length(plot_dat$brace_dat$brace_label) > 0) {
      draw_brace_label(plot_dat$brace_dat$brace_label, plot_dat$plot_settings)
    },
    ggplot2::scale_y_continuous(
      breaks = seq_over(
        from = plot_dat$plot_lims$y_axis_lims[1],
        to = plot_dat$plot_lims$y_axis_lims[2],
        by = plot_dat$plot_settings$axis_y_tick_distance
      ),
      limits = plot_dat$plot_lims$y_axis_lims,
      expand = c(0, 0)
    ),
    #ggplot2::coord_cartesian(ylim = plot_dat$plot_lims$y_lims_total, clip = "off"),
    set_scales(plot_dat$plot_settings),
    plot_x_axis(plot_dat)
  )

  # ,
  #   if (plot_settings$split_plot == TRUE) {
  #     if (plot_settings$equal_trend_line_length == TRUE) {
  #       ggplot2::facet_grid(. ~ years_Trend, scales = "free_x")
  #     } else {
  #       ggplot2::facet_grid(. ~ years_Trend, scales = "free_x", space = "free_x")
  #     }
  #   },
  #   set_plot_coords(
  #     plot_dat,
  #     plot_lims = plot_lims,
  #     plot_settings = plot_settings
  #   )
}
