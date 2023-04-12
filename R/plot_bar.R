#' Plot an IQB barplot.
#'
#' @param no_trend_list Input is a list prepared by [prep_no_trend()]. You can also use the according data.frame named `plot_bar` from this list.
#' @param x_value Character string of the column name containing the estimates that should be plotted on the x-axis. Defaults to `"est_wholeGroup"`, which are the estimates for the comparison of a state against the wholeGroup (Germany).
#' @param y_value Character string of the column name containing the labels that should be plotted on the y-axis. Defaults to `"state_var"`, so the states (Bundesländer) are depicted on the y-axis.
#' @param bar_label Character string of the column that should be used as label at the bar end. Defaults to `est_wholeGroup`. If `NULL`, no label will be plotted.
#' @param bar_sig Character string of the column containing the grouping for the pattern or the frame of the bar. Defaults to `sig_wholeGroup`, so the significances of the state vs. wholeGroup (Germany) comparison are represented in the pattern or the frame of the bars.
#' @param bar_fill Character string of the column containing the grouping for the filling of the bar. Defaults to `fill_wholeGroup`, so the significances of the state vs. wholeGroup (Germany) comparison, as well as the groups found in "data$grouping_var" are represented in the filling colours of the bars.
#' @param bar_pattern_fill Character string of the column containing the grouping for the filling of the pattern on the bar. Defaults to `grouping_var`, so the groups found in `data$grouping_var` are represented in the colours of the bar pattern.
#' @param plot_settings Named list constructed with `plotsettings_barplot()`. Defaults to a list with all settings set to `0`. There are several predefined lists with optimized settings for different plots. See `plotsettings_barplot()` for an overview.
#'
#' @return Returns a [ggplot2] barplot.
#' @export
#'
#' @examples #tbd
plot_bar <- function(no_trend_list,
                     x_value = "est_wholeGroup",
                     y_value = "state_var",
                     bar_label = "est_wholeGroup",
                     bar_sig = "sig_wholeGroup",
                     bar_fill = "fill_wholeGroup",
                     bar_pattern_fill = "grouping_var",
                     plot_settings = plotsettings_barplot()) {

  if (inherits(no_trend_list, "list")) {
    data_plot_bar <- no_trend_list[["plot_bar"]]
  } else {
    data_plot_bar <- no_trend_list
  }


# Check columns -----------------------------------------------------------
  data_plot_bar <- build_column_2(data_plot_bar, column_name = bar_sig, filling = "FALSE")
  data_plot_bar <- build_column_2(data_plot_bar, column_name = bar_fill, filling = "FALSE")
  data_plot_bar <- build_column_2(data_plot_bar, column_name = bar_pattern_fill, filling = NA)


  if(is.null(plot_settings$axis_x_lims)){
  plot_borders <- calc_plot_borders(data_plot_bar[[x_value]])
  ## Plots with only positive values can start at 0.
  if(all(data_plot_bar[[x_value]][!is.na(data_plot_bar[[x_value]])] >= 0)){
    plot_borders[1] <- 0
  }
  }else{
    plot_borders <- plot_settings$axis_x_lims
  }

  scale_breaks <- unique(c(seq(0, plot_borders[1], by = -10),
                    seq(0, plot_borders[2], by = 10))
  )

  base_plot <-
    ggplot2::ggplot(
      data = data_plot_bar,
      mapping = ggplot2::aes(
        x = .data[[x_value]],
        y = .data[[y_value]],
        fill = .data$bar_fill,
        # TODO: maybe find another interface for this argument?
        # TODO: linetype and pattern aes only works when specified here
        #   - when specified on geom, the first bar colors in the test plot
        #     for frame is exchanged!
        linetype = .data$bar_sig,
        pattern = .data$bar_sig
      )
    ) +
    ggstats::geom_stripped_rows(
      odd = plot_settings$background_stripes_colour[1],
      even = plot_settings$background_stripes_colour[2]) +
    ggplot2::geom_vline(
      xintercept = scale_breaks,
      linetype = "dashed", colour = "darkgrey"
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = "darkgrey"
    ) +
    ggplot2::scale_x_continuous(breaks = scale_breaks) +
    ggplot2::scale_fill_manual(values = plot_settings$bar_fill_colour) +
    if(!is.null(bar_label)){
      ggplot2::geom_text(ggplot2::aes(label = .data[[bar_label]]), hjust = -0.2)
    }

  if (plot_settings$bar_sig_type == "pattern") {
    base_plot +
      ## This chunk only works together with the ggpattern::scale-specifications.
      ggpattern::geom_col_pattern(
        mapping = ggplot2::aes(pattern_fill = .data$bar_pattern_fill,
                               linetype = NULL),
        position = ggplot2::position_dodge(width = 0.8),
        color = "black",
        linewidth = 0.9,
        pattern_colour = "white",
        pattern_angle = -45,
        pattern_density = 0.4, # Streifenbreite
        pattern_spacing = 0.01, # Abstand
        pattern_key_scale_factor = 0.6,
        width = 0.4
      ) +
      ggpattern::scale_pattern_manual(values = plot_settings$bar_pattern_type) +
      ggpattern::scale_pattern_fill_manual(values = plot_settings$bar_pattern_fill_colour) +
      theme_table_bar() +
      NULL
  } else if (plot_settings$bar_sig_type == "frame") {
    base_plot +
      ggplot2::geom_col(
        # TODO: deleting it above produces unexpected result
        # mapping = ggplot2::aes(linetype = .data[[bar_pattern]]),
        position = ggplot2::position_dodge(width = 0.8),
        color = "black",
        linewidth = 0.9,
        width = 0.4
      ) +
      ggplot2::scale_linetype_manual(values = plot_settings$bar_frame_linetype) +
      theme_table_bar() +
      NULL
  } else {
    message("`sig_type` must be either \"frame\" or \"pattern\"")
  }
}

