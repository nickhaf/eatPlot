#' Title
#'
#' @param plot_data Input is a list prepared by [prep_trend()].`
#' @param point_values Character string of the column name in `plot_data[["plot_points"]]` containing the y-values for the plotted points. Defaults to `est_point`.
#' @param point_sig Character string of the column name containing significance values for `point_values`. Defaults to `"sig_point"`.
#' @param line_values Character vector with two elements. Column names in `plot_data[["plot_lines"]]` containing the y-values for the plotted lines. Defaults to `c("est_point_start", "est_point_end")`.
#' @param line_sig Character string of the column name containing significance values for `line_values`. Defaults to `"sig_trend_comp_within"`.
#' @param label_est Character string of the column name containing the brace labels.
#' @param label_se Character string of the column name containing the standard errors for `label_est`. Will be put in bracktes behind `label_est`.
#' @param label_sig_high Character string of the column name containing significance values for `label_est`. Significant values will be marked by a raised 'a'.
#' @param label_sig_bold Character string of the column name containing significance values for `label_est`. Significant values will be marked as bold.
#' @param y_axis Logical, indicating whether a y-axis should be plotted to the left of each row or not. Defaults to `FALSE`.
#' @param n_cols Numeric, which indicates the number of columns the final plot should have. Defaults to `4`.
#'
#' @param split_plot Logical, indicating whether the different trends should be split or not.
#' @param nudge_x_axis Numeric. The x-axis labels will be nudged into the center by this amount, if the plot is a split lineplot. Defaults to `0.4`.
#' @return [ggplot2] object.
#' @export
#'
#' @examples # tbd
plot_lineplot <- function(plot_data,
                          seperate_plot_var = "state_var",
                          point_values = "est_point",
                          point_sig = "sig_point",
                          line_values = c("est_point_start", "est_point_end"),
                          line_sig = "sig_trend_comp_within",
                          label_est = "est_trend_no_comp",
                          label_se = "se_trend_no_comp",
                          label_sig_high = "sig_trend_comp_whole",
                          label_sig_bold = "sig_trend_no_comp",
                          split_plot = FALSE,
                          y_axis = FALSE,
                          n_cols = 4,
                          nudge_x_axis = 0.155) {
  states <- unique(plot_data[[1]]$state_var)
  tiles <- unique(plot_data[[1]][, seperate_plot_var])

  plot_list <- list()
  range_est <- range(plot_data[["plot_points"]][, point_values], na.rm = TRUE)
  position <- 1

  for (i in tiles) {
    plot_data_tile <- filter_rows(plot_data, column_name = seperate_plot_var, subsetter = i)
    if(seperate_plot_var == "competence_var"){
      plot_data_tile[["plot_background_lines"]] <- plot_data_tile[["plot_background_lines"]][plot_data_tile[["plot_background_lines"]]$competence_var == i, ]
    }

    p_state <- ggplot2::ggplot() +
      plot_single_lineplot(
        plot_data = plot_data_tile,
        y_range = range_est,
        split_plot = split_plot,
        point_values = point_values,
        point_sig = point_sig,
        line_values = line_values,
        line_sig = line_sig,
        label_est = label_est,
        label_se = label_se,
        label_sig_high = label_sig_high,
        label_sig_bold = label_sig_bold,
        nudge_x_axis = nudge_x_axis
      ) +
    ggplot2::labs(title = unique(plot_data_tile[["plot_braces"]][, seperate_plot_var])) +
      set_plot_coords(plot_data)

    ## The wholeGroup plot gets a box drawn around it.
    # if (i == "wholeGroup") {
    #   p_state <- p_state +
    #     ggplot2::theme(plot.background = ggplot2::element_rect(color = "black", linewidth = 0.5, fill = NA))
    # }

    plot_list[[i]] <- p_state
    position <- position + 1
  }


  # Add y axis --------------------------------------------------------------
  if (y_axis == TRUE) {
    y_axis_plot <- ggplot2::ggplot() +
      plot_y_axis(plot_data)

    positions_y_axis <- calc_y_positions(states, n_cols)

    for (i in positions_y_axis) {
      plot_list <- append(plot_list, list(y_axis_plot), after = i - 1)
    }

    widths_setting <- c(0.02, rep(1 - 0.02 / n_cols, times = n_cols))
    n_cols <- n_cols + 1
  } else {
    widths_setting <- 1 / n_cols
  }

  nudge_top_bottom <- 0.02 + 0.006 * (length(levels(plot_data[["plot_braces"]]$grouping_var))-1) #more brace labels need more space
  ## Build the finished plot:
  patchwork::wrap_plots(plot_list, ncol = n_cols, widths = widths_setting) &
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(nudge_top_bottom, 0.01, nudge_top_bottom, 0.01), "npc") #t, r, b, l
    )
}

