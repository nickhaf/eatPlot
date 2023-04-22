#' Title
#'
#' @param plot_data Input is a list prepared by [prep_plot()].`
#' @param seperate_plot_var Character string of the column containing the tiles. For every unique value, a new tile will be plotted. Defaults to `state_var`.
#' @param point_values Character string of the column name in `plot_data[["plot_points"]]` containing the y-values for the plotted points. Defaults to `est_noTrend_noComp`.
#' @param point_sig Character string of the column name containing significance values for `point_values`. Defaults to `"sig_point"`.
#' @param line_values Character vector with two elements. Column names in `plot_data[["plot_lines"]]` containing the y-values for the plotted lines. Defaults to `c("est_point_start", "est_point_end")`. If set to `NULL`, no lines will be plotted.
#' @param line_sig Character string of the column name containing significance values for `line_values`. Defaults to `"sig_Trend_CompWithin"`.
#' @param label_est Character string of the column name containing the brace labels.
#' @param label_se Character string of the column name containing the standard errors for `label_est`. Will be put in bracktes behind `label_est`.
#' @param label_sig_high Character string of the column name containing significance values for `label_est`. Significant values will be marked by a raised 'a'.
#' @param label_sig_bold Character string of the column name containing significance values for `label_est`. Significant values will be marked as bold.
#' @param background_lines Logical, indicating whether the whole group trend should be plotted in the background.
#' @param plot_settings Named list constructed with `plotsettings_lineplot()`. Defaults to a list with all settings set to `0`. There are several predefined lists with optimized settings for different plots. See `plotsettings_lineplot()` for an overview.
#' @return [ggplot2] object.
#' @export
#'
#' @examples # tbd
plot_lineplot <- function(plot_data,
                          seperate_plot_var = "state_var",
                          point_values = "est_noTrend_noComp",
                          point_sig = "sig_noTrend_noComp",
                          line_values = c("est_noTrendstart_noComp", "est_noTrendend_noComp"),
                          line_sig = "sig_Trend_CompWithin",
                          label_est = "est_Trend_noComp",
                          label_se = "se_Trend_noComp",
                          label_sig_high = "sig_Trend_CompWhole",
                          label_sig_bold = "sig_Trend_noComp",
                          background_lines = TRUE,
                          plot_settings = plotsettings_lineplot()) {

check_plotsettings_lineplot(plot_settings)

  plot_data[["plot_points"]] <- fill_column(  plot_data[["plot_points"]], column_name = point_values, filling = NA)
  plot_data[["plot_points"]] <- fill_column(  plot_data[["plot_points"]], column_name = point_sig, filling = FALSE)
  plot_data[["plot_lines"]] <- fill_column(  plot_data[["plot_lines"]],
                                             column_name = line_sig,
                                             filling = FALSE)

  states <- unique(plot_data[[1]]$state_var)
  tiles <- unique(plot_data[[1]][, seperate_plot_var]) #Hier die Level nehmen

  plot_list <- list()
  if(!is.null(point_values)){
  range_est <- range(plot_data[["plot_points"]][,point_values], na.rm = TRUE)
  }else{
    stop("Please provide point-values.")
  }
  position <- 1

  for (i in tiles) {
    plot_data_tile <- filter_rows(plot_data, column_name = seperate_plot_var, subsetter = i)
    if (seperate_plot_var == "competence_var") {
      plot_data_tile[["plot_background_lines"]] <- plot_data_tile[["plot_background_lines"]][plot_data_tile[["plot_background_lines"]]$competence_var == i, ]
    }

    p_state <- ggplot2::ggplot() +
      plot_single_lineplot(
        plot_data = plot_data_tile,
        y_range = range_est,
        point_values = point_values,
        point_sig = point_sig,
        line_values = line_values,
        line_sig = "line_sig",
        label_est = label_est,
        label_se = label_se,
        label_sig_high = label_sig_high,
        label_sig_bold = label_sig_bold,
        background_lines = background_lines,
        plot_settings = plot_settings
      ) +
      ggplot2::labs(title = unique(plot_data_tile[["plot_braces"]][, seperate_plot_var])) +
      set_plot_coords(plot_data, plot_settings = plot_settings)

    ## The wholeGroup plot gets a box drawn around it.
    # if (i == "wholeGroup") {
    #   p_state <- p_state +
    #     ggplot2::theme(plot.background = ggplot2::element_rect(color = "black", linewidth = 0.5, fill = NA))
    # }

    plot_list[[i]] <- p_state
    position <- position + 1
  }

  # Add y axis --------------------------------------------------------------
  if (plot_settings$y_axis == TRUE) {
    y_axis_plot <- ggplot2::ggplot() +
      plot_y_axis(plot_data)

    positions_y_axis <- calc_y_positions(states, plot_settings$n_cols)

    for (i in positions_y_axis) {
      plot_list <- append(plot_list, list(y_axis_plot), after = i - 1)
    }

    widths_setting <- c(0.02, rep(1 - 0.02 / plot_settings$n_cols, times = plot_settings$n_cols))
    plot_settings$n_cols <- plot_settings$n_cols + 1
  } else {
    widths_setting <- 1 / plot_settings$n_cols
  }

  margin_bottom <- plot_settings$margin_bottom + 0.006 * (length(levels(plot_data[["plot_braces"]]$grouping_var)) - 1) # more brace labels need more space

  ## Build the finished plot:
  patchwork::wrap_plots(plot_list, ncol = plot_settings$n_cols, widths = widths_setting) &
    ggplot2::theme(
      plot.margin = ggplot2::unit(
        c(
          plot_settings$margin_top,
          plot_settings$margin_right,
          margin_bottom,
          plot_settings$margin_left
        ),
        "npc"
      ) # t, r, b, l
    )
}
