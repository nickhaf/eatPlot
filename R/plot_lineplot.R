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
#' @param split_plot Logical, indicating whether the different trends should be split or not.
#' @return [ggplot2] object.
#' @export
#'
#' @examples # tbd
plot_lineplot <- function(plot_data,
                          point_values = "est_point",
                          point_sig = "sig_point",
                          line_values = c("est_point_start", "est_point_end"),
                          line_sig = "sig_trend_comp_within",
                          label_est = "est_trend_no_comp",
                          label_se = "se_trend_no_comp",
                          label_sig_high = "sig_trend_comp_whole",
                          label_sig_bold = "sig_trend_no_comp",
                          split_plot = FALSE) {

  states <- unique(plot_data[[1]]$state_var)

  plot_list <- list()
  range_est <- range(plot_data[["plot_points"]][, point_values], na.rm = TRUE)
  position <- 1

  ## Build dataframe for plotting manual y-axis:

  df_y <- data.frame(trend = "20112016",
                     x = min(plot_data[["plot_points"]]$year),
                     y = round(range_est[1] - 10, -1),
                     yend = round(range_est[2], -1),
                     xmax = max(plot_data[["plot_points"]]$year)
  )


  y_axis <- ggplot2::ggplot() +
    plot_y_axis(df_y) +
    ggplot2::scale_x_continuous(limits = c(min(plot_data[["plot_points"]]$year), min(plot_data[["plot_points"]]$year) + 1), expand = c(0,0)) +
    theme_line() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(),
      axis.ticks.y = ggplot2::element_line(),
     axis.line.x = ggplot2::element_blank(),
     axis.text.x = ggplot2::element_blank()
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(
        from = round(min(plot_dat[["plot_points"]]$est_point, na.rm = TRUE) - 10, -1),
        to = round(max(plot_dat[["plot_points"]]$est_point, na.rm = TRUE), -1),
        by = 20
      )
    )

  ## Assemble the plots, one for every state:
  n_rows <- ceiling(length(states)/5)
  n_cols <- 5
  n_tiles <- n_rows * n_cols

state_position <- 1

  for (i in 1:n_tiles) {
    if ((i - 1) %% 5 == 0) {
      plot_list[[i]] <- y_axis
    }else{
    state <- states[state_position]
    plot_data_state <- get_state(plot_data, state = state)
    p_state <- ggplot2::ggplot() +
      plot_single_lineplot(
        plot_data = plot_data_state,
        y_range = range_est,
        split_plot = split_plot,
        point_values = point_values,
        point_sig = point_sig,
        line_values = line_values,
        line_sig = line_sig,
        label_est = label_est,
        label_se = label_se,
        label_sig_high = label_sig_high,
        label_sig_bold = label_sig_bold
      ) +
      set_plot_coords(plot_dat)

    ## Only the left plots get a y axis:
    #if ((position - 1) %% 4 == 0) {
      # p_state <- p_state +
      #  plot_y_axis(df_y)
      #}

    ## The wholeGroup plot gets a box drawn around it.
    # if (i == "wholeGroup") {
    #   p_state <- p_state +
    #     ggplot2::theme(plot.background = ggplot2::element_rect(color = "black", linewidth = 0.5, fill = NA))
    # }

    plot_list[[i]] <- p_state
    state_position <- state_position + 1

    }

  }

  ## Build the finished plot:
  patchwork::wrap_plots(plot_list, ncol = n_cols, widths = c(0.02, rep(1-0.02/(n_cols -1), times = n_cols - 1))) &
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(0.025, 0.015, 0.025, 0.015), "npc")
    )
}


# Utils -------------------------------------------------------------------
get_state <- function(plot_data, state) {
  for (i in c("plot_points", "plot_lines", "plot_braces")) {
    plot_data[[i]] <- plot_data[[i]][plot_data[[i]]$state_var == state, ]
  }
  return(plot_data)
}
