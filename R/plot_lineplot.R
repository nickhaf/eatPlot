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
                          split_plot = FALSE,
                          y_axis = FALSE) {

  states <- unique(plot_data[[1]]$state_var)

  plot_list <- list()
  range_est <- range(plot_data[["plot_points"]][, point_values], na.rm = TRUE)
  n_cols <- ceiling(length(states)/4)
  position <- 1

  for (i in states) {

    plot_data_state <- get_state(plot_data, state = i)
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

    ## The wholeGroup plot gets a box drawn around it.
    # if (i == "wholeGroup") {
    #   p_state <- p_state +
    #     ggplot2::theme(plot.background = ggplot2::element_rect(color = "black", linewidth = 0.5, fill = NA))
    # }

    plot_list[[i]] <- p_state
    position <- position + 1
    }

## Achtung: y-Achse aligned noch nicht mit dem restlichen Plot!!!
if(y_axis == TRUE){

  y_axis_plot <- ggplot2::ggplot() +
    plot_y_axis(plot_data)

log_y_axis <- sapply(1:length(states), function(x) {
  (x-1) %% n_cols == 0
})

positions_y_axis <- c(1:length(states))[log_y_axis]

for(i in positions_y_axis){
  plot_list <- append(plot_list, list(y_axis_plot), after = i - 1)
}
widths_setting <- c(0.02, rep(1-0.02/(n_cols -1), times = n_cols - 1))
}else{
  widths_setting <- 1/n_cols
}

  ## Build the finished plot:
  patchwork::wrap_plots(plot_list, ncol = n_cols, widths = widths_setting) &
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
