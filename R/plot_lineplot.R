#' Title
#'
#' @param plot_data Input is a list prepared by [prep_trend()].
#' @param point_values Character string of the column name in `plot_data[["plot_points"]]` containing the y-values for the plotted points. Defaults to `est_point`.
#' @param point_sig Character string of the column name containing significance values for `point_values`. Defaults to `"sig_point"`.
#' @param line_values Character vector with two elements. Column names in `plot_data[["plot_lines"]]` containing the y-values for the plotted lines. Defaults to `c("est_point_start", "est_point_end")`.
#' @param line_sig Character string of the column name containing significance values for `line_values`. Defaults to `"sig_trend_comp_within"`.
#' @param label_est Character string of the column name containing the brace labels.
#' @param label_se Character string of the column name containing the standard errors for `label_est`. Will be put in bracktes behind `label_est`.
#' @param label_sig_high Character string of the column name containing significance values for `label_est`. Significant values will be marked by a raised 'a'.
#' @param label_sig_bold Character string of the column name containing significance values for `label_est`. Significant values will be marked as bold.
#'
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
                          label_sig_bold = "sig_trend_no_comp"){

  states <- unique(plot_data[[1]]$state_var)

  plot_list <- list()
  range_est <- range(plot_data[["plot_points"]][, point_values], na.rm = TRUE)
  position <- 1

  for(i in states){

    plot_data_state <- lapply(plot_data[c("plot_points", "plot_lines")], function(x){
      x[x$state_var == i, ]
    })

    p1 <- ggplot2::ggplot() +
      settings_lineplot(plot_data_state[["plot_lines"]]) +
      plot_braces(plot_data[["plot_braces"]],
                  BL = i,
                  label_est,
                  label_se,
                  label_sig_high,
                  label_sig_bold) +
      plot_background_lines(plot_data[["plot_background_lines"]],
                            line_values = line_values) +
      plot_points(plot_data_state[["plot_points"]],
                  point_values = point_values,
                  point_sig = point_sig) +
      plot_lines(plot_data_state[["plot_lines"]],
                 line_values = line_values,
                 line_sig = line_sig) +
      ggplot2::labs(title = paste0(i, "\n", " ")) + ## Title
      NULL

    if((position - 1) %% 4 == 0){
      p1 <- p1 +
        ggplot2::theme(axis.text.y = ggplot2::element_text(),
              axis.line.y = ggplot2::element_line(),
              axis.ticks.y = ggplot2::element_line()
        ) +
        ggplot2::scale_y_continuous(breaks = seq(from = round(range_est[1]-10, -1), to = round(range_est[2], -1), by = 20))
    }

    if(i == "wholeGroup"){
      p1 <- p1 +
        ggplot2::theme(plot.background = ggplot2::element_rect(color = "black", linewidth = 0.5, fill = NA))
    }

    plot_list[[i]] <- p1
    position <- position + 1
  }

  n <- length(plot_list)
  nCol <- floor(sqrt(n))

  ## Padding of white space around the plot:
  plot_margin <- ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.07, "npc"))
  #pdf(file = "Trend_Soz_Dis.pdf", width = 11, height = 22)
  line_plot <- do.call(eval(parse(text = "gridExtra::grid.arrange")), c(grobs = lapply(plot_list, "+", plot_margin), ncol = nCol))
  #dev.off()
  return(line_plot)
}
