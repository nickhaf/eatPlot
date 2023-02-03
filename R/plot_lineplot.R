plot_lineplot <- function(plot_data){

  states <- unique(plot_data[[1]]$TR_BUNDESLAND)

  plot_list <- list()
  range_est <- range(c(plot_data[["plot_lines"]]$est_point_start, plot_data[["plot_lines"]]$est_point_start))
  position <- 1

  for(i in states){

    plot_data_state <- lapply(plot_data[c("plot_points", "plot_lines")], function(x){
      x[x$TR_BUNDESLAND == i, ]
    })

    p1 <- ggplot2::ggplot() +
      settings_lineplot(plot_data_state[["plot_lines"]]) +
      plot_background_lines(plot_data[["plot_background_lines"]]) +
      plot_points(plot_data_state[["plot_points"]]) +
      plot_lines(plot_data_state[["plot_lines"]]) +
      plot_braces(plot_data[["plot_braces"]], BL = i) +
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

  plot_margin <- ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc"))
  #pdf(file = "Trend_Soz_Dis.pdf", width = 11, height = 22)
  line_plot <- do.call(eval(parse(text = "gridExtra::grid.arrange")), c(grobs = lapply(plot_list, "+", plot_margin), ncol = nCol))
  #dev.off()
  return(line_plot)
}
