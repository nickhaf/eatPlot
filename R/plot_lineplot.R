plot_lineplot <- function(prep_list){


  states <- c(unique(prep_list[[1]]$TR_BUNDESLAND), "Deutschland")

  plot_list <- list()
  range_est <- range(pointEstimates$est)
  position <- 1

  for(i in states){

    prep_list_state <- lapply(prep_list, function(x){
      x[x$TR_BUNDESLAND == i, ]
    })

    p1 <- ggplot2::ggplot() +
      #plot_settings(my_data = pointEstimates) +
      # draw_background_lines(bt21 %>% filter(group == "wholeGroup", parameter == "mean", kb == "GL")) +
      plot_points(data_point_estimates = prep_list_state[["point_estimates"]]) +
      plot_lines(data_trend_point = prep_list_state[["trend_point"]]) +
      plot_braces(data_trend_braces = prep_list[["trend_braces"]], BL = i) +
      ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc")) +
      NULL

    if((position - 1) %% 4 == 0){
      p1 <- p1 +
        theme(axis.text.y = element_text(),
              axis.line.y = element_line(),
              axis.ticks.y = element_line()
        ) +
        scale_y_continuous(breaks = seq(from = round(range_est[1]-10, -1), to = round(range_est[2], -1), by = 20))
    }

    if(i == "Deutschland"){
      p1 <- p1 +
        theme(plot.background = element_rect(color = "black", size = 0.5, fill = NA))
    }

    plot_list[[i]] <- p1
    position <- position + 1
  }

  n <- length(plot_list)
  nCol <- floor(sqrt(n))

  margin = theme(plot.margin = margin(0.05, 0.03, 0.25, 0.03, "npc"))
  pdf(file = "Trend_Soz_Dis.pdf", width = 11, height = 22)
  do.call(eval(parse(text = "gridExtra::grid.arrange")), c(grobs = lapply(plot_list, "+", margin), ncol = nCol))
  dev.off()
}
