plot_y_axis <- function(plot_data) {

  range_est <- range(plot_data[["plot_points"]]$est_point, na.rm = TRUE)

  df_y <- data.frame(trend = "20112016",
                     x = min(plot_data[["plot_points"]]$year),
                     y = round(range_est[1] - 10, -1),
                     yend = round(range_est[2], -1),
                     xmax = max(plot_data[["plot_points"]]$year)
  )


  list(
    ggplot2::geom_segment(
      data = df_y,
      ggplot2::aes(
        x = .data$x, #- (.data$xmax - .data$x) * 0.1, ## calc expansion of scale_x_continous
        xend = .data$x, #- (.data$xmax - .data$x) * 0.1, ## calc expansion of scale_x_continous
        y = .data$y,
        yend = .data$yend
      )
    ),
    ggplot2::scale_x_continuous(limits = c(min(plot_data[["plot_points"]]$year), min(plot_data[["plot_points"]]$year) + 1), expand = c(0,0)),
    theme_line(),
    ggplot2::scale_y_continuous(
      breaks = seq(
        from = round(min(plot_dat[["plot_points"]]$est_point, na.rm = TRUE) - 10, -1),
        to = round(max(plot_dat[["plot_points"]]$est_point, na.rm = TRUE), -1),
        by = 20
      ),
      limits = c(calc_coords(range_est)[1], calc_coords(range_est)[2])
    ),
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(),
      axis.ticks.y = ggplot2::element_line(),
      axis.text.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank()
    )



  )
}

