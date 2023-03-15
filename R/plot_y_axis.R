plot_y_axis <- function(df_y) {
   list(
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(),
      axis.ticks.y = ggplot2::element_line()
    ),
    ggplot2::geom_segment(
      data = df_y,
      ggplot2::aes(
        x = .data$x, #- (.data$xmax - .data$x) * 0.1, ## calc expansion of scale_x_continous
        xend = .data$x, #- (.data$xmax - .data$x) * 0.1, ## calc expansion of scale_x_continous
        y = .data$y,
        yend = .data$yend
      )
    )
  )
}
