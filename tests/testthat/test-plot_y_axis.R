test_that("multiplication works", {
  df_y <- data.frame(trend = "20112016",
                     x = 1,
                     y = 390,
                     yend = 530,
                     xmax = 1)

y_axis <- ggplot2::ggplot() +
    plot_y_axis(df_y) +
  ggplot2::scale_x_continuous(limits = c(1,1.1), expand = c(0,0)) +
  theme_line() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(),
    axis.ticks.y = ggplot2::element_line(),
    axis.line.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank()
  )




  })
