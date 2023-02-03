test_that("Pointplot is still the same", {
  df_points <- data.frame(
    grouping_var = rep(c("0", "1"), 4),
    year = c(2011, 2011, 2012, 2012, 2024, 2024, 2030, 2030),
    est_point = 100:107,
    p = seq(0.02, 0.09, by = 0.01),
    sig_point = c(TRUE, TRUE, TRUE, rep(FALSE, 5))
  )

  df_lines <- data.frame(TR_BUNDESLAND = rep("Berlin", 4),
                         year_start = c(2011, 2011, 2016, 2016),
                         year_end = c(2013, 2013, 2020, 2020),
                         sig_trend_within = c(TRUE, FALSE, TRUE, FALSE),
                         est_point_start = c(10:13),
                         est_point_end = c(14:17),
                         grouping_var = c(0, 1, 0, 1)
  )

p_points <- ggplot2::ggplot() +
    plot_points(df_points) +
    grouping_colours()
test_colour_points <- ggplot2::ggplot_build(p_points)


p_lines <- ggplot2::ggplot() +
  plot_lines(data_plot_lines = df_lines) +
  grouping_colours()
test_colour_points <- ggplot2::ggplot_build(p_points)



expect_equal(test_colour_points$data[[1]]$colour, rep(c("#A6A6A6", "black"), 4))

})
