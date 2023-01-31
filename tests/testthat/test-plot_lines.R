test_that("lineplot is still the same", {
df_lines <- data.frame(TR_BUNDESLAND = rep("Berlin", 4),
                       year_start = c(2011, 2011, 2016, 2016),
                       year_end = c(2013, 2013, 2020, 2020),
                       sig_trend_within = c(TRUE, FALSE, TRUE, FALSE),
                       est_point_start = c(10:13),
                       est_point_end = c(14:17),
                       grouping_var = c(0, 1, 0, 1)
                       )
  vdiffr::expect_doppelganger("Plotting lines", ggplot2::ggplot() +
                                 plot_lines(data_trend_point = df_lines))
})

