test_that("backgroundlines is still the same", {
  df_backgroundlines <- data.frame(
    TR_BUNDESLAND = rep("wholeGroup", 2),
    year_start = c(2011, 2013),
    year_end = c(2013, 2016),
    est_point_start = c(10:11),
    est_point_end = c(11:12),
    trend = c("20112013", "20132016")
  )
  vdiffr::expect_doppelganger("Plotting backgroundlines", ggplot2::ggplot() +
    plot_background_lines(
      data_plot_background_lines = df_backgroundlines,
      line_values = c("est_point_start", "est_point_end")
    ))
})

# plot_data <- prep_trend(data = trend_books, grouping_var = "KBuecher_imp3", competence = "GL")
# ggplot2::ggplot() +
#   plot_background_lines(data_plot_background_lines = plot_data[["plot_background_lines"]])
