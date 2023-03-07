test_that("lineplot is still the same", {
  df_lines <- data.frame(
    TR_BUNDESLAND = rep("Berlin", 4),
    year_start = c(2011, 2011, 2016, 2016),
    year_end = c(2013, 2013, 2020, 2020),
    sig_trend_comp_within = c(TRUE, FALSE, TRUE, FALSE),
    est_point_start = c(10:13),
    est_point_end = c(14:17),
    grouping_var = c(0, 1, 0, 1),
    trend = c("20112013", "20112013", "20162020", "20162020")
  )
  vdiffr::expect_doppelganger("Plotting lines", ggplot2::ggplot() +
    plot_lines(df_lines,
      line_values = c("est_point_start", "est_point_end"),
      line_sig = "sig_trend_comp_within"
    ))
})

test_that("lines can be facetted", {
  df <- data.frame(
    grouping_var = c(1, 1),
    year_start = c(1, 2),
    year_end = c(2, 3),
    est_point_start = c(10, 15),
    est_point_end = c(15, 20),
    trend = c(12, 23),
    sig_trend = c(TRUE, FALSE)
  )

  vdiffr::expect_doppelganger("wrapped lineplot",
  ggplot2::ggplot() +
    plot_lines(df,
               line_values = c("est_point_start", "est_point_end"),
               line_sig = "sig_trend"
    ) +
    ggplot2::facet_wrap(~ trend, scales = "free_x")
)
})
