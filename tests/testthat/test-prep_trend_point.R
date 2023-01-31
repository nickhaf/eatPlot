test_that("merging works correctly", {

    trend_whole <- data.frame(
      TR_BUNDESLAND = rep(c("Berlin", "Brandenburg"), 4),
      grouping_var = rep(c(0, 0, 1, 1), 2),
      year_start = c(rep(2011, 4), rep(2013, 4)),
      year_end = c(rep(2013, 4), rep(2015, 4)),
      sig_trend_whole = rep(TRUE, 8),
      est_trend_whole = rep(11, 8),
      se_trend_whole = rep(2, 8)
    )
    trend_within <- data.frame(
      TR_BUNDESLAND = rep(c("Berlin", "Brandenburg"), 4),
      grouping_var = rep(c(0, 0, 1, 1), 2),
      year_start = c(rep(2011, 4), rep(2013, 4)),
      year_end = c(rep(2013, 4), rep(2015, 4)),
      sig_trend_within = rep(TRUE, 8),
      est_trend_within = rep(11, 8),
      se_trend_within = rep(2, 8)
    )
    point_estimates  <-  data.frame(
      TR_BUNDESLAND = rep(c("Berlin", "Brandenburg"), 6),
      grouping_var = rep(c(0, 0, 1, 1), 3),
      time = c(rep(2011, 4), rep(2013, 4), rep(2015, 4)),
      est_point = 10:21
    )

  test_prep_df <- prep_trend_point(trend_whole, trend_within, point_estimates)

  expect_equal(test_prep_df$est_point_start, c(10:17))
  expect_equal(test_prep_df$est_point_end, c(14:21))
  expect_equal(test_prep_df$year_start, c(rep(2011, 4), rep(2013, 4)))
  expect_equal(test_prep_df$year_end, c(rep(2013, 4), rep(2015, 4)))
})
