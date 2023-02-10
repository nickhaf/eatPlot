test_that("merging with grouping_var", {

    trend_data_1 <- data.frame(
      state_var = rep(c("Berlin", "Brandenburg"), 4),
      grouping_var = rep(c(0, 0, 1, 1), 2),
      year_start = c(rep(2011, 4), rep(2013, 4)),
      year_end = c(rep(2013, 4), rep(2015, 4)),
      sig_trend = rep(TRUE, 8),
      est_trend = rep(11, 8),
      se_trend = rep(2, 8)
    )

    point_data_1  <-  data.frame(
      state_var = rep(c("Berlin", "Brandenburg"), 6),
      grouping_var = rep(c(0, 0, 1, 1), 3),
      year = c(rep(2011, 4), rep(2013, 4), rep(2015, 4)),
      est_point = 10:21
    )

  test_prep_df <- merge_trend_point(trend_data_1, point_data_1)

  expect_equal(test_prep_df$est_point_start, c(10:17))
  expect_equal(test_prep_df$est_point_end, c(14:21))
  expect_equal(test_prep_df$year_start, c(rep(2011, 4), rep(2013, 4)))
  expect_equal(test_prep_df$year_end, c(rep(2013, 4), rep(2015, 4)))

})

