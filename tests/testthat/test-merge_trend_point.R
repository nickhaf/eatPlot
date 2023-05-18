test_that("merging with grouping_var", {
  trend_data_1 <- data.frame(
    state_var = rep(c("Berlin", "Brandenburg"), 4),
    grouping_var = rep(c(0, 0, 1, 1), 2),
    year_start = c(rep(2011, 4), rep(2013, 4)),
    year_end = c(rep(2013, 4), rep(2015, 4)),
    sig_trend = rep(TRUE, 8),
    est_trend = rep(11, 8),
    se_trend = rep(2, 8),
    depVar = c("a", "a"),
    competence_var = "b"
  )

  point_data_1 <- data.frame(
    state_var = rep(c("Berlin", "Brandenburg"), 6),
    grouping_var = rep(c(0, 0, 1, 1), 3),
    year = c(rep(2011, 4), rep(2013, 4), rep(2015, 4)),
    point_values_noTrend_noComp = 10:21,
    depVar = c("a", "a"),
    competence_var = "b"
  )

  test_prep_df <- merge_trend_point(trend_data_1, point_data_1)

  expect_equal(test_prep_df$point_values_noTrendStart_noComp, c(10:17))
  expect_equal(test_prep_df$point_values_noTrendEnd_noComp, c(14:21))
  expect_equal(test_prep_df$year_start, c(rep(2011, 4), rep(2013, 4)))
  expect_equal(test_prep_df$year_end, c(rep(2013, 4), rep(2015, 4)))
})
