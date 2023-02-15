test_that("calc_brace_coords works", {
  df <- data.frame(
    TR_BUNDESLAND = rep("Berlin", 4),
    year_start = c(2011, 2011, 2015, 2015),
    year_end = c(2020, 2020, 2020, 2020),
    grouping_var = c(0, 1, 0, 1),
    est_trend_within = c(1:4),
    se_trend_within = c(1:4),
    sig_trend_comp_within = c(TRUE, FALSE, FALSE, TRUE),
    sig_trend_whole = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503
  )

  coords <- c(360, 530)

  test_braces <- calc_brace_coords(df, coords)

  expect_equal(test_braces$brace_upper_y, c(360, 360, 324, 324))
  expect_equal(test_braces$brace_lower_y, c(324, 324, 306, 306))
  expect_equal(test_braces$label_pos_y, c(277.2, 298.8, 277.2, 298.8))
})



test_that("Point nudge is calculated correctly", {
  expect_equal(calc_y_nudge(c(1,2,3), n_groups = 2), c(-0.5, 0.5))
  expect_equal(calc_y_nudge(c(1:100), n_groups = 4), c(-24.75, 24.75, 24.75, 24.75))
})
