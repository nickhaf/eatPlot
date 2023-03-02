test_that("calc_brace_coords works", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start = c(2011, 2011, 2012, 2012),
    year_end = c(2012, 2012, 2013, 2013),
    grouping_var = factor(c(0, 1, 0, 1)),
    brace_label = rep("a", 4)
  )

  coords <- c(360, 530)

  test_braces <- calc_brace_coords(df, coords)

  expect_equal(test_braces$brace_y, c(360, 360, 360, 360, 324, 324, 324, 324))
  expect_equal(test_braces$label_pos_y, rep(c(302.4, 324.0), 4))
})

test_that("Point nudge is calculated correctly", {
  expect_equal(calc_y_nudge(c(1, 2, 3), n_groups = 2), c(-0.5, 0.5))
  expect_equal(calc_y_nudge(c(1:100), n_groups = 4), c(-24.75, 24.75, 24.75, 24.75))
})
