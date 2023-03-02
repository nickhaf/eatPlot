test_that("calc_brace_coords works", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start = c(2011, 2011, 2015, 2015),
    year_end = c(2020, 2020, 2020, 2020),
    grouping_var = factor(c(0, 1, 0, 1))
  )

  coords <- c(360, 530)

  test_braces <- calc_brace_coords(df, coords)

  expect_equal(test_braces$upper_y, c(360, 360, 324, 324))
  expect_equal(test_braces$lower_y, c(324, 324, 306, 306))
  expect_equal(test_braces$label_pos_y, c(277.2, 298.8, 277.2, 298.8))
})

test_that("calc_brace_coords works for grouped braces", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start = c(2011, 2011, 2012, 2012),
    year_end = c(2012, 2012, 2013, 2013),
    grouping_var = factor(c(0, 1, 0, 1))
  )

  coords <- c(360, 530)

  test_braces <- calc_brace_coords(df, coords)

  expect_equal(test_braces$upper_y, c(360, 360, 324, 324))
  expect_equal(test_braces$lower_y, c(324, 324, 306, 306))
  expect_equal(test_braces$label_pos_y, c(277.2, 298.8, 277.2, 298.8))
})

test_that("Point nudge is calculated correctly", {
  expect_equal(calc_y_nudge(c(1,2,3), n_groups = 2), c(-0.5, 0.5))
  expect_equal(calc_y_nudge(c(1:100), n_groups = 4), c(-24.75, 24.75, 24.75, 24.75))
})
