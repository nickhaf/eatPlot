test_that("calc_brace_coords works for long format", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start = c(2011, 2011, 2012, 2012),
    year_end = c(2012, 2012, 2013, 2013),
    grouping_var = factor(c(0, 1, 0, 1)),
    brace_label = rep("a", 4),
    trend = c("2011", "2011", "2012", "2012")
  )

  coords <- c(360, 530)

  test_braces <- calc_brace_coords(df, coords, output_format = "long")

  expect_equal(test_braces$brace_y, c(360, 360, 360, 360, 324, 324, 324, 324))
  expect_equal(test_braces$label_pos_y, rep(c(302.4, 316.8), 4))
})

test_that("calc_brace_coords works for wide format", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start = c(2011, 2011, 2012, 2012),
    year_end = c(2012, 2012, 2013, 2013),
    grouping_var = factor(c(0, 1, 0, 1)),
    brace_label = rep("a", 4),
    trend = c("20112012", "20112012", "20122013", "20122013")
  )

  coords <- c(360, 530)

  test_braces <- calc_brace_coords(df, coords)

  expect_equal(test_braces$lower_y, rep(324, 4))
  expect_equal(test_braces$year_start, c(2011, 2011, 2012, 2012))
})




test_that("Point nudge is calculated correctly", {
    df <- data.frame(year = c(2011, 2011, 2012, 2012, 2012, 2012, 2030, 2030),
                     est_point = c(1, 2, 2, 1, 1, 2, 1, 1),
                     trend = c(1, 1, 1, 1, 2, 2, 2, 2)
    )

  expect_equal(calc_y_nudge(df), c(-0.25, 0.25, 0.25, -0.25, -0.25, 0.25, -0.25, 0.25))
})
