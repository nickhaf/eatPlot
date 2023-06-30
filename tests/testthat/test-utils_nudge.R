test_that("calc_brace_coords works for long format", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start_axis = c(2011, 2011, 2012, 2012),
    year_end_axis = c(2012, 2012, 2013, 2013),
    grouping_var = factor(c(0, 1, 0, 1)),
    brace_label = rep("a", 4),
    years_Trend = c("2011", "2011", "2012", "2012"),
    competence_var = "a"
  )

  coords <- c(360, 530)

  test_braces <- calc_brace_coords(df, coords, output_format = "long")

  expect_equal(test_braces$brace_y, c(360, 360, 360, 360, 343, 343, 343, 343))
  expect_equal(test_braces$label_pos_y, rep(c(334.5, 320.9), 4))
})

test_that("calc_brace_coords works for wide format", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start_axis = c(2011, 2011, 2012, 2012),
    year_end_axis = c(2012, 2012, 2013, 2013),
    grouping_var = factor(c(0, 1, 0, 1)),
    brace_label = rep("a", 4),
    years_Trend = c("20112012", "20112012", "20122013", "20122013"),
    competence_var = "a"
  )

  coords <- c(360, 530)

  test_braces <- calc_brace_coords(df, coords)

  expect_equal(test_braces$lower_y, rep(343, 4))
  expect_equal(test_braces$year_start_axis, c(2011, 2011, 2012, 2012))
  expect_equal(test_braces$label_pos_y, rep(c(334.5, 320.9), 2))
})


test_that("labels are calculated correctly for multiple groups", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start_axis = c(2011, 2011, 2012, 2012),
    year_end_axis = c(2012, 2012, 2013, 2013),
    grouping_var = factor(c(0, 1, 2, 3)),
    brace_label = rep("a", 4),
    years_Trend = c("2011", "2011", "2012", "2012")
  )

  test_braces <- calc_brace_label_y(df, upper_label_y = 334.5, range_coords = 170, gap_label = 0.08)
  expect_equal(test_braces, rep(c(334.5, 320.9), 2))
})


test_that("Point nudge is calculated correctly", {
  df <- data.frame(
    year_axis = c(2011, 2011, 2012, 2012, 2012, 2012, 2030, 2030),
    point_values = c(1, 2, 2, 1, 1, 2, 1, 1),
    years_Trend = c(1, 1, 1, 1, 2, 2, 2, 2),
    grouping_var = c(1, 0, 1, 0, 1, 0, 1, 0)
  )

  expect_equal(calc_y_nudge(df,
    plot_lims = list(coords_diff = 1),
    plot_settings = plotsettings_lineplot(point_label_nudge_y = 0.18)
  )$nudge_y, c(0.18, -0.18, -0.18, 0.18, 0.18, -0.18, 0.18, -0.18))
})


test_that("", {
  df <- data.frame(grouping_var = as.factor(c("a", "b", "c")))
  expect_equal(nudge_by_level(df,
    plot_settings = plotsettings_lineplot(point_label_nudge_direction = list(
      "a" = "-",
      "b" = "-",
      "c" = "+"
    )),
    nudge_val = 0.1
  )$nudge_y, c(-0.1, -0.1, 0.1))
})
