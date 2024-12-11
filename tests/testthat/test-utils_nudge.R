test_that("calc_brace_coords works", {
  years_list <- prep_years_list(list(c(2011, 2012), c(2012, 2013)),
                                list(c(2011, 2012), c(2012, 2013)
                                     ))

  test_braces <- calc_brace_coords(
                                   grouping_var_lvls = c(0,1),
                                   coords = c(360, 530),
                                   years_list = years_list,
                                   plot_settings = plotsettings_lineplot(split_plot = TRUE))

  expect_equal(test_braces$group_labels$label_pos_y, c(334.5, 320.9))
})


