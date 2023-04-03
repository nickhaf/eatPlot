test_that("checking the object works", {
  expect_error(check_plotsettings(settings_list = list("n_cols" = 2)),
    "The object provided for the 'default' argument does not have the correct length. Please use the function 'plot_settings()' for constructing a list of the correct type.",
    fixed = TRUE
  )
  expect_error(check_plotsettings(settings_list = list("wrong_name" = 2, "nudge_x_axis" = 0.1)),
    "The object provided for the 'default' argument does not have the correct names. Please use the function 'plot_settings()' for constructing a list of the correct type.",
    fixed = TRUE
  )
  expect_no_error(check_plotsettings(settings_list = list("n_cols" = 2, "nudge_x_axis" = 0.1)))
})


test_that("constructer provides the correct defaults", {
  expect_equal(new_plotsettings(), list("n_cols" = 1, "nudge_x_axis" = 0))
})


test_that("defaults can be overwritten", {
  expect_equal(
    plotsettings(n_cols = 10, nudge_x_axis = 0.3),
    list("n_cols" = 10, "nudge_x_axis" = 0.3)
  )

  old_default <- new_plotsettings(n_cols = 4, nudge_x_axis = 0.13)
  expect_equal(
    plotsettings(n_cols = 5, default = old_default),
    list("n_cols" = 5, "nudge_x_axis" = 0.13)
  )
})
