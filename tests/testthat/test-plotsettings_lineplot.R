# default_list <-  list("n_cols" = 1,
#                       "axis_x_label_centralize" = 0,
#                       "split_plot" = FALSE,
#                       "y_axis" = FALSE)
#
# test_that("checking the object works", {
#   wrong_name <- default_list
#   names(wrong_name)[1] <- "wrong_name"
#
#   expect_error(check_plotsettings_lineplot(settings_list = list("n_cols" = 2)),
#     "The object provided for the 'default_list' argument does not have the correct length. Please use the function 'plot_settings()' for constructing a list of the correct type.",
#     fixed = TRUE
#   )
#   expect_error(check_plotsettings_lineplot(settings_list = wrong_name),
#     "The object provided for the 'default_list' argument does not have the correct names. Please use the function 'plot_settings()' for constructing a list of the correct type.",
#     fixed = TRUE
#   )
#   expect_no_error(check_plotsettings_lineplot(settings_list = default_list))
# })
#
# test_that("defaults are correct", {
#   expect_equal(
#     plotsettings_lineplot(),
#     default_list
#   )
# })
#
# test_that("defaults can be overwritten", {
#   expect_equal(
#     plotsettings_lineplot(n_cols = 10, axis_x_label_centralize = 0.3),
#     list("n_cols" = 10, "axis_x_label_centralize" = 0.3, "split_plot" = FALSE, "y_axis" = FALSE)
#   )
#
#   old_default <- list("n_cols" = 2,
#                       "axis_x_label_centralize" = 0.1,
#                       "split_plot" = FALSE,
#                       "y_axis" = TRUE)
#   expect_equal(
#     plotsettings_lineplot(n_cols = 5, default = old_default),
#     list("n_cols" = 5, "axis_x_label_centralize" = 0.1, "split_plot" = FALSE, "y_axis" = TRUE)
#   )
# })
