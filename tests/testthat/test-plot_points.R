test_that("nudge is calculated correctly", {
  expect_equal(calc_y_nudge(c(1,2,3), n_groups = 2), c(-0.02, 0.02))
  expect_equal(calc_y_nudge(c(1:100), n_groups = 4), c(-0.99, 0.99, 0.99, 0.99))
})


test_that("Pointplot is still the same", {

  df <- data.frame(grouping_var = rep(c("0","1"), 4),
                   year = c(2011, 2011, 2012, 2012, 2024, 2024, 2030, 2030),
                   est_point = 100:107,
                   p = seq(0.02, 0.09, by = 0.01),
                   sig_point = c(TRUE, TRUE, TRUE, rep(FALSE, 5) )
                 )

  vdiffr::expect_doppelganger("Plotting Points", ggplot2::ggplot() +
    plot_points(df)
  )

})

# plot_data <- prep_lineplot(data = trend_books, grouping_var = "KBuecher_imp3", competence = "GL", sig_niveau = 0.05)
# ggplot2::ggplot() +
#   plot_points(plot_data[["plot_points"]][plot_data[["plot_points"]]$TR_BUNDESLAND == "Berlin", ])
#

