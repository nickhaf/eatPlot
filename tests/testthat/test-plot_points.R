test_that("simple pointplot", {

  df <- data.frame(grouping_var = rep(c("0","1"), 4),
                   year = c(2011, 2011, 2012, 2012, 2024, 2024, 2030, 2030),
                   est_point = 100:107,
                   p = seq(0.02, 0.09, by = 0.01),
                   sig_point = c(TRUE, TRUE, TRUE, rep(FALSE, 5) ),
                   trend = c(1, 1, 1, 1, 2, 2, 2, 2)
                 )

  vdiffr::expect_doppelganger("Simple pointplot",
                              ggplot2::ggplot() +
    plot_points(df,
                point_values = "est_point",
                point_sig = "sig_point",
                y_range = c(100, 107))
  )
})

test_that("Pointplot can be facetted", {

  df <- data.frame(grouping_var = rep(c("0","1"), 4),
                   year = c(2011, 2011, 2012, 2012, 2024, 2024, 2030, 2030),
                   est_point = 100:107,
                   p = seq(0.02, 0.09, by = 0.01),
                   sig_point = c(TRUE, TRUE, TRUE, rep(FALSE, 5) ),
                   trend = c(1, 1, 1, 1, 2, 2, 2, 2)
  )

  vdiffr::expect_doppelganger("Facetted Points",
                              ggplot2::ggplot() +
                                plot_points(df,
                                            point_values = "est_point",
                                            point_sig = "sig_point",
                                            y_range = c(100, 107)) +
                                ggplot2::facet_wrap(~ trend, scales = "free_x")
  )

})


# plot_data <- prep_trend(data = trend_books, grouping_var = "KBuecher_imp3", competence = "GL", sig_niveau = 0.05)
# ggplot2::ggplot() +
#   plot_points(plot_data[["plot_points"]][plot_data[["plot_points"]]$TR_BUNDESLAND == "Berlin", ])
#

