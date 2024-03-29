test_that("simple pointplot", {
  df <- data.frame(
    grouping_var = rep(c("0", "1"), 4),
    year = c(2011, 2011, 2012, 2012, 2024, 2024, 2030, 2030),
    est_point = 100:107,
    p = seq(0.02, 0.09, by = 0.01),
    sig_noTrend_noComp = c(TRUE, TRUE, TRUE, rep(FALSE, 5)),
    years_Trend = c(1, 1, 1, 1, 2, 2, 2, 2),
    year_axis = c(1, 1, 2, 2, 3, 3, 4, 4)
  )

  vdiffr::expect_doppelganger(
    "Simple pointplot",
    ggplot2::ggplot() +
      plot_points(df,
        point_sig = "sig_noTrend_noComp",
        point_values = "est_point",
        plot_lims = list(y_range = c(100, 107),
                         coords_diff = 7)
      )
  )
})

test_that("Pointplot can be facetted", {
  df <- data.frame(
    grouping_var = rep(c("0", "1"), 4),
    year = c(2011, 2011, 2012, 2012, 2024, 2024, 2030, 2030),
    est_point = 100:107,
    p = seq(0.02, 0.09, by = 0.01),
    sig_noTrend_noComp = c(TRUE, TRUE, TRUE, rep(FALSE, 5)),
    years_Trend = c(1, 1, 1, 1, 2, 2, 2, 2),
    year_axis = c(1, 1, 2, 2, 3, 3, 4, 4)
  )

  vdiffr::expect_doppelganger(
    "Facetted Points",
    ggplot2::ggplot() +
      plot_points(df,
        point_values = "est_point",
        point_sig = "sig_noTrend_noComp",
        plot_lims = list(y_range = c(100, 107),
                         coords_diff = 7)
      ) +
      ggplot2::facet_grid(. ~ years_Trend, scales = "free_x")
  )
})
