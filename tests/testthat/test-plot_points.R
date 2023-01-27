test_that("Pointplot is still the same", {

  df <- data.frame(grouping_var = rep(c("0","1"), 4),
                   time = c(2011, 2011, 2012, 2012, 2024, 2024, 2030, 2030),
                   est = 100:107,
                   p = seq(0.02, 0.09, by = 0.01),
                   sig = c(TRUE, TRUE, TRUE, rep(FALSE, 5) )
                 )

  vdiffr::expect_doppelganger("Bar plot", ggplot2::ggplot(df) +
    plot_points()
  )

})
