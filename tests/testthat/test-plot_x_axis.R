test_that("x axis can be built on plot", {
  test_df <- data.frame(
    est_points = c(3, 4),
    year = c(2000, 2005),
    trend = c(12, 12)
  )

  vdiffr::expect_doppelganger(
    "Own x axis is plotted",
    ggplot2::ggplot(
      data = test_df,
      ggplot2::aes(x = year, y = est_points)
    ) +
      ggplot2::geom_point() +
      plot_x_axis(test_df, y_range = c(3, 4))
  )
})

test_that("x axis can be built on facetted plot", {
  test_df <- data.frame(
    est_points = c(3, 4, 4, 3),
    year = c(2000, 2005, 2005, 2020),
    trend = c(11, 12)
  )

  vdiffr::expect_doppelganger(
    "Own x axis is plotted",
    ggplot2::ggplot(
      data = test_df,
      ggplot2::aes(x = year, y = est_points)
    ) +
      ggplot2::geom_point() +
      plot_x_axis(test_df, y_range = c(3, 4)) +
      ggplot2::facet_wrap(~trend, scales = "free_x")
  )
})
