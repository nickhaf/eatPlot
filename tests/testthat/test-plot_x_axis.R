test_that("x axis can be built on plot with relationaal distance x-axis", {
  test_df <- data.frame(
    est_points = c(3, 4, 10),
    year = c(2000, 2005, 2030),
    year_axis = c(2000, 2005, 2030),
    years_Trend = c(12, 12, 15)
  )

  vdiffr::expect_doppelganger(
    "x-axis with relational distance",
    ggplot2::ggplot(
      data = test_df,
      ggplot2::aes(x = year_axis, y = est_points)
    ) +
      ggplot2::geom_point() +
      plot_x_axis(test_df, y_range = c(3, 10))
  )
})

test_that("x axis can be built on plot with same distance x-axis", {
  test_df <- data.frame(
    est_points = c(3, 4, 10),
    year = c(2000, 2005, 2030),
    year_axis = c(1, 2, 3),
    years_Trend = c(12, 12, 15)
  )

  vdiffr::expect_doppelganger(
    "x-axis with same distance",
    ggplot2::ggplot(
      data = test_df,
      ggplot2::aes(x = year_axis, y = est_points)
    ) +
      ggplot2::geom_point() +
      plot_x_axis(test_df, y_range = c(3, 10))
  )
})


test_that("x axis can be built on facetted plot with relational distances", {
  test_df <- data.frame(
    est_points = c(3, 4, 4, 3),
    year = c(2000, 2005, 2005, 2020),
    year_axis = c(2000, 2005, 2005, 2020),
    years_Trend = c(11, 12)
  )

  vdiffr::expect_doppelganger(
    "Facetted x axis is plotted with relational distances",
    ggplot2::ggplot(
      data = test_df,
      ggplot2::aes(x = year_axis, y = est_points)
    ) +
      ggplot2::geom_point() +
      plot_x_axis(test_df,
        y_range = c(3, 4),
        plot_settings = plotsettings_lineplot(
          split_plot = TRUE,
          axis_x_label_centralize = 0.05
        )
      ) +
      ggplot2::facet_grid(. ~ years_Trend, scales = "free_x", space = "free_x")
  )
})


test_that("x axis can be built on facetted plot with equal distances", {
  test_df <- data.frame(
    est_points = c(3, 4, 4, 3),
    year = c(2000, 2005, 2005, 2020),
    year_axis = c(1, 2, 2, 3),
    years_Trend = c(11, 12)
  )

  vdiffr::expect_doppelganger(
    "Facetted x axis is plotted with equal distances",
    ggplot2::ggplot(
      data = test_df,
      ggplot2::aes(x = year_axis, y = est_points)
    ) +
      ggplot2::geom_point() +
      plot_x_axis(test_df,
        y_range = c(3, 4),
        plot_settings = plotsettings_lineplot(
          split_plot = TRUE,
          axis_x_label_centralize = 0.05
        )
      ) +
      ggplot2::facet_grid(. ~ years_Trend, scales = "free_x")
  )
})
