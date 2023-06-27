test_that("position is calculated correctly", {
  expect_equal(calc_y_positions(states = 1:5, n_cols = 2), c(1, 4, 7))
  expect_equal(calc_y_positions(states = 1:4, n_cols = 2), c(1, 4))
})

test_that("y-axis is plotted correctly", {

  dat_y <- data.frame(
    x_axis_coords = 0,
    y_axis_coords = seq(0, 100, by = 10),
    y_axis_labels = seq(0, 100, by = 10)
  )


  ggplot2::ggplot() +
    draw_axis_y(dat_y, range_est = c(10,100), coords = c(0, 100), y_lims = c(-5, 110)) +
    ggplot2::theme_classic()


})
