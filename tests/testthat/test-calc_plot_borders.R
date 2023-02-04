test_that("Plot bordres are calculated correctly", {
  expect_equal(calc_plot_borders(c(-114, 12, 140, -211)), c(-220, 220))
  expect_equal(calc_plot_borders(c(22, -15, -17), accuracy = 5), c(-25, 25))
  expect_error(calc_plot_borders(c(1, "a", 4)))
})

test_that("coords are calculated correctly", {
  expect_equal(calc_coords(c(10, 100)), c(0, 110))
  expect_equal(calc_coords(c(0, 1)), c(0, 10))
})
