test_that("position is calculated correctly", {
  expect_equal(calc_y_positions(states = 1:5, n_cols = 2), c(1, 4, 7))
  expect_equal(calc_y_positions(states = 1:4, n_cols = 2), c(1, 4))
})

