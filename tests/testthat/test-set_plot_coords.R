test_that("coords are calculated correctly", {
  expect_equal(calc_y_value_coords(c(10, 100)), c(-17, 140.5))
  expect_equal(calc_y_value_coords(c(0, 1)), c(-0.30, 1.45))
})
