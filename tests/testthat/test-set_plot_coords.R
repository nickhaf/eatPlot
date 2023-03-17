test_that("coords are calculated correctly", {
  expect_equal(calc_coords(c(10, 100)), c(-9, 119))
  expect_equal(calc_coords(c(0, 1)), c(-0.1, 10.1))
})
