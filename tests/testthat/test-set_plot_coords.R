test_that("coords are calculated correctly", {
  expect_equal(calc_coords(c(10, 100)), c(-6.75, 119))
  expect_equal(calc_coords(c(0, 1)), c(-0.075, 10.1))
})
