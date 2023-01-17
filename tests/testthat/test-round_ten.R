test_that("rounding works", {
  expect_equal(round_ten(c(-114, 12, 140, -211)), c(-220, 220))
  expect_equal(round_ten(c(12, -15, -17), accuracy = 5), c(-20, 20))
  expect_error(round_ten(c(1, "a", 4)))
})
