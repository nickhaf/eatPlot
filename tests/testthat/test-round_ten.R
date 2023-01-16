test_that("rounding works", {
  expect_equal(round_ten(12.5), 20)
  expect_equal(round_ten(-114), -120)
})
