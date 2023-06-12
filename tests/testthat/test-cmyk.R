test_that("rgb colours are correctly calculated from cmyk", {
  expect_equal(cmyk( 30,0,15,6), "#A8F0CC")
  expect_equal(cmyk(0, 0, 0, 100), "#000000" )
}
)
