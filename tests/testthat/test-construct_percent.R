test_that("percent construction works", {
  df <- data.frame("col_1" = c(1, 2),
          "col_2" = c("a", "b"),
          "col_3" = c("a", "b"))

  expect_equal(construct_percent(df, columns = c("col_1", "col_2"))$col_1, c(100, 200))
  expect_equal(construct_percent(df, columns = c("col_1", "col_2"))$col_2, c("a", "b"))

})
