test_that("significance niveau is working correctly", {

  expect_error(calc_sig(data.frame("p" = c(0.02, 0.1, 0.01, NA)), sig_niveau = 0.02),
               "Your p-values should not contain any missings. Please check your input data.",
               fixed = TRUE)

  df <- calc_sig(data.frame("p" = c(0.02, 0.1, 0.01, 12.4)), sig_niveau = 0.03)

  expect_equal(colnames(df), c("p", "sig"))
  expect_equal(df$sig, factor(c(TRUE, FALSE, TRUE, FALSE)))

  df_2 <- calc_sig(data.frame("pvalue" = c(0.02, 0.1, 0.01, 12.4)), sig_niveau = 0.05, p_column = "pvalue")
  expect_equal(df_2$sig, factor(c(TRUE, FALSE, TRUE, FALSE)))
})
