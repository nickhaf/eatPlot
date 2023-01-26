test_that("calc_sig(): significance niveau is working correctly", {

  df <- calc_sig(data.frame("p" = c(0.02, 0.1, 0.01, 12.4)), sig_niveau = 0.03)

  expect_equal(colnames(df), c("p", "sig"))
  expect_equal(df$sig, factor(c(TRUE, FALSE, TRUE, FALSE)))

  df_2 <- calc_sig(data.frame("pvalue" = c(0.02, 0.1, 0.01, NA)), p_column = "pvalue")
  expect_equal(df_2$sig, factor(c(TRUE, FALSE, TRUE, NA)))
})


test_that("filter_string(): correct rows are filtered", {
  df <- data.frame(BL = c("Berlin", "Berlin", "Bremen", "Bremen"),
                   col1 = c("Berlin_a", "Berlin_b", "Bremen_a", "Bremen_b"))

  expect_equal(filter_strings(identifier = unique(df$BL), paste_vec = "_a", val_vec = df$col1), c(1, 3))
})
