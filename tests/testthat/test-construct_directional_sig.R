test_that("multiplication works", {
  df <- data.frame(
    est = c(-1, 1, 0, -2),
    sig = c(TRUE, TRUE, TRUE, FALSE)
  )

  expect_equal(construct_directional_sig(df, "est", "sig")$sig_directional_sig, c("below", "above", "no_sig", "no_sig"))
})
