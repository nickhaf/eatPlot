test_that("significance niveau is working correctly", {

  df <- prep_barplot(adjusted_means, subGroups = "adjust", sigNiveau = 0.02)

  expect_equal(df$p < 0.02, as.logical(df$sig))
  expect_false(isTRUE(all.equal(df$p < 0.05, as.logical(df$sig))))

})

