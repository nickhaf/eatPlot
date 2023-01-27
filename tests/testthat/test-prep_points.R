test_that("data.frame is build correctly", {
  df <- data.frame("est_2012" = rep(12, 10),
                   "est_2015" = rep(15, 10),
                   "est_trend_2011.vs.2015" = rep(200, 10),
                   "p_2012" = rep(0.04, 10),
                   "p_2015" = rep(0.05, 10),
                   "kb" = rep("a", 10),
                   "grouping" = c(1, 0, 1, 0, 0, 1, 1, 0, 1, 0),
                   "TR_BUNDESLAND" = c("a", "a", "b", "b", NA, NA, "a", "a", "b", "b"),
                   "group" = c("a_1.vs.wholeGroup", "a_0.vs.wholeGroup", "b_1.vs.wholeGroup", "b_0.vs.wholeGroup", "0", "1", "a_1", "a_0", "b_1", "b_0"),
                   "parameter" = rep("mean", 10)
                   )
  test_prep_line <- prep_points(df, grouping_var = "grouping")

expect_equal(colnames(test_prep_line), c("group", "TR_BUNDESLAND", "grouping", "time", "est", "p", "sig"))
expect_equal(nrow(test_prep_line), 12)

})

