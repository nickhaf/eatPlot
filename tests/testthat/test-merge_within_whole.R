test_that("merging works correctly for BLs", {

  df_trend <- data.frame(
    state_var = rep("Brandenburg", 4),
    grouping_var = c(0, 0, 1, 1),
    year_start = rep(2011, 4),
    year_end = c(rep(2013, 4)),
    group = c("Brandenburg.0.vs.wholeGroup", "Brandenburg.0.vs.Brandenburg", "Brandenburg.1.vs.wholeGroup", "Brandenburg.1.vs.Brandenburg"),
    est = 1:4,
    compare_1 = rep("BL_groupingVar", 4),
    compare_2 = c("wholeGroup", "BL", "wholeGroup", "BL")
  )


  test_merge_df <- merge_within_whole(df_trend, BLs = c("Brandenburg"), compare_against = "wholeGroup")

  expect_equal(test_merge_df[["bl_vs_wholeGroup"]]$group, c("Brandenburg.0.vs.wholeGroup", "Brandenburg.1.vs.wholeGroup"))
  expect_equal(test_merge_df[["within_whole"]]$est_whole, c(1, 3))
  expect_equal(test_merge_df[["within_whole"]]$est_within, c(2, 4))

})

test_that("merging works correctly for wholeGroups", {

  df_trend <- data.frame(
    state_var = rep("wholeGroup", 2),
    grouping_var = c(0, 1),
    year_start = rep(2011, 2),
    year_end = c(rep(2013, 2)),
    group = c("0.vs.wholeGroup",  "1.vs.wholeGroup"),
    est = 1:2,
    compare_1 = c("_groupingVar", "_groupingVar"),
    compare_2 = c("wholeGroup", "wholeGroup")
  )


  test_merge <- merge_within_whole(df_trend, BLs = "")

  expect_equal(test_merge[["bl_vs_wholeGroup"]]$group, c("0.vs.wholeGroup", "1.vs.wholeGroup"))
  expect_equal(test_merge[["within_whole"]]$est_whole, c(1, 2))
  expect_equal(test_merge[["within_whole"]]$est_within, c(1, 2))

})
