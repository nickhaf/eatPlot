test_that("correct rows are filtered", {
  df_general_1 <- data.frame(
    group = c("a", "a.1.a", "a.2.a", "b"),
    TR_BUNDESLAND = c("a", "a", "a", "b"),
    my_grouping_var = c(NA, 1, 2, NA),
    kb = rep("best", 4),
    parameter = rep("mean", 4),
    comparison = c(NA, "vs", "vs", NA)
  )

  test_general_1 <- prep_general(df_general_1, grouping_var = "my_grouping_var", competence = "best")

  expect_equal(test_general_1[["bl_vs_bl"]]$group, c("a.1.a", "a.2.a"))
  expect_equal(test_general_1[["bl_point_estimates"]]$group, c("a", "b"))
  expect_equal(dim(test_general_1[["bl_vs_wholeGroup"]]), c(0, 6))
  expect_equal(dim(test_general_1[["wholeGroup_vs_wholeGroup"]]), c(0, 6))



  df_general_2 <- data.frame(
    group = c("a.wholeGroup", "1.wholeGroup", "a.2.a", "wholeGroup.vs.wholeGroup"),
    TR_BUNDESLAND = c("a", "a", "a", "b"),
    my_grouping_var = c(NA, 1, 2, NA),
    kb = rep("best", 4),
    parameter = rep("mean", 4),
    comparison = c("vs", "vs", "vs", "vs")
  )

  test_general_2 <- prep_general(df_general_2, grouping_var = "my_grouping_var", competence = "best")

  expect_equal(test_general_2[["bl_vs_wholeGroup"]]$group, "a.wholeGroup")
  expect_equal(test_general_2[["wholeGroup_vs_wholeGroup"]]$group, "1.wholeGroup")
  expect_equal(dim(test_general_2[["bl_point_estimates"]]), c(0, 6))
})

test_that("grouping_var is optional", {
  df_general <- data.frame(
    group = c("a.wholeGroup", "b.wholeGroup", "wholeGroup.vs.wholeGroup", "b"),
    TR_BUNDESLAND = c("a", "a", NA, "b"),
    kb = rep("best", 4),
    parameter = rep("mean", 4),
    comparison = c("vs", "vs", "vs", NA)
  )
test_general <- prep_general(df_general, competence = "best")
expect_equal(test_general[["wholeGroup_vs_wholeGroup"]]$group, "wholeGroup.vs.wholeGroup")
expect_equal(dim(test_general[["bl_vs_bl"]]), c(0, 5))

})
