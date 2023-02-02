# test_that("correct rows are filtered", {
#   df_general_1 <- data.frame(
#     group = c("a", "a.1.a", "a.2.a", "b"),
#     TR_BUNDESLAND = c("a", "a", "a", "b"),
#     my_grouping_var = c(NA, 1, 2, NA),
#     kb = rep("best", 4),
#     parameter = rep("mean", 4),
#     comparison = c(NA, "vs", "vs", NA)
#   )
#
#   test_general_1 <- prep_general(df_general_1, grouping_var = "my_grouping_var", competence = "best")
#
#   expect_equal(test_general_1[["bl_vs_bl"]]$group, c("a.1.a", "a.2.a"))
#   expect_equal(test_general_1[["bl_point_estimates"]]$group, c("a", "b"))
#   expect_equal(dim(test_general_1[["bl_vs_wholeGroup"]]), c(0, 6))
#   expect_equal(dim(test_general_1[["wholeGroup_vs_wholeGroup"]]), c(0, 6))
#
#
#
#   df_general_2 <- data.frame(
#     group = c("a.wholeGroup", "1.wholeGroup", "a.2.a", "wholeGroup.vs.wholeGroup"),
#     TR_BUNDESLAND = c("a", "a", "a", "b"),
#     my_grouping_var = c(NA, 1, 2, NA),
#     kb = rep("best", 4),
#     parameter = rep("mean", 4),
#     comparison = c("vs", "vs", "vs", "vs")
#   )
#
#   test_general_2 <- prep_general(df_general_2, grouping_var = "my_grouping_var", competence = "best")
#
#   expect_equal(test_general_2[["bl_vs_wholeGroup"]]$group, "a.wholeGroup")
#   expect_equal(test_general_2[["wholeGroup_vs_wholeGroup"]]$group, "1.wholeGroup")
#   expect_equal(dim(test_general_2[["bl_point_estimates"]]), c(0, 6))
# })


test_that("significance column is added", {
  prepped_list <- list(a = data.frame(p_1 = c(0.04, 0.05)),
                       b = data.frame(p_asdf = c(0.01, 0.04))
  )

  prepped_list_sig <- add_sig_col(prepped_list, sig_niveau = 0.045)
  expect_equal(prepped_list_sig[[1]]$sig, c(TRUE, FALSE))
  expect_true(all(prepped_list_sig[[2]]$sig))
})

test_that("competence and grouping_var is optional", {
  df_general <- data.frame(
    group = c("a.wholeGroup", "b.wholeGroup", "wholeGroup.vs.wholeGroup", "b"),
    grouping_var = rep(NA, 4),
    TR_BUNDESLAND = c("a", "a", NA, "b"),
    kb = rep("best", 4),
    parameter = rep("mean", 4),
    comparison = c("vs", "vs", "vs", NA),
    est.1 = 1:4,
    est.2 = 1:4,
    p.1 = 1:4,
    p.2 = 1:4,
    est.1_trend = 1:4,
    se.1_trend = 1:4,
    es.1_trend = 1:4,
    p.1_trend = 1:4,
    est.2_trend = 1:4,
    se.2_trend = 1:4,
    es.2_trend = 1:4,
    p.2_trend = 1:4
  )

  test_general <- prep_general(df_general, competence = "best")
expect_equal(test_general[["point_data"]]$grouping_var, rep("noGroup", 2))
expect_equal(dim(test_general[["trend_data"]]), NULL)


})

test_that("point estimates are optional", {
  df_general <- data.frame(
    group = c("a.wholeGroup", "b.wholeGroup", "wholeGroup.vs.wholeGroup", "b"),
    grouping_var = rep(NA, 4),
    TR_BUNDESLAND = c("a", "a", NA, "b"),
    kb = rep("best", 4),
    parameter = rep("mean", 4),
    comparison = c("crossDiff", "crossDiff", "crossDifff", "crossDiff"),
    est_1 = 1:4,
    est_2 = 1:4,
    p_1 = 1:4,
    p_2 = 1:4,
    est_trend_1vs2 = 1:4,
    se_trend_1vs2 = 1:4,
    es_trend_1vs2 = 1:4,
    p_trend_1vs2 = 1:4,
    est_trend_2vs3 = 1:4,
    se_trend_2vs3 = 1:4,
    es_trend_2vs3 = 1:4,
    p_trend_2vs3 = 1:4
  )

  test_general <- prep_general(df_general, competence = "best")

  expect_equal(dim(test_general[["point_data"]]), NULL)



})

test_that("list ist build correctly without grouping_var", {
  df_general <- data.frame(
    group = c("a.wholeGroup", "b.wholeGroup", "wholeGroup.vs.wholeGroup", "b"),
    grouping_var = rep(NA, 4),
    TR_BUNDESLAND = c("a", "a", NA, "b"),
    kb = rep("best", 4),
    parameter = rep("mean", 4),
    comparison = c(NA, NA, "crossDiff", "crossDiff"),
    est_1 = 1:4,
    est_2 = 1:4,
    p_1 = 1:4,
    p_2 = 1:4,
    est_trend_1vs2 = 1:4,
    se_trend_1vs2 = 1:4,
    es_trend_1vs2 = 1:4,
    p_trend_1vs2 = 1:4,
    est_trend_2vs3 = 1:4,
    se_trend_2vs3 = 1:4,
    es_trend_2vs3 = 1:4,
    p_trend_2vs3 = 1:4
  )


  test_general <- prep_general(df_general, competence = "best")

  expect_equal(test_general[["point_data"]]$year, c(1,1,2,2))
  expect_equal(test_general[["point_data"]]$est_point, c(1,2,1,2))
  expect_equal(test_general[["trend_data"]]$year_start, c(1,1, 2, 2))
  expect_equal(test_general[["trend_data"]]$year_end, c(2, 2, 3, 3))
  expect_equal(test_general[["trend_data"]]$est_trend, c(3, 4, 3, 4))
})

test_that("list ist build correctly with grouping_var", {
  df_general <- data.frame(
    group = c("a.0.wholeGroup", "a.1.wholeGroup", "b.0", "b.1"),
    grouping_var = c(0, 1, 0, 1),
    TR_BUNDESLAND = c("a", "a", "b", "b"),
    kb = rep("best", 4),
    parameter = rep("mean", 4),
    comparison = c(NA, NA, "crossDiff", "crossDiff"),
    est_1 = 1:4,
    est_2 = 1:4,
    p_1 = 1:4,
    p_2 = 1:4,
    est_trend_1vs2 = 1:4,
    se_trend_1vs2 = 1:4,
    es_trend_1vs2 = 1:4,
    p_trend_1vs2 = 1:4,
    est_trend_2vs3 = 1:4,
    se_trend_2vs3 = 1:4,
    es_trend_2vs3 = 1:4,
    p_trend_2vs3 = 1:4
  )


  test_general <- prep_general(df_general, competence = "best", grouping_var = "grouping_var")

  expect_equal(test_general[["point_data"]]$year, c(1,1,2,2))
  expect_equal(test_general[["point_data"]]$est_point, c(1,2,1,2))
  expect_equal(test_general[["trend_data"]]$year_start, c(1,1, 2, 2))
  expect_equal(test_general[["trend_data"]]$year_end, c(2, 2, 3, 3))
  expect_equal(test_general[["trend_data"]]$est_trend, c(3, 4, 3, 4))
})



