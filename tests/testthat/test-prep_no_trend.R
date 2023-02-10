test_that("fill column is build correctly", {
  expect_equal(
    calc_fill(data.frame(
      "sub_groups" = c("a", "b", NA, "a"),
      "sig" = c(1, 1, 2, 2)
    ),
    col_1 = "sub_groups",
    col_2 = "sig")$fill,
    c("a_1", "b_1", NA, "a_2")
  )
})


# test_that("correct rows are selected", {
#   df_raw <- data.frame(
#     "group" = c("a", "a", "b", "b", "a"),
#     "TR_BUNDESLAND" = c("a", "a", "b", "b", "a"),
#     "parameter" = c("mean", "sd", "mean", "sd", NA),
#     "comparison" = c("crossDiff", "crossDiff", NA, NA, "crossDiff"),
#     "my_grouping" = c("ohneAdj", "mitAdj", "ohneAdj", "mitAdj", "ohneAdj"),
#     "est" = 1:5,
#     "p" = c(0.02, 0.1, 0.01, 0.7, 0.1),
#     "kb" = c(rep("a", 4), "b")
#   )
#
#   test_no_trend <- prep_no_trend(dat = df_raw,
#                                  columns = "my_grouping",
#                                  competence = "a",
#                                  grouping_var = "my_grouping",
#                                  sig_niveau = 0.1)
#
#
#   expect_equal(test_no_trend[["plot_bar"]]$est_wholeGroup, c(1, NA, NA))
#   expect_equal(test_no_trend[["plot_table"]]$x_label, factor(c("ohneAdj", "ohneAdj", "y_label", "y_label"),
#                                                              levels = c("y_label", "ohneAdj"),
#                                                              ordered = TRUE)
#                )
#
#
# })
