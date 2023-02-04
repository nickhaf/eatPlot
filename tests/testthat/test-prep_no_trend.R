test_that("fill column is build correctly", {
  expect_error(
    calc_fill(data.frame(
      "sub_groups" = c("ohneAdj", "mitAdj", NA, "mitAdj"),
      "sig" = c(TRUE, FALSE, TRUE, FALSE)
    )),
    "Your subgroups should not contain any missings. Please check your input data.",
    fixed = TRUE
  )


  df <- data.frame(
    "grouping_var" = c("ohneAdj", "mitAdj", "ohneAdj", "mitAdj"),
    "sig" = c(TRUE, FALSE, TRUE, FALSE)
  )

  expect_equal(calc_fill(df)$fill, c("ohneAdj_TRUE", "mitAdj_FALSE", "ohneAdj_TRUE", "mitAdj_FALSE"))
})


test_that("correct rows are selected", {
  df_raw <- data.frame(
    "group" = c("a", "a", "b", "b", "a"),
    "TR_BUNDESLAND" = c("a", "a", "b", "b", "a"),
    "parameter" = c("mean", "sd", "mean", "sd", NA),
    "comparison" = c("crossDiff", "crossDiff", NA, NA, "crossDiff"),
    "my_grouping" = c("ohneAdj", "mitAdj", "ohneAdj", "mitAdj", "ohneAdj"),
    "est" = 1:5,
    "p" = c(0.02, 0.1, 0.01, 0.7, 0.1),
    "kb" = c(rep("a", 4), "b")
  )

  test_no_trend <- prep_no_trend(df_raw, grouping_var = "my_grouping", columns = "my_grouping", competence = "a", sig_niveau = 0.1)


  expect_equal(test_no_trend[["plot_bar"]]$est_wholeGroup, c(1, NA))
  expect_equal(test_no_trend[["plot_table"]]$x_label, factor(c("ohneAdj", "ohneAdj", "y_label", "y_label"),
                                                             levels = c("y_label", "ohneAdj"),
                                                             ordered = TRUE)
               )


})
