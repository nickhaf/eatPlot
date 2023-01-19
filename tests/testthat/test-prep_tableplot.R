test_that("", {
  df <- data.frame(
    "parameter" = c("mean", "sd", "sd", "mean"),
    "modus" = c("JK2.mean__BIFIEsurvey", "JK2.mean__BIFIEsurvey", "JK2.mean__survey", "JK2.mean__survey"),
    "depVar" = rep("bista", 4),
    "comparison" = c("crossDiff", NA, NA, "crossDiff"),
    "group" = c("Berlin", "Bayern", "wholeGroup", "Thueringen"),
    "TR_BUNDESLAND" = c(NA, NA, "Bayern", "Berlin"),
    "kb" = c("GL", "GL", "ortho", "ortho"),
    "adjust" = c("ohneAdj", "mitAdj", "ohneAdj", "mitAdj"),
    "es" = c(1, 0.2, 0.2, 0.1),
    "est" = c(34, 12, 2, 14),
    "p" = c(0.02, 0.1, 0.01, 0.05),
    "se" = c(6.0, 5, 1, 2.1)
  )


  df_prep <- prep_tableplot(df, columns = "adjust", competence = "GL", sig_niveau = 0.05)

  expect_equal(colnames(df_prep), c("group", "value", "x_labels"))
})
