test_that("fill column is build correctly", {

  df_raw_1 <- data.frame("parameter" = rep("mean", 4),
                       "comparison" = rep("crossDiff", 4),
                       "adjust" = c("ohneAdj", "mitAdj", NA, "mitAdj"),
                       "p" = c(0.02, 0.1, 0.01, 0.7)
  )

  df_raw_2 <- data.frame("parameter" = rep("mean", 4),
                         "comparison" = rep("crossDiff", 4),
                         "adjust" = c("ohneAdj", "mitAdj", "ohneAdj", "mitAdj"),
                         "p" = c(0.02, 0.1, 0.01, 0.7)
  )

  df_2 <- prep_barplot(df_raw_2, sub_groups = "adjust", sig_niveau = 0.1)

  expect_error(prep_barplot(df_raw_1, sub_groups = "adjust", sig_niveau = 0.1),
               "Your subgroups should not contain any missings. Please check your input data.",
               fixed = TRUE)


  expect_equal(df_2$fill, c("ohneAdj_TRUE", "mitAdj_FALSE", "ohneAdj_TRUE", "mitAdj_FALSE"))

})


test_that("correct rows are selected", {
  df_raw <- data.frame("parameter" = c("mean", "sd", "mean", "sd", NA),
                   "comparison" = c("crossDiff", "crossDiff", NA, NA, "crossDiff"),
                   "adjust" = c("ohneAdj", "mitAdj", "ohneAdj", "mitAdj", "ohneAdj"),
                   "p" = c(0.02, 0.1, 0.01, 0.7, 0.1)
  )

  df <- prep_barplot(df_raw, sub_groups = "adjust", sig_niveau = 0.1)

  expect_true(all(df$parameter == "mean") & all(df$comparison == "crossDiff"))


})



## Setup test dataset.

# data_prep_barplot <- data.frame("parameter" = c("mean", "sd", "sd", "mean"),
#                                 "modus" = c("JK2.mean__BIFIEsurvey", "JK2.mean__BIFIEsurvey", "JK2.mean__survey", "JK2.mean__survey"),
#                                 "depVar" = rep("bista", 4),
#                                 "comparison" = c("crossDiff", NA, NA, "crossDiff"),
#                                 "group" = c("Berlin", "Bayern", "wholeGroup", "Thueringen"),
#                                 "TR_BUNDESLAND" = c(NA, NA, "Bayern", "Berlin"),
#                                 "kb" = c("GL", "GL", "ortho", "ortho"),
#                                 "adjust" = c("ohneAdj", "mitAdj", "ohneAdj", "mitAdj"),
#                                 "es" = c(1, 0.2, 0.2, 0.1),
#                                 "est" = c(34, 12, 2, 14),
#                                 "p" = c(0.02, 0.1, 0.01, 0.05),
#                                 "se" = c(6.0, 5, 1, 2.1, 1.4)
#                                 )


