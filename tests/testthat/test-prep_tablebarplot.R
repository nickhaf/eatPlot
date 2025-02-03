test_that("simple data preperation works", {
  dat_prep <- prep_tablebarplot(trend_2, facet_var = "country", par = "mean")

  dat_out <- data.frame(
    state_var = c("countryA", "countryB", "countryC", "total"),
    subgroup_var = rep("total", 4),
    est_comparison_none_NA = c(511.563, 508.601, 534.234, 522.668),
    se_comparison_none_NA = c(3.505, 4.671, 3.257, 2.183),
    p_comparison_none_NA = c(0, 0, 0, 0),
    sig_comparison_none_NA = c(TRUE, TRUE, TRUE, TRUE),
    y_axis = 1:4
  )

  rownames(dat_prep) <- NULL
  rownames(dat_out) <- NULL

  expect_equal(dat_prep, dat_out)
})



mw_prepped <- prep_tablebarplot(trend_3,
                                subgroup_var = "mhg",
                                comparisons = NULL ## Problem: Beim Filtern braucht man ja teilweise sub-Comparisons. Die dürfen nicht entfernt werden.
)




# test_that("data preperation with comparions works", {

#
#   dat_prep <- prep_tablebarplot(trend_3)
# test <- dat_prep %>%
#   dplyr::filter(mhg == c("einET - total", "total"), comparison %in% c("none"), parameter == "mean")
# test <- test[grep(" - total", test$TR_BUNDESLAND, invert = TRUE), ]
#
# ## This has to become a function.
# ## But what to do, if there are two different CrossDiff types? -> gesonderte Aufbereitung?
# test_wide <- test %>%
#   select(TR_BUNDESLAND, year, comparison, est, p, sig) %>%
#   pivot_wider(names_from = c(comparison, year), values_from = c(est, p, sig))
#
#
# })
