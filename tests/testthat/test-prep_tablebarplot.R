test_that("simple data preperation works", {

  dat_prep <- prep_tablebarplot(trend_2)
  dat_out <- data.frame(
    label1 = c("countryA", "countryB", "countryC", "total"),
    label2 = c("country=countryA", "country=countryB", "country=countryC", "country=total"),
    country = c("countryA", "countryB", "countryC", "total"),
    parameter = c("mean", "mean", "mean", "mean"),
    modus = rep("JK2.mean__BIFIEsurvey", 4),
    depVar = c("score", "score", "score", "score"),
    comparison = rep("none", 4),
    domain = c("reading", "reading", "reading", "reading"),
    est = c(511.563, 508.601, 534.234, 522.668),
    p = c(0, 0, 0, 0),
    se = c( 3.505, 4.671, 3.257,  2.183),
    sig = c(TRUE, TRUE, TRUE, TRUE)
  )

  rownames(dat_prep) <- NULL
  rownames(dat_out) <- NULL

  expect_equal(dat_prep, dat_out)

})

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
