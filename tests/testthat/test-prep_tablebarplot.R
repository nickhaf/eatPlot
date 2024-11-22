test_that("simple data preperation works", {

  dat_prep <- prep_tablebarplot(trend_2)
  dat_out <- data.frame(
    id = c("group_1", "group_2", "group_3", "group_4"),
    country = c("total", "countryB", "countryC", "countryA"),
    domain = c("reading", "reading", "reading", "reading"),
    depVar = c("score", "score", "score", "score"),
    parameter = c("mean", "mean", "mean", "mean"),
    est = c(522.668, 508.601, 534.234, 511.563),
    se = c(2.183, 4.671, 3.257, 3.505),
    p = c(0, 0, 0, 0),
    sig = c(TRUE, TRUE, TRUE, TRUE)
  )

  expect_equal(dat_prep, dat_out)

})

test_that("data preperation with comparions works", {

  dat_prep <- prep_tablebarplot(trend_3)
test <- dat_prep %>%
  dplyr::filter(mhg == c("einET - total", "total"), comparison %in% c("none"), parameter == "mean")
test <- test[grep(" - total", test$TR_BUNDESLAND, invert = TRUE), ]

## This has to become a function.
## But what to do, if there are two different CrossDiff types? -> gesonderte Aufbereitung?
test_wide <- test %>%
  select(TR_BUNDESLAND, year, comparison, est, p, sig) %>%
  pivot_wider(names_from = c(comparison, year), values_from = c(est, p, sig))


})
