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


  ## Now, Guess I want to get the significance of the comparison of each Bundesland with
  ## mhg = einET vs. = Total in the same Bundesland, while plotting the point estimates as well.

  ## First step: unit_1 and unit_2 have to have the same TR_BUNDESLAND

  ## Vorher hatte ich für jedes Element eine eigene Spalte. Das war aber sehr durcheinander.

str(trend_3$comparisons)

  # Maybe filter plain for the comparison-Ids that I want to take into the data?:

test <- subset(trend_3$plain, mhg == "einET - total" & comparison == "crossDiff" & parameter == "mean")
test <- test[grep(" - total", test$TR_BUNDESLAND, invert = TRUE), ]
comp_ids <- test$id


View(test)


  dat_prep <- prep_tablebarplot(trend_3, comparisons = "")
})
