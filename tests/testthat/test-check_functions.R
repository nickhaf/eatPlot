example_list <- list(plain = 1, comparisons = 2, group = 3, estimate = 4)
example_list_2 <- list(wrong_name = 1, comparisons = 2, group = 3, estimate = 4)

test_that("facet checks work", {
  dat_unordered <- data.frame(unordered = c("c", "b", "c", "a"))
  dat_ordered <- data.frame(ordered = factor(c("a", "c", "c", "b"),
                                             levels = c("a", "c", "b"), ordered = TRUE))

  expect_equal(check_facets(dat_unordered, "unordered")[, "unordered"],
               factor(c("a", "b", "c", "c"), ordered = TRUE))
  expect_equal(check_facets(dat_ordered, "ordered"), dat_ordered)

})

test_that("plot data is eatRep output", {
  expect_no_error(check_eatRep_dat(example_list))
  expect_error(check_eatRep_dat(example_list[1:3]),
    regexp = "Assertion on 'plot_dat' failed: Must have length 4, but has length 3."
  )
  expect_error(check_eatRep_dat(example_list_2))
})


test_that("check_years works", {
  expect_no_error(check_years(c("2010_2011", "2011_2012"), c("2010_2011", "2011_2012"), c("2010_2011", "2011_2012")))
  expect_error(check_years(c("2010_2011", "2011_2012"), c("2010_2014", "2011_2012"), c("2010_2011", "2011_2012")), "Some of the trends you provided in 'years_lines' are not in the data.")
  expect_error(check_years(c("2010_2011", "2011_2012"), c("2010_2011", "2011_2012"), c("2010_2011", "2011_2013")), "Some of the trends you provided in 'years_braces' are not in the data.")
})
