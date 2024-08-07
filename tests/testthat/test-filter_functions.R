years_lines <- list(c(2009, 2015), c(2015, 2022))
years_braces <- list(c(2009, 2015), c(2015, 2022))

preped_dat <- prep_lineplot(trend_2, line_sig = "trend", parameter = "mean", years_lines = years_lines, years_braces = years_braces)


test_that("data preperation filters correct years", {
  expect_equal(unique(preped_dat$trend), c("2009_2015", "2015_2022"))
})
