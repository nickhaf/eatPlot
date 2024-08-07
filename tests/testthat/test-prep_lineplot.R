years_lines <- list(c(2009, 2015), c(2015, 2022))
years_braces <- list(c(2009, 2015), c(2015, 2022))

preped_dat <- prep_lineplot(trend_2, line_sig = "trend", parameter = "mean", years_lines = years_lines, years_braces = years_braces)

test_that("output of data preperation has the expected format", {
  expect_true(checkmate::test_data_frame(preped_dat))
})

test_that("data preperation filters correct parameter", {
  years_lines <- list(c(2009, 2015), c(2015, 2022))
  years_braces <- list(c(2009, 2015), c(2015, 2022))

  preped_dat <- prep_lineplot(trend_2, line_sig = "trend", parameter = "mean", years_lines = years_lines, years_braces = years_braces)

  expect_equal(unique(preped_dat$parameter), "mean")
})

test_that("correct significances are built", {
  expect_equal(preped_dat$line_sig, ifelse(preped_dat$p_comp < 0.05, TRUE, FALSE))
})
