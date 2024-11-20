years_lines <- list(c(2009, 2015), c(2015, 2022))
years_braces <- list(c(2009, 2015), c(2015, 2022))

plot_settings_expanded <- plotsettings_lineplot()
plot_settings_expanded$years_list <- lapply(list(years_lines, years_braces), prep_years)
names(plot_settings_expanded$years_list) <- c("years_lines", "years_braces")

preped_dat <- prep_lineplot(trend_3,
                            line_sig = "trend",
                            point_sig = "none",
                            brace_label_est = "trend",
                            brace_label_se = "trend",
                            brace_label_sig_high = "trend",
                            brace_label_sig_bold = "trend",
                            parameter = "mean",
                            years_lines = years_lines,
                            years_braces = years_braces,
                            plot_settings = plot_settings_expanded
                            )

test_that("output of data preperation has the expected format", {
  expect_true(checkmate::test_data_frame(preped_dat$plot_dat))
})

test_that("data preperation filters correct parameter", {
  expect_equal(unique(preped_dat$plot_dat$parameter), "mean")
})

test_that("correct significances are built", {
  expect_equal(preped_dat$plot_dat$line_sig, ifelse(preped_dat$plot_dat$p_comp < 0.05, TRUE, FALSE))
})


