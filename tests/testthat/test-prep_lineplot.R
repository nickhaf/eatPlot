trend_3$group <- subset(trend_3$group, trend_3$group$mhg %in% c("einET", "ersteGen")) #| is.na(trend_3$group$mhg))
trend_3$group <- subset(trend_3$group, TR_BUNDESLAND == "Brandenburg") #| is.na(trend_3$group$mhg))
trend_3$group$TR_BUNDESLAND <- factor(trend_3$group$TR_BUNDESLAND,
                                      levels = unique(trend_3$group$TR_BUNDESLAND)[-1]
)


prepped_dat <- prep_lineplot(
  trend_3,
  subgroup_var = "mhg",
  parameter = "mean",
  line_sig = "trend",
  # line_se = "trend",
  years_lines = list(c(2009, 2015), c(2015, 2022)),
  years_braces = list(c(2009, 2015), c(2015, 2022)),
  brace_label_est = "trend",
  brace_label_se = "trend",
  brace_label_sig_high = "trend",
  brace_label_sig_bold = "trend",
  plot_settings = plotsettings_lineplot(
    split_plot = FALSE,
    default_list = lineplot_4x4
  )
)

test_that("output of data preperation has the expected format", {
  expect_true(checkmate::test_data_frame(prepped_dat$plot_dat))
})

test_that("data preperation filters correct parameter", {
  expect_equal(unique(prepped_dat$plot_dat$parameter), "mean")
})

test_that("correct significances are built", {
  expect_equal(prepped_dat$plot_dat$line_sig, ifelse(prepped_dat$plot_dat$p_comp < 0.05, TRUE, FALSE))
})


