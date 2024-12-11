trend_3$group <- subset(trend_3$group, trend_3$group$mhg %in% c("einET", "ersteGen")) #| is.na(trend_3$group$mhg))
trend_3$group <- subset(trend_3$group, TR_BUNDESLAND == "Brandenburg") #| is.na(trend_3$group$mhg))
trend_3$group$TR_BUNDESLAND <- factor(trend_3$group$TR_BUNDESLAND,
                                      levels = unique(trend_3$group$TR_BUNDESLAND)[-1]
)


trend_3_prepped <- prep_lineplot(
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


test_that("simple pointplot", {
  vdiffr::expect_doppelganger(
    "Simple pointplot",
    ggplot2::ggplot(trend_3_prepped$plot_dat,
                     mapping = ggplot2::aes(
                       x = year,
                       y = est_point,
                       group = id,
                       colour = .data$subgroup_var
                     ))  +
      plot_points(trend_3_prepped)
  )
})


