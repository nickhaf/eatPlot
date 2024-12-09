## Zuordnung über group-dataframe. Falls also eine Gruppe nicht geplottet werden soll,
## einfach hier rausnehmen.

trend_3$group <- subset(trend_3$group, trend_3$group$mhg %in% c("einET", "ersteGen")) #| is.na(trend_3$group$mhg))
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


test_that("x axis can be built on plot with relationaal distance x-axis", {

  vdiffr::expect_doppelganger(
    "x-axis with relational distance",
    ggplot2::ggplot(trend_3_prepped$plot_dat,
                    mapping = ggplot2::aes(
                      x = year,
                      y = est_point,
                      group = id,
                      colour = .data$subgroup_var
                    ))  +
      ggplot2::geom_point() +
      plot_x_axis(trend_3_prepped)
      )
})


