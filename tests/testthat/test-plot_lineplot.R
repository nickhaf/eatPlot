## Zuordnung über group-dataframe. Falls also eine Gruppe nicht geplottet werden soll,
## einfach hier rausnehmen.

trend_3$group <- subset(trend_3$group, trend_3$group$mhg %in% c("einET", "ersteGen"))
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


test_that("simple lineplot runs through", {

p <- plot_lineplot(trend_3_prepped,
    facets = "TR_BUNDESLAND",
    seperate_plot_var_box = "wholeGroup",
    title_superscripts = NULL
  )

vdiffr::expect_doppelganger("lineplot_trend", p)


  #save_plot(p, filename = "/home/nick/Downloads/test.pdf")
})

test_that("settings do something", {

  trend_3_settings <- prep_lineplot(
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
      axis_x_background_colour = "red",
      axis_x_background_width_y = 0.08,
      axis_x_label_centralize = 0.15,
      axis_x_label_nudge_y = 0.05,
      axis_x_label_size = 2.4,
      background_lines = FALSE,
      brace_label_gap_y = 0.15,
      brace_label_nudge_x = 0.3,
      brace_label_nudge_y = 0.08,
      brace_label_size = 3,
      brace_line_width = 0.8,
      brace_span_y = 0.16,
      grouping_colours = c("einET" = "blue", "ersteGen" = "green"),
      line_type = c("TRUE" = "dotted", "FALSE" = "F1"),
      line_width = 1,
      margin_bottom = 0.05,
      margin_left = 0.03,
      margin_right = 0.001,
      margin_top = 0.005,
      n_cols = 6,
      point_label_nudge = FALSE,
      point_label_nudge_direction = list("0" = "+", "1" = "-"),
      point_label_nudge_y = 0.1,
      point_label_size = 1,
      point_shapes = c("TRUE" = 2, "FALSE" = 10),
      point_size = 1,
      split_plot = FALSE,
      split_plot_gap_width = 0.01,
      axis_y = FALSE
    )
  )

  p_line <- plot_lineplot(
    trend_3_settings,
    facets = "TR_BUNDESLAND",
    seperate_plot_var_box = "wholeGroup",
    title_superscripts = NULL
  )

  vdiffr::expect_doppelganger("lineplot random settings", p_line)
})

test_that("box around state", {

  p <- plot_lineplot(trend_3_prepped,
                     facets = "TR_BUNDESLAND",
                     seperate_plot_var_box = "Berlin",
                     title_superscripts = NULL
  )

  vdiffr::expect_doppelganger("lineplot_trend", p)


  #save_plot(p, filename = "/home/nick/Downloads/test.pdf")
})


