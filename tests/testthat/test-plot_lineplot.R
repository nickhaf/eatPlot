test_data_lineplot <- data.frame(
  id = c("comp_1", "comp_1"),
  state_var = c("Berlin", "Berlin"),
  subgroup = c("m", "m"),
  year = c(2000, 2005),
  trend = c("2000_2005", "2000_2005"),
  est_point = c(500, 560),
  sig_point = c(TRUE, FALSE),
  est_line = c(500, 560),
  sig_line = c(FALSE, FALSE)
)


test_that("simple lineplot with one group", {

  test_data_lineplot <- data.frame(
    id = c("comp_1", "comp_1"),
    state_var = c("Berlin", "Berlin"),
    subgroup = c("m", "m"),
    year = c(2000, 2005),
    trend = c("2000_2005", "2000_2005"),
    est_point = c(500, 560),
    sig_point = c(TRUE, FALSE),
    est_line = c(500, 560),
    sig_line = c(FALSE, FALSE)
  )


  p <- plot_lineplot(test_data_lineplot,
                     point_est = "est_point",
                     point_sig = "sig_point",
                     line_est = "est_line",
                     line_sig = "sig_line",
                     years_lines = list(c(2000, 2005)),
                     subgroup_var = "subgroup",
                     facet_var = "state_var")

vdiffr::expect_doppelganger("lineplot_oneGroup", p)

})


# test_that("settings do something", {
#
#   trend_3_settings <- prep_lineplot(
#     trend_3_prepped,
#     subgroup_var = "mhg",
#     line_sig = "trend",
#     background_level = "total", ## level of the facet-factor that is not plotted as facet, but as background-line.
#     background_group = NA,
#     # line_se = "trend",
#     years_lines = list(c(2009, 2015), c(2015, 2022)),
#     years_braces = list(c(2009, 2015), c(2015, 2022)),
#     brace_label_est = "trend",
#     brace_label_se = "trend",
#     brace_label_sig_high = "trend",
#     brace_label_sig_bold = "trend",
#     plot_settings = plotsettings_lineplot(
#       axis_x_background_colour = "red",
#       axis_x_background_width_y = 0.08,
#       axis_x_label_centralize = 0.15,
#       axis_x_label_nudge_y = 0.05,
#       axis_x_label_size = 2.4,
#       background_lines = FALSE,
#       brace_label_gap_y = 0.15,
#       brace_label_nudge_x = 0.3,
#       brace_label_nudge_y = 0.08,
#       brace_label_size = 3,
#       brace_line_width = 0.8,
#       brace_span_y = 0.16,
#       grouping_colours = c("einET" = "blue", "ersteGen" = "green"),
#       line_type = c("TRUE" = "dotted", "FALSE" = "F1"),
#       line_width = 1,
#       margin_bottom = 0.05,
#       margin_left = 0.03,
#       margin_right = 0.001,
#       margin_top = 0.005,
#       n_cols = 6,
#       point_label_nudge = FALSE,
#       point_label_nudge_direction = list("0" = "+", "1" = "-"),
#       point_label_nudge_y = 0.1,
#       point_label_size = 1,
#       point_shapes = c("TRUE" = 2, "FALSE" = 10),
#       point_size = 1,
#       split_plot = FALSE,
#       split_plot_gap_width = 0.01,
#       axis_y = FALSE
#     )
#
#   )
#
#   p_line <- plot_lineplot(
#     trend_3_settings,
#     seperate_plot_var_box = "wholeGroup",
#     title_superscripts = NULL
#   )
#
#   vdiffr::expect_doppelganger("lineplot random settings", p_line)
# })
#
# test_that("box around state", {
#
#   p <- plot_lineplot(trend_3_prepped,
#                      seperate_plot_var_box = "Berlin",
#                      title_superscripts = NULL
#   )
#
#   vdiffr::expect_doppelganger("lineplot_trend_box", p)
#
#
#   #save_plot(p, filename = "/home/nick/Downloads/test.pdf")
# })


