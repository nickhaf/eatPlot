trend_3$group <- subset(trend_3$group, trend_3$group$mhg %in% c("einET", "ersteGen")) #| is.na(trend_3$group$mhg))
trend_3$group <- subset(trend_3$group, TR_BUNDESLAND == "Brandenburg") #| is.na(trend_3$group$mhg))



trend_3_prepped <- prep_lineplot(
  trend_3,
  subgroup_var = "mhg",
  parameter = "mean",
  line_sig = "trend",
  # line_se = "trend",
  background_level = "total", ## level of the facet-factor that is not plotted as facet, but as background-line.
  background_group = NA,
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


test_that("single_lineplot with one grouping_var is plotted correctly (same distances between years)", {

  vdiffr::expect_doppelganger(
    "single lineplot",
    ggplot2::ggplot(trend_3_prepped$plot_dat,
                    mapping = ggplot2::aes(
                      x = year,
                      y = est_point,
                      group = id,
                      colour = .data$subgroup_var
                    ))  +
      plot_single_lineplot(
trend_3_prepped
      )
  )
})


# test_that("single_lineplot with one grouping_var is plotted correctly (different distances between years)", {
#   test_plot <- list(
#     plot_points = data.frame(
#       state_var = rep("a", 3),
#       grouping_var = factor(rep("noGroup", 3), levels = "noGroup"),
#       year_axis = c(1, 2, 11),
#       year = c(2010, 2011, 2020),
#       point_values = c(200, 210, 220),
#       sig_noTrend_noComp = c(TRUE, FALSE, TRUE),
#       years_Trend = c(12, 12, 23),
#       competence_var = "a"
#     ),
#     plot_lines = data.frame(
#       state_var = rep("a", 2),
#       grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
#       year_start_axis = c(1, 2),
#       year_end_axis = c(2, 11),
#       year_start = c(2010, 2011),
#       year_end = c(2011, 2020),
#       est_noTrendStart_noComp = c(200, 210),
#       est_noTrendEnd_noComp = c(210, 220),
#       sig_trend = c(TRUE, FALSE),
#       years_Trend = c(12, 23),
#       competence_var = "a"
#     ),
#     plot_background_lines = data.frame(
#       state_var = rep("", 2),
#       grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
#       year_start_axis = c(1, 2),
#       year_end_axis = c(2, 11),
#       year_start = c(2010, 2011),
#       year_end = c(2011, 2020),
#       est_noTrendStart_noComp_wholeGroup = c(190, 225),
#       est_noTrendEnd_noComp_wholeGroup = c(225, 230),
#       sig_trend = c(TRUE, FALSE),
#       years_Trend = c(12, 23),
#       competence_var = "a"
#     ),
#     plot_braces = data.frame(
#       state_var = rep("a", 2),
#       grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
#       year_start_axis = c(1, 2),
#       year_end_axis = c(2, 11),
#       year_start = c(2010, 2011),
#       year_end = c(2011, 2020),
#       est_label = c(10, 40),
#       se_label = c(1, 4),
#       sig_label_1 = c(TRUE, FALSE),
#       sig_label_2 = c(FALSE, TRUE),
#       years_Trend = c(12, 23),
#       competence_var = "a"
#     )
#   )
#
#   vdiffr::expect_doppelganger(
#     "single lineplot with relational distances",
#     ggplot2::ggplot() +
#       plot_single_lineplot(
#         test_plot,
#         plot_lims = calc_plot_lims(test_plot,
#           "point_values",
#           c("est_noTrendStart_noComp", "est_noTrendEnd_noComp"),
#           plot_settings = plotsettings_lineplot()
#         ),
#         line_sig = "sig_trend",
#         label_est = "est_label",
#         label_se = "se_label",
#         label_sig_high = "sig_label_1",
#         label_sig_bold = "sig_label_2",
#         point_values = "point_values",
#         point_sig = "sig_noTrend_noComp"
#       )
#   )
# })


# test_that("split lineplot with no groups is plotted correctly with equal line length", {
#   test_plot_split <- list(
#     plot_points = data.frame(
#       state_var = rep("a", 3),
#       grouping_var = factor(c("group1", "group1", "group1", "group2", "group2", "group2"), levels = c("group1", "group2")),
#       year = rep(c(2010, 2011, 2020), 2),
#       year_axis = rep(c(1, 2, 3), 2),
#       point_values = c(200, 210, 220, 205, 215, 225),
#       sig_noTrend_noComp = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
#       years_Trend = c(12, 12, 23, 12, 12, 23),
#       competence_var = "a"
#     ),
#     plot_lines = data.frame(
#       state_var = rep("a", 4),
#       grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
#       year_start_axis = c(1, 1, 2, 2),
#       year_end_axis = c(2, 2, 3, 3),
#       year_start = c(2010, 2010, 2011, 2011),
#       year_end = c(2011, 2011, 2020, 2020),
#       est_noTrendStart_noComp = c(200, 205, 210, 215),
#       est_noTrendEnd_noComp = c(210, 215, 220, 225),
#       sig_trend = c(TRUE, FALSE, FALSE, TRUE),
#       years_Trend = c(12, 12, 23, 23),
#       competence_var = "a"
#     ),
#     plot_background_lines = data.frame(
#       state_var = rep("", 2),
#       grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
#       year_start_axis = c(1, 2),
#       year_end_axis = c(2, 3),
#       year_start = c(2010, 2011),
#       year_end = c(2011, 2020),
#       est_noTrendStart_noComp_wholeGroup = c(190, 225),
#       est_noTrendEnd_noComp_wholeGroup = c(225, 230),
#       sig_trend = c(TRUE, FALSE),
#       years_Trend = c(12, 23),
#       competence_var = "a"
#     ),
#     plot_braces = data.frame(
#       state_var = rep("a", 4),
#       grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
#       year_start_axis = c(1, 1, 2, 2),
#       year_end_axis = c(2, 2, 3, 3),
#       year_start = c(2010, 2010, 2011, 2011),
#       year_end = c(2011, 2011, 2020, 2020),
#       est_label = c(10, 20, 30, 40),
#       se_label = c(1, 2, 3, 4),
#       sig_label_1 = c(TRUE, FALSE, FALSE, TRUE),
#       sig_label_2 = c(FALSE, TRUE, FALSE, TRUE),
#       years_Trend = c(12, 12, 23, 23),
#       competence_var = "a"
#     )
#   )
#
#   vdiffr::expect_doppelganger(
#     "splitlineplot equal distances",
#     ggplot2::ggplot() +
#       plot_single_lineplot(test_plot_split,
#         plot_lims = calc_plot_lims(test_plot_split, "point_values", c("est_noTrendStart_noComp", "est_noTrendEnd_noComp"), plot_settings = plotsettings_lineplot()),
#         line_sig = "sig_trend",
#         label_est = "est_label",
#         label_se = "se_label",
#         label_sig_high = "sig_label_1",
#         label_sig_bold = "sig_label_2",
#         point_values = "point_values",
#         plot_settings = plotsettings_lineplot(
#           split_plot = TRUE,
#           equal_trend_line_length = TRUE,
#           axis_x_label_centralize = 0.05,
#           point_label_nudge_x = 0.02
#         )
#       )
#   )
# })
#
#
# test_that("split lineplot with no groups is plotted correctly with relational line length", {
#   test_plot_split <- list(
#     plot_points = data.frame(
#       state_var = "a",
#       grouping_var = factor(rep(c("group1", "group2"), 4), levels = c("group1", "group2")),
#       year = c(rep(c(2010, 2011), 2), rep(c(2011, 2020), 2)),
#       year_axis = c(rep(c(1, 2), 2), rep(c(2, 11), 2)),
#       point_values = c(200, 210, 212, 220, 205, 220, 215, 225),
#       sig_noTrend_noComp = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE),
#       years_Trend = c(12, 12, 12, 12, 23, 23, 23, 23),
#       competence_var = "a"
#     ),
#     plot_lines = data.frame(
#       state_var = rep("a", 4),
#       grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
#       year_start_axis = c(1, 1, 2, 2),
#       year_end_axis = c(2, 2, 11, 11),
#       year_start = c(2010, 2010, 2011, 2011),
#       year_end = c(2011, 2011, 2020, 2020),
#       est_noTrendStart_noComp = c(200, 205, 210, 215),
#       est_noTrendEnd_noComp = c(210, 215, 220, 225),
#       sig_trend = c(TRUE, FALSE, FALSE, TRUE),
#       years_Trend = c(12, 12, 23, 23),
#       competence_var = "a"
#     ),
#     plot_background_lines = data.frame(
#       state_var = rep("", 2),
#       grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
#       year_start_axis = c(1, 2),
#       year_end_axis = c(2, 11),
#       year_start = c(2010, 2011),
#       year_end = c(2011, 2020),
#       est_noTrendStart_noComp_wholeGroup = c(190, 225),
#       est_noTrendEnd_noComp_wholeGroup = c(225, 230),
#       sig_trend = c(TRUE, FALSE),
#       years_Trend = c(12, 23),
#       competence_var = "a"
#     ),
#     plot_braces = data.frame(
#       state_var = rep("a", 4),
#       grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
#       year_start_axis = c(1, 1, 2, 2),
#       year_end_axis = c(2, 2, 11, 11),
#       year_start = c(2010, 2010, 2011, 2011),
#       year_end = c(2011, 2011, 2020, 2020),
#       est_label = c(10, 20, 30, 40),
#       se_label = c(1, 2, 3, 4),
#       sig_label_1 = c(TRUE, FALSE, FALSE, TRUE),
#       sig_label_2 = c(FALSE, TRUE, FALSE, TRUE),
#       years_Trend = c(12, 12, 23, 23),
#       competence_var = "a"
#     )
#   )
#
#   vdiffr::expect_doppelganger(
#     "splitlineplot relational distances",
#     ggplot2::ggplot() +
#       plot_single_lineplot(test_plot_split,
#         plot_lims = calc_plot_lims(test_plot_split,
#           "point_values",
#           c("est_noTrendStart_noComp", "est_noTrendEnd_noComp"),
#           plot_settings = plotsettings_lineplot()
#         ),
#         line_sig = "sig_trend",
#         label_est = "est_label",
#         label_se = "se_label",
#         label_sig_high = "sig_label_1",
#         label_sig_bold = "sig_label_2",
#         point_values = "point_values",
#         plot_settings = plotsettings_lineplot(
#           split_plot = TRUE,
#           equal_trend_line_length = FALSE,
#           axis_x_label_centralize = 0.02,
#           point_label_nudge_x = 0.02
#         )
#       )
#   )
# })
