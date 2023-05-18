test_that("single_lineplot with one grouping_var is plotted correctly", {
  test_plot <- list(
    plot_points = data.frame(
      state_var = rep("a", 3),
      grouping_var = factor(rep("noGroup", 3), levels = "noGroup"),
      year = c(1, 2, 3),
      point_values = c(200, 210, 220),
      sig_noTrend_noComp = c(TRUE, FALSE, TRUE),
      years_Trend = c(12, 12, 23),
      competence_var = "a"
    ),
    plot_lines = data.frame(
      state_var = rep("a", 2),
      grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
      year_start = c(1, 2),
      year_end = c(2, 3),
      est_noTrendStart_noComp = c(200, 210),
      est_noTrendEnd_noComp = c(210, 220),
      sig_trend = c(TRUE, FALSE),
      years_Trend = c(12, 23),
      competence_var = "a"
    ),
    plot_background_lines = data.frame(
      state_var = rep("", 2),
      grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
      year_start = c(1, 2),
      year_end = c(2, 3),
      est_noTrendStart_noComp_wholeGroup = c(190, 225),
      est_noTrendEnd_noComp_wholeGroup = c(225, 230),
      sig_trend = c(TRUE, FALSE),
      years_Trend = c(12, 23),
      competence_var = "a"
    ),
    plot_braces = data.frame(
      state_var = rep("a", 2),
      grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
      year_start = c(1, 2),
      year_end = c(2, 3),
      est_label = c(10, 40),
      se_label = c(1, 4),
      sig_label_1 = c(TRUE, FALSE),
      sig_label_2 = c(FALSE, TRUE),
      years_Trend = c(12, 23),
      competence_var = "a"
    )
  )

  vdiffr::expect_doppelganger(
    "single lineplot",
    ggplot2::ggplot() +
      plot_single_lineplot(
        test_plot,
        y_range = c(180, 240),
        line_sig = "sig_trend",
        label_est = "est_label",
        label_se = "se_label",
        label_sig_high = "sig_label_1",
        label_sig_bold = "sig_label_2",
        point_values = "point_values",
        point_sig = "sig_noTrend_noComp"
      )
  )
})

test_that("single_lineplot with two groups is plotted correctly", {
  test_plot_2 <- list(
    plot_points = data.frame(
      state_var = rep("a", 3),
      grouping_var = factor(c("group1", "group1", "group1", "group2", "group2", "group2"), levels = c("group1", "group2")),
      year = rep(c(1, 2, 3), 2),
      point_values = c(200, 210, 220, 205, 215, 225),
      sig_noTrend_noComp = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
      years_Trend = c(12, 12, 23, 12, 12, 23),
      competence_var = "a"
    ),
    plot_lines = data.frame(
      state_var = rep("a", 4),
      grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
      year_start = c(1, 1, 2, 2),
      year_end = c(2, 2, 3, 3),
      est_noTrendStart_noComp = c(200, 205, 210, 215),
      est_noTrendEnd_noComp = c(210, 215, 220, 225),
      sig_trend = c(TRUE, FALSE, FALSE, TRUE),
      years_Trend = c(12, 12, 23, 23),
      competence_var = "a"
    ),
    plot_background_lines = data.frame(
      state_var = rep("", 2),
      grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
      year_start = c(1, 2),
      year_end = c(2, 3),
      est_noTrendStart_noComp_wholeGroup = c(190, 225),
      est_noTrendEnd_noComp_wholeGroup = c(225, 230),
      sig_trend = c(TRUE, FALSE),
      years_Trend = c(12, 23),
      competence_var = "a"
    ),
    plot_braces = data.frame(
      state_var = rep("a", 4),
      grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
      year_start = c(1, 1, 2, 2),
      year_end = c(2, 2, 3, 3),
      est_label = c(10, 20, 30, 40),
      se_label = c(1, 2, 3, 4),
      sig_label_1 = c(TRUE, FALSE, FALSE, TRUE),
      sig_label_2 = c(FALSE, TRUE, FALSE, TRUE),
      years_Trend = c(12, 12, 23, 23),
      competence_var = "a"
    )
  )

  vdiffr::expect_doppelganger(
    "single grouped lineplot",
    ggplot2::ggplot() +
      plot_single_lineplot(test_plot_2,
        y_range = c(180, 240),
        line_sig = "sig_trend",
        label_est = "est_label",
        label_se = "se_label",
        label_sig_high = "sig_label_1",
        label_sig_bold = "sig_label_2",
        point_values = "point_values",
        plot_settings = plotsettings_lineplot(split_plot = FALSE)
      )
  )
})

test_that("split lineplot with no groups is plotted correctly", {
  test_plot_split <- list(
    plot_points = data.frame(
      state_var = rep("a", 3),
      grouping_var = factor(c("group1", "group1", "group1", "group2", "group2", "group2"), levels = c("group1", "group2")),
      year = rep(c(1, 2, 3), 2),
      point_values = c(200, 210, 220, 205, 215, 225),
      sig_noTrend_noComp = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
      years_Trend = c(12, 12, 23, 12, 12, 23),
      competence_var = "a"
    ),
    plot_lines = data.frame(
      state_var = rep("a", 4),
      grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
      year_start = c(1, 1, 2, 2),
      year_end = c(2, 2, 3, 3),
      est_noTrendStart_noComp = c(200, 205, 210, 215),
      est_noTrendEnd_noComp = c(210, 215, 220, 225),
      sig_trend = c(TRUE, FALSE, FALSE, TRUE),
      years_Trend = c(12, 12, 23, 23),
      competence_var = "a"
    ),
    plot_background_lines = data.frame(
      state_var = rep("", 2),
      grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
      year_start = c(1, 2),
      year_end = c(2, 3),
      est_noTrendStart_noComp_wholeGroup = c(190, 225),
      est_noTrendEnd_noComp_wholeGroup = c(225, 230),
      sig_trend = c(TRUE, FALSE),
      years_Trend = c(12, 23),
      competence_var = "a"
    ),
    plot_braces = data.frame(
      state_var = rep("a", 4),
      grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
      year_start = c(1, 1, 2, 2),
      year_end = c(2, 2, 3, 3),
      est_label = c(10, 20, 30, 40),
      se_label = c(1, 2, 3, 4),
      sig_label_1 = c(TRUE, FALSE, FALSE, TRUE),
      sig_label_2 = c(FALSE, TRUE, FALSE, TRUE),
      years_Trend = c(12, 12, 23, 23),
      competence_var = "a"
    )
  )

  vdiffr::expect_doppelganger(
    "splitlineplot",
    ggplot2::ggplot() +
      plot_single_lineplot(test_plot_split,
        y_range = c(180, 230),
        line_sig = "sig_trend",
        label_est = "est_label",
        label_se = "se_label",
        label_sig_high = "sig_label_1",
        label_sig_bold = "sig_label_2",
        point_values = "point_values",
        plot_settings = plotsettings_lineplot(
          split_plot = TRUE,
          axis_x_label_centralize = 0.05,
          point_label_nudge_x = 0.02
        )
      )
  )
})
