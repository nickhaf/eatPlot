test_that("single_lineplot with one grouping_var is plotted correctly", {
  test_plot <- list(
    plot_points = data.frame(
      state_var = rep("a", 3),
      grouping_var = factor(rep("noGroup", 3), levels = "noGroup"),
      year = c(1, 2, 3),
      est_point = c(200, 210, 220),
      sig_point = c(TRUE, FALSE, TRUE)
    ),
    plot_lines = data.frame(
      state_var = rep("a", 2),
      grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
      year_start = c(1, 2),
      year_end = c(2, 3),
      est_point_start = c(200, 210),
      est_point_end = c(210, 220),
      sig_trend = c(TRUE, FALSE)
    ),
    plot_background_lines = data.frame(
      state_var = rep("", 2),
      grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
      year_start = c(1, 2),
      year_end = c(2, 3),
      est_point_start = c(190, 225),
      est_point_end = c(225, 230),
      sig_trend = c(TRUE, FALSE)
    ),
    plot_braces = data.frame(
      state_var = rep("a", 2),
      grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
      year_start = c(1, 2),
      year_end = c(2, 3),
      est_label = c(10, 40),
      se_label = c(1, 4),
      sig_label_1 = c(TRUE, FALSE),
      sig_label_2 = c(FALSE, TRUE)
    )
  )

  vdiffr::expect_doppelganger(
    "single lineplot",
    ggplot2::ggplot() +
      plot_single_lineplot(test_plot,
        y_range = c(180, 240),
        line_sig = "sig_trend",
        label_est = "est_label",
        label_se = "se_label",
        label_sig_high = "sig_label_1",
        label_sig_bold = "sig_label_2"
      )
  )
})


test_that("single_lineplot with two groups is plotted correctly", {
  test_plot_2 <- list(
    plot_points = data.frame(
      state_var = rep("a", 3),
      grouping_var = factor(c("group1", "group1", "group1", "group2", "group2", "group2"), levels = c("group1", "group2")),
      year = rep(c(1, 2, 3), 2),
      est_point = c(200, 210, 220, 205, 215, 225),
      sig_point = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
    ),
    plot_lines = data.frame(
      state_var = rep("a", 4),
      grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
      year_start = c(1, 1, 2, 2),
      year_end = c(2, 2, 3, 3),
      est_point_start = c(200, 205, 210, 215),
      est_point_end = c(210, 215, 220, 225),
      sig_trend = c(TRUE, FALSE, FALSE, TRUE)
    ),
    plot_background_lines = data.frame(
      state_var = rep("", 2),
      grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
      year_start = c(1, 2),
      year_end = c(2, 3),
      est_point_start = c(190, 225),
      est_point_end = c(225, 230),
      sig_trend = c(TRUE, FALSE)
    ),
    plot_braces = data.frame(
      state_var = rep("a", 4),
      grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
      year_start = c(1, 1, 2, 2),
      year_end = c(2, 2, 3, 3),
      est_label = c(10, 20, 30, 40),
      se_label = c(1, 2, 3, 4),
      sig_label_1 = c(TRUE, FALSE, FALSE, TRUE),
      sig_label_2 = c(FALSE, TRUE, FALSE, TRUE)
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
        label_sig_bold = "sig_label_2"
      )
  )
})


test_that("split plots are plotted correctly", {
  test_plot_l <- list(
    plot_points = data.frame(
      state_var = rep("a", 4),
      grouping_var = factor(c("group1", "group1", "group2", "group2"), levels = c("group1", "group2")),
      year = rep(c(1, 2), 2),
      est_point = c(200, 210, 220, 205),
      sig_point = c(TRUE, FALSE, TRUE, FALSE)
    ),
    plot_lines = data.frame(
      state_var = rep("a", 2),
      grouping_var = factor(c("group1", "group2"), levels = c("group1", "group2")),
      year_start = c(1, 1),
      year_end = c(2, 2),
      est_point_start = c(200, 205),
      est_point_end = c(210, 215),
      sig_trend = c(TRUE, FALSE)
    ),
    plot_background_lines = data.frame(
      state_var = rep("", 1),
      grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
      year_start = c(1),
      year_end = c(2),
      est_point_start = c(190),
      est_point_end = c(225),
      sig_trend = c(TRUE)
    ),
    plot_braces = data.frame(
      state_var = rep("a", 2),
      grouping_var = factor(c("group1", "group2"), levels = c("group1", "group2")),
      year_start = c(1, 1),
      year_end = c(2, 2),
      est_label = c(10, 20),
      se_label = c(1, 2),
      sig_label_1 = c(TRUE, FALSE),
      sig_label_2 = c(FALSE, TRUE)
    )
  )

  test_plot_r <- list(
    plot_points = data.frame(
      state_var = rep("a", 4),
      grouping_var = factor(c("group1", "group1", "group2", "group2"), levels = c("group1", "group2")),
      year = rep(c(2, 3), 2),
      est_point = c(210, 215, 220, 225),
      sig_point = c(TRUE, FALSE, TRUE, FALSE)
    ),
    plot_lines = data.frame(
      state_var = rep("a", 2),
      grouping_var = factor(c("group1", "group2"), levels = c("group1", "group2")),
      year_start = c(2, 2),
      year_end = c(3, 3),
      est_point_start = c(210, 220),
      est_point_end = c(215, 225),
      sig_trend = c(TRUE, FALSE)
    ),
    plot_background_lines = data.frame(
      state_var = rep("", 1),
      grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
      year_start = c(2),
      year_end = c(3),
      est_point_start = c(230),
      est_point_end = c(235),
      sig_trend = c(TRUE)
    ),
    plot_braces = data.frame(
      state_var = rep("a", 2),
      grouping_var = factor(c("group1", "group2"), levels = c("group1", "group2")),
      year_start = c(2, 2),
      year_end = c(3, 3),
      est_label = c(100, 200),
      se_label = c(10, 20),
      sig_label_1 = c(TRUE, FALSE),
      sig_label_2 = c(FALSE, TRUE)
    )
  )

  vdiffr::expect_doppelganger(
    "split grouped lineplot",
    plot_split_lineplot(
      left_plot_data = test_plot_l,
      right_plot_data = test_plot_r,
      y_range = c(180, 240),
      line_sig = "sig_trend",
      label_est = "est_label",
      label_se = "se_label",
      label_sig_high = "sig_label_1",
      label_sig_bold = "sig_label_2"
    )
  )
})
