# plot_data <- prep_trend(dat = trend_books, grouping_var = "KBuecher_imp3", competence = "GL", x_braces = list(c(2011, 2016), c(2016, 2021)))
#
# p1 <- plot_lineplot(plot_data)

test_that("correct states are extracted", {
  test_plot_2 <- list(
    plot_points = data.frame(
      state_var = c("a", "b", "noGroup"),
      grouping_var = factor(c("group1", "group1", "group1", "group2", "group2", "group2"), levels = c("group1", "group2")),
      year = rep(c(1, 2, 3), 2),
      est_point = c(200, 210, 220, 205, 215, 225),
      sig_point = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
    ),
    plot_lines = data.frame(
      state_var = c("a", "a", "b", "b"),
      grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
      year_start = c(1, 1, 2, 2),
      year_end = c(2, 2, 3, 3),
      est_point_start = c(200, 205, 210, 215),
      est_point_end = c(210, 215, 220, 225),
      sig_trend = c(TRUE, FALSE, FALSE, TRUE)
    ),
    plot_background_lines = data.frame(
      state_var = rep("wholeGroup", 4),
      grouping_var = factor(rep("noGroup", 2), levels = "noGroup"),
      year_start = c(1, 2),
      year_end = c(2, 3),
      est_point_start = c(190, 225),
      est_point_end = c(225, 230),
      sig_trend = c(TRUE, FALSE)
    ),
    plot_braces = data.frame(
      state_var = c("a", "a", "b", "b"),
      grouping_var = factor(c("group1", "group2", "group1", "group2"), levels = c("group1", "group2")),
      year_start = c(1, 1, 2, 2),
      year_end = c(2, 2, 3, 3),
      est_label = c(10, 20, 30, 40),
      se_label = c(1, 2, 3, 4),
      sig_label_1 = c(TRUE, FALSE, FALSE, TRUE),
      sig_label_2 = c(FALSE, TRUE, FALSE, TRUE)
    )
  )

expect_equal(filter_rows(test_plot_2, state = "a")$plot_points$state_var, c("a", "a"))
expect_equal(filter_rows(test_plot_2, state = "a")$plot_lines$state_var, c("a", "a"))

})

