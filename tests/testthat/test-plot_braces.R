
test_that("y limits are set correctly", {
  df <- data.frame(
    TR_BUNDESLAND = rep("Berlin", 4),
    year_start = c(2011, 2011, 2015, 2015),
    year_end = c(2020, 2020, 2020, 2020),
    grouping_var = c(0, 1, 0, 1),
    est_trend_within = c(1:4),
    se_trend_within = c(1:4),
    sig_trend_within = c(TRUE, FALSE, FALSE, TRUE),
    sig_trend_whole = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503
  )

  test_p <- ggplot2::ggplot() +
    plot_braces(df, BL = "Berlin")
  coords <- c(360, 530)

  expect_equal(test_p$coordinates$limits$y, coords)
})



test_that("calc_brace_coords works", {
  df <- data.frame(
    TR_BUNDESLAND = rep("Berlin", 4),
    year_start = c(2011, 2011, 2015, 2015),
    year_end = c(2020, 2020, 2020, 2020),
    grouping_var = c(0, 1, 0, 1),
    est_trend_within = c(1:4),
    se_trend_within = c(1:4),
    sig_trend_within = c(TRUE, FALSE, FALSE, TRUE),
    sig_trend_whole = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503
  )

  coords <- c(360, 530)

  test_braces <- calc_brace_coords(df, coords)

  expect_equal(test_braces$brace_upper_y, c(360, 360, 324, 324))
  expect_equal(test_braces$brace_lower_y, c(324, 324, 306, 306))
  expect_equal(test_braces$label_pos_y, c(298.8, 306.0, 298.8, 306.0))
})

test_that("x-position of brace label is calculated correctly", {
  df <- data.frame(
    TR_BUNDESLAND = rep("Berlin", 4),
    year_start = c(2011, 2011, 2015, 2015),
    year_end = c(2020, 2020, 2020, 2020),
    grouping_var = c(0, 1, 0, 1),
    est_trend_within = c(1:4),
    se_trend_within = c(1:4),
    sig_trend_within = c(TRUE, FALSE, FALSE, TRUE),
    sig_trend_whole = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503
  )

  expect_equal(calc_pos_label_x(year_start = 0, year_end = 10, brace_indent_pos = 0.25), 2.5)
  expect_equal(
    ifelse(df$year_start == min(df$year_start),
      calc_pos_label_x(df$year_start, df$year_end, 0.25),
      calc_pos_label_x(df$year_start, df$year_end, 0.5)
    ),
    c(2013.25, 2013.25, 2017.5, 2017.5)
  )
})

test_that("braces are plotted correctly", {
  df <- data.frame(
    TR_BUNDESLAND = rep("Berlin", 4),
    year_start = c(2011, 2011, 2015, 2015),
    year_end = c(2020, 2020, 2020, 2020),
    grouping_var = c(0, 1, 0, 1),
    est_trend_within = c(1:4),
    se_trend_within = c(1:4),
    sig_trend_within = c(TRUE, FALSE, FALSE, TRUE),
    sig_trend_whole = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503
  )



  vdiffr::expect_doppelganger("Brace plot", ggplot2::ggplot() +
    plot_braces(df, BL = "Berlin") +
    ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc")))
})


test_that("significances are displayed correctly in the labels", {
  df <- data.frame(
    TR_BUNDESLAND = rep("Berlin", 4),
    year_start = c(2011, 2011, 2015, 2015),
    year_end = c(2020, 2020, 2020, 2020),
    grouping_var = c(0, 1, 0, 1),
    est_trend_within = c(1:4),
    se_trend_within = c(1:4),
    sig_trend_within = c(TRUE, FALSE, FALSE, TRUE),
    sig_trend_whole = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503
  )

  plot_brace_build <- ggplot2::ggplot_build(ggplot2::ggplot() +
    plot_braces(df, BL = "Berlin") +
    ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc")))

  expect_equal(plot_brace_build$data[[3]]$label, c("**1** (1)", "2<sup>a</sup> (2)", "3 (3)", "**4**<sup>a</sup> (4)"))
})

test_that("Example brace plot is still the same", {
plot_data <- prep_lineplot(data = trend_books, grouping_var = "KBuecher_imp3", competence = "GL")

vdiffr::expect_doppelganger("Brace plot trend_books", ggplot2::ggplot() +
  plot_braces(plot_data[["plot_braces"]], BL = "Berlin") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc"))
)

})
