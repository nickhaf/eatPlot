
test_that("y limits are set correctly", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start = c(2011, 2011, 2015, 2015),
    year_end = c(2020, 2020, 2020, 2020),
    grouping_var = factor(c(0, 1, 0, 1)),
    est = c(1:4),
    se = c(1:4),
    sig_1 = c(TRUE, FALSE, FALSE, TRUE),
    sig_2 = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503,
    trend = c("20112020", "20112020", "2152020", "20152020"),
    competence_var = "a"
  )

  test_p <- ggplot2::ggplot() +
    plot_braces(df,
      split = TRUE,
      y_range = c(400, 503),
      label_est = "est",
      label_se = "se",
      label_sig_high = "sig_1",
      label_sig_bold = "sig_2"
    )
  coords <- calc_coords(c(400, 503))

  expect_equal(test_p$coordinates$limits$y, coords)
})


test_that("x-position of brace label is calculated correctly", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start = c(2011, 2011, 2015, 2015),
    year_end = c(2020, 2020, 2020, 2020),
    grouping_var = factor(c(0, 1, 0, 1)),
    est = c(1:4),
    se = c(1:4),
    sig_1 = c(TRUE, FALSE, FALSE, TRUE),
    sig_2 = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503,
    trend = c("20112020", "20112020", "2152020", "20152020"),
    competence_var = "a"
  )
range_years <- diff(range(c(2011, 2020), na.rm = TRUE))

  expect_equal(calc_brace_label_x(year_start = 0, year_end = 10, range_total = range_years, brace_indent_pos = 0.25), 2.5)
  expect_equal(
    ifelse(df$year_start == min(df$year_start),
      calc_brace_label_x(df$year_start, df$year_end, range_years, 0.25),
      calc_brace_label_x(df$year_start, df$year_end, range_years, 0.5)
    ),
    c(2013.25, 2013.25, 2017.5, 2017.5)
  )
})



test_that("single brace is drawn", {
  test_brace <- data.frame(
    overlap = FALSE,
    upper_y = 280,
    lower_y = 250,
    year_start = c(2015),
    year_end = c(2016),
    mid = 0.25,
    competence_var = "a"
  )

  vdiffr::expect_doppelganger(
    "single brace",
    ggplot2::ggplot() +
      draw_braces(test_brace, split = FALSE)
  )
})

test_that("double brace is drawn", {
  test_brace_double <- data.frame(
    year_start = c(2011, 2015),
    year_end = c(2020, 2020),
    upper_y = c(360, 324),
    lower_y = c(324, 306),
    mid = c(0.25, 0.5)
  )
  vdiffr::expect_doppelganger("double brace", ggplot2::ggplot() +
    draw_braces(test_brace_double, split = FALSE))
})

test_that("brace label is drawn", {
  test_label <- data.frame(
    grouping_var = rep(0, 2),
    overlap = c(FALSE, TRUE),
    label_pos_x = c(1, 2),
    label_pos_y = c(1, 1),
    brace_label = c("label_1<sup>a</sup>", "label_2")
  )


  vdiffr::expect_doppelganger(
    "brace label",
    ggplot2::ggplot() +
      draw_brace_label(test_label)
  )
})

test_that("braces are plotted correctly", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start = c(2011, 2011, 2015, 2015),
    year_end = c(2020, 2020, 2020, 2020),
    grouping_var = factor(c(0, 1, 0, 1)),
    est = c(1:4),
    se = c(1:4),
    sig_1 = c(TRUE, FALSE, FALSE, TRUE),
    sig_2 = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503,
    trend = c("20112020", "20112020", "20152020", "20152020"),
    competence_var = "a"
  )

  vdiffr::expect_doppelganger(
    "Brace plot",
    ggplot2::ggplot() +
      plot_braces(df,
        y_range = c(400, 503),
        label_est = "est",
        label_se = "se",
        label_sig_high = "sig_1",
        label_sig_bold = "sig_2"
      ) +
      ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc"))
  )
})


test_that("significances are displayed correctly in the labels", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start = c(2011, 2011, 2015, 2015),
    year_end = c(2020, 2020, 2020, 2020),
    grouping_var = factor(c(0, 1, 0, 1)),
    est = c(1:4),
    se = c(1:4),
    sig_1 = c(TRUE, FALSE, FALSE, TRUE),
    sig_2 = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503,
    trend = c("20112020", "20112020", "20152020", "20152020"),
    competence_var = "a"
  )

  plot_brace_build <- ggplot2::ggplot_build(ggplot2::ggplot() +
    plot_braces(df,
      y_range = c(400, 503),
      label_est = "est",
      label_se = "se",
      label_sig_high = "sig_2",
      label_sig_bold = "sig_1"
    ) +
    ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc")))

  expect_equal(plot_brace_build$data[[3]]$label, c("**1** (1)", "2<sup>a</sup> (2)", "3 (3)", "**4**<sup>a</sup> (4)"))
})

test_that("Example brace plot is still the same", {
  plot_data <- prep_trend(
    dat = trend_books,
    grouping_var = "KBuecher_imp3",
    competence = "GL"
  )

  vdiffr::expect_doppelganger(
    "Brace plot trend_books",
    ggplot2::ggplot() +
      plot_braces(plot_data[["plot_braces"]][plot_data[["plot_braces"]]$state_var == "Berlin" & !is.na(plot_data[["plot_braces"]]$state_var), ],
        y_range = c(397, 552),
        label_est = "est_trend_no_comp",
        label_se = "se_trend_no_comp",
        label_sig_high = "sig_trend_comp_whole",
        label_sig_bold = "sig_trend_no_comp"
      ) +
      ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc"))
  )
})


test_that("Adjacent braces", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start = c(2011, 2011, 2015, 2015),
    year_end = c(2015, 2015, 2023, 2023),
    grouping_var = factor(c(0, 1, 0, 1)),
    est = c(1:4),
    se = c(1:4),
    sig_1 = c(TRUE, FALSE, FALSE, TRUE),
    sig_2 = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503,
    trend = c("20112015", "20112015", "20152023", "20152023")
  )

  vdiffr::expect_doppelganger(
    "Adjacent braces",
    ggplot2::ggplot() +
      plot_braces(df,
        y_range = c(400, 503),
        label_est = "est",
        label_se = "se",
        label_sig_high = "sig_2",
        label_sig_bold = "sig_1"
      ) +
      ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc"))
  )
})



test_that("Overlapping braces are looking good", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start = c(2011, 2011, 2013, 2013),
    year_end = c(2015, 2015, 2023, 2023),
    grouping_var = factor(c(0, 1, 0, 1)),
    est = c(1:4),
    se = c(1:4),
    sig_1 = c(TRUE, FALSE, FALSE, TRUE),
    sig_2 = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503,
    trend = c("20112015", "20112015", "20152023", "20152023")
  )

  p_braces <- ggplot2::ggplot() +
    plot_braces(df,
      y_range = c(400, 503),
      label_est = "est",
      label_se = "se",
      label_sig_high = "sig_2",
      label_sig_bold = "sig_1"
    ) +
    ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc"))

  vdiffr::expect_doppelganger(
    "Overlapping braces",
    p_braces
  )
})

test_that("Braces can be facet wrapped", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start = c(2011, 2011, 2013, 2013),
    year_end = c(2015, 2015, 2023, 2023),
    grouping_var = factor(c(0, 1, 0, 1)),
    est = c(1:4),
    se = c(1:4),
    sig_1 = c(TRUE, FALSE, FALSE, TRUE),
    sig_2 = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503,
    trend = c("20112015", "20112015", "20152023", "20152023")
  )


  vdiffr::expect_doppelganger(
    "Facetted braces",
    ggplot2::ggplot() +
      plot_braces(df,
        split_plot = TRUE,
        y_range = c(400, 503),
        label_est = "est",
        label_se = "se",
        label_sig_high = "sig_2",
        label_sig_bold = "sig_1"
      ) +
      ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc")) +
      ggplot2::facet_wrap(~trend, scales = "free_x")
  )
})
