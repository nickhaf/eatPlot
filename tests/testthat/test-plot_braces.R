
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
    est_point_end = 500:503
  )

  test_p <- ggplot2::ggplot() +
    plot_braces(df,
                BL = "Berlin",
                label_est = "est",
                label_se = "se",
                label_sig_high = "sig_1",
                label_sig_bold = "sig_2")
  coords <- c(360, 530)

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
    state_var = rep("Berlin", 4),
    year_start = c(2011, 2011, 2015, 2015),
    year_end = c(2020, 2020, 2020, 2020),
    grouping_var = factor(c(0, 1, 0, 1)),
    est = c(1:4),
    se = c(1:4),
    sig_1 = c(TRUE, FALSE, FALSE, TRUE),
    sig_2 = c(FALSE, TRUE, FALSE, TRUE),
    est_point_start = 400:403,
    est_point_end = 500:503
  )



  vdiffr::expect_doppelganger(
    "Brace plot",
    ggplot2::ggplot() +
      plot_braces(df,
        BL = "Berlin",
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
    est_point_end = 500:503
  )

  plot_brace_build <- ggplot2::ggplot_build(ggplot2::ggplot() +
    plot_braces(df,
      BL = "Berlin",
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

  vdiffr::expect_doppelganger("Brace plot trend_books", ggplot2::ggplot() +
    plot_braces(plot_data[["plot_braces"]],
                BL = "Berlin",
                label_est = "est_trend_no_comp",
                label_se = "se_trend_no_comp",
                label_sig_high = "sig_trend_whole",
                label_sig_bold = "sig_trend_no_comp") +
    ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc")))
})


test_that("Braces are plotted next to each other", {
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
    est_point_end = 500:503
  )

  p_braces <- ggplot2::ggplot() +
    plot_braces(df,
      BL = "Berlin",
      label_est = "est",
      label_se = "se",
      label_sig_high = "sig_2",
      label_sig_bold = "sig_1"
    ) +
    ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc"))

  vdiffr::expect_doppelganger("Braces are plotted next to each other", {
    p_braces
  })
})
