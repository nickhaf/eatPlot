test_that("y limits are set correctly", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start_axis = c(2011, 2011, 2015, 2015),
    year_end_axis = c(2020, 2020, 2020, 2020),
    grouping_var = factor(c(0, 1, 0, 1)),
    est = c(1:4),
    se = c(1:4),
    sig_1 = c(TRUE, FALSE, FALSE, TRUE),
    sig_2 = c(FALSE, TRUE, FALSE, TRUE),
    est_noTrendStart_noComp = 400:403,
    est_noTrendEnd_noComp = 500:503,
    years_Trend = c("20112020", "20112020", "2152020", "20152020"),
    competence_var = "a"
  )

  test_p <- ggplot2::ggplot() +
    plot_braces(df,
      plot_lims = list(
        coords = calc_y_value_coords(y_range = c(400, 503)),
        y_range = c(400, 503)
      ),
      label_est = "est",
      label_se = "se",
      label_sig_high = "sig_1",
      label_sig_bold = "sig_2",
      plot_settings = plotsettings_lineplot(split_plot = TRUE)
    )
  coords <- calc_y_value_coords(c(400, 503)) ## need the new function for calculating then

  skip("test new function for y-lims here")
  expect_equal(test_p$coordinates$limits$y, coords)
})


test_that("single brace is drawn", {
  test_brace <- data.frame(
    overlap = rep(FALSE, 4),
    y = c(280, 250, 280, 250),
    year = c(2015, 2015, 2016, 2016),
    trend = rep("2015_2016", 4),
    mid = rep(0.25, 4),
    competence_var = rep("a", 4)
  )

  vdiffr::expect_doppelganger(
    "single brace",
    ggplot2::ggplot() +
      draw_braces(test_brace,
        plot_settings = plotsettings_lineplot(split_plot = FALSE)
      )
  )
})

test_that("double brace is drawn", {
  test_brace_double <- data.frame(
    overlap = rep(FALSE, 8),
    y = c(280, 250, 280, 250, 280, 250, 280, 250),
    year = c(2015, 2015, 2016, 2016, 2016, 2016, 2017, 2017),
    trend = c(rep("2015_2016", 4), rep("2016_2017", 4)),
    mid = rep(0.25, 8),
    competence_var = rep("a", 8)
  )
  vdiffr::expect_doppelganger(
    "double brace",
    ggplot2::ggplot() +
      draw_braces(test_brace_double,
        plot_settings = plotsettings_lineplot(split_plot = FALSE)
      )
  )
})



test_that("brace label is drawn", {
  test_label <- data.frame(
    grouping_vars = rep(0, 2),
    overlap = c(FALSE, TRUE),
    label_pos_x = c(1, 2),
    label_pos_y = c(1, 1),
    brace_label = c("label_1<sup>a</sup>", "label_2")
  )


  vdiffr::expect_doppelganger(
    "brace label",
    ggplot2::ggplot() +
      draw_brace_label(test_label, plot_settings = plotsettings_lineplot())
  )
})



test_that("braces are prepped", {
  # Create the coord_dat data frame
  coord_dat <- data.frame(
    year_start = c(2009, 2015),
    year_end = c(2015, 2022),
    range = c(6, 7),
    brace_position_x = c(0.5, 0.5),
    brace_position_y = c("middle", "middle"),
    upper_y = c(248.6513, 248.6513),
    lower_y = c(211.149, 211.149),
    label_pos_x = c(2013.085, 2019.585)
  )

  # Create the group_labels data frame
  group_labels <- data.frame(
    grouping_lvls = c("einET", "ersteGen"),
    label_pos_y = c(192.3978, 162.3960)
  )

  brace_coords <- list(
    "coord_dat" = coord_dat,
    "group_labels" = group_labels
  )

  plot_dat <- data.frame(
    TR_BUNDESLAND = c("Berlin", "Berlin"),
    id = c(1, 2),
    brace_label_est = c(400, 500.34, 401.3, 501.34),
    brace_label_se = c(30, 20, 10, 20),
    brace_label_sig_high = c(FALSE, TRUE, TRUE, FALSE),
    brace_label_sig_bold = c(FALSE, TRUE, FALSE, TRUE),
    mhg = c("einET", "ersteGen", "einET", "ersteGen"),
    trend = c("2009_2015", "2009_2015", "2015_2022", "2015_2022")
  )

brace_dat <- prep_brace(plot_dat, brace_coords)
  # Combine the data frames into a list

plot_brace_build <- ggplot2::ggplot_build(ggplot2::ggplot() +
  draw_braces(brace_dat, plot_settings = plotsettings_lineplot(split_plot = FALSE)) +
  draw_brace_label(brace_dat, plot_settings = plotsettings_lineplot())
)

expect_equal(plot_brace_build$data[[2]]$label, c("400 (30.0)", "**500**<sup>a</sup> (20.0)", "401<sup>a</sup> (10.0)", "**501** (20.0)"))

})


test_that("Overlapping braces are looking good", {
  coord_dat <- data.frame(
    year_start = c(2009, 2015),
    year_end = c(2013, 2022),
    range = c(6, 7),
    brace_position_x = c(0.5, 0.5),
    brace_position_y = c("middle", "middle"),
    upper_y = c(248.6513, 248.6513),
    lower_y = c(211.149, 211.149),
    label_pos_x = c(2013.085, 2019.585)
  )

  # Create the group_labels data frame
  group_labels <- data.frame(
    grouping_lvls = c("einET", "ersteGen"),
    label_pos_y = c(192.3978, 162.3960)
  )

  brace_coords <- list(
    "coord_dat" = coord_dat,
    "group_labels" = group_labels
  )

  plot_dat <- data.frame(
    TR_BUNDESLAND = c("Berlin", "Berlin"),
    id = c(1, 2),
    brace_label_est = c(400, 500.34, 401.3, 501.34),
    brace_label_se = c(30, 20, 10, 20),
    brace_label_sig_high = c(FALSE, TRUE, TRUE, FALSE),
    brace_label_sig_bold = c(FALSE, TRUE, FALSE, TRUE),
    mhg = c("einET", "ersteGen", "einET", "ersteGen"),
    trend = c("2009_2015", "2009_2015", "2015_2022", "2015_2022")
  )

  brace_dat <- prep_brace(plot_dat, brace_coords)


})

test_that("Overlapping braces in the other direction", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start_axis = c(2011, 2011, 2013, 2013),
    year_end_axis = c(2023, 2023, 2023, 2023),
    grouping_var = factor(c(0, 1, 0, 1)),
    est = c(1:4),
    se = c(1:4),
    sig_1 = c(TRUE, FALSE, FALSE, TRUE),
    sig_2 = c(FALSE, TRUE, FALSE, TRUE),
    est_noTrendStart_noComp = 400:403,
    est_noTrendEnd_noComp = 500:503,
    years_Trend = c("20112015", "20112015", "20152023", "20152023"),
    competence_var = "a"
  )

  p_braces <- ggplot2::ggplot() +
    plot_braces(df,
      plot_lims = list(
        coords = calc_y_value_coords(y_range = c(400, 503)),
        y_range = c(400, 503)
      ),
      label_est = "est",
      label_se = "se",
      label_sig_high = "sig_2",
      label_sig_bold = "sig_1"
    ) +
    ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc"))

  vdiffr::expect_doppelganger(
    "Overlapping braces in other direction",
    p_braces
  )
})


test_that("Braces can be facet wrapped", {
  df <- data.frame(
    state_var = rep("Berlin", 4),
    year_start_axis = c(2011, 2011, 2013, 2013),
    year_end_axis = c(2015, 2015, 2023, 2023),
    grouping_var = factor(c(0, 1, 0, 1)),
    est = c(1:4),
    se = c(1:4),
    sig_1 = c(TRUE, FALSE, FALSE, TRUE),
    sig_2 = c(FALSE, TRUE, FALSE, TRUE),
    est_noTrendStart_noComp = 400:403,
    est_noTrendEnd_noComp = 500:503,
    years_Trend = c("20112015", "20112015", "20152023", "20152023"),
    competence_var = "a"
  )


  vdiffr::expect_doppelganger(
    "Facetted braces",
    ggplot2::ggplot() +
      plot_braces(df,
        plot_lims = list(
          coords = calc_y_value_coords(y_range = c(400, 503)),
          y_range = c(400, 503)
        ),
        label_est = "est",
        label_se = "se",
        label_sig_high = "sig_2",
        label_sig_bold = "sig_1",
        plot_settings = plotsettings_lineplot(split_plot = TRUE)
      ) +
      ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc")) +
      ggplot2::facet_grid(. ~ years_Trend, scales = "free_x")
  )
})
