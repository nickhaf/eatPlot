years_list <- prep_years_list(
  years_lines = list(c(2009, 2015), c(2015, 2022)),
  years_braces = list(c(2009, 2015), c(2009, 2022))
)
plot_dat <- data.frame(
  facet_var = rep("Berlin", 12),
  id = 1:12,
  est_point = 400:411, # isn't plotted in the braces, only sets the range
  brace_label_est = rep(400.1902, 12),
  brace_label_se = rep(30, 12),
  brace_label_sig_high = rep(c(TRUE), 12),
  brace_label_sig_bold = rep(TRUE, 12),
  subgroup_var = rep(c("einET", "ersteGen"), 6),
  year = c(rep(2009, 2), rep(2015, 4), rep(2022, 2), rep(2009, 2), rep(2022, 2)),
  trend = c(rep("2009_2015", 4), rep("2015_2022", 4), rep("2009_2022", 4))
)

plot_lims <- calc_plot_lims(
  plot_dat,
  subgroup_lvls = c("einET", "ersteGen"),
  years_list = years_list,
  plot_settings = plotsettings_lineplot()
)

brace_dat <- prep_brace(plot_dat, plot_lims, plotsettings_lineplot())



test_that("Overlapping braces are looking good", {
  vdiffr::expect_doppelganger(
    "overlapping brace",
    ggplot2::ggplot() +
      draw_braces(brace_dat$brace_coords$coord_dat_test1,
        plot_settings = plotsettings_lineplot(split_plot = FALSE)
      ) +
      draw_brace_label(brace_dat$brace_label, plot_settings = plotsettings_lineplot())
  )
})


test_that("single brace is drawn", {
  test_brace <- data.frame(
    overlap = rep(FALSE, 4),
    y = c(280, 250, 280, 250),
    year = c(2015, 2015, 2016, 2016),
    trend = rep("2015_2016", 4),
    label_pos_x = c(2015, 2015, 2016, 2016),
    brace_position_x = rep(0.5, 4)
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
    y = rep(c(280, 250), 4),
    year = c(2015, 2015, 2016, 2016, 2016, 2016, 2018, 2018),
    trend = c(rep("2015_2016", 4), rep("2016_2018", 4)),
    label_pos_x = c(2015, 2015, 2016, 2016, 2016, 2016, 2018, 2018),
    brace_position_x = rep(0.5, 8)
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
  plot_brace_build <- ggplot2::ggplot_build(ggplot2::ggplot() +
    draw_braces(brace_dat$brace_coords$coord_dat_test1,
      plot_settings = plotsettings_lineplot(split_plot = FALSE)
    ) +
    draw_brace_label(brace_dat$brace_label, plot_settings = plotsettings_lineplot()))

  expect_equal(plot_brace_build$data[[3]]$label, rep("**400**<sup>a</sup> (30.0)", 4))
})




test_that("Overlapping braces in the other direction", {
  plot_dat <- data.frame(
    facet_var = rep("Berlin", 12),
    id = 1:12,
    est_point = 400:411, # isn't plotted in the braces, only sets the range
    brace_label_est = rep(400.1902, 12),
    brace_label_se = rep(30, 12),
    brace_label_sig_high = rep(c(TRUE), 12),
    brace_label_sig_bold = rep(TRUE, 12),
    subgroup_var = rep(c("einET", "ersteGen"), 6),
    year = c(rep(2009, 2), rep(2015, 4), rep(2022, 2), rep(2009, 2), rep(2022, 2)),
    trend = c(rep("2009_2015", 4), rep("2015_2022", 4), rep("2009_2022", 4))
  )

  plot_lims <- calc_plot_lims(plot_dat,
    subgroup_lvls = c("einET", "ersteGen"),
    years_list = years_list,
    plot_settings = plotsettings_lineplot()
  )

  brace_dat <- prep_brace(plot_dat, plot_lims, plotsettings_lineplot())

  vdiffr::expect_doppelganger(
    "overlapping brace",
    ggplot2::ggplot() +
      draw_braces(brace_dat$brace_coords$coord_dat_test1,
        plot_settings = plotsettings_lineplot(split_plot = FALSE)
      ) +
      draw_brace_label(brace_dat$brace_label, plot_settings = plotsettings_lineplot())
  )
})

#
# test_that("Braces can be facet wrapped", {
#   df <- data.frame(
#     state_var = rep("Berlin", 4),
#     year_start_axis = c(2011, 2011, 2013, 2013),
#     year_end_axis = c(2015, 2015, 2023, 2023),
#     grouping_var = factor(c(0, 1, 0, 1)),
#     est = c(1:4),
#     se = c(1:4),
#     sig_1 = c(TRUE, FALSE, FALSE, TRUE),
#     sig_2 = c(FALSE, TRUE, FALSE, TRUE),
#     est_noTrendStart_noComp = 400:403,
#     est_noTrendEnd_noComp = 500:503,
#     years_Trend = c("20112015", "20112015", "20152023", "20152023"),
#     competence_var = "a"
#   )
#
#
#   vdiffr::expect_doppelganger(
#     "Facetted braces",
#     ggplot2::ggplot() +
#       plot_braces(df,
#         plot_lims = list(
#           coords = calc_y_value_coords(y_range = c(400, 503)),
#           y_range = c(400, 503)
#         ),
#         label_est = "est",
#         label_se = "se",
#         label_sig_high = "sig_2",
#         label_sig_bold = "sig_1",
#         plot_settings = plotsettings_lineplot(split_plot = TRUE)
#       ) +
#       ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc")) +
#       ggplot2::facet_grid(. ~ years_Trend, scales = "free_x")
#   )
# })
