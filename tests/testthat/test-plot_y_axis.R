test_that("position is calculated correctly", {
  expect_equal(calc_y_positions(facets = 1:5, n_cols = 2), c(1, 4, 7))
  expect_equal(calc_y_positions(facets = 1:4, n_cols = 2), c(1, 4))
})


## Write Test for y-Axis calculation and plotting

test_that("y-axis can be plotted", {
y_plot <- ggplot2::ggplot() +
  plot_y_axis(y_axis_min = 0, y_axis_max = 10, y_total_min =-1, y_total_max = 11, tick_distance = 2, plot_settings = plotsettings_lineplot())

vdiffr::expect_doppelganger("y-axis", y_plot)

})


test_that("y-axis can be combined with single lineplot", {

test_data_lineplot <- data.frame(
  id = c("comp_1", "comp_1"),
  state_var = c("Berlin", "Berlin"),
  subgroup_var = c("m", "m"),
  year = c(2000, 2005),
  trend = c("2000_2005", "2000_2005"),
  est_point = c(500, 560),
  sig_point = c(TRUE, FALSE),
  sig_line = c(FALSE, FALSE)
)

common_y_scale <-  ggplot2::scale_y_continuous(
  breaks = seq_over(
    from = 400,
    to = 600,
    by = 50
  ),
  limits = c(400, 600),
  expand = c(0, 0)
)

y_plot <- ggplot2::ggplot() +
  plot_y_axis(y_axis_min = 450, y_axis_max = 550, y_total_min = 400, y_total_max = 600, tick_distance = 10, plot_settings = plotsettings_lineplot()) +
  ggplot2::theme(plot.margin = ggplot2::unit(c(
    plotsettings_lineplot()$margin_top,
    0,
    plotsettings_lineplot()$margin_bottom,
    0
  ), "npc"))




p <- plot_lineplot(test_data_lineplot,
                   point_est = "est_point",
                   point_sig = "sig_point",
                   line_sig = "sig_line",
                   years_lines = list(c(2000, 2005)),
                   facet_var = "state_var"
) +
  ggplot2::theme_classic() +
  common_y_scale +
  NULL



plot_list <- list(y_plot, p)

p_patched <- patchwork::wrap_plots(plot_list,
                      ncol = 2,
                      widths = c(0.01, 0.99))

vdiffr::expect_doppelganger("aligned y-axis", p_patched)

})
