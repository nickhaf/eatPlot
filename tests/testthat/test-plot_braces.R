test_that("calc_brace_coords works", {
  expect_equal(2 * 2, 4)
})






data <- prep_lineplot(trend_books, grouping_var = "KBuecher_imp3", competence = "GL")
data_trend_point <- data[["trend_braces"]]
data_trend_point <- data_trend_point[data_trend_point$TR_BUNDESLAND == "Berlin", ]


ggplot2::ggplot() +
  plot_lines(data_lines = data[["trend_point"]]) +
  plot_braces(data_trend_point) +
  ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc")) +
  plot_brace_label(brace_coords = brace_coords, BL = "Berlin")
# ## coord_cartesian: ylims, also wo ist die y-Achse? Ohne diese Limits werden die braces noch im Plot gezeichnet, sie sollen ja aber Unter der x-Achse losgehen.
# ## Plot margin: Wie viel weiße Fläche (auf die auch z.B. die Braces passen), bleibt neben den Achsen frei?
#
#
