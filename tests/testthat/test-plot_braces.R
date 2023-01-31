test_that("calc_brace_coords works", {
df <- data.frame(TR_BUNDESLAND = rep("Berlin", 4),
                 year_start = c(2011, 2011, 2015, 2015),
                 year_end = c(2020, 2020, 2020, 2020),
                 grouping_var = c(0, 1, 0, 1),
                 est_trend_within = c(1:4),
                 se_trend_within = c(1:4),
                 sig_trend_wihtin = c(TRUE, FALSE, FALSE, TRUE),
                 est_point_start = 400:403,
                 est_point_end = 500:503
                 )


ggplot2::ggplot() +
  ggplot2::geom_point(df, mapping = ggplot2::aes(x = year_start, y = est_point_start)) +
  ggplot2::geom_point(df, mapping = ggplot2::aes(x = year_end, y = est_point_end)) +
plot_braces(df, BL = "Berlin") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc"))


  })


test_that("coordinate system ist build correctly", {



})



data <- prep_lineplot(trend_books, grouping_var = "KBuecher_imp3", competence = "GL")
data_trend_point <- data[["trend_braces"]]
data_trend_point <- data_trend_point[data_trend_point$TR_BUNDESLAND == "Berlin", ]


ggplot2::ggplot() +
  plot_lines(data_lines = data[["trend_point"]]) +
  plot_braces(data_trend_point, BL = "Berlin") +
  ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc"))


# ## coord_cartesian: ylims, also wo ist die y-Achse? Ohne diese Limits werden die braces noch im Plot gezeichnet, sie sollen ja aber Unter der x-Achse losgehen.
# ## Plot margin: Wie viel weiße Fläche (auf die auch z.B. die Braces passen), bleibt neben den Achsen frei?
#
#
