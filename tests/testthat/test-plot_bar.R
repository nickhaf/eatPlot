test_that("Barplot is still the same", {
  df_bar <- data.frame(x = 1:4,
                       y = rep("a", 4),
                       bar_pattern = c(TRUE, TRUE, FALSE, FALSE),
                       bar_fill = c("a", "b", "c", "d"),
                       bar_pattern_fill = c("a", "a", "b", "b")
  )


  vdiffr::expect_doppelganger("Bar plot",
                              plot_bar(df_bar,
                                       x_value = "x", y_value = "y",
                                       grouping = "bar_pattern",
                                       bar_fill = "bar_fill",
                                       bar_pattern_fill = "bar_pattern_fill",
                                       bar_fill_setting = c(
                                         "a" = "red",
                                         "b" = "blue",
                                         "c" = "green",
                                         "d" = "grey"
                                       ),
                                       bar_pattern_fill_setting = c(
                                         "a" = "yellow",
                                         "b" = "orange"
                                       ))
  )
})

test_that("Example barplot is still the same", {
  plot_data <- prep_no_trend(dat = adjusted_means,
                             columns = "adjust",
                             grouping_var = "adjust",
                             competence = "GL",
                             sig_niveau = 0.05)

  vdiffr::expect_doppelganger("Bar plot for trend_books",
                              plot_bar(plot_data[["plot_bar"]]))
})

test_that("Example barplot can be plotted with different frames", {
  plot_data <- prep_no_trend(dat = adjusted_means,
                             columns = "adjust",
                             grouping_var = "adjust",
                             competence = "GL",
                             sig_niveau = 0.05)

  vdiffr::expect_doppelganger("Bar plot with frames for trend_books",
                              plot_bar(plot_data[["plot_bar"]],
                                       grouping_type = "frame"))
})



# Umsetzung der neuen Plots -----------------------------------------------
# df_min <- data.frame(land = factor(c("Berlin", "Bremen", "Hessen")),
#                      min_nicht = c(19, 14, 15),
#                                  reg_erreicht = c(60, 50, 45),
#                                  sig_reg_erreicht = c("over", "same", "under")
#                                  )


min_stand <- readxl::read_xlsx("Q:/BT2022/BT/60_Bericht/_Probegrafiken/2023-01-26 Vorlagen Balken und Linien/Abb3.9_Balken_2021_Lesen.xlsx", sheet = "Daten BT21")

View(min_stand)

data_plot_new <- prep_trend(min_stand, competence = "lesen")
data_bar <- data_plot_new[["plot_points"]]
data_bar <- data_bar[grepl("minVerfehlt", data_bar$keyword) & data_bar$year == "2021", ]

## How to deal with the sig erreicht? Muss ja ein beidseitiger test sein, da muuss die Signifikanz anders berechnet werden

plot_r <- ggplot2::ggplot(
  data = data_bar,
  mapping = ggplot2::aes(
    x = est_point * 100,
    y = state_var,
    linetype = sig_point
  )) +
  ggstats::geom_stripped_rows(
    odd = "lightgrey",
    even = "#00000000") +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.8),
    fill = grDevices::rgb(75, 172, 198, maxColorValue = 255),
    color = "black",
    linewidth = 0.9,
    width = 0.4
  ) +
  ggplot2::scale_linetype_manual(values = c("over" = "solid", "same" = "blank", "under" = "dashed")) +
  theme_table_bar()

# plot_l <- ggplot2::ggplot(
#   data = df_min,
#   mapping = ggplot2::aes(
#     x = min_nicht,
#     y = land,
#     linetype = sig_reg_erreicht
#   )) +
#   ggstats::geom_stripped_rows(
#     odd = "lightgrey",
#     even = "#00000000") +
#   ggplot2::geom_col(
#     position = ggplot2::position_dodge(width = 0.8),
#     fill = grDevices::rgb(49, 133, 156, maxColorValue = 255),
#     color = "black",
#     linewidth = 0.9,
#     width = 0.4
#   ) +
#   ggplot2::scale_linetype_manual(values = c("over" = "solid", "same" = "blank", "under" = "dashed")) +
#   theme_table_bar()
# cowplot::plot_grid(plot_l, plot_r, nrow = 1, align = "h")
