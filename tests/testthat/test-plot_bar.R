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

data_plot_new <- prep_trend(min_stand, competence = "lesen", parameter = "1")
data_bar <- data_plot_new[["plot_bar"]]
data_bar_l <- data_bar[data_bar$depVar == "minVerfehlt" & data_bar$year == "2021", ]
data_bar_r <- data_bar[data_bar$depVar == "regErreicht" & data_bar$year == "2021", ]

## How to deal with the sig erreicht? Muss ja ein beidseitiger test sein, da muuss die Signifikanz anders berechnet werden


plot_l <- ggplot2::ggplot(
  data = data_bar_l,
  mapping = ggplot2::aes(
    x = est_point_no_comp * 100,
    y = state_var,
    linetype = sig_point_comp
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
  ggplot2::scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "dashed")) +
  ggplot2::geom_text(ggplot2::aes(label = est_point_no_comp * 100), hjust = -0.2) +
  theme_table_bar()

plot_r <- ggplot2::ggplot(
  data = data_bar_r,
  mapping = ggplot2::aes(
    x = est_point_no_comp * 100,
    y = state_var,
    linetype = sig_point_comp
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
  ggplot2::scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "dashed")) +
  ggplot2::geom_text(ggplot2::aes(label = est_point_no_comp * 100), hjust = -0.2) +
  theme_table_bar()

plot_table_bar(plot_l, plot_r)


