#
# # Versuch eines Barplots mit continous scale ------------------------------
#
#
# min_stand <- readxl::read_xlsx("Q:/BT2022/BT/60_Bericht/_Probegrafiken/2023-01-26 Vorlagen Balken und Linien/BT2021_Abb3.9.xlsx", sheet = "Daten BT21")
# data_plot_new <- prep_trend(min_stand, competence = "lesen", parameter = "1")$plot_bar
#
# ## Nötig wäre: sehr breiter Plot, der eine durchgezogene Skala hat. Die Bars werden dann irgendwo geplotted, und daneben wird annotated
#
# test_data <- data.frame(y_axis = 1:4,
#                         x_min = rep(0, 4),
#                         x_max = c(1, -2, 0.4, 3),
#                         group = c("a", "a", "b", "b"),
#                         group_2 = c("12", "10", "8", "4")
#                         )
#
#   ggplot2::ggplot(
#     data = test_data,
#     ) +
#   ggpattern::geom_rect_pattern(aes(
#     xmin = x_min,
#     xmax = x_max,
#     ymin = y_axis - 0.4,
#     ymax = y_axis + 0.4),
#     pattern = "stripe",
#     linetype = 2,
#     colour = "red"
#   ) +
#     geom_text(mapping = aes(
#       x = -3,
#       y = y_axis,
#       label = group
#     )) +
#     geom_text(mapping = aes(
#       x = -4,
#       y = y_axis,
#       label = group_2
#     ))
#

  ## Jede Spalte einzeln vorliegend mit variierendem label
  # verschiedene Plots dann mit patchwork verbinden
