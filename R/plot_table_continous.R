#
# # Versuch eines Barplots mit continous scale ------------------------------
#
#
# min_stand <- readxl::read_xlsx("Q:/BT2022/BT/60_Bericht/_Probegrafiken/2023-01-26 Vorlagen Balken und Linien/BT2021_Abb3.9.xlsx", sheet = "Daten BT21")
# data_plot_new <- prep_trend(min_stand, competence = "lesen", parameter = "1")$plot_bar
#
# ## Nötig wäre: sehr breiter Plot, der eine durchgezogene Skala hat. Die Bars werden dann irgendwo geplotted, und daneben wird annotated
#
# test_data <- data.frame(
#   y_axis = 1:4,
#   x_min = rep(0, 4),
#   x_max = c(1, -2, 0.4, 3),
#   group = c("a", "a", "b", "b"),
#   group_2 = c("12", "10", "8", "4")
# )
#
# ggplot2::ggplot(
#   data = test_data,
# ) +
#   ggpattern::geom_rect_pattern(
#     aes(
#       xmin = x_min,
#       xmax = x_max,
#       ymin = y_axis - 0.4,
#       ymax = y_axis + 0.4
#     ),
#     pattern = "stripe",
#     linetype = 2,
#     colour = "red"
#   ) +
#   build_columns_2(test_data, cols = c("group", "group_2")) +
#   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = Inf), colour = "lightblue", fill = "lightblue") +
#   annotate("text", x = 0, y = 4.8, label = "Header") +
#   annotate("text", x = -3.5, y = 4.8, label = "Colspanner") +
#   annotate("segment", x = -4.5, xend = -2.5, y = 4.7, yend = 4.7) +
#   scale_x_continuous(breaks = c(-2, 0, 2), expand = c(0,0))
#
#
#
# build_columns_2 <- function(df, cols) {
#   x_axis <- min(c(df$x_min, df$x_max))
#
#   c(
#     lapply(1:length(cols), function(i) {
#       column_name <- cols[i]
#       x_axis <- x_axis - i # Das muss natürlich Skalenabhängig sein, und skalierbar
#       ggplot2::geom_text(
#         data = df,
#         aes(
#           x = x_axis,
#           y = y_axis,
#           label = .data[[column_name]]
#         )
#       )
#     })
#   )
# }
#
# ## Jede Spalte einzeln vorliegend mit variierendem label
# # verschiedene Plots dann mit patchwork verbinden
# # Manuelle Funktion mit geom_rect für die stripes.
# # Berechnen der Spaltenbreite (evtl von stringmenge abhängig machen?), auf alle Fälle manuell justierbar lassen.
