
# Versuch eines Barplots mit continous scale ------------------------------


# min_stand <- readxl::read_xlsx("Q:/BT2022/BT/60_Bericht/_Probegrafiken/2023-01-26 Vorlagen Balken und Linien/BT2021_Abb3.9.xlsx", sheet = "Daten BT21")
# data_plot_new <- prep_trend(min_stand, competence = "lesen", parameter = "1")$plot_bar

## Nötig wäre: sehr breiter Plot, der eine durchgezogene Skala hat. Die Bars werden dann irgendwo geplotted, und daneben wird annotated

## Liste mit Einstellungen für jede Spalte.

#
# test_data <- data.frame(
#   y_axis = 1:4,
#   x_min = rep(0, 4),
#   x_max = c(1, -2, 0.4, 3),
#   est_1 = c("12", "12", "15", "23"),
#   se_1 = c("12", "10", "8", "4"),
#   sig_2 = c(TRUE, FALSE, TRUE, FALSE)
# )
#
#
#
# plot_tablebar(dat = test_data,
#                columns_table = list("est_1", "se_1" ),
#                columns_table_sig_bold = list(NULL, "sig_2"),
#                columns_table_sig_high = list("sig_2", "sig_2"),
#                column_bar = "est_1")
