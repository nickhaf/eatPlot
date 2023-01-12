## code to prepare `adjusted_means` dataset goes here
adjusted_means <- as.data.frame(readxl::read_excel("Q:/BT2021/BT/60_Bericht/04_Mittelwerte/adjustierte Mittelwerte/Ergebnisse/01_ohneTrend_z_standard.xlsx"))

usethis::use_data(adjusted_means, overwrite = TRUE)
