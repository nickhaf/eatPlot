## code to prepare `adjusted_means` dataset goes here
adjusted_means <- as.data.frame(readxl::read_excel("Q:/BT2021/BT/60_Bericht/04_Mittelwerte/adjustierte Mittelwerte/Ergebnisse/01_ohneTrend_z_standard.xlsx"))

adjusted_means_diff <- adjusted_means[!is.na(adjusted_means$comparison), ]

# randomly permute values
for(i in c("es", "est", "p", "se")){

adjusted_means_diff[, i] <- sample(adjusted_means_diff[, i], replace = TRUE)

}

usethis::use_data(adjusted_means_diff, overwrite = TRUE)
