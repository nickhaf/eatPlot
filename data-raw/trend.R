trend_books <- read.csv2("Q:/BT2021/BT/60_Bericht/06_Soziale_Disparitäten/01_Rechnungen/KAS/Abb65Buecher_mitTrend.csv", na.strings = "")

usethis::use_data(trend_books, overwrite = TRUE)
