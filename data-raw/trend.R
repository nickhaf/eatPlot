trend_books <- read.csv2("Q:/BT2021/BT/60_Bericht/06_Soziale_Disparitäten/03_Syntaxen/KAS/Abb65Buecher_mitTrend.csv",
                         na.strings = "")
trend_books$KBuecher_imp3 <- as.factor(trend_books$KBuecher_imp3)

usethis::use_data(trend_books, overwrite = TRUE)
