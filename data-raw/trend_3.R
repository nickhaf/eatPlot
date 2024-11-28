d1 <- readRDS(file = "P:/Methoden/02_R_Pakete/eatRep/Outputstruktur/report2_outputs/03_mig_verschachtelt.rds")
bt_example <- d1[[1]][[1]][[1]]

trend_3 <- bt_example
trend_3$group$TR_BUNDESLAND[is.na(trend_3$group$TR_BUNDESLAND)] <- "total"

usethis::use_data(trend_3, overwrite = TRUE)
