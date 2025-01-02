d1 <- readRDS(file = "P:/Methoden/02_R_Pakete/eatRep/Outputstruktur/report2_outputs/03_mig_verschachtelt.rds")
bt_example <- d1[[1]][[1]][[1]]

trend_mw <- bt_example
trend_mw$group$TR_BUNDESLAND[is.na(trend_mw$group$TR_BUNDESLAND)] <- "total"
trend_mw$group$mhg[is.na(trend_mw$group$mhg)] <- "total"

trend_mw$plain <- trend_mw$plain %>%
  mutate(across(where(is.character), ~ gsub("ersteGen", "m", gsub("aohneZWH", "w", .))))

trend_mw$group <- trend_mw$group %>%
  mutate(across(where(is.character), ~ gsub("ersteGen", "m", gsub("aohneZWH", "w", .))))

trend_mw$group <- subset(trend_mw$group, grepl("^total|^w -|^m -", mhg) & !grepl("m - einET", x = mhg))
trend_mw$plain <- subset(trend_mw$plain, grepl("^total|^w -|^m -", mhg) & !grepl("m - einET", x = mhg))

usethis::use_data(trend_mw, overwrite = TRUE)




