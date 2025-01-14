d1 <- readRDS(file = "P:/Methoden/02_R_Pakete/eatRep/Outputstruktur/report2_outputs/03_mig_verschachtelt.rds")
bt_example <- d1[[1]][[1]][[1]]

trend_zwh <- bt_example
trend_zwh$group$TR_BUNDESLAND[is.na(trend_zwh$group$TR_BUNDESLAND)] <- "total"
trend_zwh$group$mhg[is.na(trend_zwh$group$mhg)] <- "total"

trend_zwh$plain <- trend_zwh$plain %>%
  mutate(across(where(is.character), ~ gsub("ersteGen", "mitZWH", gsub("aohneZWH", "ohneZWH", .))))

trend_zwh$group <- trend_zwh$group %>%
  mutate(across(where(is.character), ~ gsub("ersteGen", "mitZWH", gsub("aohneZWH", "ohneZWH", .))))

trend_zwh$group <- subset(trend_zwh$group, grepl("^total|^ohneZWH -|^mitZWH -|^mitZWH$|^ohneZWH$", mhg) & !grepl("mitZWH - einET", x = mhg))
trend_zwh$plain <- subset(trend_zwh$plain, grepl("^total|^ohneZWH -|^mitZWH -|^mitZWH$|^ohneZWH$", mhg) & !grepl("mitZWH - einET", x = mhg))

colnames(trend_zwh$group)[colnames(trend_zwh$group) == "mhg"] <- "zwh"
colnames(trend_zwh$plain)[colnames(trend_zwh$plain) == "mhg"] <- "zwh"

trend_zwh$plain$es <- rnorm(nrow(trend_zwh$plain), mean = 0, sd = 1)
trend_zwh$plain$est <- rnorm(nrow(trend_zwh$plain), mean = 40, sd = 10)
trend_zwh$plain$p <- abs(rnorm(nrow(trend_zwh$plain), mean = 0.1, sd = 0.25))

usethis::use_data(trend_zwh, overwrite = TRUE)
