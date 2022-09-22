## Hier auf alle Fälle anfangs und Enddatensatz vergleichen


# Trend Comparisons within group
trendEsts_w <- data_prep[!is.na(data_prep$TR_BUNDESLAND) | data_prep$group %in% c("0", "1"), ]
trendEsts_w <- trendEsts_w[!is.na(trendEsts_w[,grouping_var]), ]
trendEsts_w <- trendEsts_w[is.na(trendEsts_w$comparison), ]


# Trend Comparisons against Germany
trendEsts <- data_prep[grepl("wholeGroup", data_prep$group),]
trendEsts[, grouping_var] <- stringr::str_extract(trendEsts$group, "0|1")
trendEsts <- trendEsts[!is.na(trendEsts$KBuecher_imp3),]

trend_within <- prepare_trend(trendEsts_w, "KBuecher_imp3", suffix = "within")
trend_germany <- prepare_trend(trendEsts, "KBuecher_imp3", suffix = "vsGermany")
pointEstimates <- prepare_pointEstimates(bt21_prep, competence = "GL", grouping_var = "KBuecher_imp3")


res <- merge_trend_point(trend1 = trend_within, trend2 = trend_germany,
                         point = pointEstimates, grouping_var = "KBuecher_imp3")
