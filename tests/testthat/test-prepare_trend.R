# Trend Comparisons within group
trendEsts_w <- data_prep[!is.na(data_prep$TR_BUNDESLAND) | data_prep$group %in% c("0", "1"), ]
trendEsts_w <- trendEsts_w[!is.na(trendEsts_w[,grouping_var]), ]
trendEsts_w <- trendEsts_w[is.na(trendEsts_w$comparison), ]


# Trend Comparisons against Germany
trendEsts <- data_prep[grepl("wholeGroup", data_prep$group),]
trendEsts[, grouping_var] <- stringr::str_extract(trendEsts$group, "0|1")
trendEsts <- trendEsts[!is.na(trendEsts$KBuecher_imp3),]

