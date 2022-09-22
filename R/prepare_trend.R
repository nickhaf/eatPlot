
prepare_trend <- function(my_data, grouping_var, suffix){
  var_pos <- which(colnames(my_data) %in% c("est_trend_2016.vs.2021",
                                           "est_trend_2011.vs.2021",
                                           "est_trend_2011.vs.2016",
                                           "p_trend_2016.vs.2021",
                                           "p_trend_2011.vs.2021",
                                           "p_trend_2011.vs.2016")
                  )

  colnames(my_data)[var_pos] <- gsub("\\.", "",
                                     gsub("_", "", colnames(my_data)[var_pos]))

  colnames(my_data)[var_pos] <- sapply(colnames(my_data)[var_pos], function(x) sub("2", "_2", x))

  dat_trend <- my_data[,c("group", "TR_BUNDESLAND", grouping_var, colnames(my_data)[var_pos])]

dat_trend2 <- dat_trend %>%
  gather(parameter, estimate, colnames(my_data)[var_pos]) %>%
  unique %>%
  separate(parameter, c("parameter", "trendyears")) %>%
  separate(col = trendyears, into = c("year_start", "year_end"), sep = "vs") %>%
  mutate(year_start = as.numeric(year_start),
         year_end = as.numeric(year_end)) %>%
  spread(parameter, estimate) %>%
  mutate(sig_trend = ifelse(ptrend < 0.05, "bold", "plain")
         )


dat_trend2$TR_BUNDESLAND <- sub("_.*", "", dat_trend2$group)
dat_trend2[grep("wholeGroup", dat_trend2$TR_BUNDESLAND), "TR_BUNDESLAND"] <- "Deutschland"
dat_trend2[dat_trend2$TR_BUNDESLAND %in% c("0", "1"), "TR_BUNDESLAND"] <- "Deutschland"
dat_trend2$group <- NULL

colnames(dat_trend2)[colnames(dat_trend2) %in% c("esttrend", "ptrend", "sig_trend")] <- c( paste0("estTrend_", suffix), paste0("pTrend_", suffix), paste0("sigTrend_", suffix))

return(dat_trend2)

}
