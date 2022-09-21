
prepare_trend <- function(data_prep, grouping_var){

  trendEsts <- data_prep[grepl("wholeGroup", data_prep$group),]
  trendEsts[, grouping_var] <- stringr::str_extract(trendEsts$group, "0|1")
  trendEsts <- trendEsts[!is.na(trendEsts$KBuecher_imp3),]

dat_trend <- trendEsts %>%
  select(group, TR_BUNDESLAND, KBuecher_imp3, est_trend_2016.vs.2021, est_trend_2011.vs.2021, p_trend_2016.vs.2021, p_trend_2011.vs.2021) %>%
  rename(estTrend_2016vs2021 = est_trend_2016.vs.2021,
         estTrend_2011vs2021 = est_trend_2011.vs.2021,
         pTrend_2016vs2021 = p_trend_2016.vs.2021,
         pTrend_2011vs2021 = p_trend_2011.vs.2021) %>%
  gather(parameter, estimate, c(estTrend_2011vs2021, estTrend_2016vs2021,
                                pTrend_2011vs2021, pTrend_2016vs2021)) %>%
  unique %>%
  separate(parameter, c("parameter", "trendyears")) %>%
  separate(col = trendyears, into = c("year_start", "year_end"), sep = "vs") %>%
  mutate(year_start = as.numeric(year_start),
         year_end = as.numeric(year_end)) %>%
  spread(parameter, estimate) %>%
  mutate(sig_trend = ifelse(pTrend < 0.05, "bold", "plain")) #%>%
  # mutate(brace_upper_y = ifelse(year_start == min(year_start), range_est[1] - 30, range_est[1] -62)) %>%
  # mutate(brace_lower_y = ifelse(year_start == min(year_start), range_est[1] - 60, range_est[1] -92)) %>%
  # mutate(label_pos = ifelse(KBuecher_imp3 == 1, range_est[1] - 103, range_est[1] - 123))


dat_trend$TR_BUNDESLAND <- sub("_.*", "", dat_trend$group)
dat_trend[grep("wholeGroup", dat_trend$TR_BUNDESLAND), "TR_BUNDESLAND"] <- "Deutschland"
dat_trend$group <- NULL
return(dat_trend)

}
