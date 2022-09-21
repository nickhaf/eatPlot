
data_wide <- prepare_wide(bt21, "GL")

prepare_trend <- function(data_wide){

data_long <- data_wide %>%
  filter(is.na(comparison)) %>%
  filter(!is.na(.[[grouping_var]])) %>%
  #filter(!(group %in% c("0","1"))) %>%
  gather(parameter, estimate, c(est_2011:est_2021,
                                p_2011:p_2021,
                                se_2011:se_2021)
  ) %>%
  separate(parameter, c("parameter", "year")) %>%
  spread(parameter, estimate)


data_long <- data_long[,c("kb", "TR_BUNDESLAND", grouping_var, "year", "est", "p")] %>%
  rename(p_est = p) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(sig = ifelse(p_est < 0.05, "Sig", "noSig"))






dat_long <- bt21_long %>%
  filter(TR_BUNDESLAND == i)

## Trenddatensatz bauen --> trennen von Trend und Punktestimates. Außerdem year_start und year_end als eigene Spalten
dat_trend <- data_wide %>%
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
  mutate(sig_trend_2016 = ifelse(pTrend < 0.05, "bold", "plain")) %>%
  mutate(brace_upper_y = ifelse(year_start == min(year_start), range_est[1] - 30, range_est[1] -62)) %>%
  mutate(brace_lower_y = ifelse(year_start == min(year_start), range_est[1] - 60, range_est[1] -92)) %>%
  mutate(label_pos = ifelse(KBuecher_imp3 == 1, range_est[1] - 103, range_est[1] - 123))



}
