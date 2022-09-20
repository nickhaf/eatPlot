draw_brace <- function(dat_long, range_est, upper_label, lower_label, year_vec){

  upper_label_2016 <- round(unique(dat_long %>% filter(KBuecher_imp3 == 1) %>%
                                     .$est_trend_2016.vs.2021), 0)
  lower_label_2016 <- round(unique(dat_long %>% filter(KBuecher_imp3 == 0)
                                   %>% .$est_trend_2016.vs.2021), 0)

  upper_label_2011 <- round(unique(dat_long %>% filter(KBuecher_imp3 == 1)
                                   %>% .$est_trend_2011.vs.2021), 0)
  lower_label_2011 <- round(unique(dat_long %>% filter(KBuecher_imp3 == 0)
                                   %>% .$est_trend_2011.vs.2021), 0)


  ## Trenddatensatz bauen --> trennen von Trend und Punktestimates. Außerdem year_start und year_end als eigene Spalten


dat_trend <- dat_long %>%
  select(group, KBuecher_imp3, est_trend_2016.vs.2021, est_trend_2011.vs.2021, p_trend_2016.vs.2021, p_trend_2011.vs.2021) %>%
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



c(lapply(unique(dat_trend$year_start), function(x){

  brace_coordinates <- dat_trend %>%
    select(year_start, year_end, brace_upper_y, brace_lower_y) %>%
    filter(year_start == x) %>%
    unique

ggbrace::geom_brace(
    mapping = aes(
      x = c(brace_coordinates$year_start, brace_coordinates$year_end),
      y = c(brace_coordinates$brace_upper_y, brace_coordinates$brace_lower_y)
        ),
    mid = ifelse(brace_coordinates$year_start == min(dat_trend$year_start), 0.25, 0.5),
    inherit.data = F,
    rotate = 180,
    size = 0.8,
    npoints = 200
    )
}),
geom_text(data = dat_trend,
          mapping = aes(
            x = year_start + (year_end - max(year_start))/2,
            y = label_pos, # Definition ist oben ja auch datenabhängig, geht hier genauso, -20 oder so drauf
            label = estTrend,
            fontface = sig_trend_2016
          ),
          size = 3
)
)
}
