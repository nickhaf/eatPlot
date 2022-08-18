bt21_Trend <- read_excel("Q:/BT2021/BT/60_Bericht/04_Mittelwerte/adjustierte Mittelwerte/Ergebnisse/02_mitTrend_z_standard.xlsx")

bt21_Trend_years <- bt21_Trend %>%
  gather(parameter, estimate, c(est_2011:est_2021,
                                p_2011:p_2021,
                                se_2011:se_2021)#,
         # est_trend_2011.vs.2016_mean,
         # est_trend_2011.vs.2021_mean,
         # est_trend_2016.vs.2021_mean,
         # se_trend_2011.vs.2016_mean,
         # se_trend_2011.vs.2021_mean,
         # se_trend_2016.vs.2021_mean,
         # p_trend_2011.vs.2016_mean,
         # p_trend_2011.vs.2021_mean,
         # p_trend_2016.vs.2021_mean)
  ) %>%
  separate(parameter, c("parameter", "year")) %>%
  spread(parameter, estimate) %>%
  select(kb, adjust, TR_BUNDESLAND, year, est, p, se )
test <- bt21_Trend_years %>%
  mutate(year = as.factor(year))


levels(test$year) <- 1:3
levels(test$year) <- as.numeric(levels(test$year))
test$year <- as.numeric(test$year)

test %>%
  filter(kb == "GL", TR_BUNDESLAND == "Berlin")


# statt grid.arrange: https://stackoverflow.com/questions/34838870/grid-arrange-from-gridextras-exiting-with-only-grobs-allowed-in-glist-afte
