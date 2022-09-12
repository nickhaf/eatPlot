library(readxl)
library(tidyverse)

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
         #p_trend_2011.vs.2016_mean,
         #p_trend_2011.vs.2021_mean,
         #p_trend_2016.vs.2021_mean)
  ) %>%
  separate(parameter, c("parameter", "year")) %>%
  spread(parameter, estimate) %>%
  filter(kb == "GL", TR_BUNDESLAND == "Berlin") %>%
  #select(kb, adjust, TR_BUNDESLAND, year, est, p, se )
  mutate(year = as.numeric(year)) %>%
  mutate(sig = ifelse(p < 0.05, "Sig", "noSig"))

bt21_Trend_sig <- bt21_Trend %>%
  filter(kb == "GL", TR_BUNDESLAND == "Berlin") %>%
  mutate(sig_2011.vs.2016 = ifelse(p_trend_2011.vs.2016_mean < 0.05, "Sig", "noSig")) %>%
  mutate(sig_2016.vs.2021 = ifelse(p_trend_2016.vs.2021_mean < 0.05, "Sig", "noSig"))


plot_points(bt21_Trend_years) +
  connect_points(bt21_Trend_sig, "2011", "2016", grouping_var = "adjust")



