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


p1 <- plot_points(bt21_Trend_years)
p1 +
  draw_brace(dat_long = bt21_Trend_years)

  ggbrace::geom_brace(aes(
                          x = c(2016, 2021),
                          y = c(min(bt21_Trend_years$est) - 15, min(bt21_Trend_years$est) - 12),
                          label = "my_label \n my_label2"
                          ),
                      inherit.data = F,
                      rotate = 180,
                      size = 0.8,
                      npoints = 200,
                      labelsize = 3) +
  ggbrace::geom_brace(aes(
                          x = c(2011, 2021),
                          y = c(min(bt21_Trend_years$est) - 10, min(bt21_Trend_years$est) - 7),
                          label = "my_label"
                          ),
                      mid = 0.25,
                      inherit.data = F,
                      rotate = 180,
                      size = 0.8,
                      npoints = 200) +
  coord_cartesian(ylim=range(bt21_Trend_years$est), clip = "off") + #for the range just use the data for the respective axis
  theme(plot.margin = unit(c(0, 0, 0.40, 0), units="npc"))
  #ggbrace::geom_brace(aes(c(2,3), y = c(410, 425)), inherit.data = F, rotate = 180) +
  #ggbrace::geom_brace(aes(c(1,3), y = c(430, 445)), mid = 0.25, inherit.data = F, rotate = 180) +
  NULL

