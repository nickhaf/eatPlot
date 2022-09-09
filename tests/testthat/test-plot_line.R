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
  filter(kb == "GL") %>%
  #select(kb, adjust, TR_BUNDESLAND, year, est, p, se )
  mutate(year = as.numeric(year)) %>%
  mutate(sig = ifelse(p < 0.05, "Sig", "noSig"))

plot_list <- list()
min_est <- min(bt21_Trend_years$est)
range_est <- range(bt21_Trend_years$est)
position <- 1

for(i in unique(bt21_Trend$TR_BUNDESLAND)){
dat_long <- bt21_Trend_years %>%
  filter(TR_BUNDESLAND == i)

bt21_Trend_sig <- bt21_Trend %>%
  filter(kb == "GL", TR_BUNDESLAND == i) %>%
  mutate(sig_2011.vs.2016 = ifelse(p_trend_2011.vs.2016_mean < 0.05, "Sig", "noSig")) %>%
  mutate(sig_2016.vs.2021 = ifelse(p_trend_2016.vs.2021_mean < 0.05, "Sig", "noSig"))


p1 <- plot_points(dat_long)

p2 <- p1 +
  connect_points(bt21_Trend_sig, "2011", "2016") +
  connect_points(bt21_Trend_sig, "2016", "2021") +
  linetype_iqb +
    labs(title = i) +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5)
    ) +
NULL

p3 <- p2 +
  coord_cartesian(ylim = c(range_est[1] - 30, range_est[2]), clip = "off") + # necessary, so the brace can be drawn inside the plot
  draw_brace_small(dat_long = dat_long, range_est = range_est) +
  #draw_brace_large(dat_long  = dat_long, range_est = range_est) +
  theme(plot.margin = unit(c(0, 0, 0.30, 0), units="npc")) +
  NULL

if((position - 1) %% 4 == 0){
p3 <- p3 +
  theme(axis.text.y = element_text(),
        axis.line.y = element_line(),
        axis.ticks.y = element_line()
        ) +
  scale_y_continuous(breaks = seq(from = round(range_est[1]-10, -1), to = round(range_est[2], -1), by = 20))
}

plot_list[[i]] <- p3

position <-  position + 1

}

n <- length(plot_list)
nCol <- floor(sqrt(n))
do.call(eval(parse(text = "gridExtra::grid.arrange")), c(plot_list, ncol = nCol))


## Soziale Disparitäten lieber nehmen
