library(tidyverse)

bt21 <- read_csv2("Q:/BT2021/BT/60_Bericht/06_Soziale_Disparitäten/Abbildungen/01_KAS/Nicklas/Daten/Abb65Buecher_mitTrend_KOPIE.csv")
bt21_long <- bt21
colnames(bt21_long) <- gsub("sig", "p", colnames(bt21_long))
bt21_long <- bt21_long %>%
  #filter(group %in% unique(bt21$TR_BUNDESLAND)[-1]) %>%
  filter(parameter == "mean") %>%
  filter(kb == "GL") %>%
  filter(is.na(comparison)) %>%
  filter(!is.na(KBuecher_imp3)) %>%
  filter(!(group %in% c("0","1"))) %>%
  gather(parameter, estimate, c(est_2011:est_2021,
                                p_2011:p_2021,
                                se_2011:se_2021)
  ) %>%
  separate(parameter, c("parameter", "year")) %>%
  spread(parameter, estimate) %>%
  #select(kb, adjust, TR_BUNDESLAND, year, est, p, se )
  mutate(year = as.numeric(year)) %>%
  mutate(sig = ifelse(p < 0.05, "Sig", "noSig"))

for(i in unique(bt21$TR_BUNDESLAND)[-1]){
  bt21_long[grepl(i, bt21_long$group),"TR_BUNDESLAND"] <- i
}


plot_list <- list()
min_est <- min(bt21_long$est)
range_est <- range(bt21_long$est)
position <- 1


for(i in unique(bt21$TR_BUNDESLAND)){
  dat_long <- bt21_long %>%
    filter(TR_BUNDESLAND == i)

  bt21_sig <- bt21_long %>%
    filter(kb == "GL", TR_BUNDESLAND == i) %>%
    mutate(sig_2011.vs.2016 = ifelse(p_trend_2011.vs.2016 < 0.05, "Sig", "noSig")) %>%
    mutate(sig_2016.vs.2021 = ifelse(p_trend_2016.vs.2021 < 0.05, "Sig", "noSig"))


  p1 <- plot_points(dat_long, grouping_var = "KBuecher_imp3")

  p2 <- p1 +
    connect_points(bt21_sig, "2011", "2016", grouping_var = "KBuecher_imp3") +
    connect_points(bt21_sig, "2016", "2021", grouping_var = "KBuecher_imp3") +
    linetype_iqb +
    labs(title = i) +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5)
    ) +
    NULL


  p3 <- p2 +
    coord_cartesian(ylim = c(range_est[1] - 30, range_est[2]), clip = "off") + # necessary, so the brace can be drawn inside the plot
    draw_brace_small(dat_long = dat_long, range_est = range_est) +
    draw_brace_large(dat_long  = dat_long, range_est = range_est) +
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

