library(tidyverse)

bt21 <- read_csv2("Q:/BT2021/BT/60_Bericht/06_Soziale_Disparitäten/Abbildungen/01_KAS/Nicklas/Daten/Abb65Buecher_mitTrend_KOPIE.csv")
colnames(bt21) <- gsub("sig", "p", colnames(bt21))

bt21a <- bt21 %>%
  #filter(group %in% unique(bt21$TR_BUNDESLAND)[-1]) %>%
  filter(parameter == "mean") %>%
  filter(kb == "GL")


for(i in unique(bt21a$TR_BUNDESLAND)[!is.na(unique(bt21a$TR_BUNDESLAND))]){
  for(j in c("2011", "2016", "2021")){
  bt21a[grepl(i, bt21a$group),"TR_BUNDESLAND"] <- i
  # Eintragen der p-Werte in die entsprchende Zeile
  bt21a[grepl(paste0(i, "_0"), bt21a$group), paste0("p_", j)] <- bt21a[grepl(paste0(i, "_0", ".vs.wholeGroup"), bt21a$group), paste0("p_",j )]
}
}

bt21a <- bt21a %>%
  filter(!is.na(TR_BUNDESLAND))


bt21a_long <- bt21a %>%
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

for(i in unique(bt21a$TR_BUNDESLAND)){
  bt21a_long[grepl(i, bt21a_long$group),"TR_BUNDESLAND"] <- i
}


plot_list <- list()
min_est <- min(bt21a_long$est)
range_est <- range(bt21a_long$est)
position <- 1


for(i in unique(bt21a$TR_BUNDESLAND)){
  dat_long <- bt21a_long %>%
    filter(TR_BUNDESLAND == i)

  bt21a_sig <- bt21a %>%
    filter(kb == "GL",
           TR_BUNDESLAND == i,
           parameter == "mean",
           kb == "GL",
           is.na(comparison),
           !is.na(KBuecher_imp3),
           !(group %in% c("0","1"))) %>%
    mutate(sig_2011.vs.2016 = ifelse(p_trend_2011.vs.2016 < 0.05, "Sig", "noSig")) %>%
    mutate(sig_2016.vs.2021 = ifelse(p_trend_2016.vs.2021 < 0.05, "Sig", "noSig")) %>%
    mutate(sig = "")

  whole_group <- bt21 %>% filter(group == "wholeGroup", parameter == "mean", kb == "GL") %>%
    mutate(sig = "")

    p1 <- ggplot2::ggplot(data = dat_long) +
    geom_segment(data = whole_group,
                 aes(
                   x = rep(as.numeric(2011), nrow(whole_group)),
                   xend = rep(as.numeric(2016), nrow(whole_group)),
                   y = get(paste0("est_", 2011)),
                   yend = get(paste0("est_", 2016))
                 ),
                 size = 0.7,
                 color = rgb(147, 205, 221,
                             maxColorValue = 255)) +
    geom_segment(data = whole_group,
                 aes(
                   x = rep(as.numeric(2016), nrow(whole_group)),
                   xend = rep(as.numeric(2021), nrow(whole_group)),
                   y = get(paste0("est_", 2016)),
                   yend = get(paste0("est_", 2021))
                 ),
                 size = 0.7,
                 color = rgb(147, 205, 221,
                             maxColorValue = 255)) +
    plot_points(my_data = dat_long, grouping_var = "KBuecher_imp3")+
    scale_shape_manual(values = c(16, 17, 16)) # Nochmal anschauen!



p2 <- p1 +
    connect_points(bt21a_sig, "2011", "2016", grouping_var = "KBuecher_imp3") +
    connect_points(bt21a_sig, "2016", "2021", grouping_var = "KBuecher_imp3") +
    linetype_iqb +
    labs(title = i) +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5)
    )
library(latex2exp)

upper_label_2016 <- round(unique(dat_long %>% filter(KBuecher_imp3 == 1) %>% .$est_trend_2016.vs.2021), 0)
lower_label_2016 <- round(unique(dat_long %>% filter(KBuecher_imp3 == 0) %>% .$est_trend_2016.vs.2021), 0)

upper_label_2011 <- round(unique(dat_long %>% filter(KBuecher_imp3 == 1) %>% .$est_trend_2011.vs.2021), 0)
lower_label_2011 <- round(unique(dat_long %>% filter(KBuecher_imp3 == 0) %>% .$est_trend_2011.vs.2021), 0)

p3 <- p2 +
    coord_cartesian(ylim = c(range_est[1] - 30, range_est[2]), clip = "off") + # necessary, so the brace can be drawn inside the plot
    draw_brace_small(dat_long = dat_long, range_est = range_est, upper_label = upper_label_2016, lower_label = lower_label_2016) +
    draw_brace_large(dat_long  = dat_long, range_est = range_est, upper_label = upper_label_2011, lower_label = lower_label_2011) +
    theme(plot.margin = unit(c(0, 0, 0.30, 0), units="npc")) +
    annotate("text", x = 2013.47, y = 330, size = 2,
           label = "~bold(-24)",
           parse = TRUE)
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
plot(plot_list[[i]])

  position <- position + 1

}

n <- length(plot_list)
nCol <- floor(sqrt(n))
do.call(eval(parse(text = "gridExtra::grid.arrange")), c(plot_list, ncol = nCol))

