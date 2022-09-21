library(tidyverse)


## Create necessary data sets: 1 long Trendset, and one long point estimate set


bt21 <- read.csv2("Q:/BT2021/BT/60_Bericht/06_Soziale_Disparitäten/Abbildungen/01_KAS/Nicklas/Daten/Abb65Buecher_mitTrend_KOPIE.csv", na.strings = "")

bt21_prep <- prepare_general(bt21)
bt21_pointEstimates <- prepare_pointEstimates(bt21_prep, competence = "GL", grouping_var = "KBuecher_imp3")

bundeslaender <- unique(bt21_pointEstimates$TR_BUNDESLAND)

plot_list <- list()
range_est <- range(bt21a_long$est)
position <- 1

for(i in bundeslaender){

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

  whole_group <- bt21 %>%
    filter(group == "wholeGroup", parameter == "mean", kb == "GL") %>%
    mutate(sig = "")

    p1 <- ggplot2::ggplot(data = bt21_pointEstimates) +
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
    plot_points(my_data = dat_long, grouping_var = "KBuecher_imp3") +
    scale_shape_manual(values = c(16, 17, 16)) # Nochmal anschauen!



p2 <- p1 +
    connect_points(bt21a_sig, "2011", "2016", grouping_var = "KBuecher_imp3") +
    connect_points(bt21a_sig, "2016", "2021", grouping_var = "KBuecher_imp3") +
    linetype_iqb +
    labs(title = i) +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5)
    )


p3 <- p2 +
    draw_brace(dat_trend = dat_trend, range_est = range_est) +
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
plot(plot_list[[i]])


  position <- position + 1



}

n <- length(plot_list)
nCol <- floor(sqrt(n))
do.call(eval(parse(text = "gridExtra::grid.arrange")), c(plot_list, ncol = nCol))

