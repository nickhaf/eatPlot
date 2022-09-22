library(tidyverse)


## Create necessary data sets: 1 long Trendset, and one long point estimate set


bt21 <- read.csv2("Q:/BT2021/BT/60_Bericht/06_Soziale_Disparitäten/Abbildungen/01_KAS/Nicklas/Daten/Abb65Buecher_mitTrend_KOPIE.csv", na.strings = "") %>%
  filter(kb == "GL")

bt21_prep <- prepare_general(bt21, competence = "GL")
bt21_pointEstimates <- prepare_pointEstimates(bt21_prep, competence = "GL", grouping_var = "KBuecher_imp3")


# Trend Comparisons within group
trendEsts_w <- bt21_prep[!is.na(bt21_prep$TR_BUNDESLAND) | bt21_prep$group %in% c("0", "1"), ]
trendEsts_w <- trendEsts_w[!is.na(trendEsts_w[,grouping_var]), ]
trendEsts_w <- trendEsts_w[is.na(trendEsts_w$comparison), ]


# Trend Comparisons against Germany
trendEsts <- data_prep[grepl("wholeGroup", data_prep$group),]
trendEsts[, grouping_var] <- stringr::str_extract(trendEsts$group, "0|1")
trendEsts <- trendEsts[!is.na(trendEsts$KBuecher_imp3),]

trend_within <- prepare_trend(trendEsts_w, "KBuecher_imp3", suffix = "within")
trend_germany <- prepare_trend(trendEsts, "KBuecher_imp3", suffix = "vsGermany")
pointEstimates <- prepare_pointEstimates(bt21_prep, competence = "GL", grouping_var = "KBuecher_imp3")
final_dat <- merge_trend_point(trend1 = trend_within, trend2 = trend_germany,
                         point = pointEstimates, grouping_var = "KBuecher_imp3")


bundeslaender <- unique(bt21_pointEstimates$TR_BUNDESLAND)

plot_list <- list()
range_est <- range(bt21_pointEstimates$est)
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

  bt21_pointEstimates <- bt21_pointEstimates %>% filter(TR_BUNDESLAND == i)

  whole_group <- bt21 %>%
    filter(group == "wholeGroup", parameter == "mean", kb == "GL")

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
    plot_points(my_data = bt21_pointEstimates, grouping_var = "KBuecher_imp3") +
    scale_shape_manual(values = c(16, 17, 16)) # Nochmal anschauen!

final_dat2 <- final_dat %>%
  filter(TR_BUNDESLAND == i) %>%
  filter(!(year_start == 2011 & year_end == 2021))

p2 <- p1 +
  connect_points(final_dat2, grouping_var = "KBuecher_imp3") +
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

