library(tidyverse)


## Create necessary data sets: 1 long Trendset, and one long point estimate set


bt21 <- read.csv2("Q:/BT2021/BT/60_Bericht/06_Soziale_Disparitäten/Abbildungen/01_KAS/Nicklas/Daten/Abb65Buecher_mitTrend_KOPIE.csv", na.strings = "") %>%
  filter(kb == "GL")

bt21_prep <- prepare_general(bt21, competence = "GL")


# Trend Comparisons within group
trendEsts_w <- bt21_prep[!is.na(bt21_prep$TR_BUNDESLAND) | bt21_prep$group %in% c("0", "1"), ]
trendEsts_w <- trendEsts_w[!is.na(trendEsts_w[,"KBuecher_imp3"]), ]
trendEsts_w <- trendEsts_w[is.na(trendEsts_w$comparison), ]


# Trend Comparisons against Germany
trendEsts <- bt21_prep[grepl("wholeGroup", bt21_prep$group),]
trendEsts[, "KBuecher_imp3"] <- stringr::str_extract(trendEsts$group, "0|1")
trendEsts <- trendEsts[!is.na(trendEsts$KBuecher_imp3),]

trend_within <- prepare_trend(trendEsts_w, "KBuecher_imp3", suffix = "within")
trend_germany <- prepare_trend(trendEsts, "KBuecher_imp3", suffix = "vsGermany")
pointEstimates <- prepare_pointEstimates(bt21_prep, competence = "GL", grouping_var = "KBuecher_imp3")
final_dat <- merge_trend_point(trend1 = trend_within, trend2 = trend_germany,
                         point = pointEstimates, grouping_var = "KBuecher_imp3")


bundeslaender <- c(unique(pointEstimates$TR_BUNDESLAND)[-1], "Deutschland")

plot_list <- list()
range_est <- range(pointEstimates$est)
position <- 1

for(i in bundeslaender){

  pointEstimates2 <- pointEstimates %>%
    filter(TR_BUNDESLAND == i)
  final_dat2 <- final_dat %>%
    filter(TR_BUNDESLAND == i) %>%
    filter(!(year_start == 2011 & year_end == 2021))

    p1 <- ggplot2::ggplot() +
      plot_settings(my_data = pointEstimates) +
      draw_background_lines(bt21 %>% filter(group == "wholeGroup", parameter == "mean", kb == "GL")) +
      plot_points(my_data = pointEstimates2, grouping_var = "KBuecher_imp3") +
      connect_points(final_dat2, grouping_var = "KBuecher_imp3") +
      draw_brace(dat_trend = final_dat, bundesland = i) +
      NULL

  if((position - 1) %% 4 == 0){
    p1 <- p1 +
      theme(axis.text.y = element_text(),
            axis.line.y = element_line(),
            axis.ticks.y = element_line()
      ) +
      scale_y_continuous(breaks = seq(from = round(range_est[1]-10, -1), to = round(range_est[2], -1), by = 20))
  }

  if(i == "Deutschland"){
    p1 <- p1 +
      theme(plot.background = element_rect(color = "black", size = 0.5, fill = NA))
  }

  plot_list[[i]] <- p1
  plot(plot_list[[i]])

  position <- position + 1
}

n <- length(plot_list)
nCol <- floor(sqrt(n))
do.call(eval(parse(text = "gridExtra::grid.arrange")), c(plot_list, ncol = nCol))


