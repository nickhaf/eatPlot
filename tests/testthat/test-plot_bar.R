library(readxl)
library(tidyverse)

## https://stackoverflow.com/questions/31038709/how-to-write-a-test-for-a-ggplot-plot

bt21_NoTrend <- read_excel("Q:/BT2021/BT/60_Bericht/04_Mittelwerte/adjustierte Mittelwerte/Ergebnisse/01_ohneTrend_z_standard.xlsx")


bt21_NoTrend_prep <- bt21_NoTrend %>%
  mutate(significant = factor(ifelse(p < 0.05, "yes", "no"))) %>%
  mutate(adj_sig = case_when(
    adjust == "ohneAdj" & significant == "no" ~ "noAdj_noSig",
    adjust == "ohneAdj" & significant == "yes" ~ "noAdj_Sig",
    adjust == "mitAdj" & significant == "no" ~ "Adj_noSig",
    adjust == "mitAdj" & significant == "yes" ~ "Adj_Sig"
  )) %>%
  filter(kb == "GL" & comparison == "crossDiff")

plot_bar(bt21_NoTrend_prep)
