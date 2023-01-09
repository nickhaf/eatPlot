bt21_NoTrend <- readxl::read_excel("Q:/BT2021/BT/60_Bericht/04_Mittelwerte/adjustierte Mittelwerte/Ergebnisse/01_ohneTrend_z_standard.xlsx")

# Datenaufbereitung
bt21_NoTrend_prep <- bt21_NoTrend %>%
  mutate(significant = factor(ifelse(p < 0.05, "yes", "no"))) %>%
  mutate(adj_sig = case_when(
    adjust == "ohneAdj" & significant == "no" ~ "noAdj_noSig",
    adjust == "ohneAdj" & significant == "yes" ~ "noAdj_Sig",
    adjust == "mitAdj" & significant == "no" ~ "Adj_noSig",
    adjust == "mitAdj" & significant == "yes" ~ "Adj_Sig"
  )) %>%
  filter(kb == "GL" & comparison == "crossDiff")

# Tabelle erstellen
t1 <- bt21_NoTrend %>%
  mutate(significant = factor(ifelse(p < 0.05, "yes", "no"))) %>%
  mutate(adj_sig = case_when(
    adjust == "ohneAdj" & significant == "no" ~ "noAdj_noSig",
    adjust == "ohneAdj" & significant == "yes" ~ "noAdj_Sig",
    adjust == "mitAdj" & significant == "no" ~ "Adj_noSig",
    adjust == "mitAdj" & significant == "yes" ~ "Adj_Sig"
  )) %>%
  filter(kb == "GL" & is.na(comparison) & group != "wholeGroup") %>%
  select(group, est, adjust) %>%
  as.data.frame %>%
  reshape(idvar = "group", timevar = "adjust" , direction = "wide", v.names = c("est"))



## Plot plot
p1 <- plot_bar(bt21_NoTrend_prep) +
  #scale_x_continuous(expand = c(0, 0)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_x_continuous(sec.axis= sec_axis(
    trans = ~ . * 1,
    labels = c("", "", "Abweichung vom Mittelwert für \n Deutschland insgesamt", "", "")
  )
  ) +
  theme(axis.text.x.top = element_text(size = 10, hjust = 0.5, color = "black", face = "bold"),
        axis.line.x.top = element_line()#,
        #legend.position = "bottom"
  )



p_bartable <- cowplot::plot_grid(p3, p1, nrow = 1, align = "h")
p_bartable

