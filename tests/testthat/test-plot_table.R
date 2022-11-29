bt21_NoTrend <- readxl::read_excel("Q:/BT2021/BT/60_Bericht/04_Mittelwerte/adjustierte Mittelwerte/Ergebnisse/01_ohneTrend_z_standard.xlsx")


bt21_NoTrend_prep <- bt21_NoTrend %>%
  mutate(significant = factor(ifelse(p < 0.05, "yes", "no"))) %>%
  mutate(adj_sig = case_when(
    adjust == "ohneAdj" & significant == "no" ~ "noAdj_noSig",
    adjust == "ohneAdj" & significant == "yes" ~ "noAdj_Sig",
    adjust == "mitAdj" & significant == "no" ~ "Adj_noSig",
    adjust == "mitAdj" & significant == "yes" ~ "Adj_Sig"
  )) %>%
  filter(kb == "GL" & comparison == "crossDiff")



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


p1 <- plot_bar(bt21_NoTrend_prep) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


# table als ggplot --------------------------------------------------------

t1_b <- t1 %>%
  mutate_at(c("est.ohneAdj", "est.mitAdj"), round) %>%
  mutate_at(c("est.ohneAdj", "est.mitAdj"), as.character) %>%
  mutate(Bundesland = group) %>%
  pivot_longer(cols = c("est.ohneAdj", "est.mitAdj", "Bundesland"), names_to = "estimate" )

p2 <- ggplot(t1_b, aes(x = estimate, y = group)) +
  ggforestplot::geom_stripes(odd = rgb(219, 238, 244, maxColorValue = 255),
                             even = "#00000000") +
  geom_text(aes(label = value), hjust = 0) +
  theme_bar_iqb() +
  scale_x_discrete(position = "top") +
  theme(plot.margin = unit(c(0, -0.1, 0, 0), "cm")) +
  NULL


library(cowplot)
plot_grid(p2, p1, nrow = 1, align = "h")
