t1 <- bt21_NoTrend_prep %>%
  filter(kb == "GL" & is.na(comparison) & group != "wholeGroup") %>%
  select(group, est, adjust) %>%
  as.data.frame %>%
  reshape(idvar = "group", timevar = "adjust" , direction = "wide", v.names = c("est"))

grid.table(t1, theme = t1_theme)
