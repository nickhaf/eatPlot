
t1 <- bt21_NoTrend_prep %>%
  filter(kb == "GL" & is.na(comparison) & group != "wholeGroup") %>%
  select(group, est, adjust) %>%
  as.data.frame %>%
  reshape(idvar = "group", timevar = "adjust" , direction = "wide", v.names = c("est"))

t1_theme <- ttheme_minimal(
  core=list(bg_params = list(fill = c(rep(c("white", rgb(219, 238, 244, maxColorValue = 255)))))
  )
)

grid.table(t1, theme = t1_theme)
