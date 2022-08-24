test <- bt21_Trend_years %>%
  mutate(year = as.factor(year)) %>%
  mutate(year_fac = as.factor(year)) %>%
  filter(kb == "GL", TR_BUNDESLAND == "Berlin")


levels(test$year_fac) <- 1:3
levels(test$year_fac) <- as.numeric(levels(test$year_fac))
test$year_fac <- as.numeric(test$year_fac)

dat_long <- bt21_Trend_years %>%
  filter(TR_BUNDESLAND == "Berlin")

p1 <- plot_points(dat_long)


draw_brace(dat_long = dat_long, my_plot = p1)
