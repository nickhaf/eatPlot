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

range_est <- range(dat_long$est)

p1 +
draw_brace_small(dat_long = dat_long, range_est = range_est) +
draw_brace_large(dat_long  = dat_long, range_est = range_est) +
coord_cartesian(ylim = c(range_est[1] - 30, range_est[2] + 30), clip = "off") + # limits without droping observations
theme(plot.margin = unit(c(0, 0, 0.30, 0), units="npc")) +
NULL
