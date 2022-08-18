p2 <- bt21_Trend_years %>%
  filter(kb == "GL") %>%
  ggplot(aes(x = year, y = est, colour = adjust, group = adjust)) +
  geom_brace(aes()) +
  geom_point() +
  geom_line() +
  facet_wrap(~ TR_BUNDESLAND, ncol = 4) +
  theme_bar_iqb() +
  colour_iqb  #https://github.com/NicolasH2/ggbrace

test <- bt21_Trend_years %>%
  mutate(year = as.factor(year))


levels(test$year) <- 1:3
levels(test$year) <- as.numeric(levels(test$year))
test$year <- as.numeric(test$year)


test %>%
  filter(kb == "GL", TR_BUNDESLAND == "Berlin") %>%
  ggplot(aes(x = year, y = est, colour = adjust, group = adjust)) +
  geom_brace(aes(c(2,3), y = c(410, 425)), inherit.data = F, rotate = 180) +
  geom_brace(aes(c(1,3), y = c(430, 445)), mid = 0.25, inherit.data = F, rotate = 180) +
  geom_point() +
  geom_line() +
  theme_line_iqb() +
  facet_wrap(~ TR_BUNDESLAND, ncol = 4) +
  colour_iqb



# statt grid.arrange: https://stackoverflow.com/questions/34838870/grid-arrange-from-gridextras-exiting-with-only-grobs-allowed-in-glist-afte
