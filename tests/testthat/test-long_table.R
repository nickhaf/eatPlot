
# Try long format ---------------------------------------------------------

library(tidyverse)
library(ggtext)


long <- trend_gender[[1]]$plain %>%
  mutate(sig = ifelse(p < 0.05, "TRUE", "FALSE")) %>%
  pivot_longer(cols = c("est", "se", "es")) %>%
  filter(parameter %in% c("mean", "sd"), comparison %in% c("none", "trend"), Kgender %in% c("weiblich", "maennlich"), name %in% c("est", "sd") ) %>%
  mutate(id_col = paste(.$parameter, .$name, .$year, sep = "_")) %>%
  mutate(y_axis = paste(.$TR_BUNDESLAND, .$Kgender, sep = "_")) %>%
  mutate(value = ifelse(.$sig, paste0("**", .$value, "**"), .$value))

## Signifikanz muss als eigene Spalte daneben.

ggplot(dat = long,
       aes(
       x = id_col,
       y = y_axis,
       label = value)) +
  geom_richtext(    label.padding = grid::unit(rep(0, 4), "pt"),
                    fill = NA,
                    label.color = NA)
