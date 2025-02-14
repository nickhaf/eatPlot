
# Try long format ---------------------------------------------------------

library(tidyverse)
library(ggtext)


## Argument: Bar-cols, where you have to provide the column ids for all bar cols.

long <- trend_gender[[1]]$plain %>%
  mutate(sig = ifelse(p < 0.05, "TRUE", "FALSE")) %>%
  pivot_longer(cols = c("est", "se", "es")) %>%
  filter(parameter %in% c("mean", "sd"), comparison %in% c("none", "trend"), Kgender %in% c("weiblich", "maennlich"), name %in% c("est", "sd") ) %>%
  mutate(id_col = paste(.$parameter, .$name, .$year, sep = "_")) %>%
  mutate(y_axis = paste(.$TR_BUNDESLAND, .$Kgender, sep = "_")) %>%
  mutate(value_label = ifelse(.$sig, paste0("**", .$value, "**"), .$value)) %>%
  mutate(x_axis = as.numeric(as.factor(id_col))) %>%
  arrange(x_axis) %>%
  mutate(bar = ifelse(.$id_col == "mean_est_2015 - 2009", 1, 0))%>%
  mutate(width = ifelse(bar == 1, 70, 6.363636)) %>%
  mutate(y_axis = as.numeric(as.factor(y_axis))) %>%
  mutate(value_label = ifelse(.$bar == 1, "", .$value_label))

colnumber <- length(unique(long$x_axis)) - 1

accuracy = 10
long_bar <- long %>%
  filter(bar == 1, name == "est") %>%
  group_by(bar)


x_lims <- long_bar %>%
  summarize(min = plyr::round_any(min(value), accuracy = accuracy, floor),
            max = plyr::round_any(max(value), accuracy = accuracy, ceiling))

bar_1_range <- diff(range(x_lims$min, x_lims$max))


## Same for other bars
total_range <- bar_1_range/0.5 ## Das muss die range vom ersten bar-Plot sein. der wird zur Justierung genommen.

## WArning: Range set for multiple bars. Only using the first one to keep them on the same scale

## now, give each column the same range:

col_range <- (total_range - bar_1_range)/colnumber

## Set x-axis according to this!! Afterwards I can adjust depending on the column width
## Just start at 0. Then cummulate!

position_width <- long %>%
  group_by(x_axis) %>%
  summarise(total_width = mean(width)) %>%
  mutate(col_min = lag(cumsum(total_width), default = 0),
         col_max = cumsum(total_width)) %>%
  mutate(col_mean = (col_min + col_max)/2)

long_pos <- long %>%
  left_join(position_width, by = "x_axis") %>%
  mutate(bar_value = ifelse(bar == 1, .$value + col_mean, 0)) %>%
  mutate(bar_zero =  ifelse(bar == 1, 0 + col_mean, 0))


## For the barplot: Just add everything before to the bars. Might need to use geom_rect?
## Also: Calc column min and max, so I can set cell colours.
## Also: leave space in the plot tablebar function, and add the plot manually. This enables to add other plottypes as well, eg. distribution etc.

p1 <- ggplot(dat = long_pos,
       aes(
       x = col_mean,
       y = y_axis,
       label = value_label)) +
  geom_richtext(    label.padding = grid::unit(rep(0, 4), "pt"),
                    fill = NA,
                    label.color = NA) +
  ggpattern::geom_rect_pattern(
    aes(xmin = bar_zero , xmax = bar_value,
        ymin = .data$y_axis -0.25, ymax = .data$y_axis + 0.25))


## Kann ich den dan weiter bearbeiten, um die Headers etc. zu adden?
#- evtl. ließe sich ein dritter Plot darüber legen.

## How to add the bars? That's what might get annoying
## - The values are just another column, just like the values.
## - x_axis is numeric as well. But: It has to be build, depending on the ranges of the barplots.
## So: Another column with bar = true/false is  needed.
## I filter for it in the function first. Then I have to bring the column Abstände und the Bar Abstände ontot the same scale.


## 1) calculate the axis lims for the plot. If no barplot is there, not necessary
## 2) calcualte the column coordinates, depending on the barplot coordinates. (just add them to the data frame - can be used for setting the y-axis)
## 3) Instead of nudging, I can just write the correct x-value into the data.frame directly
