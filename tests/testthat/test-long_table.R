
# Try long format ---------------------------------------------------------

library(tidyverse)
library(ggtext)


## Argument: Bar-cols, where you have to provide the column ids for all bar cols.

long <- trend_gender[[1]]$plain %>%
  mutate(sig = ifelse(p < 0.05, "TRUE", "FALSE")) %>%
  pivot_longer(cols = c("est", "se", "es")) %>%
  filter(parameter %in% c("mean", "sd"), comparison %in% c("none", "trend"), Kgender %in% c("weiblich", "maennlich"), name %in% c("est", "se") ) %>%
  mutate(value_label = ifelse(.$sig, paste0("**", .$value, "**"), .$value)) %>%
  mutate(group_id = paste(.$TR_BUNDESLAND, .$Kgender, sep = "_"))  %>%
  mutate(y_axis = as.numeric(as.factor(group_id)))


id_cols <- c("TR_BUNDESLAND", "Kgender")


## Die ID-cols müssen als eigene Columns ganz vorne ran.

id_cols <- unique(long[, c(id_cols, "y_axis")]) %>%
  pivot_longer(cols = all_of(id_cols), values_to = "value_label") %>%
  group_by(value_label) %>%
  mutate(value_label = if_else(name == "TR_BUNDESLAND" & duplicated(value_label), "", value_label)) %>%
  ungroup()

long_2 <- long %>%
  dplyr::bind_rows(id_cols) %>%
  mutate(id_col = paste(.$parameter, .$name, .$year, sep = "_")) %>%
  mutate(x_axis = as.numeric(as.factor(id_col))) %>%
  arrange(x_axis) %>%
  mutate(bar = ifelse(.$id_col == "mean_est_2015 - 2009", 1, 0))%>%
  mutate(value_label = ifelse(.$bar == 1, "", .$value_label)) %>%
  mutate(x_axis = case_when(name == "TR_BUNDESLAND" ~ 1,
                            name == "Kgender" ~ 2,
                            year == 2009 & parameter == "mean" & comparison == "none" & name == "est" ~ 3,
                            year == 2009 & parameter == "sd" & comparison == "none" &  name == "est" ~ 4,
                            year == 2015 & parameter == "mean" & comparison == "none"  & name == "est" ~ 5,
                            year == 2015 & parameter == "sd" & comparison == "none"  & name == "est"~ 6,
                            year == 2022 & parameter == "mean" & comparison == "none" & name == "est" ~ 7,
                            year == 2022 & parameter == "sd" & comparison == "none" & name == "est" ~ 8,
                            year == "2022 - 2015" & parameter == "mean" & comparison == "trend"  & name == "est" ~ 9)) %>%
  filter(!is.na(x_axis)) %>%
  mutate(bar = ifelse(comparison == "trend", 1, 0 )) %>%
  mutate(width = ifelse(bar != 1 |is.na(bar) , 6.25, 50))

colnumber <- length(unique(long_2$x_axis)) - 1

accuracy = 10
long_bar <- long_2 %>%
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

position_width <- long_2 %>%
  group_by(x_axis) %>%
  summarise(total_width = mean(width)) %>%
  mutate(col_min = lag(cumsum(total_width), default = 0),
         col_max = cumsum(total_width)) %>%
  mutate(col_mean = (col_min + col_max)/2)

## Es hängt davon ab,  wo der 0-Punkt gesetzt werden soll, ob alle Werte negativ oder manch postiv und negativ sind.
## Und auch davon, wie die Aufteilung auf der Achse ist. Ob sie von -30 - 40 oder von -50 - 0 etc. geht.
## Okay, auf dieser Achse ist der Startpunkt immer NULL.

long_pos <- long_2 %>%
  left_join(position_width, by = "x_axis") %>%
  mutate(bar_value = ifelse(bar == 1, col_max + .$value, 0)) %>%
  mutate(bar_zero =  ifelse(bar == 1, col_max, 0))


## For the barplot: Just add everything before to the bars. Might need to use geom_rect?
## Also: Calc column min and max, so I can set cell colours.
## Also: leave space in the plot tablebar function, and add the plot manually. This enables to add other plottypes as well, eg. distribution etc.
y_max <- max(long_pos$y_axis)

header_dat <- unique(long_pos[, c("id_col", "col_mean")]) %>%
  mutate(y_axis = y_max + 1) %>%
  mutate(value_label = paste0("**", id_col, "**"))

  ## specify header-col in function.
## Check if x_axis is single. If not, there are some duplicates.

p1 <- ggplot(dat = long_pos,
       aes(
       x = col_mean,
       y = y_axis,
       label = value_label)) +
  geom_richtext(    label.padding = grid::unit(rep(0, 4), "pt"),
                    fill = NA,
                    label.color = NA) +
  ## evtl. diese dann gesondert hinzufügen in die Lücken, mit gesonderten Daten:
  ggpattern::geom_rect_pattern(
    aes(xmin = bar_zero , xmax = bar_value,
        ymin = .data$y_axis -0.25, ymax = .data$y_axis + 0.25)) +
  geom_header(dat = header_dat, aes(text = value_label))



long_pos %>%
  stat_table(mapping = aes(group = col_mean, label = "value_label"))

# Header grob -------------------------------------------------------------
ggplot(dat = long_pos,
             aes(
               x = col_mean,
             #  y = y_axis,
               text = value_label,
             group = y_axis)) +
  geom_table(stat=StatTableDebug) #+
  #geom_header(dat = header_dat)

  cdata <- ggdebug::get_data_cache()
  head(cdata$compute_layer$args$data)
  cdata$finish_layer$return
# geom_header <- function(){
#   list(
#   ggplot2::geom_rect(
#     ggplot2::aes(
#       xmin = -Inf, xmax = Inf,
#       ymin = max(.data$y_axis) + 0.5, ymax = max(.data$y_axis) + 1.5
#     ),
#     colour = NA,
#     fill = "green"
#   ) ,
#     geom_richtext(dat = header_dat,
#                   label.padding = grid::unit(rep(0, 4), "pt"),
#                   fill = NA,
#                   label.color = NA)
#   )
# }


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
