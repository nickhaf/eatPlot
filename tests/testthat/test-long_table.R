library(tidyverse)
library(ggtext)
library(ggdebug)

prep_table <- function(eatRep_dat, row_ids = "TR_BUNDESLAND", parameters = c("")) {
  long <- eatRep_dat[[1]]$plain %>%
    mutate(sig = ifelse(p < 0.05, "TRUE", "FALSE")) %>%
    pivot_longer(cols = c("est", "se", "es")) %>%
    mutate(value_label = ifelse(.$sig, paste0("**", .$value, "**"), .$value)) %>%
    mutate(row_id = paste(.$TR_BUNDESLAND, .$Kgender, sep = "_")) %>%
    ## How to make more functional?
    filter(
      parameter %in% c("mean", "sd"),
      comparison %in% c("none", "trend"),
      Kgender %in% c("weiblich", "maennlich"),
      name %in% c("est", "se", "es")
    )

  column_ids <- long %>%
    select(all_of(row_ids), row_id) %>%
    unique() %>%
    pivot_longer(cols = all_of(row_ids), values_to = "value_label") %>%
    bind_rows(long) %>%
    mutate(column_id = paste(.$parameter, .$name, .$year, sep = "_"))

  return(column_ids)
}

dat_prepped <- prep_table(trend_gender, row_ids = c("TR_BUNDESLAND", "Kgender")) %>%
  group_by(value_label) %>%
  mutate(value_label = if_else(name == "TR_BUNDESLAND" & duplicated(value_label), "", value_label)) %>%
  ungroup()



# Header grob -------------------------------------------------------------

dat_prepped <- dat_prepped %>%
  mutate(column_id = factor(column_id,
                            levels = c(
    "NA_TR_BUNDESLAND_NA",
    "NA_Kgender_NA",
    "mean_est_2009",
    "sd_est_2009",
    "mean_est_2015",
    "sd_est_2015",
    "mean_est_2022",
    "sd_est_2022",
    "mean_est_2015 - 2009",
    "mean_se_2015 - 2009",
    "mean_es_2015 - 2009",
    "mean_est_2015 - 2009_bar"
  ),
  ordered = TRUE)) %>%
  filter(!is.na(column_id)) %>%
  mutate(width = ifelse(column_id == "NA_TR_BUNDESLAND_NA", 1/12, 1/12)) %>%
  mutate(adjustment = 0) %>%
  mutate(background_colour = ifelse(column_id == "NA_TR_BUNDESLAND_NA", "red", "green")) %>%
  mutate(xmin = 0.9166667, xmax = 1)

column_id_levels <- levels(dat_prepped$column_id)

dat_bar <- dat_prepped %>% filter(column_id == "mean_est_2015 - 2009") %>%
  mutate(column_id = paste0(column_id, "_bar")) %>%
  mutate(column_id = factor(column_id, levels = column_id_levels)) %>%
  mutate(value_label = "") %>%
  rbind(dat_prepped)


header_dat <- unique(dat_prepped[, c("column_id", "width", "adjustment", "background_colour", "row_id")]) %>%
  mutate(value_label = paste0("**", column_id, "**"))

## I need a factor of the row-goups.

dat_bar$column_id

p <- ggplot(
  dat = dat_bar,
  aes(
    text = value_label,
    column = column_id,
    row = row_id,
    col_width = width,
    hjust = adjustment,
    fill = background_colour,
    colour = background_colour
  )
) +
  geom_table(stat = StatTableDebugg) +
  geom_header(aes(column_header = column_id), stat = StatTableDebugg) +
  # ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = c(0, 2))) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = c(0, 0))) +
   ggpattern::geom_rect_pattern(
    dat = dat_bar,
    mapping = aes(x = value, xmin_col = xmin, xmax_col = xmax), ## use column instead. Can the measures be extracted from before?
    stat = StatTableColumnDebugg,
    colour = "black"
  ) +
  plot_capped_x_axis(c(0.7, 0.9)) ## AUch hier müssen die Werte wieder umgerechnet weren


## Optimally, I'd have a own scale_x_continuous only for this plot. Alternatively, I could of course just draw the
## y axis again

p

## next: Build scale for col width.

## Zurodnung zur Spalte einfach über Gruppe. Aber gruppe ist vdann weg oder? Wie kann ich die Zuordnung über
cdata <- ggdebug::get_data_cache()
head(cdata$compute_layer$args$data)
View(cdata$finish_layer$return)

## Okay, obviously ther is only one group in the data now. I somehow need to pass the coordinates of the previous
## column. Easiest way would be to just use the same data, with all values but the ones that should be plotted set to zero.
## Then the statTable would be enough.
