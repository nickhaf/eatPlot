library(tidyverse)
library(ggtext)
library(ggdebug)


add_column_spanner <- function(label, xmin_col, xmax_col, ...){
  list(ggplot2::annotate("segment",
                         x = xmin_col,
                         xend = xmax_col,
                         y = 36,
                         yend = 36,
                         linewidth = 0.15
  ),
  ggtext::geom_richtext(
    data = data.frame(),
    ggplot2::aes(
      x = (xmin_col + xmax_col)/2,
      y = 36.5
    ),
    inherit.aes = FALSE,
    label.padding = grid::unit(rep(0, 4), "pt"),
    fill = NA,
    label.color = NA,
    label = label)
  )

}


prep_table <- function(eatRep_dat, row_ids = "TR_BUNDESLAND", parameters = c("")) {
  long <- eatRep_dat[[1]]$plain %>%
    mutate(sig = ifelse(p < 0.05, "TRUE", "FALSE")) %>%
    pivot_longer(cols = c("est", "se", "es")) %>%
    mutate(value_label = ifelse(.$sig, paste0("**", round(.$value, 1), "**"), round(.$value, 1))) %>%
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
    distinct() %>%
    mutate(across(all_of(row_ids), ~ ., .names = "{.col}_original")) %>%  # Copy original columns
    pivot_longer(cols = all_of(row_ids), values_to = "value_label")
  colnames(column_ids) <- gsub("_original$", "", colnames(column_ids))


    column_ids <- column_ids %>%
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
    "mean_est_2022 - 2015",
    "mean_se_2015 - 2009",
    "mean_es_2015 - 2009",
    "mean_est_2015 - 2009_bar",
    "mean_est_2022 - 2015_bar"
  ),
  ordered = TRUE)) %>%
  filter(!is.na(column_id)) %>%
  mutate(width = ifelse(column_id == "NA_TR_BUNDESLAND_NA", 1/14, 1/14)) %>%
  mutate(adjustment = 0.5) %>%
  group_by(TR_BUNDESLAND) %>%
  mutate(group_id = cur_group_id()) %>%  # Assigns a unique ID per row_id
  ungroup() %>%
  mutate(background_colour = ifelse(group_id %% 2 == 1, "A", "B"))

 column_id_levels <- levels(dat_prepped$column_id)

dat_bar <- dat_prepped %>%
  filter(column_id %in% c("mean_est_2015 - 2009", "mean_est_2022 - 2015")) %>%
  mutate(column_id = paste0(column_id, "_bar")) %>%
  mutate(column_id = factor(column_id, levels = column_id_levels)) %>%
  mutate(value_label = "")  %>%
  rbind(dat_prepped) %>%
  mutate(
    column_names = case_when(
      column_id == "NA_TR_BUNDESLAND_NA" ~ "**Land**",
      column_id == "NA_Kgender_NA" ~ "**Geschlecht**",
      column_id == "mean_est_2009" ~ "*M*",
      column_id == "sd_est_2009" ~ "*SD*",
      column_id == "mean_est_2015" ~ "*M*",
      column_id == "sd_est_2015" ~ "*SD*",
      column_id == "mean_est_2022" ~ "*M*",
      column_id == "sd_est_2022" ~ "*SD*",
      column_id == "mean_est_2015 - 2009" ~ "*M<sub>2015</sub> - M<sub>2009</sub>*",
      column_id == "mean_se_2015 - 2009" ~ "*SE*",
      column_id == "mean_es_2015 - 2009" ~ "*d*",
      column_id == "mean_est_2015 - 2009_bar" ~ "",
      column_id == "mean_est_2022 - 2015_bar" ~ ""),
  ) %>%
  mutate(xmin = ifelse(column_id == "mean_est_2015 - 2009_bar", 0.9, 0.95),
         xmax = ifelse(column_id == "mean_est_2015 - 2009_bar", 0.95, 1)) %>%
  mutate(scale_min = ifelse(column_id == "mean_est_2015 - 2009_bar", -40, -30),
         scale_max = ifelse(column_id == "mean_est_2022 - 2015_bar", 20, 20)) %>%
  mutate(value = ifelse(column_id %in% c("mean_est_2015 - 2009_bar", "mean_est_2022 - 2015_bar"), value, NA))



header_dat <- unique(dat_prepped[, c("column_id", "width", "adjustment", "background_colour", "row_id")]) %>%
  mutate(value_label = paste0("**", column_id, "**"))


## How to add another row on top, if the geom_header is used?

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
  geom_header(aes(column_header = column_names), stat = StatTable, fill = "white") +
  scale_fill_manual(values = c(A = "#20D479", B = "#8DEBBC")) +
  scale_colour_manual(values = c("#20D479", "#8DEBBC")) +
   ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = c(2, 2))) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = c(0, 0)),
                              breaks = c(0.9, 0.95),
                              labels = c(-30, -25)) +
  ggpattern::geom_rect_pattern(
    mapping = aes(x = value, scale_min = scale_min, scale_max = scale_max),
    stat = StatTableColumnDebugg,
    colour = "black",
    na.rm = FALSE
  ) +
    ## Spanner
   add_column_spanner(label = "**2009<sup>a</sup>**", xmin_col = 0.3, xmax_col = 0.5) +
  theme_classic()


# ggplot2::annotate(
#   "segment",
#   x = x_intercepts,
#   xend = x_intercepts,
#   y = y_1 + 0.2,
#   yend = y_2 - 0.2,
#   colour = plot_settings$bar_background_lines_colour,
#   linetype = plot_settings$bar_background_lines_linetype,
#   linewidth = 0.1
# ) +
#   ggplot2::geom_segment(stat = StatTableColumnDebugg,
#                         mapping = aes(x = value, xmin_col = xmin, xmax_col = xmax),
#                         colour= "black") +
plot_capped_x_axis(c(0.91, 1)) ## AUch hier müssen die Werte wieder umgerechnet weren

p

cdata <- ggdebug::get_data_cache()
head(cdata$compute_layer$args$data)
head(cdata$finish_layer$return)

