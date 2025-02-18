draw_header <- function(data, panel_scales, coord) {

  data_header <- data %>%
    select(-c(text, ymin, ymax)) %>%
    mutate(y = max(data$ymax) + 1) %>%
    mutate(group = as.numeric(factor(column))) %>%
    unique

  coords <- unique(coord$transform(data_header, panel_scales))

  coords <- coords %>%
    mutate(x = case_when(hjust == 0 ~ xmin,
                         hjust == 1 ~ xmax,
                         TRUE ~ (coords$xmin + coords$xmax)/2,
    ))

  gridtext::richtext_grob(
    text = coords$column, ## search for original group levels in data
    x = coords$x,
    y = max(coords$y),
    hjust = coords$hjust)
}

GeomHeader <- ggproto("GeomTable", Geom,
                     required_aes = c("column", "text"),
                     #default_aes = aes(xmin = -Inf, xmax = Inf),
                     draw_panel = draw_header)

geom_header <- function(mapping = NULL, data = NULL, stat = "table",
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomHeader, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

