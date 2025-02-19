draw_header <- function(data, panel_scales, coord) {

  ## column levels are enough here!
  ## Actually it might be a good Idea to ADD this to the previous data, so it gets returned in the next section.


  data_header <- data %>%
    select(-c(text, ymin, ymax, row)) %>%
    mutate(y = max(data$ymax) + 1) %>%
    mutate(group = as.numeric(factor(column_header))) %>%
    unique() %>%
    mutate(ymin = y - 0.5, ymax = y +0.5)

  coords <- unique(coord$transform(data_header, panel_scales))

  coords <- coords %>%
    mutate(x = case_when(hjust == 0 ~ xmin,
                         hjust == 1 ~ xmax,
                         TRUE ~ (coords$xmin + coords$xmax)/2,
    ))

  text <- gridtext::richtext_grob(
    text = coords$column_header, ## search for original group levels in data
    x = coords$x,
    y = max(coords$y),
    hjust = coords$hjust)

  background <- grid::rectGrob(
    x = (coords$xmin + coords$xmax)/2,
    y = (coords$ymin + coords$ymax)/2,
    width = coords$xmax - coords$xmin,
    height = coords$ymax - coords$ymin,
    default.units = "native",
    gp = grid::gpar(fill = coords$fill,
                    col = coords$colour)
  )
  grid::gTree(children = grid::gList(background, text))
}

GeomHeader <- ggproto("GeomTable", Geom,
                     required_aes = c("column_header", "text"),
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

