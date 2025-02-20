draw_table <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)

  coords <- coords %>%
    mutate(x = case_when(hjust == 0 ~ xmin,
                         hjust == 1 ~ xmax,
                         TRUE ~ (coords$xmin + coords$xmax)/2
    ))

  text <- gridtext::richtext_grob(
    text = coords$text,
    x = coords$x,
    y = coords$y,
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

GeomTable <- ggproto("GeomTable", Geom,
                     required_aes = c("group", "text", "column", "row"),
                     draw_panel = draw_table)

geom_table <- function(mapping = NULL, data = NULL, stat = "table",
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTable, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
