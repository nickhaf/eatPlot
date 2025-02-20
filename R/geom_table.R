draw_table <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales)
  coords <- coords %>%
    mutate(x = case_when(hjust == 0 ~ xmin_col,
                         hjust == 1 ~ xmax_col,
                         TRUE ~ (coords$xmin_col + coords$xmax_col)/2
    ))

  text <- gridtext::richtext_grob(
    text = coords$text,
    x = coords$x,
    y = coords$y,
    hjust = coords$hjust)

  background <- grid::rectGrob(
    x = (coords$xmin_col + coords$xmax_col)/2,
    y = (coords$ymin_row + coords$ymax_row)/2,
    width = coords$xmax_col - coords$xmin_col,
    height = coords$ymax_row - coords$ymin_row,
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
