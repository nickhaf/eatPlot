## Eventuell nur nötig wegen stats-Umformung!

draw_table <- function(data, panel_scales, coord) {
  ## Transform the data first
  coords <- coord$transform(data, panel_scales)

  gridtext::richtext_grob(
    text = coords$text,
    x = coords$x,
    y = coords$y)

  # background <- grid::rectGrob(
  #   xmin = coords$xmin,
  #   xmax = coords$xmax,
  #   ymin = coords$ymin,
  #   ymax = coords$ymax
  # )

}

GeomTable <- ggproto("GeomTable", Geom,
                      required_aes = c("x", "y", "text"),
                      default_aes = aes(xmin = -Inf, xmax = Inf),
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
