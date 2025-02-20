draw_header <- function(data, panel_scales, coord) {

  data_header <- data %>%
    select(-c(text, ymin, ymax, row)) %>%
    mutate(y = max(data$ymax) + 1) %>%
    mutate(group = as.numeric(factor(column_header))) %>%
    unique() %>%
    mutate(ymin = y - 1, ymax = y + 1)

  coords <- unique(coord$transform(data_header, panel_scales))

  coords <- coords %>%
    mutate(x = case_when(hjust == 0 ~ xmin,
                         hjust == 1 ~ xmax,
                         TRUE ~ (coords$xmin + coords$xmax) / 2
    ))

  text <- gridtext::richtext_grob(
    text = coords$column_header,
    x = coords$x,
    y = max(coords$y),
    hjust = coords$hjust
  )

  background <- grid::rectGrob(
    x = (coords$xmin + coords$xmax) / 2,
    y = (coords$ymin + coords$ymax) / 2,
    width = coords$xmax - coords$xmin,
    height = coords$ymax - coords$ymin,
    default.units = "native",
    gp = grid::gpar(fill = unique(data$fill),  # Use the fill parameter
                    col = coords$colour)
  )

  grid::gTree(children = grid::gList(background, text))
}

## Modify GeomHeader to Accept Fill Parameter
GeomHeader <- ggproto("GeomHeader", Geom,
                      required_aes = c("column_header", "text"),
                      default_aes = aes(fill = NA),  ## Set default fill
                      draw_panel = function(self, data, panel_scales, coord, fill = NA, colour = NA) {
                        data$fill <- fill  # Ensure `fill` is used
                        draw_header(data, panel_scales, coord)
                      }
)

## Modify geom_header to Pass Fill Properly
geom_header <- function(mapping = NULL, data = NULL, stat = "table",
                        position = "identity", na.rm = FALSE,
                        fill = NA, colour = NA,
                        show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomHeader, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill = fill, ...)  # Pass fill as parameter
  )
}
