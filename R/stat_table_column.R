# Define the custom ggplot2 stat
StatTableColumn <- ggproto("StatTableColumn", Stat,
                           compute_panel =  function(data, scales) {

                       data <- data %>%
                         ## Can I put this into the compute_group argument?
                         group_by(column) %>%
                         mutate(x = scales::rescale(x, to =  c(unique(xmin_col), unique(xmax_col)), from = c(unique(scale_min), unique(scale_max)))) %>%
                         mutate(xmin = scales::rescale(0, to = c(unique(xmin_col), unique(xmax_col)), from = c(unique(scale_min), unique(scale_max))),
                                xmax = x,
                                xintercept = xmin)
                       data <- calc_y_coords(data) %>%
                         mutate(ymin = ymin + 0.25, ymax = ymax - 0.25,
                                yend = max(ymax),
                                y = min(ymin)) %>%
                         ungroup()

                       return(data)
                     },
                     required_aes = c("x", "xmin_col", "xmax_col", "scale_min", "scale_max", "column")
)

# Custom function to use the StatTableColumn stat
stat_table_column <- function(mapping = NULL, data = NULL, geom = "bar",
                              position = "identity", show.legend = NA,
                              inherit.aes = TRUE, xmin_col = NA, xmax_col = NA, ...) {
  ggplot2::layer(
    stat = StatTableColumn,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(xmin_col = xmin_col, xmax_col = xmax_col, ...)
  )
}

StatTableColumnDebugg <- ggdebug::create_stat_with_caching(
  StatTableColumn
)
