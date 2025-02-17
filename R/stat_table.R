## Takes some data with values (this are the values in the table) and some Variable defining the columns
## Needs to take some values defining the column widths.
## Returns a new column with built x and y axes. Best with the width of the x and y axis already defined.

## What about ID cols?


# Create an example data frame
data <- data.frame(
  group = factor(c("A", "A", "B", "B", "B", "C", "C", "C", "C"), ordered = TRUE, levels = c("B", "A", "C")),
  text = c(5, 3, 2, 4, 1, 7, 6, 3, 2),
  col_width = 1/3
)

# View the data
data

calc_table_coords <- function(data, scales) { ## remove scales?

## Calc x axis
## Standardisieren von 0 bis 1? Oder nutzen der tatsächlichen Value range? Maybe it would be better to use an own coordinate system?
## If not specified correctly, do what? Warning and set to same width?

  position_width <- data %>%
    select(group, col_width) %>%
    unique() %>%
    arrange(as.numeric(group)) %>%
    mutate(xmin = lag(cumsum(col_width), default = 0),
           xmax = cumsum(col_width))

 data <- merge(data, position_width) %>%
   group_by(group) %>%
   mutate(ymin = row_number() - 1, ymax = row_number() ) %>%
   ungroup()

 return(data)
#  mutate(x_axis = as.numeric(as.factor(id_col)))
}

StatTable <- ggproto("StatTable", Stat,
                         compute_panel = calc_table_coords,
                         required_aes = c("text", "group", "col_width") ## x and y are optional, if provided the respective values are not computed
)

StatTableDebug <- ggdebug::create_stat_with_caching(
  StatTable
)

stat_table <- function(mapping = NULL, data = NULL, geom = "table",
                           position = "identity", show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTable,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}
