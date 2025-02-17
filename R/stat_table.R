## Takes some data with values (this are the values in the table) and some Variable defining the columns
## Needs to take some values defining the column widths.
## Returns a new column with built x and y axes. Best with the width of the x and y axis already defined.

## What about ID cols?


# Create an example data frame
data <- data.frame(
  group = c("A", "A", "B", "B", "B", "C", "C", "C", "C"),
  text = c(5, 3, 2, 4, 1, 7, 6, 3, 2)
)

# View the data
data

calc_table_coords <- function(data, scales) {
 data_y <- data %>%
    mutate(y = as.integer(factor(group)) - 1)

 return(data_y)
#  mutate(x_axis = as.numeric(as.factor(id_col)))
}

StatTable <- ggproto("StatTable", Stat,
                         compute_panel = calc_table_coords,
                         required_aes = c("text", "group") ## x and y are optional, if provided the respective values are not computed
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
