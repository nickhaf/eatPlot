plot_settings <- function(my_data){
  list(
    theme_line_iqb(),
    grouping_colours,
    scale_x_continuous(position = "top",
                       breaks = unique(my_data$year)
                       ),
    linetype_iqb,
    labs(title = i)
  )
}
