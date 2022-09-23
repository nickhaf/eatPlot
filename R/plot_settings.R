plot_settings <- function(my_data){
  list(
    theme_line_iqb(),
    grouping_colours,
    scale_x_continuous(position = "top",
                       breaks = unique(my_data$year)
                       ),
    linetype_iqb,
    labs(title = i),
    annotation_custom(
      grob = grid::rectGrob( gp = grid::gpar(fill = rgb(128, 196, 214,
                                                        maxColorValue = 255), alpha = .4, col = 0)), #, alpha = 165.7
      xmin = -Inf, xmax = Inf, ymin = 580, ymax = 600
    )

  )
}
