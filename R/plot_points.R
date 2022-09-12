plot_points <- function(my_data, grouping_var){
  ggplot2::ggplot(data = my_data,
                  aes(x = year,
                      y = est,
                      colour = .data[[grouping_var]],
                      group = .data[[grouping_var]])
                  ) +
    ggplot2::geom_point() +
    geom_text(aes(label = round(est, 0)), nudge_y = c(15, -15), size = 3) +
    theme_line_iqb() +
    grouping_colours +
    scale_x_continuous(position = "top") # ins theme mit rein?
}

