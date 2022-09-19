plot_points <- function(my_data, grouping_var, ...){

list(
  ggplot2::geom_point(data = my_data,
                      aes(x = year,
                          y = est,
                          colour = .data[[grouping_var]],
                          group = .data[[grouping_var]],
                          shape = sig),
                      size = 2.3),
    geom_text(aes(x = year,
                  y = est,
                  label = round(est, 0)),
              nudge_y = c(-15, 15), size = 3, ...),
    theme_line_iqb(),
    grouping_colours,
    scale_x_continuous(position = "top"),
  scale_shape_manual(values = c(16, 17, 16)
)
}

