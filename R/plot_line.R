plot_line <- function(my_data){
  ggplot2::ggplot(data = my_data,
                  aes(x = year,
                      y = est,
                      colour = adjust,
                      group = adjust)
                  ) +
  ggbrace::geom_brace() +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ TR_BUNDESLAND, ncol = 4) +
  theme_bar_iqb() +
  colour_iqb
}


plot_line2 <- function(my_data){
  ggplot2::ggplot(data = my_data, aes(x = year, y = est, colour = adjust, group = adjust)) +
  ggbrace::geom_brace(aes(c(2,3), y = c(410, 425)), inherit.data = F, rotate = 180) +
  ggbrace::geom_brace(aes(c(1,3), y = c(430, 445)), mid = 0.25, inherit.data = F, rotate = 180) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  theme_line_iqb() +
  ggplot2::facet_wrap(~ TR_BUNDESLAND, ncol = 4) +
  colour_iqb
}
