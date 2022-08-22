plot_points <- function(my_data){
  ggplot2::ggplot(data = my_data, aes(x = year, y = est, colour = adjust, group = adjust)) + #, shape = sig
    #ggbrace::geom_brace(aes(c(2,3), y = c(410, 425)), inherit.data = F, rotate = 180) +
    #ggbrace::geom_brace(aes(c(1,3), y = c(430, 445)), mid = 0.25, inherit.data = F, rotate = 180) +
    ggplot2::geom_point() +
    theme_line_iqb() +
    colour_iqb
}
