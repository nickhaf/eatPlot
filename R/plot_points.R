# plot_points <- function(my_data, grouping_var, ...){
#
#   list(
#     ggplot2::geom_point(data = my_data,
#                         aes(x = year,
#                             y = est,
#                             colour = .data[[grouping_var]],
#                             group = .data[[grouping_var]],
#                             shape = sig_vsGermany),
#                         size = 2.3),
#     ggplot2::geom_text(data = my_data,
#                        aes(x = year,
#                            y = est,
#                            color = .data[[grouping_var]],
#                            label = round(est, 0)),
#                        nudge_y = c(-10, 10), size = 3, ...),
#     pointshape_iqb
#   )
# }
#
# my_data <- prep_lineplot(trend_books, grouping_var = "KBuecher_imp3")
