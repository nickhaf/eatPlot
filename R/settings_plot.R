# settings_lineplot <- function(data_long){
#   list(
#     theme_line_iqb(),
#     grouping_colours(),
#     scale_x_continuous(position = "top",
#                        breaks = unique(.data_long$time)
#     ),
#     sig_linetypes(),
#     labs(title = paste0(i, "\n", " ")),
#     annotation_custom(
#       grob = grid::rectGrob(gp = grid::gpar(fill = rgb(128, 196, 214,
#                                                        maxColorValue = 255),
#                                             alpha = .4,
#                                             col = 0)),
#       xmin = -Inf, xmax = Inf, ymin = 580, ymax = 597
#     )
#
#   )
# }
