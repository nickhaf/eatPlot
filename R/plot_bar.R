plot_bar <- function(my.dat){

  ggplot2::ggplot(data = my.dat,
         mapping = aes(
           x = est, y = group,
           fill = adj_sig,
           pattern = significant)) +

    ggforestplot::geom_stripes(odd = rgb(219, 238, 244, maxColorValue = 255),
                 even = "#00000000") +

    ggplot2::geom_vline(xintercept = seq(-40, 40, by = 10),
               linetype = "dashed", colour = "darkgrey") +

    ggplot2::geom_vline(xintercept = 0,
               colour = "darkgrey") +

    ggpattern::geom_col_pattern(mapping = aes(
      x = est,
      y = group,
      pattern_fill = adjust
    ),
    position = position_dodge(width = 0.8),
    color = "black",
    size = 0.6,
    pattern_colour = "white",
    pattern_angle = - 45,
    pattern_density = 0.4, #Streifenbreite
    pattern_spacing = 0.01, #Abstand
    pattern_key_scale_factor = 0.6,
    width = 0.4) +

    ggpattern::scale_pattern_manual(values = c(yes = "none",
                                               no = "stripe")) +

    ggpattern::scale_pattern_fill_manual(values = c("ohneAdj" = rgb(147, 205, 221, maxColorValue = 255),
                                                    "mitAdj" = rgb(33, 89, 104, maxColorValue = 255))) +

    ggplot2::scale_x_continuous(breaks = seq(-40, 40, by = 10)) +
    fill_iqb_adj_sig +
    theme_bar_iqb()
}
