#' Plot an IQB barplot.
#'
#' @param prep_dat Data for the barplot. Has to be in the right format, ideally prepared with \link{prep_barplot}.
#'
#' @return ggplot object
#' @export
#'
#' @examples #tbd
#' @details text describing parameter inputs in more detail.
##' \itemize{
##'  \item{"prep_dat"}{Needs a specific data format.}
##' }
##'
plot_bar <- function(prep_dat){

  ggplot2::ggplot(data = prep_dat,
                  mapping = ggplot2::aes_string(
                    x = 'est', y = 'group',
                    fill = 'fill',
                    pattern = 'sig')) +

    ggforestplot::geom_stripes(odd = grDevices::rgb(219, 238, 244, maxColorValue = 255),
                               even = "#00000000") +

    ggplot2::geom_vline(xintercept = seq(-40, 40, by = 10),
                        linetype = "dashed", colour = "darkgrey") +

    ggplot2::geom_vline(xintercept = 0,
                        colour = "darkgrey") +

    ggpattern::geom_col_pattern(mapping = ggplot2::aes_string(
      x = 'est',
      y = 'group',
      pattern_fill = 'sub_groups'
    ),
    position = ggplot2::position_dodge(width = 0.8),
    color = "black",
    size = 0.6,
    pattern_colour = "white",
    pattern_angle = - 45,
    pattern_density = 0.4, #Streifenbreite
    pattern_spacing = 0.01, #Abstand
    pattern_key_scale_factor = 0.6,
    width = 0.4) +
    ggplot2::scale_x_continuous(breaks = seq(-40, 40, by = 10)) +
    ggpattern::scale_pattern_manual(values = c("TRUE" = "none",
                                               "FALSE" = "stripe")) +

    ggpattern::scale_pattern_fill_manual(values = c("ohneAdj" = grDevices::rgb(147, 205, 221, maxColorValue = 255),
                                                    "mitAdj" = grDevices::rgb(33, 89, 104, maxColorValue = 255))) +
    #fill_iqb_adj_sig +
    #theme_bar_iqb() +
  NULL
}
