
#' Wrapper around [ggplot2::scale_colour_manual()] with two predefined colours.
#'
#' @details Cases belonging to a group are plotted black, cases not belonging to this group are plotted blue.
#' @param ... Arguments for [ggplot2::scale_colour_manual()].
#' @return ggplot2
#' @export
#'
#' @examples #tbd
grouping_colours <- function(...){
  ggplot2::scale_colour_manual(
    values = c(
    "0" = grDevices::rgb(166, 166, 166, maxColorValue = 255),
    "1" = "black"
  ), ... )
}



#' Wrapper around [ggplot2::scale_colour_manual()] with two predefined shapes.
#'
#' @details Turns points int rectangles if the according p-value is significant.
#' @param ... Arguments for [ggplot2::scale_colour_manual()].
#'
#' @return ggplot2
#' @export
#'
#' @examples #tbd
sig_pointshapes <- function(...){
  ggplot2::scale_shape_manual(values = c(
    `TRUE` = 17,
    `FALSE` = 16
  ) , ...)
}


#' Wrapper around [ggplot2::scale_colour_manual()] with two predefined linetypes.
#'
#' @details Significant lines are solid, not-significant lines are dashed, according to the respective p-values.
#' @param ... Arguments for [ggplot2::scale_colour_manual()].
#'
#' @return ggplot2
#' @export
#'
#' @examples #tbd
sig_linetypes <- function(...){
  ggplot2::scale_linetype_manual(values = c(
    `TRUE` = "solid",
    `FALSE` = "dashed"
  ))
}

# Pattern settings --------------------------------------------------------
sig_pattern <- c(
  "TRUE" = "none",
  "FALSE" = "stripe"
)

adj_pattern_fill <- c(
  "ohneAdj" = grDevices::rgb(147, 205, 221, maxColorValue = 255),
  "mitAdj" = grDevices::rgb(33, 89, 104, maxColorValue = 255)
)

# Fill settings -----------------------------------------------------------
adj_fill <- c(
  "ohneAdj_TRUE" = grDevices::rgb(147, 205, 221, maxColorValue = 255),
  "mitAdj_TRUE" = grDevices::rgb(33, 89, 104, maxColorValue = 255),
  "ohneAdj_FALSE" = "white",
  "mitAdj_FALSE" = "white"
)

# Frame settings ----------------------------------------------------------
sig_frame <- c(
  "FALSE" = "dashed",
  "TRUE" = "solid"
)
# TODO: Original linetype (without manual setting) seems to be better but cannot be reproduced?
