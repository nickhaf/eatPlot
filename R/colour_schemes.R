
#' Wrapper around [ggplot2::scale_colour_manual()] with two predefined colours.
#'
#' @details Cases belonging to a group are plotted black, cases not belonging to this group are plotted blue.
#' @param ... Arguments for [ggplot2::scale_colour_manual()].
#' @return ggplot2
#' @export
#'
#' @examples #tbd
grouping_colours <- function(...){
  ggplot2::scale_colour_manual(values = c(
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
sig_linetypes <- function(){
  ggplot2::scale_linetype_manual(values = c(
  `TRUE` = "solid",
  `FALSE` = "dashed"
))
}
