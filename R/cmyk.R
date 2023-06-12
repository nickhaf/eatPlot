#' Set colours as cmyk values.
#'
#' @param C Numeric cyan value.
#' @param M Numeric magenta value.
#' @param Y Numeric yellow value.
#' @param K Numeric key (black) value.
#'
#' @return RGB colour value.
#' @export
#'
#' @examples
#' cmyk(0, 0, 0, 1)
cmyk <- function(C,M,Y,K) {

  C <- C / 100.0
  M <- M / 100.0
  Y <- Y / 100.0
  K <- K / 100.0

  n.c <- (C * (1-K) + K)
  n.m <- (M * (1-K) + K)
  n.y <- (Y * (1-K) + K)

  r.col <- ceiling(255 * (1-n.c))
  g.col <- ceiling(255 * (1-n.m))
  b.col <- ceiling(255 * (1-n.y))



  return(grDevices::rgb(red = r.col,
                        blue = b.col,
                        green = g.col,
                        maxColorValue = 255)
  )
}
