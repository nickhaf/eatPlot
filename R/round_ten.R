#' Get min and max of a vector and round them to 10.
#' @description Currently only works, if the plot center is zero. Min and Max values of the input variable are calculated. The higher of these two values is put out as negative and positive value.
#' @param x Numeric vector.
#'
#' @return
#' @export
#'
#' @examples
#' round_ten(11)
#' round_ten(-19)
round_ten <- function(x, accuracy = 10){

  min_x <- min(x)
  max_x <- max(x)

borders <- vapply(c(min_x, max_x), function(y){
  if(y < 0){
    plyr::round_any(y, accuracy = accuracy, floor)
  }else{
    plyr::round_any(y, accuracy = accuracy, ceiling)
    }
    },
  FUN.VALUE = numeric(1))

max_border <- max(abs(borders[1]), abs(borders[2]))

return(c(-1 * max_border, max_border))
}
