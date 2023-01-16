#' Round to 10
#'
#' @param x Numeric.
#'
#' @return Numeric.
#' @export
#'
#' @examples
#' round_ten(11)
#' round_ten(-19)
round_ten <- function(x){
  if(x < 0){
    plyr::round_any(x, 10, floor)
  }else{plyr::round_any(x, 10, ceiling)}
}

