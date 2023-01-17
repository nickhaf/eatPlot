#' Compute if a p value is significant.
#'
#' @description Helper function.
#' @param data Input data.frame.
#' @param sig_niveau Significance niveau.
#' @param p_column Column in data which contains the p-values.
#'
#' @return The data.frame with a new column named 'sig'
#' @export
#'
#' @examples
calc_sig <- function(data, sig_niveau, p_column = "p"){
  if(any(is.na(data[ , p_column]))){
    stop("Your p-values should not contain any missings. Please check your input data.")
  }else{
    data$sig <- as.factor(ifelse(data[ , p_column] < sig_niveau & !is.na(data[ , p_column]), TRUE, FALSE))
    return(data)
  }
}

