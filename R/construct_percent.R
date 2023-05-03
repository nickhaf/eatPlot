#' Multiply Values in Columns by 100 to build percentages.
#'
#' @param dat Input data.frame.
#' @param columns Character vector with the column names of the columns you want to multiply by 100.
#'
#' @return The input data.frame with the new columns multiplied by 100. New columns have the suffix "_percent" in their name.
#' @export
#'
#' @examples
#' df <- data.frame(col_1 = c(1, 2), col_2 = c(3, 4), col_3 = c("a", "b"))
#' construct_percent(df, columns = c("col_1", "col_2"))
construct_percent <- function(df, columns){
  for(i in columns){
    if(is.numeric(df[, i])){
    df[, paste0(i, "_percent")] <- df[,i] * 100
    }
  }
  return(df)
}
