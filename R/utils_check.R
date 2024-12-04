## Check if columns are part of a data.frame using checkmate
check_columns <- function(dat, cols){
  checkmate::assert_data_frame(dat)
  checkmate::assert_set_equal(cols, colnames(dat))
}
