## Check if columns are part of a data.frame using checkmate
check_columns <- function(dat, cols) {
  checkmate::assert_data_frame(dat)
  checkmate::assert_subset(cols, colnames(dat))
}

check_no_columns <- function(dat, cols) {
  checkmate::assert_data_frame(dat)
  checkmate::assert_disjunct(cols, colnames(dat))
}

test_no_columns <- function(dat, cols) {
  checkmate::test_data_frame(dat)
  checkmate::test_disjunct(cols, colnames(dat))
}

test_no_columns <- function(dat, cols) {
  checkmate::test_data_frame(dat)
  checkmate::test_disjunct(cols, colnames(dat))
}
