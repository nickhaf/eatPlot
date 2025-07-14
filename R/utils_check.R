## Check if columns are part of a data.frame using checkmate
check_columns <- function(dat, cols) {
  checkmate::assert_data_frame(dat)

  missing_cols <- setdiff(cols, colnames(dat))

  if (length(missing_cols) > 0) {
    stop(sprintf("The following columns are missing from the data frame: %s", paste(missing_cols, collapse = ", ")))
  }

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

check_colour_vector_length <- function(dat, colname, colours) {
  if (length(colours) < length(unique(dat[[colname]][!is.na(dat[[colname]])]))) {
    stop("You need to provide as many colours in `plotsettings$bar_fill_colour` or `plotsettings$bar_label_colour` as there are unique values in your `bar_fill` column.")
  }
}
