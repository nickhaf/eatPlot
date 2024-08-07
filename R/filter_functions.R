
filter_years <- function(dat, l, b){
  if(is.null(l)){
    l <- consecutive_numbers(dat$year)
  }

  years_lines_vec <- paste_trend_years(l)
  years_braces_vec <- paste_trend_years(b)

  check_years(dat$trend)

  dat_final <- dat[dat$trend %in% years_lines_vec, ]
}
