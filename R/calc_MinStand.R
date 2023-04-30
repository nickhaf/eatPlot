calc_MinStand <- function(dat_bar, sig_column, est_column){

  dat_bar$sig_minstand <- ifelse(dat_bar[, sig_column] == "TRUE" & dat_bar[, est_column] < 0,
                                  "below",
                                  ifelse(dat_bar[, sig_column] == "TRUE" & dat_bar[, est_column] > 0,
                                         "above",
                                         "no_sig"
                                  )
  )
  return(dat_bar)
}
