#' Calculate the significance descriptions for the 'Mindeststandards'
#'
#' @param dat_bar Data prepared by `prep_plot()`.
#' @param sig_column Column that contains the significances.
#' @param est_column Column that contains the estimates.
#'
#' @return Dataframe with added `sig_minstand` column.
#' @export
#'
#' @examples #tbd
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
