#' Calculate the significance descriptions for the 'Mindeststandards'
#'
#' @param dat_bar Data prepared by `prep_lineplot()`.
#' @param est_column Column that contains the estimates.
#' @param sig_column Column that contains the significances.
#'
#' @return Dataframe with an added column with a name that ends with `_directional_sig`.
#' @export
#'
#' @examples # tbd
construct_directional_sig <- function(dat_bar, est_column, sig_column) {
  dat_bar[, paste0(sig_column, "_directional_sig")] <- ifelse(dat_bar[, sig_column] == "TRUE" & dat_bar[, est_column] < 0,
    "below",
    ifelse(dat_bar[, sig_column] == "TRUE" & dat_bar[, est_column] > 0,
      "above",
      "FALSE"
    )
  )
  return(dat_bar)
}
