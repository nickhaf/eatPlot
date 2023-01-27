#' Title
#'
#' @param data Trend data.
#' @param competence Competence area.
#' @param grouping_var Variable that groups the cases.
#' @param sig_niveau When do p-values become significant?
#'
#' @return Data frame in long format, which includes the relevant point estimates.
#' @export
#'
#' @examples # tbd
prep_points <- function(data, grouping_var, sig_niveau = 0.05){

  p_cols <- grep("p_", colnames(data), value = TRUE)
  est_cols <- grep("est", colnames(data), value = TRUE)
  est_cols <- grep("trend", est_cols, invert = TRUE, value = TRUE) # remove trend estimates
  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]

## Filter all rows with the pattern: "BL + _0|_1 + .vs.wholeGroup". This
## are the rows containing the p-Values for the point estimates of each
## Bundesland vs. the whole Group (Germany).
  p_rows <- c(filter_strings(identifier = BLs, paste_vec = "_0.vs.wholeGroup"), val_vec = data$group),
              filter_strings(identifier = BLs, paste_vec = "_1.vs.wholeGroup"), val_vec = data$group))
  data_p <- data[p_rows, c("TR_BUNDESLAND", "group", p_cols)]

  data_p$group <- gsub(".vs.wholeGroup", "", data_p$group)
  data_p$TR_BUNDESLAND <- data_p$group
  data_p$TR_BUNDESLAND <- gsub("_0", "", data_p$TR_BUNDESLAND)
  data_p$TR_BUNDESLAND <- gsub("_1", "", data_p$TR_BUNDESLAND)

## Filter all rows with the pattern: "BL + _0|_1 . This
## are the rows containing the point estimates of each
## Bundesland vs. the whole Group (Germany).
  est_rows <- c(## Germany:
              grep("^0$", data$group), grep("^1$", data$group),
              filter_strings(identifier = BLs, paste_vec = "_0$", val_vec = data$group),
              filter_strings(identifier = BLs, paste_vec = "_1$", val_vec = data$group))

  data_est <- data[est_rows, c("TR_BUNDESLAND", "group", grouping_var, est_cols)]

  ## Put Deutschland in the respective fields for the respective subgroups.
  data_est[is.na(data_est$TR_BUNDESLAND), "TR_BUNDESLAND"] <- rep("Deutschland", length(which(is.na(data_est$TR_BUNDESLAND))))


  data_wide <- merge(data_est, data_p, by = c("group", "TR_BUNDESLAND"), all.x = TRUE)

## Into Long Format

  colnames(data_wide) <- gsub("_", "\\.", colnames(data_wide))

  data_long <- stats::reshape(data_wide, direction = "long", varying = grep("est\\.|p\\.", colnames(data_wide), value = TRUE))
  data_long <- calc_sig(data_long, sig_niveau = sig_niveau)

  colnames(data_long) <- gsub("\\.", "_", colnames(data_long))
  data_long$id <- NULL

  return(data_long)

}

