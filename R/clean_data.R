#' Fill in and rename necessary columns
#'
#' `clean_data()` performs some simple data wrangling to prepare the input `data.frame` for the more plot specific data transformations.
#'
#' The following operations are performed on `dat`:
#' * For consistency, points in column names are subbed with underscores.
#' * Only rows containing `mean` in `dat$parameter` are returend.
#' * The group `wholeGroup` currently contains `NAs` in the columns `dat$TR_BUNDESLAND` and `dat$grouping_var`. These are subbed with `wholeGroup` or `noGroup` respectively.
#' * The column `dat$TR_BUNDESLAND` is filled up by extracting the first state found in `dat$groups` for the respective row.
#' @inheritParams prep_trend
#' @param states Character vector of the different groups that should be plotted seperatly. Normally these should be
#' @param sub_groups grouping_var groups
#'
#' @return `clean_data()` returns a `data.frame` with renamend columns and filled in `NAs`.
#' @export
#'
#' @examples # tbd
clean_data <- function(dat, grouping_var, states, sub_groups, competence) {
  if (grouping_var == "") {
    dat$grouping_var <- rep(NA, nrow(dat))
  } else {
    rename_column(data = data, old = grouping_var, new = "grouping_var")
  }

  dat[is.na(dat$TR_BUNDESLAND), "TR_BUNDESLAND"] <- write_group(dat[is.na(dat$TR_BUNDESLAND), "group"], groups = BLs)
  dat[is.na(dat$TR_BUNDESLAND) , "TR_BUNDESLAND"] <- "wholeGroup" #& dat$group == "wholeGroup"
  dat[is.na(dat$grouping_var), "grouping_var"] <- write_group(dat[is.na(dat$grouping_var), "group"], groups = sub_groups)
  dat[is.na(dat$grouping_var), "grouping_var"] <- "noGroup"

  colnames(dat) <- gsub("\\.", "_", colnames(dat))
  colnames(dat) <- gsub("sig_", "p_", colnames(dat))
  dat <- dat[dat$kb == competence & dat$parameter == "mean", ]
  dat <- dat[, !colnames(dat) %in% c("modus", "depVar", "modus", "parameter", "kb")]


  return(dat)
}

## Aims of this function:
# Do global data cleaning, like removing unnecessary rows or columns, rename variables so they can be found be the other functions, and fill up NAs in some columns by values derived from the "group-column".

