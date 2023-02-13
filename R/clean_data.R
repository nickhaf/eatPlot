#' Fill in and rename necessary columns
#'
#' `clean_data()` performs some simple data wrangling to prepare the input `data.frame` for the more plot specific data transformations.
#'
#' The following operations are performed on `dat`:
#' * For consistency, points in column names are subbed with underscores.
#' * Only rows containing `mean` in `dat$parameter` are returend.
#' * The group `wholeGroup` currently contains `NAs` in the columns `dat[ , state_var]` and `dat$grouping_var`. These are subbed with `wholeGroup` or `noGroup` respectively.
#' * The column `dat[ , state_var]` is filled up by extracting the first state found in `dat$groups` for the respective row.
#' @inheritParams prep_trend
#' @param states Character vector of the different groups that should be plotted seperatly. Normally these should be the states (Bundesländer).
#' @param sub_groups Character vector of the different sub_groups.
#'
#' @return `clean_data()` returns a `data.frame` with renamend columns and filled in `NAs`.
#' @export
#'
#' @examples # tbd
clean_data <- function(dat, states, sub_groups, competence, grouping_var = "", group_var = "group", state_var = "TR_BUNDESLAND", competence_var = "kb") {

  if(!(group_var %in% colnames(dat))){stop(paste0("group_var: '", group_var, "' not found in dat."))}
  if(!(state_var %in% colnames(dat))){stop(paste0("state_var: '", state_var, "' not found in dat."))}
  if(!(competence_var %in% colnames(dat))){stop(paste0("competence_var: '", competence_var, "' not found in dat."))}

  # Select relevant rows
  if ("parameter" %in% colnames(dat)) {
    dat <- dat[dat$parameter == "mean", ]
  }
  dat <- dat[dat[, competence_var] == competence, ]

  # Rename columns
  if (grouping_var == "") {
    dat$grouping_var <- rep(NA, nrow(dat))
  } else {
    dat <- rename_column(data = dat, old = grouping_var, new = "grouping_var")
  }
  dat <- rename_column(data = dat, old = state_var, new = "state_var")
  dat <- rename_column(data = dat, old = group_var, new = "group_var")
  colnames(dat) <- gsub("\\.", "_", colnames(dat))
  colnames(dat) <- gsub("sig_", "p_", colnames(dat))
  colnames(dat) <- gsub("^sig$", "p", colnames(dat))
  dat <- dat[, !colnames(dat) %in% c("modus", "depVar", "modus", "parameter", "kb")]

  # Fill up NAs
  dat <- fill_up_na(dat, info_to = "state_var", filling_groups = states)
  dat <- fill_up_na(dat, info_to = "grouping_var", filling_groups = sub_groups)
  dat[is.na(dat$grouping_var), "grouping_var"] <- "noGroup"
  dat[is.na(dat$state_var) & (
    grepl("wholeGroup", dat$group_var) |
      grepl(
        pattern = paste0(
          "^",
          paste0(sub_groups, collapse = "|"),
          "$"
        ),
        dat$group_var
      )
  ), "state_var"] <- "wholeGroup"

  return(dat)
}

# Utils
fill_up_na <- function(dat, info_from = "group_var", info_to, filling_groups) {
  dat[is.na(dat[, info_to]), info_to] <- write_group(dat[is.na(dat[, info_to]), info_from], groups = filling_groups)

  return(dat)
}