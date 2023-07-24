#' Fill in and rename necessary columns
#'
#' `clean_data()` performs some simple data wrangling to prepare the input `data.frame` for the more plot specific data transformations.
#'
#' The following operations are performed on `dat`:
#' * For consistency, points in column names are subbed with underscores.
#' * Only rows containing `mean` in `dat$parameter` are returend.
#' * The group `wholeGroup` currently contains `NAs` in the columns `dat[ , state_var]` and `dat$grouping_var`. These are subbed with `wholeGroup` or `noGroup` respectively.
#' * The column `dat[ , state_var]` is filled up by extracting the first state found in `dat$groups` for the respective row.
#' @inheritParams prep_plot
#' @param all_states Character vector of the different groups that should be plotted seperatly. Normally these should be the states (Bundesländer).
#' @param sub_groups Character vector of the different sub_groups.
#'
#' @return `clean_data()` returns a `data.frame` with renamend columns and filled in `NAs`.
#' @export
#'
#' @examples # tbd
clean_data <- function(dat,
                       all_states,
                       states = NULL,
                       sub_groups = NULL,
                       competence = NULL,
                       parameter = "mean") {
  sapply(c("parameter", "competence_var", "state_var", "grouping_var", "group_var"), check_column, dat = dat)

  # Fill up NAs
  # dat$state_var[is.na(dat$state_var)] <- "wholeGroup"
  if (!is.null(all_states)) {
    dat <- fill_up_na(dat,
      info_to = "state_var",
      filling_groups = all_states
    )
  }


  # Select relevant rows
  dat <- dat[dat$parameter == parameter, ]
  if (!is.null(competence)) {
    dat <- dat[dat$competence_var %in% competence, ]
  }

  dat <- dat[, !colnames(dat) %in% c("modus", "parameter")]

  dat[is.na(dat$state_var) & (
    grepl("^wholeGroup", dat$group_var) |
      grepl(
        pattern = paste0(
          "^",
          sub_groups,
          "$",
          collapse = "|"
        ),
        dat$group_var
      ) |
      grepl(paste0("^", sub_groups, "\\.vs\\.", collapse = "|"), dat$group_var)
  ), "state_var"] <- "wholeGroup"

  if (!is.null(states)) {
    dat <- dat[dat$state_var %in% states, ]
  }

  dat$state_var <- gsub("ue", "\u00fc", dat$state_var)

  # dat <- dat[, grep("depVar|est_|p_|se_|competence_var|grouping_var|state_var|group_var|comparison", colnames(dat))]

  return(dat)
}

# Utils
fill_up_na <- function(dat, info_from = "group_var", info_to, filling_groups) {
  if (is.factor(dat[, info_to])) {
    new_levels <- filling_groups[!(filling_groups %in% levels(dat[, info_to]))]
    levels(dat[, info_to]) <- c(levels(dat[, info_to]), new_levels)
  }
  dat[is.na(dat[, info_to]), info_to] <- write_group(dat[is.na(dat[, info_to]), info_from], groups = filling_groups)

  return(dat)
}
