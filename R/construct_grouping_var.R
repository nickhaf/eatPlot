#' Merge two grouping var columns into one.
#'
#' @param dat
#' @param grouping_vars
#'
#' @return
#' @export
#'
#' @examples
construct_grouping_var <- function(dat, grouping_vars, group_var = "group") {
  if (is.null(grouping_vars)) {
    message("Are you sure your data isn't grouped? If it is, but you didn't provide any grouping_vars, this might lead to duplicated rows in the prepared data.frames.")
  } else if (length(grouping_vars) == 1) {
    dat <- check_factor(dat, grouping_vars, "grouping_var")
    dat <- fill_column(dat, grouping_vars)
    dat <- rename_columns(dat, "grouping_vars", "grouping_var")
    return(dat)
  } else if (length(grouping_vars) > 2) {
    stop("You can only provide two grouping vars at maximum.")
  } else if (length(grouping_vars) == 2) {
    dat$grouping_var <- dat[, grouping_vars[1]]
    grep_groups <- paste0(unique(dat[, grouping_vars[1]]), collapse = "|")

    if (any(grepl(grep_groups, dat[, group_var]))) {
      for (group_1 in unique(dat[!is.na(dat[, grouping_vars[1]]), grouping_vars[1]])) {
        for (group_2 in unique(dat[!is.na(dat[, grouping_vars[2]]), grouping_vars[2]])) {


          dat[, group_var] <- ifelse(
            !is.na(dat[, grouping_vars[2]]) & dat[, grouping_vars[2]] == group_2,
              gsub(group_1, paste0(group_1, "-", group_2), dat[, group_var]),
              dat[, group_var]
              )

          dat[, "grouping_var"] <- ifelse(
            is.na(dat[, grouping_vars[1]]),
                   dat[, grouping_vars[2]],
                   ifelse(is.na(dat[, grouping_vars[2]]),
                          dat[, grouping_vars[1]],
                          ifelse(
                            dat[, grouping_vars[2]] == group_2,
              gsub(group_1, paste0(group_1, "-", group_2), dat[, "grouping_var"]),
            dat[, "grouping_var"]
          )
            )
          )
        }
      }

      message("Your provdided two grouping_vars. The merged grouping_var will be sorted alphabetically, which might result in an unwanted factor order. If you want another factor order, please use construct_grouping_var() to construct your grouping_var yourself.")
      dat$grouping_var <- as.factor(dat$grouping_var)

      grouping_var <- "grouping_var"
      dat <- fill_column(dat, grouping_var)

      return(dat)
    }else {
      stop("Your first grouping_var has to be found in the group_var-column of your data.")
    }
  }
}
