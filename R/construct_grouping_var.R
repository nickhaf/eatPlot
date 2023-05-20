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
  ### Hier weiter:
  # Erst NAs auffüllen: grouping_var konstruieren und NAs auffüllen, aber welche Reihenfolge? Erst NAs auffüllen, weil sonst einige Felder nicht mehr NAs sind wenn das constructed wird. Aufpassen beim Auffüllen: Gruppe muss vor dem .vs. stehen!

  ## 1) Grouping_var constructen
  ## 2) NAs aus group - Spalte auffüllen
  ## 3) noch verbleibende NAs aus den jeweiligen grouping_var - Spalten auffüllen.


  ## Construct grouping_var:
  # - some checks.
  # - Wenn 1 grouping_var: auffüllen und zurückgeben
  # - Wenn 2: pasten, auffüllen, verbleibende NAs aus grouping_vars auffüllen:
  # - Immer nur die erste grouping_var nehmen bei den Vergleichen

  if (is.null(grouping_vars)) {
    message("Are you sure your data isn't grouped? If it is, but you didn't provide any grouping_vars, this might lead to duplicated rows in the prepared data.frames.")
    dat$grouping_var <- factor(rep("noGroup", nrow(dat)))
    return(dat)
  }

  dat$group_var <- gsub("all.group=1____", "", dat$group_var)

  dat <- paste_grouping_vars(dat, grouping_vars, group_var)

  dat <- fill_up_na(dat,
    info_from = "group_var",
    info_to = "grouping_var",
    filling_groups = unique(dat[!is.na(dat$grouping_var), "grouping_var"])
  )

  if (length(grouping_vars) == 2) {
  dat <- fill_grouping_na(dat, grouping_vars)
  }

  levels(dat$grouping_var) <- gsub("\\.vs\\..*", "", levels(dat$grouping_var))

  return(dat)

  #stop("Your first grouping_var has to be found in the group_var-column of your data.")
}




paste_grouping_vars <- function(dat, grouping_vars, group_var) {
if (length(grouping_vars) == 1) {
    dat <- check_factor(dat, grouping_vars, "grouping_var")
    dat <- fill_column(dat, grouping_vars)
    dat <- rename_columns(dat, "grouping_vars", "grouping_var")
    return(dat)
  } else if (length(grouping_vars) > 2) {
    stop("You can only provide two grouping vars at maximum.")
  } else if (length(grouping_vars) == 2) {

    message("Your provdided two grouping_vars. The merged grouping_var will be sorted alphabetically, which might result in an unwanted factor order. If you want another factor order, please use construct_grouping_var() to construct your grouping_var yourself.")


    dat$grouping_var <- dat[, grouping_vars[1]]
    grep_groups <- paste0(unique(dat[!is.na(dat[, grouping_vars[1]]), grouping_vars[1]]), collapse = "|")

    if (any(grepl(grep_groups, dat[, group_var]))) {
      dat[, grouping_vars[1]] <- gsub("\\.vs\\..*", "", dat[, grouping_vars[1]])
      dat[, grouping_vars[2]] <- gsub("\\.vs\\..*", "", dat[, grouping_vars[2]])


      for (group_1 in unique(dat[!is.na(dat[, grouping_vars[1]]), grouping_vars[1]])) {
        for (group_2 in unique(dat[!is.na(dat[, grouping_vars[2]]), grouping_vars[2]])) {
          dat[, group_var] <- ifelse(
            !is.na(dat[, grouping_vars[2]]) & dat[, grouping_vars[2]] == group_2,
            gsub(group_1, paste0(group_1, "-", group_2), dat[, group_var]),
            dat[, group_var]
          )

          dat$grouping_var <- ifelse(
            dat[, grouping_vars[2]] == group_2 & !is.na(dat[, grouping_vars[2]]),
            yes = gsub(group_1, paste0(group_1, "-", group_2), dat$grouping_var),
            no = dat$grouping_var
          )
        }
      }
    }
  }
  return(dat)
}



## Fill up remaining NAs with content of columns that don't have NAs:
fill_grouping_na <- function(dat, grouping_vars) {
  dat$grouping_var <-
    ifelse(
      !is.na(dat$grouping_var),
      yes = dat$grouping_var,
      no = ifelse(
        !is.na(dat[, grouping_vars[1]]) & !is.na(dat[, grouping_vars[2]]),
        yes = dat$grouping_var,
        no = ifelse(
          is.na(dat[, grouping_vars[1]]),
          yes = dat[, grouping_vars[2]],
          no = ifelse(
            is.na(dat[, grouping_vars[2]]),
            yes = dat[, grouping_vars[1]],
            no = dat$grouping_var
          )
        )
      )
    )
  dat$grouping_var <- factor(dat$grouping_var)

  return(dat)
}
