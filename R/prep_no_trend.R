#' Prepare data for plotting a barplot.
#'
#' @inheritParams prep_trend
#' @param columns Character string containing the column in `data` that contains the column names for the table_plot.
#'
#' @return Returns a data.frame which can be used as input for plot_bar.
#' @export
#'
#' @examples # tbd
prep_no_trend <- function(dat,
                          columns,
                          competence,
                          grouping_var = "",
                          group_var = "group",
                          state_var = "TR_BUNDESLAND",
                          competence_var = "kb",
                          sig_niveau = 0.05){

  states <- unique(dat[ , state_var])[!is.na(unique(dat[ , state_var]))]
  if(grouping_var != ""){
    sub_groups <- unique(dat[, grouping_var][!is.na(dat[, grouping_var])])
  }else{
    sub_groups <- NULL
  }

  dat <- clean_data(dat,
                    states = states,
                    sub_groups = sub_groups,
                    competence = competence,
                    grouping_var = grouping_var,
                    group_var = group_var,
                    state_var = state_var,
                    competence_var = competence_var)

  dat$sig <- calc_sig(dat[, grep("^p_|^p$", colnames(dat))], sig_niveau = sig_niveau)
  dat <- calc_fill(dat, col_1 = "grouping_var", col_2 = "sig")

  dat_wholeGroup <- dat[dat$comparison == "crossDiff" & !is.na(dat$comparison), ]
  dat_point <- dat[is.na(dat$comparison), ]

  dat_no_trend <- merge(dat_wholeGroup[, colnames(dat_wholeGroup) != "comparison"],
                        dat_point[, colnames(dat_wholeGroup) != "comparison"],
                         by = c("group_var", "grouping_var", "state_var"),
                         suffixes = c("_wholeGroup", "_point"),
                         all = TRUE)


  # wholeGroup must be plotted as empty row - fill up NAs with 0-values (bit hacky)---------------------
  dat_bar <- dat_no_trend
  dat_bar[grepl("^wholeGroup$", dat_bar$group_var) & is.na(dat_bar$sig_wholeGroup), "sig_wholeGroup" ] <- FALSE
  dat_bar[grepl("^wholeGroup$", dat_bar$group_var) & is.na(dat_bar$fill_wholeGroup), "fill_wholeGroup" ] <- paste0(dat_bar[grepl("^wholeGroup$", dat_bar$group_var) &
                                                                                                                             is.na(dat_bar$fill_wholeGroup), "grouping_var" ],
                                                                                                                   "_",
                                                                                                                   dat_bar[grepl("^wholeGroup$", dat_bar$group_var) &
                                                                                                                             is.na(dat_bar$fill_wholeGroup), "sig_wholeGroup" ])
  dat_bar[grepl("^wholeGroup$", dat_bar$group_var) & is.na(dat_bar$est_wholeGroup), "est_wholeGroup" ] <- 0


  plot_dat <- list()
  plot_dat[["plot_bar"]] <- dat_bar


  # Prepare tableplot -------------------------------------------------------
  plot_dat[["plot_table"]] <- dat_no_trend

  return(plot_dat)
}


## Helpers

calc_fill <- function(dat, col_1, col_2) {
  dat$fill <- ifelse(is.na(dat[, col_1]) | is.na(dat[, col_2]), NA,  paste0(dat[, col_1], "_", dat[, col_2]))
  return(dat)
}
