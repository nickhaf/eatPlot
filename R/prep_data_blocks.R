#' Prepare distinct building blocks, from which the data.frames for the plot-functions can be build by combining them.
#'
#' @inheritParams prep_plot
#' @param data_clean Input data.frame, that has already been cleaned with [clean_data()].
#' @param states Character vector of the different states (Bundesländer) found in the data.
#' @param sub_groups Character vector of the different groups found in `grouping_var`.
#' @param merging_columns Columns that won't get a suffix.
#'
#' @return `prep_data_blocks()` returns a list containing five data.frames which can be used as the building blocks for more specific data.frames needed for the `plot()` functions. These data.frames contain distinct information, and can be combined according to the requirements of the respective plots. The returned list includes the data.frames:
#' * `noTrend_noComp` contains point estimates for every years.
#' * `Trend_Comp` contains all trend variables performing some kind of comparison, e.g., state vs. germany.
#' * `trend_no_comp_data` contains the trend estimates without comparisons.
#' * `noTrend_noComp_wholeGroup` contains the point estimates of the wholeGroup.
#' * `Trend_noComp_wholeGroup` contains the trend estimates for the wholeGroup.
#' @export
#'
#' @examples # tbd
prep_data_blocks <- function(data_clean, sig_niveau, states, sub_groups, merging_columns) {

  filtered_list <- list()
cols_remove <- c("group_var", "comparison")
merging_columns <- c(merging_columns, cols_remove)

  # Prepare point estimates, which don't include any trend estimates -------------------------------------------------
  if (any(is.na(data_clean$comparison))) {
    point_long_no_comp <- prep_point_long(dat = data_clean[is.na(data_clean$comparison), ])
    filtered_list[["noTrend_noComp"]] <- point_long_no_comp[, !(colnames(point_long_no_comp) %in% c("compare_1", "compare_2")), ]
    filtered_list[["noTrend_noComp"]] <- add_suffix(filtered_list[["noTrend_noComp"]],
                                                    merging_columns = merging_columns,
                                                    suffix = "_noComp")
    } else {
    filtered_list["noTrend_noComp"] <- list(data.frame())
  }



  if (any(!is.na(data_clean$comparison))) {
    filtered_list[["noTrend_Comp"]] <- prep_point_long(dat = data_clean[!is.na(data_clean$comparison), ])

    filtered_list[["noTrend_Comp"]] <- add_suffix(filtered_list[["noTrend_Comp"]],
                                                    merging_columns = merging_columns,
                                                    suffix = "_Comp")

    } else {
    filtered_list["noTrend_Comp"] <- list(data.frame())
  }

  # Prepare trend comparison data ------------------------------------------------------

if(any(grepl("trend", colnames(data_clean))) == TRUE){
  ## Data.frame containing all rows which make some kind of comparison, e.g. state vs. germany.
  years_colnames <- extract_numbers(colnames(data_clean))
  exclude_cols <- get_year_cols(vec = colnames(data_clean), years_colnames)

  data_trend_comp <- data_clean[!is.na(data_clean$comparison), ]
  filtered_list <- prep_trend_long(data_trend_comp,
                                   filtered_list,
                                   "Trend_Comp",
                                   remove_cols = exclude_cols)
  filtered_list[["Trend_Comp"]] <- add_suffix(filtered_list[["Trend_Comp"]],
                                                  merging_columns = merging_columns,
                                                  suffix = "_Trend_Comp")

  # Prepare trend_point data ------------------------------------------------------
  ## Data.frame containing all trend rows which do not make a trend comparison
  data_trend_no_comp <- data_clean[is.na(data_clean$comparison), ]
  data_trend_no_comp <- remove_columns(data_trend_no_comp, c("compare_1", "compare_2"))
  filtered_list <- prep_trend_long(data_trend_no_comp,
                                   filtered_list,
                                   "Trend_noComp",
                                   remove_cols = exclude_cols)

  filtered_list[["Trend_noComp"]] <- add_suffix(filtered_list[["Trend_noComp"]],
                                              merging_columns = merging_columns,
                                              suffix = "_Trend_noComp")

} else{
  filtered_list[["Trend_Comp"]] <- data.frame()
  filtered_list[["Trend_noComp"]] <- data.frame()
}
  # Prepare CrossDiffWholeGroup ------------------------------------------------------
  ## Might be necessary to deal with the wholeGroup a bit differently, so it is include in two extra data frames
  data_wholeGroup <- data_clean[data_clean$group_var == "wholeGroup", ]
  data_wholeGroup <- remove_columns(data_wholeGroup, c("compare_1", "compare_2"))

  if (nrow(data_wholeGroup) != 0) {
    filtered_list[["noTrend_noComp_wholeGroup"]] <- prep_long(data_wholeGroup,
      include_pattern = c("est_|^p_|se_|es_"),
      remove_pattern = "trend"#,
     # suffix = "_noTrend"
    )
    filtered_list[["noTrend_noComp_wholeGroup"]] <- add_suffix(filtered_list[["noTrend_noComp_wholeGroup"]],
                                                merging_columns = merging_columns,
                                                suffix = "_noTrend_noComp_wholeGroup")

  }else{
    filtered_list[["noTrend_noComp_wholeGroup"]] <- data.frame()
  }

  if(any(grepl("trend", colnames(data_clean))) == TRUE){
  filtered_list <- prep_trend_long(dat = data_wholeGroup,
                                   filtered_list,
                                   "Trend_noComp_wholeGroup",
                                   remove_cols = exclude_cols)
  filtered_list[["Trend_noComp_wholeGroup"]] <- add_suffix(filtered_list[["Trend_noComp_wholeGroup"]],
                                                             merging_columns = merging_columns,
                                                             suffix = "_Trend_noComp_wholeGroup")
  }else{
    filtered_list[["Trend_noComp_wholeGroup"]] <- data.frame()
  }

# Add significances -------------------------------------------------------
  filtered_list <- add_sig_col(filtered_list, sig_niveau = sig_niveau)

  filtered_list <- lapply(filtered_list, remove_columns, cols_remove)

  return(filtered_list)
}


# Utils -------------------------------------------------------------------

## Add a column with significance values = TRUE/FALSE, depending on a p-value
add_sig_col <- function(filtered_list, sig_niveau) {
  lapply(filtered_list, function(x) {
    p_col <- grep("^p_", colnames(x), value = TRUE)
    sig_col <- gsub("p_", "sig_", p_col)
    x[, sig_col] <- calc_sig(x[, p_col], sig_niveau)
    return(x)
  })
}

## Extract Numbers from a string vector
extract_numbers <- function(vec) {
  unique(unlist(regmatches(vec, gregexpr("[[:digit:]]+", vec))))
}

# Remove the point-estimates for the years. These are found in columns which end with a year.
get_year_cols <- function(vec, years) {
  unique(unlist(
    sapply(c("es_", "est_", "se_", "p_"), function(val) {
      sapply(years, function(year) {
        grep(
          paste0("^", val, year, "$"),
          vec,
          value = TRUE
        )
      },
      USE.NAMES = FALSE
      )
    },
    USE.NAMES = FALSE
    )
  ))
}

# Wrapper for preparing the trend data
prep_trend_long <- function(dat, filtered_list, dat_name, remove_cols) {
  if (nrow(dat) != 0) {
    dat <- prep_long(dat,
      include_pattern = "est_trend|p_trend|se_trend|es_trend",
      remove_pattern = paste0(paste0("^", remove_cols, "$"), collapse = "|")
    )
    dat <- rename_columns(dat, old_names = "year", new_names = "years_Trend")
    filtered_list[[dat_name]] <- split_years(dat, year_col = "years_Trend")

  } else {
    filtered_list[dat_name] <- list(data.frame())
  }
  return(filtered_list)
}


prep_point_long <- function(dat)
  point_long <- prep_long(dat,
                          include_pattern = "^est|^p$|^p_|^se|^es",
                          remove_pattern = "trend",
                          suffix = "_noTrend"
  )

## Rename directly here, all columns that aren't used for merging get the according suffix
add_suffix <- function(dat, merging_columns, suffix ){
  colnames(dat)[!colnames(dat) %in% merging_columns] <- paste0(colnames(dat)[!colnames(dat) %in% merging_columns], suffix)
  colnames(dat) <- gsub("_trend_Trend", "_Trend", colnames(dat))
  return(dat)
}
