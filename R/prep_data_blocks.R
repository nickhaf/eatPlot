#' Prepare distinct building blocks, from which the data.frames for the plot-functions can be build by combining them.
#'
#' @inheritParams prep_trend
#' @param data_clean Input data.frame, that has already been cleaned with [clean_data()].
#' @param states Bundesländer.
#' @param sub_groups grouping_var sub_groups
#'
#' @return `prep_data_blocks()` returns a list containing five data.frames which can be used as the building blocks for more specific data.frames needed for the `plot()` functions. These data.frames contain distinct information, and can be combined according to the requirements of the respective plots. The returned list includes the data.frames:
#' * `point_data` contains point estimates for every years.
#' * `trend_comp_data` contains all trend variables performing some kind of comparison, e.g., state vs. germany.
#' * `trend_no_comp_data` contains the trend estimates.
#' * `wholeGroup_point` contains the point estimates of the wholeGroup.
#' * `wholeGroup_trend` contains the trend estimates for the wholeGroup.
#' @export
#'
#' @examples # tbd
prep_data_blocks <- function(data_clean, sig_niveau, states, sub_groups) {
  filtered_list <- list()

  # Prepare point estimates -------------------------------------------------
  if (any(is.na(data_clean$comparison))) {
    point_long <- prep_long(data_clean[is.na(data_clean$comparison), ],
      include_pattern = "^est|^p$|^p_|^se|^es",
      remove_pattern = "trend",
      suffix = "_point"
    )
    filtered_list[["point_data"]] <- point_long[, !(colnames(point_long) %in% c("compare_1", "compare_2")), ]
  } else {
    filtered_list["point_data"] <- list(NULL)
  }

  # Prepare trend comparison data ------------------------------------------------------
  ## Data.frame containing all rows which make some kind of comparison, e.g. state vs. germany.
  years_colnames <- extract_numbers(colnames(data_clean))
  remove_columns <- get_year_cols(vec = colnames(data_clean), years_colnames)

  data_trend_comp <- data_clean[!is.na(data_clean$comparison) & data_clean$comparison == "crossDiff", ]
  filtered_list <- prep_trend_long(data_trend_comp, filtered_list, "trend_comp_data", remove_cols = remove_columns)


  # Prepare trend_point data ------------------------------------------------------
  data_trend_point <- data_clean[is.na(data_clean$comparison), ]
  filtered_list <- prep_trend_long(data_trend_point, filtered_list, "trend_no_comp_data", remove_cols = remove_columns)


  # Prepare WholeGroup ------------------------------------------------------
  data_wholeGroup <- data_clean[data_clean$group_var == "wholeGroup", ]

  if (nrow(data_wholeGroup) != 0) {
    filtered_list[["wholeGroup_point"]] <- prep_long(data_wholeGroup,
      include_pattern = c("est_|^p_|se_|es_"),
      remove_pattern = "trend",
      suffix = "_point"
    )
  }
  filtered_list <- prep_trend_long(data_wholeGroup, filtered_list, "wholeGroup_trend", remove_cols = remove_columns)

# Add significances -------------------------------------------------------
  filtered_list <- add_sig_col(filtered_list, sig_niveau = sig_niveau)

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
    filtered_list[[dat_name]] <- split_years(dat)
  } else {
    filtered_list[dat_name] <- list(NULL)
  }
  return(filtered_list)
}
