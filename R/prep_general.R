#' Prepare eatRep output for further data wrangling.
#'
#' @inheritParams prep_trend
#' @param data_clean Input data.frame, that has already been cleaned with [clean_data()].
#' @param states Bundesländer.
#' @param sub_groups grouping_var sub_groups
#'
#' @return List of data frames with different filters.
#' @export
#'
#' @examples # tbd
prep_general <- function(data_clean, sig_niveau, states, sub_groups) {
  filtered_list <- list()

  # Prepare point estimates -------------------------------------------------
  if (any(is.na(data_clean$comparison))) {
    point_long <- prep_long(dat[is.na(dat$comparison), ],
                          include_pattern = "^est|^p$|^p_|^se|^es",
                          remove_pattern = "trend",
                          suffix = "_point"
    )
    filtered_list[["point_data"]] <- point_long[, !(colnames(point_long) %in% c("compare_1", "compare_2")), ]
  } else {
    filtered_list["point_data"] <- list(NULL)
  }

  # Prepare trend comparison data ------------------------------------------------------
  years_colnames <- unique(unlist(regmatches(colnames(data_clean), gregexpr("[[:digit:]]+", colnames(data_clean)))))
  remove_columns <- unique(as.vector(
    sapply(c("es_", "est_", "se_", "p_"), function(val) {
      sapply(years_colnames, function(year) {
        grep(
          paste0("^", val, year, "$"),
          colnames(data_clean),
          value = TRUE
        )
      },
      USE.NAMES = FALSE
      )
    },
    USE.NAMES = FALSE
    )
  ))

  data_trend_raw <- data_clean[!is.na(data_clean$comparison) & data_clean$comparison == "crossDiff", ]

  if (nrow(data_trend_raw) != 0) {
    filtered_list[["trend_data"]] <- prep_long(data_trend_raw,
      include_pattern = "est_trend|p_trend|se_trend|es_trend",
      remove_pattern = paste0(paste0("^", remove_columns, "$"), collapse = "|")
    )
    filtered_list[["trend_data"]] <- split_years(filtered_list[["trend_data"]])
  } else {
    filtered_list["trend_data"] <- list(NULL)
  }



  # Prepare trend_point data ------------------------------------------------------
  data_trend_point <- data_clean[is.na(data_clean$comparison), ]
  if (nrow(data_trend_point) != 0) {
    filtered_list[["trend_no_comp_data"]] <- prep_long(data_trend_point,
      include_pattern = "est_trend|p_trend|se_trend|es_trend",
      remove_pattern = paste0(paste0("^", remove_columns, "$"), collapse = "|")
    )
    filtered_list[["trend_no_comp_data"]] <- split_years(filtered_list[["trend_no_comp_data"]])
  } else {
    filtered_list["trend_no_comp_data"] <- list(NULL)
  }

  # Prepare WholeGroup ------------------------------------------------------
  data_wholeGroup <- data_clean[data_clean$group_var == "wholeGroup", ]


  if (nrow(data_wholeGroup) != 0) {
    filtered_list[["wholeGroup_point"]] <- prep_long(data_wholeGroup,
      include_pattern = c("est_|^p_|se_|es_"),
      remove_pattern = "trend",
      suffix = "_point"
    )



    filtered_list[["wholeGroup_trend"]] <- prep_long(
      data = data_wholeGroup,
      include_pattern = c("est_trend|p_trend|se_trend|es_trend"),
      remove_pattern = paste0(paste0("^", remove_columns, "$"), collapse = "|")
    )
    filtered_list[["wholeGroup_trend"]] <- split_years(filtered_list[["wholeGroup_trend"]])
  }


  filtered_list <- add_sig_col(filtered_list, sig_niveau = sig_niveau)

  return(filtered_list)
}


# Utils -------------------------------------------------------------------

add_sig_col <- function(filtered_list, sig_niveau) {
  lapply(filtered_list, function(x) {
    p_col <- grep("^p_", colnames(x), value = TRUE)
    sig_col <- gsub("p_", "sig_", p_col)
    x[, sig_col] <- calc_sig(x[, p_col], sig_niveau)
    return(x)
  })
}
