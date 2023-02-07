#' Prepare eatRep output for further data wrangling.
#'
#' @inheritParams prep_trend
#' @param data_clean Input data.frame, that has already been cleaned with [clean_data()].
#'
#' @return List of data frames with different filters.
#' @export
#'
#' @examples # tbd
prep_general <- function(data_clean, sig_niveau, BLs, groups) {

  ## filter rows, so different types of data.frames can be build:
  filtered_list <- list()

  # Prepare point estimates -------------------------------------------------
  if (any(is.na(data_clean$comparison))) {
    filtered_list[["point_data"]] <- prep_long(data_clean[is.na(data_clean$comparison), ],
      include_pattern = "^est|^p$|^p_|^se|^es",
      remove_pattern = "trend",
      suffix = "_point"
    )
    filtered_list[["point_data"]][is.na(filtered_list[["point_data"]]$TR_BUNDESLAND) & (get_wholeGroup(filtered_list[["point_data"]]$group) | filtered_list[["point_data"]]$group %in% groups), "TR_BUNDESLAND"] <- "wholeGroup"
    filtered_list[["point_data"]][is.na(filtered_list[["point_data"]]$grouping_var), "grouping_var"] <- "noGroup"
    filtered_list[["point_data"]] <- filtered_list[["point_data"]][, !(colnames(filtered_list[["point_data"]]) %in% c("compare_1", "compare_2")), ]
  } else {
    filtered_list["point_data"] <- list(NULL)
  }

  # Prepare trend data ------------------------------------------------------
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

  # Prepare WholeGroup ------------------------------------------------------
  data_wholeGroup <- data_clean[data_clean$group == "wholeGroup", ]


  if (nrow(data_wholeGroup) != 0) {
    filtered_list[["wholeGroup_point"]] <- prep_long(data_wholeGroup,
      include_pattern = c("est_|p_|se_|es_"),
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
    p_col <- grep("p_", colnames(x), value = TRUE)
    sig_col <- gsub("p_", "sig_", p_col)
    x[, sig_col] <- calc_sig(x[, p_col], sig_niveau)
    return(x)
  })
}
