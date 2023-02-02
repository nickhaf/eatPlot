#' Prepare eatRep output for further data wrangling.
#'
#' @param data eatRep output data.frame.
#' @param grouping_var Variable with subgroups.
#' @param competence Competence variable.
#'
#' @return List of data frames with different filters.
#' @export
#'
#' @examples # tbd
prep_general <- function(data, grouping_var = "", competence = "") {
  colnames(data)[colnames(data) == grouping_var] <- "grouping_var"
  colnames(data) <- gsub("\\.", "_", colnames(data))
  colnames(data) <- gsub("sig_", "p_", colnames(data))
  data <- data[data$kb == competence & data$parameter == "mean", ]
  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]
  groups <- unique(data$grouping_var[!is.na(data$grouping_var)])



  ## filter rows, so different types of data.frames can be build:
  filtered_list <- list()



  # Prepare point estimates -------------------------------------------------
  filtered_list[["point_data"]] <- prep_long(data[is.na(data$comparison), ],
    include_pattern = "est_|p_|se_|es_",
    remove_pattern = "trend",
    suffix = "point"
  )
  filtered_list[["point_data"]][is.na(filtered_list[["point_data"]]$TR_BUNDESLAND) & (get_wholeGroup(filtered_list[["point_data"]]$group) | filtered_list[["point_data"]]$group %in% groups), "TR_BUNDESLAND"] <- "wholeGroup"


  # Prepare trend data ------------------------------------------------------
  years_colnames <- unique(unlist(regmatches(colnames(data), gregexpr("[[:digit:]]+", colnames(data)))))
  remove_columns <- unique(as.vector(
    sapply(c("es_", "est_", "se_", "p_"), function(val) {
      sapply(years_colnames, function(year) {
        grep(
          paste0("^", val, year, "$"),
          colnames(data),
          value = TRUE
        )
      },
      USE.NAMES = FALSE
      )
    },
    USE.NAMES = FALSE
    )
  ))


  filtered_list[["trend_data"]] <- prep_long(data[!is.na(data$comparison) & data$comparison == "crossDiff", ],
    include_pattern = "est_trend|p_trend|se_trend|es_trend",
    remove_pattern = paste0(paste0("^", remove_columns, "$"), collapse = "|")
  )
  filtered_list[["trend_data"]] <- split_years(filtered_list[["trend_data"]])
  colnames(filtered_list[["trend_data"]]) <- gsub("trend", "_trend", colnames(filtered_list[["trend_data"]]))
  filtered_list[["trend_data"]]$grouping_var <- write_group(filtered_list[["trend_data"]]$group, groups = groups)
  filtered_list[["trend_data"]]$TR_BUNDESLAND <- write_group(filtered_list[["trend_data"]]$group, groups = BLs)
  filtered_list[["trend_data"]][is.na(filtered_list[["trend_data"]]$TR_BUNDESLAND) & get_wholeGroup(filtered_list[["trend_data"]]$group), "TR_BUNDESLAND"] <- "wholeGroup"

  filtered_list <- add_sig_col(filtered_list, sig_niveau = sig_niveau)

  return(filtered_list)
}



# Utils -------------------------------------------------------------------

add_sig_col <- function(filtered_list, sig_niveau = sig_niveau) {
  lapply(filtered_list, function(x) {
    p_col <- grep("p_", colnames(x), value = TRUE)
    sig_col <- gsub("p_", "sig_", p_col)
    x[, sig_col] <- calc_sig(x[, p_col], sig_niveau = sig_niveau)
    return(x)
  })
}
