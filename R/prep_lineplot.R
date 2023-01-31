#' Title
#'
#' @param data Trend data from eatRep.
#' @param grouping_var Grouping variable.
#' @param competence Competence area.
#' @param sig_niveau Significance niveau.
#'
#' @return List of data.frames needed for different plot-functions.
#' @export
#'
#' @examples # tbd
prep_lineplot <- function(data, grouping_var, competence, sig_niveau = 0.05) {

  colnames(data)[colnames(data) == grouping_var] <- "grouping_var"
  data <- data[data$kb == competence & data$parameter == "mean", ]
  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]
  groups <- unique(data$grouping_var[!is.na(data$grouping_var)])

  prep_list <- list()

  # whole group -------------------------------------------------------------
  whole_group_rows <- unlist(sapply(groups, function(x) {
    filter_strings(identifier = BLs, paste_vec = paste0("_", x, ".vs.wholeGroup"), val_vec = data$group)
  }, USE.NAMES = FALSE))

  trend_whole <- data[whole_group_rows, ]
  trend_whole$grouping_var <- get_group(trend_whole$group, groups = groups)
  trend_whole$TR_BUNDESLAND <- get_group(trend_whole$group, groups = BLs)
  trend_whole_long <- prep_trend(data = trend_whole, sig_niveau = sig_niveau)
  trend_whole_long <- rename_column(trend_whole_long, old = "esttrend", new = "est_trend_whole")
  trend_whole_long <- rename_column(trend_whole_long, old = "sig_trend", new = "sig_trend_whole")
  trend_whole_long <- rename_column(trend_whole_long, old = "setrend", new = "se_trend_whole")




  # within group ------------------------------------------------------------
  within_group_rows <- unlist(sapply(groups, function(group) {
    sapply(BLs, function(BL) {
      grep(paste0(BL, "_", group, ".vs.", BL), data$group)
    }, USE.NAMES = FALSE)
  }))

  trend_within <- data[within_group_rows, ]
  trend_within$grouping_var <- get_group(trend_within$group, groups = groups)
  trend_within_long <- prep_trend(data = trend_within, sig_niveau = sig_niveau)
  trend_within_long <- rename_column(trend_within_long, old = "esttrend", new = "est_trend_within")
  trend_within_long <- rename_column(trend_within_long, old = "sig_trend", new = "sig_trend_within")
  trend_within_long <- rename_column(trend_within_long, old = "setrend", new = "se_trend_within")
  point_estimates <- prep_points(data, sig_niveau = sig_niveau)
  point_estimates <- rename_column(point_estimates, old = "est", new = "est_point")
  point_estimates <- rename_column(point_estimates, old = "sig", new = "sig_point")



  # Put into list -----------------------------------------------------------
  prep_list[["trend_whole"]] <- trend_whole_long
  prep_list[["trend_within"]] <- trend_within_long
  prep_list[["point_estimates"]] <- point_estimates
  prep_list[["trend_point"]] <- prep_trend_point(trend_whole = trend_whole_long, trend_within = trend_within_long, point_estimates = point_estimates)
  prep_list[["trend_braces"]] <- prep_list[["trend_point"]]


  ## filter respective rows:
  ## only use consecutive years:
  years_whole <- c(prep_list[["trend_whole"]]$year_start, prep_list[["trend_whole"]]$year_end)
  plot_years <- consecutive_numbers(years_whole)
  prep_list[["trend_whole"]] <- prep_list[["trend_whole"]][filter_years(prep_list[["trend_whole"]], plot_years), ]

  years_within <- c(prep_list[["trend_within"]]$year_start, prep_list[["trend_within"]]$year_end)
  plot_years <- consecutive_numbers(years_within)
  prep_list[["trend_within"]] <- prep_list[["trend_within"]][filter_years(prep_list[["trend_within"]], plot_years), ]

  years_trend_point <- c(prep_list[["trend_point"]]$year_start, prep_list[["trend_point"]]$year_end)
  plot_years <- consecutive_numbers(years_within)
  prep_list[["trend_point"]] <- prep_list[["trend_point"]][filter_years(prep_list[["trend_point"]], plot_years), ]


  ## for braces
  prep_list[["trend_braces"]] <- prep_list[["trend_braces"]][filter_years(
    prep_list[["trend_braces"]],
    list(c(2011, 2021), c(2016, 2021))
  ), ]
  return(prep_list)
}



# utils

# Extract group membership from group column. Splits String by "." and extracts the values that are found in the "gruops"-Vektor.
get_group <- function(val_vec, groups) {
  val_vec <- gsub("_", "\\.", val_vec)
  group_vec <- strsplit(val_vec, split = "\\.")

  res_vec <- unlist(
    lapply(group_vec, function(x) {
      log_vec <- x %in% groups
      if (all(log_vec == FALSE)) {
        res <- NA
      } else {
        res <- x[log_vec]
      }
      if (length(res) > 1) {
        stop("Multiple groups in your grouping column.")
      } else {
        return(res)
      }
    })
  )
  return(res_vec)
}

# Return rows with respective start and end years.
filter_years <- function(data, year_list) {
  # Filter the respective rows
  year_rows <- unlist(lapply(year_list, function(x) {
    which(data$year_start == x[1] & data$year_end == x[2])
  }))
  return(year_rows)
}
