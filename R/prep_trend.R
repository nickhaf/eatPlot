
#' Title
#'
#' @param data Data for line plot.
#' @param sig_niveau Significance niveau.
#'
#' @return Data frame in long format, which includes the relevant trend variables.
#' @export
#'
#' @examples #tbd
prep_trend <- function(data, sig_niveau = 0.05){

# Trend data --------------------------------------------------------------
  trend_cols_pos <- grep("est_trend|sig_trend|se_trend", colnames(data))
  trend_cols <- colnames(data)[grep("est_trend|sig_trend|se_trend", colnames(data))]

  colnames(data)[trend_cols_pos] <- gsub("\\.", "",
                                         gsub("_", "", trend_cols))
  ## before first number, insert ".". Needed by reshape() for automatically building the new columns.
  colnames(data)[trend_cols_pos] <- sapply(colnames(data)[trend_cols_pos], function(x) sub("2", ".2", x)) ## use first number here

  data <- data[, c("group", "TR_BUNDESLAND", "grouping_var", colnames(data)[trend_cols_pos])]
  trend_cols <- colnames(data)[grep("trend", colnames(data))]


  # Longformat mit den TrendSpalten in einer:
  data_l <- stats::reshape(data, varying = trend_cols, direction = "long")
  years <- regmatches(data_l$time, gregexpr("[[:digit:]]+", data_l$time))

  # extract the years and add them to the long data frame
  year_cols <- data.frame()

  for(i in 1:length(years)){
  year_cols <- rbind(year_cols, years[[i]])
  }

  colnames(year_cols) <- c("year_start", "year_end")
  data_l <- cbind(data_l, year_cols)
  data_l$sig_trend <- calc_sig(data_l$sigtrend, sig_niveau = sig_niveau)

  return(data_l)

}
