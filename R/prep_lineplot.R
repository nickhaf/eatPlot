
prepare_trend <- function(data, grouping_var, sig_niveau){

# Trend data --------------------------------------------------------------
  trend_cols_pos <- grep("est_trend|sig_trend", colnames(data))
  trend_cols <- colnames(data)[grep("est_trend|sig_trend", colnames(data))]

  colnames(data)[trend_cols_pos] <- gsub("\\.", "",
                                     gsub("_", "", trend_cols))
  ## bevfore first number, insert ".". Needed by reshape() for automatically building the new columns.
  colnames(data)[trend_cols_pos] <- sapply(colnames(data)[trend_cols_pos], function(x) sub("2", ".2", x)) ## use first number here

  data <- data[, c("group", "TR_BUNDESLAND", grouping_var, colnames(data)[trend_cols_pos])]
  trend_cols <- colnames(data)[grep("trend", colnames(data))]


  # Longformat mit den TrendSpalten in einer:
  data_l <- reshape(data, varying = trend_cols, direction = "long")
  years <- regmatches(data_l$time, gregexpr("[[:digit:]]+", data_l$time))

  # extract the years and add them to the long data frame
  year_cols <- data.frame()

  for(i in 1:length(years)){
  year_cols <- rbind(year_cols, years[[i]])
  }

  colnames(year_cols) <- sapply(1:length(years[[1]]), function(x){paste0("year_", x)})
  data_l <- cbind(data_l, year_cols)
  data_l <- calc_sig(data_l, p_column = "sigtrend", sig_niveau = sig_niveau)

  ## Noch die richtigen Zeilen rausziehen (wahrscheinlich am Anfang). Evtl müssen die years doch in eine Spalte, also nochmal langgezogen werden.

  # dat_trend2$TR_BUNDESLAND <- sub("_.*", "", dat_trend2$group)
  # dat_trend2[grep("wholeGroup", dat_trend2$TR_BUNDESLAND), "TR_BUNDESLAND"] <- "Deutschland"
  # dat_trend2[dat_trend2$TR_BUNDESLAND %in% c("0", "1"), "TR_BUNDESLAND"] <- "Deutschland"
  # dat_trend2$group <- NULL
  #
  # colnames(dat_trend2)[colnames(dat_trend2) %in% c("esttrend", "ptrend", "setrend","sig_trend")] <- c( paste0("estTrend_", suffix), paste0("pTrend_", suffix), paste0("seTrend_", suffix), paste0("sigTrend_", suffix))
  return(data_l)

}
