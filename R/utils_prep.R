calc_sig <- function(data, sig_niveau = 0.05, p_column = "p") {
  if (any(is.na(data[, p_column]))) {
    stop("Your p-values should not contain any missings. Please check your input data.")
  } else {
    data$sig <- as.factor(ifelse(data[, p_column] < sig_niveau & !is.na(data[, p_column]), TRUE, FALSE))
    return(data)
  }
}
