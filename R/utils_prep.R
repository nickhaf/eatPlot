calc_sig <- function(data, sig_niveau = 0.05, p_column = "p") {

    data$sig <- as.factor(ifelse(is.na(data[, p_column]), NA,
                                 ifelse(data[, p_column] < sig_niveau & !is.na(data[, p_column]), TRUE, FALSE))
                          )
    return(data)
}


filter_strings <- function(identifier, paste_vec, val_vec){
  vapply(identifier, function(x){
      grep(paste0(x, paste_vec ), val_vec)
  },
  FUN.VALUE = numeric(1),
  USE.NAMES = FALSE)
}

