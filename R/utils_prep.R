calc_sig <- function(p_vec, sig_niveau = 0.05) {

    res <- ifelse(is.na(p_vec), NA,
                                 ifelse(p_vec < sig_niveau & !is.na(p_vec), TRUE, FALSE))

    return(res)
}


filter_strings <- function(identifier, paste_vec, val_vec){


  if(any(duplicated(val_vec))){
    stop("Duplicated groups. For example, there might be two groups of the same type within the same Bundesland.")
  }else{
  res <- sapply(identifier, function(x){
      grep(paste0(x, paste_vec ), val_vec)
  },
  USE.NAMES = FALSE)
  }
  return(res)
}


rename_column <- function(data, old, new){
  colnames(data)[colnames(data) == old] <- new
  return(data)
}

## Find the years that can be plotted as trend. Returns all unique consecutive year combinations.
consecutive_numbers <- function(vec){

  vec_ordered <- vec[order(vec)]
  res <- list()
i <- 1
  while(i < length(vec_ordered)){
    res_numbers <- c(vec_ordered[i], vec_ordered[i + 1])
    if(res_numbers[[1]] != res_numbers[[2]]){
    res[[i]] <- res_numbers
    i <- i + 1}else{
      i <- i + 1
    }
  }

res <- unique(Filter(Negate(is.null), res))
  return(res)
}



# extractor for specific types of rows ------------------------------------

get_group <- function(val_vec, groups, starts_with = "", ends_with = "", log_res = TRUE){
  if(log_res == TRUE){
  grepl(paste0(
    paste0(starts_with, groups, ends_with), collapse = "|"),
        val_vec)
  }else{
    grep(paste0(
      paste0(starts_with, groups, ends_with), collapse = "|"),
      val_vec,
      value = TRUE)
}
    }

get_wholeGroup <- function(val_vec){
  grepl("wholeGroup", val_vec)
}


# Extract group membership from group column. Splits String by "." and extracts the first value that is found in the group_vector
write_group <- function(val_vec, groups) {
  val_vec <- gsub("_", "\\.", val_vec)
  group_vec <- strsplit(val_vec, split = "\\.")

unlist(
    lapply(group_vec, function(x) {
      log_vec <- x %in% groups
      if (all(log_vec == FALSE)) {
        res <- NA
      } else {
        res <- x[log_vec][1]
      }
        return(res)

    })
  )

}


# Helper functions for reshaping to long format ---------------------------
prep_long <- function(data, include_pattern, remove_pattern = NULL, suffix = ""){

  if(!is.null(remove_pattern)){
  cols_removed <- grep(remove_pattern, colnames(data), invert = TRUE, value = TRUE)
  data <- data[, cols_removed]
  }

  col_pos <- grep(include_pattern, colnames(data))
  colnames(data)[col_pos] <- gsub("\\.|_", "", colnames(data)[col_pos])

  ## before first number, insert ".". Needed by reshape() for automatically building the new columns.
  colnames(data)[col_pos] <- sapply(colnames(data)[col_pos], function(x) sub("2", ".2", x)) ## HACKY: use first number here instead of 2

  data_long <- stats::reshape(data, direction = "long", varying = colnames(data)[col_pos])
  data_long$id <- NULL

  # put suffix on all new columns containing the values:
  new_colnames <- colnames(data_long)[!(colnames(data_long) %in% colnames(data))]
  for(i in new_colnames){
  data_long <- rename_column(data_long, old = i, new = paste0(i, "_", suffix))
  }

  colnames(data_long) <- gsub("\\.", "_", colnames(data_long))
  data_long <- rename_column(data_long, old = paste0("time_", suffix), new = "year")

  return(data_long)
}

## Split the time column with the two comparisons years into two columns, so start- and endyear both have a seperate column
split_years <- function(data){

  years <- regmatches(data$year, gregexpr("[[:digit:]]+", data$year))

  # extract the years and add them to the long data frame
  year_cols <- data.frame()

  for(i in 1:length(years)){
    year_cols <- rbind(year_cols, as.numeric(years[[i]]))
  }
  colnames(year_cols) <- c("year_start", "year_end")
  data <- cbind(data, year_cols)

  return(data)
}
