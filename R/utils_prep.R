calc_sig <- function(p_vec, sig_niveau = 0.05) {

    res <- as.factor(ifelse(is.na(p_vec), NA,
                                 ifelse(p_vec < sig_niveau & !is.na(p_vec), TRUE, FALSE))
                          )
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

