prepare_general <- function(raw_data, competence){
  colnames(raw_data) <- gsub("sig", "p", colnames(raw_data))
  raw_data <- raw_data[raw_data$kb == competence, ]
  raw_data <- raw_data[raw_data$parameter == "mean", ]
  return(raw_data)
}
