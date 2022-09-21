prepare_general <- function(raw_data){
  colnames(bt_data) <- gsub("sig", "p", colnames(bt_data))
  bt_data <- bt_data[bt_data$kb == competence, ]
  bt_data <- bt_data[bt_data$parameter == "mean", ]
  return(bt_data)
}
