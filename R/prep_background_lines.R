#
# data <- trend_books
# competence = "GL"
#
# prep_background_lines <- function(data, background_group = "wholeGroup", competence){
#
#   data <- data[c(data$group == background_group & data$parameter == "mean" & data$kb == competence), ]
#   data <- data[, c("group", )]
#
#   return(data)
#
# }
#
# prep_background_lines(data, competence = "GL")
