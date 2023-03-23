## Idee: Eine variables() - Funktion, die als Constructer für die Spaltennamen für die jeweiligen Plots nötig ist. Man gibt also die entsprechenden Spaltennamen ein, die Funktion checkt, ob diese in den Daten vorhanden sind, und erzeugt eine Liste mit den entsprechenden Spaltennamen.


# constructor
# new_variables <- function(
#     dat = data.frame(),
#     point_values = character(),
#     point_sig = character(),
#     line_values = character(),
#     line_sig = character(),
#     label_est = character(),
#     label_se = character(),
#     label_sig_high = character(),
#     label_sig_bold = character()
#     ){
#
#
#   value <- list(point_values = point_values,
#                 point_sig = point_sig,
#                 line_values = line_values,
#                 line_sig = line_sig,
#                 label_est = label_est,
#                 label_se = label_se,
#                 label_sig_high = label_sig_high,
#                 label_sig_bold = label_sig_bold
#                 )
#
#   attr(value, "class") <- "variables_eatPlot"
#   value
#
# }


# Helper ------------------------------------------------------------------

variables <- function(
    dat = data.frame(),
    point_values = "est_point",
    point_sig = "sig_point",
    line_values = c("est_point_start", "est_point_end"),
    line_sig = "sig_trend_comp_within",
    label_est = "est_trend_no_comp",
    label_se = "se_trend_no_comp",
    label_sig_high = "sig_trend_comp_whole",
    label_sig_bold = "sig_trend_no_comp"){


  value <- list(point_values = check_columns(dat, point_values),
                point_sig = check_columns(dat, point_sig),
                line_values = check_columns(dat, line_values),
                line_sig = check_columns(dat, line_sig),
                label_est = check_columns(dat, label_est),
                label_se = check_columns(dat, label_se),
                label_sig_high = check_columns(dat, label_sig_high),
                label_sig_bold = check_columns(dat, label_sig_bold)
  )

  attr(value, "class") <- "variables_eatPlot"
  value

}

is.variables <- function(obj){inherits(obj, "variables_eatPlot")}


# Utils -------------------------------------------------------------------
check_columns <-  function(dat, column){
  if(column %in% colnames(dat)){
    return(column)
  }else{
    warning(paste0("The column '", column, "' was not found in data and will not be considered for the plot."))
    return(NULL)
  }
}
