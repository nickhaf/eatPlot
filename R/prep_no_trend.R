#' Prepare data for plotting a barplot.
#'
#' @inheritParams prep_trend
#' @param columns Character string containing the column in `data` that contains the column names for the table_plot.
#'
#' @return Returns a data.frame which can be used as input for plot_bar.
#' @export
#'
#' @examples # tbd
prep_no_trend <- function(data, grouping_var, columns, competence, sig_niveau){

  BLs <- unique(data$TR_BUNDESLAND)[!is.na(unique(data$TR_BUNDESLAND))]
  groups <- unique(data[ , grouping_var][!is.na(data[ ,grouping_var])])

  data <- clean_data(data, grouping_var = grouping_var, competence = competence, BLs = BLs, groups = groups)

  data$sig <- calc_sig(data[, grep("^p_|^p$", colnames(data))], sig_niveau = sig_niveau)
  data <- calc_fill(data)

  data_wholeGroup <- data[data$comparison == "crossDiff" & !is.na(data$comparison), ]
  data_point <- data[is.na(data$comparison), ]

  data_no_trend <- merge(data_wholeGroup[, colnames(data_wholeGroup) != "comparison"], data_point[, colnames(data_wholeGroup) != "comparison"],
                         by = c("group", "grouping_var", "TR_BUNDESLAND"),
                         suffixes = c("_wholeGroup", "_point"),
                         all = TRUE)


  # wholeGroup must be plotted as empty row - fill up NAs with 0-values (bit hacky)---------------------
  data_bar <- data_no_trend
  data_bar[data_bar$group == "wholeGroup" & is.na(data_bar$sig_wholeGroup), "sig_wholeGroup" ] <- FALSE
  data_bar[data_bar$group == "wholeGroup" & is.na(data_bar$fill_wholeGroup), "fill_wholeGroup" ] <- paste0(data_bar[data_bar$group == "wholeGroup" & is.na(data_bar$fill_wholeGroup), "grouping_var" ], "_", data_bar[data_bar$group == "wholeGroup" & is.na(data_bar$fill_wholeGroup), "sig_wholeGroup" ])
  data_bar[data_bar$group == "wholeGroup" & is.na(data_bar$est_wholeGroup), "est_wholeGroup" ] <- 0


  plot_data <- list()
  plot_data[["plot_bar"]] <- data_bar


# Prepare tableplot -------------------------------------------------------

  if(grouping_var == columns){
    data_no_trend$x_label <- data_no_trend$grouping_var
  }else{
    data_no_trend$x_label <- data_no_trend[, columns]
  }


  data_y_labels <- data.frame(group = unique(data_no_trend$group),
                              x_label = rep("y_label",
                                            length(unique(data_no_trend$group))
                                            )
                              )


  # fill in non-overlapping columns with NAs
  data_no_trend[setdiff(names(data_y_labels), names(data_no_trend))] <- NA
  data_y_labels[setdiff(names(data_no_trend), names(data_y_labels))] <- NA


  data_table <- rbind(data_no_trend,
                data_y_labels)


  data_table$x_label <- factor(data_table$x_label, levels = c("y_label", unique(data_table$x_label[data_table$x_label != "y_label"])), ordered = TRUE)



  plot_data[["plot_table"]] <- data_table

  return(plot_data)
  }


## Helpers

calc_fill <- function(data) {
  if (any(is.na(data$sub_groups))) {
    stop("Your subgroups should not contain any missings. Please check your input data.")
  } else {
    data$fill <- paste0(data$grouping_var, "_", data$sig)
    return(data)
  }
}
