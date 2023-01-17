## Helper functions

#' Compute if a p value is significant.
#'
#' @description Helper function.
#' @param data Input data.frame.
#' @param sig_niveau Significance niveau.
#' @param p_column Column in data which contains the p-values.
#'
#' @return The data.frame with a new column named 'sig'
#' @export
#'
#' @examples calc_sig(data.frame("pvalue" = c(0.02, 0.1, 0.01, 12.4)), sig_niveau = 0.05, p_column = "pvalue")
calc_sig <- function(data, sig_niveau = 0.05, p_column = "p"){
  if(any(is.na(data[ , p_column]))){
    stop("Your p-values should not contain any missings. Please check your input data.")
  }else{
    data$sig <- as.factor(ifelse(data[ , p_column] < sig_niveau & !is.na(data[ , p_column]), TRUE, FALSE))
    return(data)
  }
}


#' Merge to columns to a fill-column
#'
#' @description Merges sub_groups and sig-column to a new fill column.
#' @param data
#'
#' @return Data frame with a new fill column.
#' @export
#'
#' @examples
calc_fill <- function(data){
if(any(is.na(data$sub_groups))){
  stop("Your subgroups should not contain any missings. Please check your input data.")
}else{
  data$fill <- paste0(data$sub_groups, "_", data$sig)
  return(data)
}
}


#' Prepare data for plotting a barplot.
#'
#' @param data Output from \code{}
#' @param sub_groups sub_groups which should be plotted in the same plot tile. E.g. you can distinguish between adjusted and not adjusted means.
#' @param sig_niveau At which significance niveau do p-values become significant?
#'
#' @return Returns a \code{data.frame} which can be used as input for \code{plot_bar}.
#' @export
#'
#' @examples #tbd
prep_barplot <- function(data, sub_groups, sig_niveau){

  colnames(data)[colnames(data) == sub_groups] <- "sub_groups"
  data <- calc_sig(data, sig_niveau)
  data <- calc_fill(data)

  data <- subset(data, data$parameter == "mean" & !is.na(data$parameter) &
                       data$comparison == "crossDiff" & !is.na(data$comparison))
}

## Mehere Dataframes in eine Liste


############

# Bespielplot: "Q:\BT2021\BT\60_Bericht\04_Mittelwerte\adjustierte Mittelwerte\Druckfahnen\Druckfahne1_4.3_IQB.pdf"

bt21_NoTrend <- adjusted_means
library(tidyverse)

prep_tableplot <- function(data, sub_groups, competence, sig_niveau = 0.05){

data <- adjusted_means
colnames(data)[colnames(data) == sub_groups] <- "sub_groups"
data <- calc_sig(data)
data <- data[data$group != "wholeGroup" & data$kb == competence, ]
## crossdiff p with and without adjustment in NA p at the respective place
data <- data[!is.na(data$comparison), ] #lookout: later we need the p values from data$comparison == NA

data <- data[ , c("group", "est" ,"sub_groups")]

groups <- unique(data$group)
data <- rbind(data, data.frame(group = groups, est = rep(NA, length(groups)), sub_groups = rep("y_label", length(groups))))
colnames(data) <- c("group", "value", "x_labels")
data$x_label <- factor(data$x_label, levels = c("y_label", unique(data$x_labels[data$x_labels != "y_label"])),  ordered = TRUE)

data$value <- as.character(round(data$value))
return(data)

}

t1_long <- prep_tableplot(data = adjusted_means, sub_groups = "adjust", competence = "GL", sig_niveau = 0.05)


build_columns <- function(data_prep){


c(

  ggplot2::geom_text(data = data_prep[data_prep$x_labels == "y_label" , ],
                     aes(x = "y_label", label = group),
                     size = 3,
                     hjust = "left",
                     nudge_x = -0.55),

lapply(unique(data_prep$x_labels[data_prep$x_labels != "y_label"]), function(x_label){

  ggplot2::geom_text(data = data_prep[data_prep$x_labels == x_label, ],
                     size = 3,
                     hjust = "center")
  }
)
)


}

plot_table <- function(data_prep, first_column)

ggplot2::ggplot(t1_long, aes(x = x_labels, y = group, label = value)) +
  ggforestplot::geom_stripes(odd = rgb(219, 238, 244, maxColorValue = 255),
                             even = "#00000000") +

  build_columns(t1_long)
  ## jede spalte einzeln und mit lapply aufrufen?
  ggplot2::geom_text(data = t1_long %>% filter(x_labels == "ohneAdj"),
                     size = 3,
                     hjust = "center"
                     #nudge_x = c(-0.3, 0, 0)
  ) +
  ggplot2::geom_text(data = t1_long %>% filter(estimate == "Bundesland"),
                     aes(x = "Bundesland", label = group),
                     size = 3,
                     hjust = "left",
                     nudge_x = -0.55) +
  ggplot2::scale_x_discrete(position = "top",
                            labels = c("Land",
                                       expression(atop(underline(atop("nicht", "adjustiert")), italic("M"))),
                                       "adjustiert")) +
  labs(y = NULL, x = NULL) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, hjust = 0.5, color = "black", face = "bold"),
    axis.ticks = element_blank(),
    #axis.title = element_text(face = "bold", hjust = 0),
    plot.margin = unit(c(0, -0.1, 0, 0), "cm"),
    plot.caption = element_text(hjust = 0),
    legend.position = "bottom"
  ) +
  labs(caption = "Anmerkungen. In der Tabelle werden gerundete Werte angegeben. \n
        R2 = Determinationskoeffizient; M = Mittelwert. \n
        Fett gedruckte Werte unterscheiden sind statistisch signifikant (p < .05) vom Wert für Deutschland insgesamt. \n
        Schraffierte Balken zeigen eine statistisch nicht signifikante Differenz vom Wert für Deutschland insgesamt an.")







