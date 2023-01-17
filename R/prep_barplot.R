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

  if(any(is.na(data$sub_groups))){
    stop("Your subgroups should not contain any missings. Please check your input data.")
  }else{
    data$fill <- paste0(data$sub_groups, "_", data$sig)
  }

  data <- subset(data, data$parameter == "mean" & !is.na(data$parameter) &
                       data$comparison == "crossDiff" & !is.na(data$comparison))
}

## Mehere Dataframes in eine Liste


############

# Bespielplot: "Q:\BT2021\BT\60_Bericht\04_Mittelwerte\adjustierte Mittelwerte\Druckfahnen\Druckfahne1_4.3_IQB.pdf"
#
# bt21_NoTrend <- adjusted_means
# library(tidyverse)
#
# prep_tableplot <- function(data){
#
#
#
#
#
# }
# # Tabelle erstellen
# t1 <- bt21_NoTrend %>%
#   mutate(significant = factor(ifelse(p < 0.05, "yes", "no"))) %>%
#   mutate(adj_sig = case_when(
#     adjust == "ohneAdj" & significant == "no" ~ "noAdj_noSig",
#     adjust == "ohneAdj" & significant == "yes" ~ "noAdj_Sig",
#     adjust == "mitAdj" & significant == "no" ~ "Adj_noSig",
#     adjust == "mitAdj" & significant == "yes" ~ "Adj_Sig"
#   )) %>%
#   filter(kb == "GL" & is.na(comparison) & group != "wholeGroup") %>%
#   select(group, est, adjust) %>%
#   as.data.frame %>%
#   reshape(idvar = "group", timevar = "adjust" , direction = "wide", v.names = c("est"))
#
# # Longformat
# t1_long <- t1 %>%
#   mutate_at(c("est.ohneAdj", "est.mitAdj"), round) %>%
#   mutate_at(c("est.ohneAdj", "est.mitAdj"), as.character) %>%
#   mutate(Bundesland = group) %>%
#   pivot_longer(cols = c("est.ohneAdj", "est.mitAdj", "Bundesland"), names_to = "estimate" ) %>%
#   mutate(value = ifelse(estimate == "Bundesland", NA, value))
#
#
# ggplot2::ggplot(t1_long, aes(x = estimate, y = group, label = value)) + # x-Achse adjustieren
#   ggforestplot::geom_stripes(odd = rgb(219, 238, 244, maxColorValue = 255),
#                              even = "#00000000") +
#
#   ## jede spalte einzeln und mit lapply aufrufen?
#   ggplot2::geom_text(size = 3,
#                      hjust = "center"
#                      #nudge_x = c(-0.3, 0, 0)
#   ) +
#   ggplot2::geom_text(data = t1_long %>% filter(estimate == "Bundesland"),
#                      aes(x = "Bundesland", label = group),
#                      size = 3,
#                      hjust = "left",
#                      nudge_x = -0.55) +
#   ggplot2::scale_x_discrete(position = "top",
#                             labels = c("Land",
#                                        expression(atop(underline(atop("nicht", "adjustiert")), italic("M"))),
#                                        "adjustiert")) +
#   labs(y = NULL, x = NULL) +
#   theme_classic() +
#   theme(
#     strip.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.border = element_blank(),
#     axis.line.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.text.x = element_text(size = 10, hjust = 0.5, color = "black", face = "bold"),
#     axis.ticks = element_blank(),
#     #axis.title = element_text(face = "bold", hjust = 0),
#     plot.margin = unit(c(0, -0.1, 0, 0), "cm"),
#     plot.caption = element_text(hjust = 0),
#     legend.position = "bottom"
#   ) +
#   labs(caption = "Anmerkungen. In der Tabelle werden gerundete Werte angegeben. \n
#         R2 = Determinationskoeffizient; M = Mittelwert. \n
#         Fett gedruckte Werte unterscheiden sind statistisch signifikant (p < .05) vom Wert für Deutschland insgesamt. \n
#         Schraffierte Balken zeigen eine statistisch nicht signifikante Differenz vom Wert für Deutschland insgesamt an.")
#
#
#
#
#
#
#
