
# Bespielplot: "Q:\BT2021\BT\60_Bericht\04_Mittelwerte\adjustierte Mittelwerte\Druckfahnen\Druckfahne1_4.3_IQB.pdf"


bt21_NoTrend <- adjusted_means
library(tidyverse)


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







