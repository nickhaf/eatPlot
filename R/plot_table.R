
#' Build columns from geom_text
#'
#' @param data_prep Data prepared with \code{prep_tableplot()}.
#'
#' @return
#' @export
#'
#' @examples
build_columns <- function(data_prep) {
  c(
    ## y_label is build extra.
    ggplot2::geom_text(
      data = data_prep[data_prep$x_labels == "y_label", ],
      ggplot2::aes(x = "y_label", label = .data$group),
      #size = 3,
      hjust = "left",
      nudge_x = -0.55
    ),
    ## If more flexibility in column look is wanted, maybe add here:
    lapply(unique(data_prep$x_labels[data_prep$x_labels != "y_label"]), function(x_label) {
      ggplot2::geom_text(
        data = data_prep[data_prep$x_labels == x_label, ],
        #size = 3,
        hjust = "center"
      )
    }),
    ggplot2::scale_x_discrete(position = "top", limits = c("y_label", levels(data_prep$x_labels)[levels(data_prep$x_labels) != "y_label"]))
  )
}


#' Draw a table in ggplot2.
#'
#' @param data_prep Data prepered with \code{prep_tableplot()}.
#'
#' @return ggplot2 object.
#' @export
#'
#' @examples #tbd
plot_table <- function(data_prep) {
  ggplot2::ggplot(data_prep, ggplot2::aes(x = .data$x_labels, y = .data$group, label = .data$value)) +
    ggforestplot::geom_stripes(
      odd = grDevices::rgb(219, 238, 244, maxColorValue = 255),
      even = "#00000000"
    ) +
    build_columns(data_prep) +
    theme_table() +
    ggplot2::labs(caption = "Anmerkungen. In der Tabelle werden gerundete Werte angegeben. \n
        R2 = Determinationskoeffizient; M = Mittelwert. \n
        Fett gedruckte Werte unterscheiden sind statistisch signifikant (p < .05) vom Wert fuer Deutschland insgesamt. \n
        Schraffierte Balken zeigen eine statistisch nicht signifikante Differenz vom Wert fuer Deutschland insgesamt an.") +
    NULL
}
