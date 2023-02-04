
#' Build columns from geom_text
#'
#' @param data_plot_table Data prepared with \code{prep_tableplot()}.
#'
#' @return ggplot2 object
#' @export
#'
#' @examples #tbd
build_columns <- function(data_plot_table) {
 column_settings <-  c(
    ## y_label is build extra.
    ggplot2::geom_text(
      data = data_plot_table[data_plot_table$x_label == "y_label", ],
      ggplot2::aes(x = "y_label", label = .data$group),
      size = 3,
      hjust = "left",
      nudge_x = -0.55
    ),
    ## If more flexibility in column look is wanted, maybe add here:
    lapply(unique(data_plot_table$x_label[data_plot_table$x_label != "y_label"]), function(x_label) {
      ggplot2::geom_text(
        data = data_plot_table[data_plot_table$x_label == x_label, ],
        size = 3,
        hjust = "center"
      )
    }),
    ggplot2::scale_x_discrete(position = "top", limits = c("y_label", levels(data_plot_table$x_label)[levels(data_plot_table$x_label) != "y_label"]))
  )
 return(column_settings)
}


#' Draw an IQB table.
#'
#' `plot_table()` builds a table as ggplot2 object.
#'
#' @param no_trend_list Input is a list prepared by [prep_no_trend()]. You can also use the according data.frame named `plot_table` from this list.
#' @param y_value Character string of the column name containing values that should be written in the table columns. Defaults to `"est_point"`, which are the ability estimates in the states.

#'
#' @return ggplot2 table.
#' @export
#'
#' @examples #tbd
plot_table <- function(no_trend_list, y_value = "est_point") {

  if(inherits(no_trend_list, "list")){
    data_plot_table <- no_trend_list[["plot_bar"]]
  }else{
    data_plot_table <- no_trend_list
  }


  ggplot2::ggplot(data_plot_table, ggplot2::aes(x = .data$x_label, y = .data$group, label = .data[[y_value]])) +
    ggplot2::geom_text(
      data = data_plot_table[data_plot_table$x_label == "y_label", ],
      ggplot2::aes(x = "y_label", label = .data$group),
      size = 3,
      hjust = "left",
      nudge_x = -0.55
    ) +
    ggstats::geom_stripped_rows(
      odd = grDevices::rgb(219, 238, 244, maxColorValue = 255),
      even = "#00000000"
    ) +
    build_columns(data_plot_table) +
    theme_table() +
    ggplot2::labs(caption = "Anmerkungen. In der Tabelle werden gerundete Werte angegeben. \n
        R2 = Determinationskoeffizient; M = Mittelwert. \n
        Fett gedruckte Werte unterscheiden sind statistisch signifikant (p < .05) vom Wert fuer Deutschland insgesamt. \n
        Schraffierte Balken zeigen eine statistisch nicht signifikante Differenz vom Wert fuer Deutschland insgesamt an.") +
    NULL
}
