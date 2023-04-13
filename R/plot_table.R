#' Draw an IQB table.
#'
#' `plot_table()` builds a table as ggplot2 object.
#'
#' @param no_trend_list Input is a list prepared by [prep_no_trend()]. You can also use the according data.frame named `plot_table` from this list.
#' @param x_axis Character string of the column name containing the x-axis labels, meaning the column names. Defaults to `"grouping_var"`.
#' @param y_axis Character string of the column name containing the y-axis labels. Defaults to `"state_var"`.
#' @param y_value Character string of the column name containing values that should be written in the table columns. Defaults to `"est_point"`, which are the ability estimates in the states.
#' @param plot_settings tbd

#'
#' @return ggplot2 table.
#' @export
#'
#' @examples #tbd
plot_table <- function(no_trend_list,
                       x_axis = "grouping_var",
                       y_axis = "state_var",
                       y_value = "est_point",
                       plot_settings = plotsettings_barplot()) {

  if(inherits(no_trend_list, "list")){
    data_plot_table <- no_trend_list[["plot_bar"]]
  }else{
    data_plot_table <- no_trend_list
  }


y_plot <- plot_column(vec = unique(data_plot_table[[y_axis]][!is.na(data_plot_table[[y_axis]])]),
                      plot_settings = plot_settings) +
  theme_table_col()


table_plot <- ggplot2::ggplot(
  data_plot_table,
  ggplot2::aes(
    x = .data[[x_axis]],
    y = .data[[y_axis]],
    label = .data[[y_value]]
  )) +
    plot_stripes() +
    build_columns(data_plot_table, x_axis) +
    theme_table()

table_final <- patchwork::wrap_plots(y_plot, table_plot, widths = c(0.3, 1))  &
  ggplot2::theme(
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
    # As margin is not perfectly eliminated
    axis.ticks.length.y = ggplot2::unit(0, "pt")
  )

# table_final +
#   patchwork::plot_annotation(
#   caption = "Anmerkungen. In der Tabelle werden gerundete Werte angegeben. \n
#         R2 = Determinationskoeffizient; M = Mittelwert. \n
#         Fett gedruckte Werte unterscheiden sind statistisch signifikant (p < .05) vom Wert fuer Deutschland insgesamt. \n
#         Schraffierte Balken zeigen eine statistisch nicht signifikante Differenz vom Wert fuer Deutschland insgesamt an.") +
#   NULL

return(table_final)

}



#' Build columns
#'
#' @param data_plot_table Data.
#' @inheritParams plot_table
#'
#' @return ggplot2 object
#' @export
#'
#' @examples #tbd
build_columns <- function(data_plot_table, x_axis) {
  column_settings <-  c(
    lapply(unique(data_plot_table[[x_axis]]), function(x_label) { ## Hier könnte es auch Spaltennamen geben, es braucht nur die x-Achse als extra Spalte, oder?
## hier auch noch die Spalten angeben, die geplotted werden sollen.

      ggplot2::geom_text(
        data = data_plot_table[data_plot_table[[x_axis]] == x_label, ],
        size = 3,
        hjust = "center"
      )
    }),
    ggplot2::scale_x_discrete(position = "top", name = NULL)
  )
  return(column_settings)
}



build_columns_2 <- function(data_plot_table, x_axis, plot_settings) {
  column_settings <-  c(
    lapply(1:length(unique(data_plot_table[[x_axis]])), function(i) {

      x_label <- levels(data_plot_table[[x_axis]])[i]
      ggplot2::geom_text(
        data = data_plot_table[data_plot_table[[x_axis]] == x_label, ],
        size = 3,
        hjust = plot_settings$col_adjustment[i],
        nudge_x = plot_settings$col_nudge_x[i]
      )
    }),
    ggplot2::scale_x_discrete(position = "top", name = NULL)
  )
  return(column_settings)
}
