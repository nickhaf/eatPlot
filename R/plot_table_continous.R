

plot_tablebar <- function(dat,
                           column_bar = NULL,
                           columns_table = NULL,
                           columns_table_sig_bold = NULL){

res_plot <- ggplot2::ggplot(
    data = dat,
  )

if(!is.null(column_bar)){
  res_plot <- res_plot +
    ggpattern::geom_rect_pattern(
      ggplot2::aes(
        xmin = x_min,
        xmax = x_max,
        ymin = y_axis - 0.4,
        ymax = y_axis + 0.4
      ),
      pattern = "stripe",
      linetype = 2,
      colour = "red"
    )
}

if(any(!is.null(columns_table))){
  res_plot <- res_plot +
    build_columns_2(test_data, cols = rev(columns_table)) +
    # column headers:
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf,
                                    ymin = 4.5, ymax = Inf),
                       colour = "lightblue",
                       fill = "lightblue") +
    ggplot2::annotate("text", x = 0, y = 4.8, label = "Header") +
    ggplot2::annotate("text", x = -3.5, y = 4.8, label = "Colspanner") +
    ggplot2::annotate("segment", x = -4.5, xend = -2.5, y = 4.7, yend = 4.7) +
    ggplot2::scale_x_continuous(breaks = c(-2, 0, 2), expand = c(0,0))
}

return(res_plot)
}



# ggplot2::ggplot(
#   data = test_data,
# ) +
#   ggpattern::geom_rect_pattern(
#     ggplot2::aes(
#       xmin = x_min,
#       xmax = x_max,
#       ymin = y_axis - 0.4,
#       ymax = y_axis + 0.4
#     ),
#     pattern = "stripe",
#     linetype = 2,
#     colour = "red"
#   ) +
#   build_columns_2(test_data, cols = c("group", "group_2")) +
#   ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = Inf), colour = "lightblue", fill = "lightblue") +
#   ggplot2::annotate("text", x = 0, y = 4.8, label = "Header") +
#   ggplot2::annotate("text", x = -3.5, y = 4.8, label = "Colspanner") +
#   ggplot2::annotate("segment", x = -4.5, xend = -2.5, y = 4.7, yend = 4.7) +
#   ggplot2::scale_x_continuous(breaks = c(-2, 0, 2), expand = c(0,0))



build_columns_2 <- function(df, cols) {
  x_axis <- min(c(df$x_min, df$x_max))

  c(
    lapply(1:length(cols), function(i) {
      column_name <- cols[i]
      x_axis <- x_axis - i # Das muss natürlich Skalenabhängig sein, und skalierbar
      ggplot2::geom_text(
        data = df,
        ggplot2::aes(
          x = x_axis,
          y = y_axis,
          label = .data[[column_name]]
        )
      )
    })
  )
}

## Jede Spalte einzeln vorliegend mit variierendem label
# verschiedene Plots dann mit patchwork verbinden
# Manuelle Funktion mit geom_rect für die stripes.
# Berechnen der Spaltenbreite (evtl von stringmenge abhängig machen?), auf alle Fälle manuell justierbar lassen.
