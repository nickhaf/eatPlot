## Predefined ggplot-themes for iqb plots.


#' Theme for barplot that will be combined with a table.
#'
#' @return ggplot2 theme.
#' @export
theme_table_bar <- function() {
  ggplot2::theme_minimal() %+replace%

    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.position = "none",
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )
}



#' Theme for table plotted with ggplot2.
#'
#' @return ggplot2 theme.
#' @export
#'
#' @examples # tbd
theme_table <- function() {
  ggplot2::theme_classic() %+replace%
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 10, hjust = 0.5, color = "black", face = "bold"),
      axis.ticks = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(0, -0.1, 0, 0), "cm"),
      plot.caption = ggplot2::element_text(hjust = 0),
      legend.position = "bottom"
    )
}



#' Theme for lineplot.
#'
#' @return ggplot2 theme
#' @export
#'
#' @examples #tbd
theme_line <- function(){

  ggplot2::theme_minimal() %+replace%
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.position = "none",
      axis.title.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 10, hjust = 0.5),
      plot.margin = ggplot2::unit(c(0, 0, 0.30, 0), units="npc")#,
      #plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc")
    )
}


