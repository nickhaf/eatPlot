## Predefined ggplot-themes for iqb plots.


#' Theme for barplot that will be combined with a table.
#'
#' @return ggplot theme.
#' @export
theme_table_bar <- function(){

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
