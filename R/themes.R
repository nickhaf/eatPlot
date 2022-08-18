theme_line_iqb <- function(){

  theme_minimal() %+replace%

    theme(

      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.y = element_blank()

    )
}


theme_bar_iqb <- function(){

  theme_minimal() %+replace%

    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )
}
