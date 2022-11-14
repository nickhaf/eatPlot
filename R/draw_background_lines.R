# Draws wholeGroup lines

draw_background_lines <- function(wholeGroup) {
  list(
    ggplot2::geom_segment(
      data = wholeGroup,
      aes(
        x = rep(as.numeric(2011), nrow(wholeGroup)),
        xend = rep(as.numeric(2016), nrow(wholeGroup)),
        y = get(paste0("est_", 2011)),
        yend = get(paste0("est_", 2016))
      ),
      size = 1.6,
      color = rgb(147, 205, 221,
        maxColorValue = 255
      )
    ),
    ggplot2::geom_segment(
      data = wholeGroup,
      aes(
        x = rep(as.numeric(2016), nrow(wholeGroup)),
        xend = rep(as.numeric(2021), nrow(wholeGroup)),
        y = get(paste0("est_", 2016)),
        yend = get(paste0("est_", 2021))
      ),
      size = 1.6,
      color = rgb(147, 205, 221,
        maxColorValue = 255
      )
    )
  )
}
