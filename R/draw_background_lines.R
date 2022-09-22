draw_background_lines <- function(wholeGroup){
 list(geom_segment(data = whole_group,
               aes(
                 x = rep(as.numeric(2011), nrow(whole_group)),
                 xend = rep(as.numeric(2016), nrow(whole_group)),
                 y = get(paste0("est_", 2011)),
                 yend = get(paste0("est_", 2016))
               ),
               size = 0.7,
               color = rgb(147, 205, 221,
                           maxColorValue = 255)),
    geom_segment(data = whole_group,
                 aes(
                   x = rep(as.numeric(2016), nrow(whole_group)),
                   xend = rep(as.numeric(2021), nrow(whole_group)),
                   y = get(paste0("est_", 2016)),
                   yend = get(paste0("est_", 2021))
                 ),
                 size = 0.7,
                 color = rgb(147, 205, 221,
                             maxColorValue = 255))
 )
}
