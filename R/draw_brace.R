draw_brace <- function(dat_trend, range_est, upper_label, lower_label, year_vec){
  c(
    coord_cartesian(ylim = c(range_est[1] - 30, range_est[2]), clip = "off"), # necessary, so the brace can be drawn inside the plot

    lapply(unique(dat_trend$year_start), function(x){
      brace_coordinates <- dat_trend %>%
        select(year_start, year_end, brace_upper_y, brace_lower_y) %>%
        filter(year_start == x) %>%
        unique
      ggbrace::geom_brace(
        mapping = aes(
          x = c(brace_coordinates$year_start, brace_coordinates$year_end),
          y = c(brace_coordinates$brace_upper_y, brace_coordinates$brace_lower_y)
          ),
        mid = ifelse(brace_coordinates$year_start == min(dat_trend$year_start), 0.25, 0.5),
        inherit.data = F,
        rotate = 180,
        size = 0.8,
        npoints = 200
        )
      }),

    geom_text(data = dat_trend,
              mapping = aes(
                x = year_start + (year_end - max(year_start))/2,
                y = label_pos,
                label = estTrend,
                fontface = sig_trend_2016
                ),
              size = 3
              )
    )}
