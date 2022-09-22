draw_brace <- function(dat_trend, range_est, upper_label, lower_label, year_vec, bundesland){

  range_est <- range(c(dat_trend$est_start, dat_trend$est_end))

  brace_coordinates <- dat_trend %>%
    mutate(brace_upper_y = ifelse(year_start == min(year_start), range_est[1] - 30, range_est[1] -62)) %>%
    mutate(brace_lower_y = ifelse(year_start == min(year_start), range_est[1] - 60, range_est[1] -92)) %>%
    mutate(label_pos = ifelse(KBuecher_imp3 == 1, range_est[1] - 103, range_est[1] - 123))
  c(
    coord_cartesian(ylim = c(range_est[1] - 30, range_est[2]), clip = "off"), # necessary, so the brace can be drawn inside the plot

    lapply(unique(dat_trend$year_start), function(x){
      coordinates <- brace_coordinates %>%
        filter(year_start == x) %>%
        select(year_start, year_end, brace_upper_y, brace_lower_y) %>%
        unique

      ggbrace::geom_brace(
        mapping = aes(
          x = c(coordinates$year_start, coordinates$year_end),
          y = c(coordinates$brace_upper_y, coordinates$brace_lower_y)
          ),
        mid = ifelse(coordinates$year_start == min(brace_coordinates$year_start), 0.25, 0.5),
        inherit.data = F,
        rotate = 180,
        size = 0.8,
        npoints = 200
        )
      }),

    geom_text(data = brace_coordinates[brace_coordinates$TR_BUNDESLAND == bundesland &
                                         !(brace_coordinates$year_start == 2011 &
                                            brace_coordinates$year_end == 2016), ],
              mapping = aes(
                x = year_start + (year_end - max(year_start))/2,
                y = label_pos,
                label = estTrend_within,
                fontface = sigTrend_within
                ),
              size = 3
              )
    )}
