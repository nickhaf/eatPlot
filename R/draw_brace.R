draw_brace <- function(dat_trend, upper_label, lower_label, year_vec, bundesland){

  range_est <- range(c(dat_trend$est_start, dat_trend$est_end))

  brace_coordinates <- dat_trend %>%
    mutate(brace_upper_y = ifelse(year_start == min(year_start), range_est[1] - 40, range_est[1] -72)) %>%
    mutate(brace_lower_y = ifelse(year_start == min(year_start), range_est[1] - 70, range_est[1] -102)) %>%
    mutate(label_pos = ifelse(KBuecher_imp3 == 1, range_est[1] - 113, range_est[1] - 143)) %>%
    mutate(sigTrend_vsGermany = ifelse(pTrend_vsGermany < 0.05, "<sup>a</sup>", ""))
  c(
    coord_cartesian(ylim = c(range_est[1] - 40, range_est[2] + 20), clip = "off"), # necessary, so the brace can be drawn inside the plot

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

    ggtext::geom_richtext(data = brace_coordinates[brace_coordinates$TR_BUNDESLAND == bundesland &
                                         !(brace_coordinates$year_start == 2011 &
                                            brace_coordinates$year_end == 2016), ],
              mapping = aes(
                x = year_start + (year_end - max(year_start))/2 - 0.35 ,
                y = label_pos,
                label = paste0(round(estTrend_within, 0), sigTrend_vsGermany),
                fontface = sigTrend_within
                ),
              size = 3,
              label.padding = grid::unit(rep(0, 4), "pt"),
              fill = NA,
              label.color = NA
              ),
    geom_text(data = brace_coordinates[brace_coordinates$TR_BUNDESLAND == bundesland &
                                         !(brace_coordinates$year_start == 2011 &
                                             brace_coordinates$year_end == 2016), ],
              mapping = aes(
                x = year_start + (year_end - max(year_start))/2 + 0.35,
                y = label_pos,
                label = paste0("(", format(round(seTrend_within, 1), nsmall = 1), ")")
              ),
              size = 3
    )
  )}

## sigTrend_vsGermany
