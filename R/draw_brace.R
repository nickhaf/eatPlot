# Build braces + labels

draw_brace <- function(dat_trend, upper_label, lower_label, year_vec, bundesland) {

  # Get range for y-axis
  range_est <- range(c(dat_trend$est_start, dat_trend$est_end))

  # Calculate brace coordinates
  brace_coordinates <- dat_trend
  brace_coordinates$brace_upper_y <- ifelse(dat_trend$year_start == min(dat_trend$year_start), range_est[1] - 40, range_est[1] - 72)
  brace_coordinates$brace_lower_y <- ifelse(dat_trend$year_start == min(dat_trend$year_start), range_est[1] - 70, range_est[1] - 102)
  brace_coordinates$label_pos <- ifelse(dat_trend$KBuecher_imp3 == 1, range_est[1] - 113, range_est[1] - 133)
  brace_coordinates$sigTrend_vsGermany <- ifelse(dat_trend$pTrend_vsGermany < 0.05, "<sup>a</sup>", "")
  brace_coordinates$estTrend_within_label <- ifelse(dat_trend$sigTrend_within == "bold", paste0("**", round(dat_trend$estTrend_within, 0), "**"), round(dat_trend$estTrend_within, 0))

  # Combine different ggplot functions
  c(
    # Clip Coordinate system. Necessary, so the brace can be drawn inside the plot
    ggplot2::coord_cartesian(ylim = c(range_est[1] - 40, range_est[2] + 20), clip = "off"),
    lapply(unique(dat_trend$year_start), function(x) {

      coordinates <- brace_coordinates[brace_coordinates$year_start == x, ]
      coordiantes <- brace_coordinates[,c("year_start", "year_end", "brace_upper_y", "brace_lower_y")]
      coordinates <- unique(coordinates)

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

    # Get bracelabel for est
    ggtext::geom_richtext(
      data = brace_coordinates[brace_coordinates$TR_BUNDESLAND == bundesland &
        !(brace_coordinates$year_start == 2011 &
          brace_coordinates$year_end == 2016), ],
      mapping = aes(
        x = year_start + (year_end - max(year_start)) / 2,
        y = label_pos,
        label = paste0(estTrend_within_label, sigTrend_vsGermany, " (", round(seTrend_within, 1), ")")
      ),
      size = 3,
      label.padding = grid::unit(rep(0, 4), "pt"),
      fill = NA,
      label.color = NA
    )
  )
}
