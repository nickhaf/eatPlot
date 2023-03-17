
# Plot_braces -------------------------------------------------------------
calc_brace_coords <- function(dat, coords, output_format = c("wide", "long")) {
  output_format <- match.arg(output_format)
  dat <- dat[, c("grouping_var", "state_var", "year_start", "year_end", "brace_label", "trend")]
  dat$overlap <- calc_overlap(dat$year_start, dat$year_end)

  brace_label_y <- 0.14

  if (any(dat$overlap == TRUE)) {
    # Upper and lower brace ---------------------------------------------------

    # Upper brace coordinates:
    dat$upper_y <- ifelse(dat$year_start == min(dat$year_start),
      coords[1],
      coords[1] - coords[1] * 0.10
    )

    # Lower brace coordinates:
    dat$lower_y <- ifelse(dat$year_start == min(dat$year_start),
      coords[1] - coords[1] * 0.10,
      coords[1] - coords[1] * 0.15
    )

    # Label
    dat$label_pos_y <- ifelse(dat$grouping_var == levels(dat$grouping_var)[1],
      coords[1] - coords[1] * (brace_label_y + 0.12), # Position lower brace label
      coords[1] - coords[1] * (brace_label_y + 0.07) # Position upper brace label
    )
    dat$label_pos_x <- ifelse(dat$year_start == min(dat$year_start),
      calc_pos_label_x(dat$year_start, dat$year_end, 0.25),
      calc_pos_label_x(dat$year_start, dat$year_end, 0.5)
    )

    # indent the first brace
    dat$mid <- ifelse(dat$year_start == min(dat$year_start), 0.25, 0.5)
  } else {
    dat$upper_y <- coords[1]

    dat$lower_y <- coords[1] - coords[1] * 0.1

    dat$label_pos_y <- ifelse(dat$grouping_var == levels(dat$grouping_var)[1],
      coords[1] - coords[1] * (brace_label_y + 0.05), # Position lower brace label
      coords[1] - coords[1] * brace_label_y # Position upper brace label
    )

    dat$label_pos_x <- calc_pos_label_x(dat$year_start, dat$year_end, 0.5)
    dat$mid <- rep(0.5, nrow(dat))
  }

  if (output_format == "long") {
    ## Long oder wide format argument

    dat_long <- stats::reshape(
      dat,
      idvar = c("grouping_var", "trend"),
      varying = c("upper_y", "year_end", "lower_y", "year_start"),
      v.names = c("year", "value"),
      direction = "long"
    )

    dat <- unique(dat_long[, c("grouping_var", "state_var", "overlap", "label_pos_y", "label_pos_x", "year", "value", "brace_label", "trend")])
    dat <- rename_column(dat, old = "value", new = "brace_y")
  }

  return(dat)
}

calc_pos_label_x <- function(year_start, year_end, brace_indent_pos) {
  year_start + (year_end - year_start) * brace_indent_pos
}

# Plot_points -------------------------------------------------------------
calc_y_nudge <- function(plot_points_dat, y_range, nudge_param = 0.1) {
  range_est <- diff(y_range)
  nudge_val <- range_est * nudge_param

  # The smallest value in each year is nudged lower, the bigger ones are nudged higher. For facetted plots, the trend has to be taken into account as well.
  nudge_neg <- by(plot_points_dat, list(plot_points_dat$year, plot_points_dat$trend), function(year_df) {
    res_frame <- data.frame(
      nudge_y = ifelse(
        year_df$est_point == min(year_df$est_point) & length(year_df$est_point) > 1,
        nudge_val * -1,
        nudge_val
      ),
      trend = year_df$trend,
      year = year_df$year,
      grouping_var = year_df$grouping_var
    )
    ## If duplicated estimate values exist:
    dup_nudge <- duplicated(year_df$est_point == min(year_df$est_point))
    res_frame[dup_nudge, "nudge_y"] <- res_frame[dup_nudge, "nudge_y"] * -1
    return(res_frame)
  })

  res_frame <- data.frame(do.call("rbind", nudge_neg))

  out <- merge(plot_points_dat, res_frame,
    by = c("trend", "year", "grouping_var"),
    all.x = TRUE, all.y = FALSE
  )
  return(out)
}
