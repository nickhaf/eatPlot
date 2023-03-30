
# Plot_braces -------------------------------------------------------------
calc_brace_coords <- function(dat, coords, output_format = c("wide", "long"), nudge_x_axis = 0) {
  output_format <- match.arg(output_format)
  dat <- dat[, c("grouping_var", "state_var", "year_start", "year_end", "brace_label", "trend")]
  dat$overlap <- calc_overlap(dat$year_start, dat$year_end)

  range_coords <- diff(range(coords))
  range_years <- diff(range(c(dat$year_start, dat$year_end), na.rm = TRUE))
  ## Starting point for the highest brace label
  start_label <- 0.05
  gap_label <- 0.08
  brace_width <- 0.1

  if (any(dat$overlap == TRUE)) {
    # Upper and lower brace ---------------------------------------------------

    lower_brace_y_a <- calc_pos(coords[1], range_coords, brace_width)
    lower_brace_y_b <- lower_brace_y_a - range_coords * brace_width
    upper_label_y <- lower_brace_y_b - range_coords * start_label

    # Upper brace coordinates:
    dat$upper_y <- ifelse(dat$year_start == min(dat$year_start),
      coords[1],
      lower_brace_y_a
    )

    # Lower brace coordinates:
    dat$lower_y <- ifelse(dat$year_start == min(dat$year_start),
      lower_brace_y_a,
      lower_brace_y_b
    )

    # Label
    dat$label_pos_y <- calc_brace_label_y(dat, upper_label_y, range_coords, gap_label)

    #dat$label_pos_y <- ifelse(dat$grouping_var != levels(dat$grouping_var)[1],
    #  upper_label_y - range_coords * gap_label, # Position lower brace label
    #  upper_label_y # Position upper brace label
    #)
        dat$label_pos_x <- ifelse(dat$year_start == min(dat$year_start),
      calc_brace_label_x(dat$year_start, dat$year_end, range_total = range_years, brace_indent_pos = 0.25, nudge_x_axis = nudge_x_axis),
      calc_brace_label_x(dat$year_start, dat$year_end, range_total = range_years, brace_indent_pos = 0.5, nudge_x_axis = nudge_x_axis)
    )

    # indent the first brace
    dat$mid <- ifelse(dat$year_start == min(dat$year_start), 0.25, 0.5)
  } else {
    lower_brace_y <- calc_pos(coords[1], range_coords, brace_width)
    upper_label_y <- lower_brace_y - range_coords * start_label

    dat$upper_y <- coords[1]
    dat$lower_y <- lower_brace_y

    #dat$label_pos_y <- ifelse(dat$grouping_var != levels(dat$grouping_var)[1],
    #  upper_label_y - range_coords * gap_label, # Position lower brace label
    #  upper_label_y # Position upper brace label
    #)

    dat$label_pos_x <- calc_brace_label_x(dat$year_start, dat$year_end, range_total = range_years, brace_indent_pos = 0.5, nudge_x_axis = nudge_x_axis)
    dat$label_pos_y <- calc_brace_label_y(dat, upper_label_y, range_coords, gap_label)
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
    dat <- build_column(dat, old = "value", new = "brace_y")
  }

  return(dat)
}

calc_brace_label_x <- function(year_start, year_end, range_total, brace_indent_pos, nudge_x_axis = 0) {
  range_est <- year_end - year_start
  year_start + range_est * brace_indent_pos + (range_total * nudge_x_axis)
}

# Plot_points -------------------------------------------------------------
calc_x_nudge <- function(dat, nudge_x = 0.05){

  range_years <- diff(range(dat$year))
  min_max_trend <- get_min_max(dat)

  dat <- merge(dat, min_max_trend,
                      by = "trend",
                      all.x = TRUE,
                      all.y = FALSE)

  dat$x_coords <- ifelse(dat$year == dat$minimum,
                                yes = dat$year + range_years * nudge_x,
                                no = ifelse(dat$year == dat$maximum,
                                            yes = dat$year - range_years * nudge_x,
                                            no = dat$year)
  )
  return(dat)
}


calc_y_nudge <- function(plot_points_dat, y_range, nudge_param = 0.18) { # nudge parameter increases distance between label and point
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

calc_pos <- function(coords_min, range_coords, width){
  coords_min - (range_coords * width)
}




calc_brace_label_y <- function(dat, upper_label_y, range_coords, gap_label){

  for(i in 1:length(levels(dat$grouping_var))){
    lvl <- levels(dat$grouping_var)[i]
    dat_lvl <- dat[dat$grouping_var == lvl, ]
    dat[dat$grouping_var == lvl, "label_pos_y"] <- upper_label_y - (range_coords * gap_label * (i - 1))
  }
  return(dat$label_pos_y)
}
