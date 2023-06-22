# Plot_braces -------------------------------------------------------------
calc_brace_coords <- function(dat, coords, output_format = c("wide", "long"), plot_settings = plotsettings_lineplot()) {
  output_format <- match.arg(output_format)
  sapply(c("grouping_var", "competence_var", "state_var", "year_start_axis", "year_end_axis", "brace_label", "years_Trend"), check_column_warn, dat = dat)
  dat <- dat[, c("grouping_var", "competence_var", "state_var", "year_start_axis", "year_end_axis", "brace_label", "years_Trend")]
  dat$overlap <- calc_overlap(dat$year_start_axis, dat$year_end_axis)

  range_coords <- diff(range(coords))
  range_years <- diff(range(c(dat$year_start_axis, dat$year_end_axis), na.rm = TRUE))
  ## Starting point for the highest brace label
  start_label <- plot_settings$brace_label_nudge_y

  if (any(dat$overlap == TRUE)) {
    # Upper and lower brace ---------------------------------------------------

    lower_brace_y_a <- calc_pos(coords[1], range_coords, plot_settings$brace_span_y)
    lower_brace_y_b <- lower_brace_y_a - range_coords * plot_settings$brace_span_y
    upper_label_y <- lower_brace_y_b - range_coords * start_label

    # Upper brace coordinates:
    dat$upper_y <- ifelse(dat$year_start_axis == min(dat$year_start_axis),
      coords[1],
      lower_brace_y_a
    )

    # Lower brace coordinates:
    dat$lower_y <- ifelse(dat$year_start_axis == min(dat$year_start_axis),
      lower_brace_y_a,
      lower_brace_y_b
    )

    # Label
    dat$label_pos_y <- calc_brace_label_y(dat, upper_label_y, range_coords, gap_label = plot_settings$brace_label_gap_y)

    dat$label_pos_x <- ifelse(dat$year_start_axis == min(dat$year_start_axis),
      calc_brace_label_x(dat$year_start_axis,
        dat$year_end_axis,
        range_total = range_years,
        brace_indent_pos = 0.25,
        brace_label_nudge_x = plot_settings$brace_label_nudge_x
      ),
      calc_brace_label_x(dat$year_start_axis,
        dat$year_end_axis,
        range_total = range_years,
        brace_indent_pos = 0.5,
        brace_label_nudge_x = plot_settings$brace_label_nudge_x
      )
    )

    # indent the first brace
    dat$mid <- ifelse(dat$year_start_axis == min(dat$year_start_axis), 0.25, 0.5)
  } else {
    lower_brace_y <- calc_pos(coords[1], range_coords, plot_settings$brace_span_y)
    upper_label_y <- lower_brace_y - range_coords * start_label

    dat$upper_y <- coords[1]
    dat$lower_y <- lower_brace_y

    dat$label_pos_x <- calc_brace_label_x(dat$year_start_axis,
      dat$year_end_axis,
      range_total = range_years,
      brace_indent_pos = 0.5,
      brace_label_nudge_x = plot_settings$brace_label_nudge_x
    )
    dat$label_pos_y <- calc_brace_label_y(dat,
      upper_label_y,
      range_coords,
      gap_label = plot_settings$brace_label_gap_y
    )
    dat$mid <- rep(0.5, nrow(dat))
  }

  if (output_format == "long") {
    ## Long oder wide format argument

    dat_long <- stats::reshape(
      dat,
      idvar = c("grouping_var", "years_Trend", "competence_var"),
      varying = c("upper_y", "year_end_axis", "lower_y", "year_start_axis"),
      v.names = c("year_axis", "value"),
      direction = "long"
    )

    dat <- unique(dat_long[, c("grouping_var", "state_var", "overlap", "label_pos_y", "label_pos_x", "year_axis", "value", "brace_label", "years_Trend")])
    dat$brace_y <- dat$value
    dat <- build_column(dat, old = "value", new = "brace_y")
  }

  return(dat)
}

calc_brace_label_x <- function(year_start_axis,
                               year_end_axis,
                               range_total,
                               brace_indent_pos,
                               brace_label_nudge_x = 0) {
  range_est <- year_end_axis - year_start_axis
  year_start_axis + range_est * brace_indent_pos + (range_total * brace_label_nudge_x)
}

# Plot_points -------------------------------------------------------------
calc_x_nudge <- function(dat, nudge_x) {
  range_years <- diff(range(dat$year_axis))
  min_max_trend <- get_min_max(dat)

  dat <- merge(dat, min_max_trend,
    by = "years_Trend",
    all.x = TRUE,
    all.y = FALSE
  )

  dat$x_coords <- ifelse(dat$year_axis == dat$minimum,
    yes = dat$year_axis + range_years * nudge_x,
    no = ifelse(dat$year_axis == dat$maximum,
      yes = dat$year_axis - range_years * nudge_x,
      no = dat$year_axis
    )
  )
  return(dat)
}


calc_y_nudge <- function(plot_points_dat, y_range, plot_settings = plotsettings_lineplot()) {
  range_est <- diff(y_range)
  nudge_val <- range_est * plot_settings$point_label_nudge_y

  # The smallest value in each year_axis is nudged lower, the bigger ones are nudged higher. For facetted plots, the trend has to be taken into account as well.

  ## Wenn in plot_settings ein named vector angegeben wurde mit "+" oder "-", dann das nutzen, sonst versuchen selber zu berechnen:

  if (!is.null(plot_settings$point_label_nudge_direction)) {
    ## Checks
    stopifnot(all(names(plot_settings$point_label_nudge_direction) %in% levels(plot_points_dat$grouping_var)))


    res_frame_1 <- data.frame(
      years_Trend = plot_points_dat$years_Trend,
      year_axis = plot_points_dat$year_axis,
      grouping_var = plot_points_dat$grouping_var
    )

    res_frame <- nudge_by_level(res_frame_1, plot_settings = plot_settings, nudge_val = nudge_val)
  } else {
    nudge_neg <- by(plot_points_dat, list(plot_points_dat$year_axis, plot_points_dat$years_Trend), function(year_df) {
      res_frame <- data.frame(
        nudge_y = ifelse(
          year_df$point_values == min(year_df$point_values) & length(year_df$point_values) > 1,
          nudge_val * -1,
          nudge_val
        ),
        years_Trend = year_df$years_Trend,
        year_axis = year_df$year_axis,
        grouping_var = year_df$grouping_var
      )
      ## If duplicated estimate values exist:
      dup_nudge <- duplicated(year_df$point_values == min(year_df$point_values))
      res_frame[dup_nudge, "nudge_y"] <- res_frame[dup_nudge, "nudge_y"] * -1
      return(res_frame)
    })

    res_frame <- data.frame(do.call("rbind", nudge_neg))
  }

  out <- merge(plot_points_dat, res_frame,
    by = c("years_Trend", "year_axis", "grouping_var"),
    all.x = TRUE, all.y = FALSE
  )
  return(out)
}

calc_pos <- function(coords_min, range_coords, width) {
  coords_min - (range_coords * width)
}




calc_brace_label_y <- function(dat, upper_label_y, range_coords, gap_label) {
  for (i in unique(dat$years_Trend)) {
    dat_trend <- dat[dat$years_Trend == i, ]
    dat_trend$grouping_var <- droplevels(dat_trend$grouping_var)
    for (j in seq_along(levels(dat_trend$grouping_var))) {
      lvl <- levels(dat_trend$grouping_var)[j]
      dat_lvl <- dat[dat$years_Trend == i & dat$grouping_var == lvl, ]
      dat[dat$years_Trend == i & dat$grouping_var == lvl, "label_pos_y"] <- upper_label_y - (range_coords * gap_label * (j - 1))
    }
  }
  return(dat$label_pos_y)
}


nudge_by_level <- function(df, plot_settings, nudge_val) {
  for (i in levels(df$grouping_var)) {
    df[df$grouping_var == i, "nudge_y"] <- nudge_val * as.numeric(paste0(plot_settings$point_label_nudge_direction[[i]], "1"))
  }
  return(df)
}
