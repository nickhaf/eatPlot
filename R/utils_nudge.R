calc_plot_lims_y <- function(dat, coords, plot_settings) {
  overlap <- calc_overlap(dat$year_start_axis, dat$year_end_axis)

  range_coords <- diff(range(coords))
  range_years <- diff(range(c(dat$year_start_axis, dat$year_end_axis), na.rm = TRUE))

  brace_label_nudge_y <- plot_settings$brace_label_nudge_y
  starting_points <- calc_brace_starting_points(
    overlap = overlap,
    coords,
    range_coords,
    brace_label_nudge_y,
    plot_settings
  )

  dat <- calc_brace_position(dat, overlap)

  dat <- calc_brace_indent(dat)

  y_label <- calc_brace_label_coords(
    dat,
    starting_points,
    range_coords,
    range_years,
    plot_settings
  )$label_pos_y

  y_lims_total <- c(min(y_label) - diff(range(coords)) * 0.06, max(coords)) # a bit smaller, so the labels don't get cut off

  return(y_lims_total)
}

# Plot_braces -------------------------------------------------------------
#' Calculate the coordinates for the braces and their labels.
#'
#' @keywords internal
#' @noRd
#'
#' @inheritParams plot_lineplot()
#'
#' @param dat Data.frame, normally `plot_dat$plot_braces`.
#' @param coords Numeric vector with minimum and maximum value of the y-range between brace and upper x-axis. Can be calculated by [calc_y_value_coords()].
#'
#' @return Data.frame containing the coordinates for plotting braces and their labels.
#'
#' @examples # tbd
calc_brace_coords <- function(dat, coords, plot_settings = plotsettings_lineplot()) {



  ## next step: clean this up!

  years_braces <- plot_settings_expanded$years_list$years_braces

  ## Calculate if any braces overlap. If that's the case, they need to be plotted below each other.
  overlap <- calc_overlap(
    df = years_braces
  )

  starting_points <- calc_brace_starting_points(
    overlap = overlap,
    coords = coords,
    plot_settings = plot_settings
  )

  ## This gives hints on where the brace will be positioned.
  brace_positions <- calc_brace_position(plot_settings, overlap)

  ## Calculate the y coordinates for the braces:
  brace_positions_2 <- calc_brace_coords_y(
    brace_positions,
    coords = coords,
    starting_points = starting_points)

  ## Why not doing this directly, instead of middle, left, top ...?
  brace_positions_3 <- calc_brace_indent(brace_positions_2)

  ## Calculate the coordinates for the brace labels:
  dat <- calc_brace_label_coords(
    brace_positions_3,
    starting_points = starting_points,
    range_coords = range_coords,
    plot_settings = plot_settings
  )


  ## PUt the above stuff together in a better way!
  ## Merge starting points and brace_positions_3 ...


  # Change Format if needed -------------------------------------------------
  # if (plot_settings$split_plot == TRUE) {
  #   ## Long oder wide format argument
  #   dat_long <- stats::reshape(
  #     dat,
  #     idvar = c("grouping_var", "years_Trend", "competence_var"),
  #     varying = c("upper_y", "year_end_axis", "lower_y", "year_start_axis"),
  #     v.names = c("year_axis", "value"),
  #     direction = "long"
  #   )
  #
  #   dat <- unique(dat_long[, c("grouping_var", "state_var", "label_pos_y", "label_pos_x", "year_axis", "value", "brace_label", "years_Trend")])
  #   dat$brace_y <- dat$value
  #   dat <- build_column(dat, old = "value", new = "brace_y")
  # }

  return(dat)
}




calc_brace_coords_y <- function(dat, coords, starting_points) {
  # if ("lower_brace_y_a" %in% names(starting_points)) { # in this case we have an overlap of braces
  #   ## larger brace on top, smaller one below. If equal, just put the one with the smallest start year on top:
  #
  #   dat$upper_y <- ifelse(dat$brace_position_y == "top",
  #     yes = coords[1],
  #     no = starting_points$lower_brace_y_a
  #   )
  #
  #   # Lower brace coordinates:
  #   dat$lower_y <- ifelse(dat$brace_position_y == "top",
  #     starting_points$lower_brace_y_a,
  #     starting_points$lower_brace_y_b
  #   )
  #} else {
    dat$upper_y <- coords[1]
    dat$lower_y <- starting_points$lower_brace_y
 # }
  return(dat)
}


# Plot_points -------------------------------------------------------------
calc_x_nudge <- function(dat, nudge_x, split_plot) {
  range_years <- diff(range(dat$year_axis))
  min_max_trend <- get_min_max(dat)

  dat <- merge(dat, min_max_trend,
    by = "years_Trend",
    all.x = TRUE,
    all.y = FALSE
  )

  if (split_plot == TRUE) {
    dat$x_coords <- ifelse(dat$year == dat$minimum,
      yes = dat$year_axis + range_years * nudge_x,
      no = ifelse(dat$year == dat$maximum,
        yes = dat$year_axis - range_years * nudge_x,
        no = dat$year_axis
      )
    )
  } else {
    dat$x_coords <- ifelse(dat$year == min(dat$year, na.rm = TRUE),
      yes = dat$year_axis + range_years * nudge_x,
      no = ifelse(dat$year == max(dat$year, na.rm = TRUE),
        yes = dat$year_axis - range_years * nudge_x,
        no = dat$year_axis
      )
    )
  }
  return(dat)
}



calc_y_nudge <- function(plot_points_dat,
                         plot_lims,
                         plot_settings = plotsettings_lineplot()) {
  nudge_val <- plot_lims$coords_diff * plot_settings$point_label_nudge_y

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





nudge_by_level <- function(df, plot_settings, nudge_val) {
  for (i in levels(df$grouping_var)) {
    df[df$grouping_var == i, "nudge_y"] <- nudge_val * as.numeric(paste0(plot_settings$point_label_nudge_direction[[i]], "1"))
  }
  return(df)
}

