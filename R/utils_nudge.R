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
  # Checks ------------------------------------------------------------------
  sapply(c("grouping_var", "competence_var", "state_var", "year_start_axis", "year_end_axis", "brace_label", "years_Trend"),
    check_column_warn,
    dat = dat
  )

  # Prepare dat -------------------------------------------------------------
  dat <- dat[, c("grouping_var", "competence_var", "state_var", "year_start_axis", "year_end_axis", "brace_label", "years_Trend")]

  ## Calculate if any braces overlap. If that's the case, they need to be plotted under each other.
  ## Do I need the concrete overlap information, or is an any() enough here?
  overlap <- calc_overlap(
    year_start = dat$year_start_axis,
    year_end = dat$year_end_axis
  )

  range_coords <- diff(range(coords))

  ## Can be removed, correct?
  range_years <- diff(
    range(
      c(
        dat$year_start_axis,
        dat$year_end_axis
      ),
      na.rm = TRUE
    )
  )

  dat <- calc_brace_position(dat, overlap)

  # Actual coordinates calculation ------------------------------------------

  ## Calculate the bottom of the plot, from wich on the braces will be drawn:
  starting_points <- calc_brace_starting_points(
    overlap = overlap,
    coords = coords,
    range_coords = range_coords,
    brace_label_nudge_y = plot_settings$brace_label_nudge_y,
    plot_settings = plot_settings
  )

  ## Calculate the y coordinates for the braces:
  dat <- calc_brace_coords_y(dat,
    coords = coords,
    starting_points = starting_points
  )

  ## Calculate the indetion of the braces.
  dat <- calc_brace_indent(dat)

  ## Calculate the coordinates for the brace labels:
  dat <- calc_brace_label_coords(dat,
    starting_points = starting_points,
    range_coords = range_coords,
    range_years = range_years,
    plot_settings = plot_settings
  )



  # Change Format if needed -------------------------------------------------
  if (plot_settings$split_plot == TRUE) {
    ## Long oder wide format argument
    dat_long <- stats::reshape(
      dat,
      idvar = c("grouping_var", "years_Trend", "competence_var"),
      varying = c("upper_y", "year_end_axis", "lower_y", "year_start_axis"),
      v.names = c("year_axis", "value"),
      direction = "long"
    )

    dat <- unique(dat_long[, c("grouping_var", "state_var", "label_pos_y", "label_pos_x", "year_axis", "value", "brace_label", "years_Trend")])
    dat$brace_y <- dat$value
    dat <- build_column(dat, old = "value", new = "brace_y")
  }

  return(dat)
}


calc_brace_starting_points <- function(overlap, coords, range_coords, brace_label_nudge_y, plot_settings) {
  if (overlap == TRUE) {
    lower_brace_y_a <- calc_pos(coords[1], range_coords, plot_settings$brace_span_y)
    lower_brace_y_b <- lower_brace_y_a - range_coords * plot_settings$brace_span_y
    upper_label_y <- lower_brace_y_b - range_coords * brace_label_nudge_y

    res_list <- list(
      "lower_brace_y_a" = lower_brace_y_a,
      "lower_brace_y_b" = lower_brace_y_b,
      "upper_label_y" = upper_label_y
    )
  } else {
    lower_brace_y <- calc_pos(coords[1], range_coords, plot_settings$brace_span_y)
    upper_label_y <- lower_brace_y - range_coords * brace_label_nudge_y

    res_list <- list(
      "lower_brace_y" = lower_brace_y,
      "upper_label_y" = upper_label_y
    )
  }
  return(res_list)
}


calc_brace_coords_y <- function(dat, coords, starting_points) {
  if ("lower_brace_y_a" %in% names(starting_points)) { # in this case we have an overlap of braces
    ## larger brace on top, smaller one below. If equal, just put the one with the smallest start year on top:

    dat$upper_y <- ifelse(dat$brace_position_y == "top",
      yes = coords[1],
      no = starting_points$lower_brace_y_a
    )

    # Lower brace coordinates:
    dat$lower_y <- ifelse(dat$brace_position_y == "top",
      starting_points$lower_brace_y_a,
      starting_points$lower_brace_y_b
    )
  } else {
    dat$upper_y <- coords[1]
    dat$lower_y <- starting_points$lower_brace_y
  }
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

calc_brace_indent <- function(dat) {
  dat$mid <- ifelse(dat$brace_position_x == "left",
    yes = 0.25,
    no = ifelse(dat$brace_position_x == "right",
      yes = 0.75,
      no = 0.5
    )
  )
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

calc_pos <- function(coords_min, range_coords, width) {
  coords_min - (range_coords * width)
}


calc_brace_label_coords <- function(dat, starting_points, range_coords, range_years, plot_settings) {

  dat$label_pos_x <- calc_brace_label_x(dat, range_total = range_years, plot_settings)

  dat$label_pos_y <- calc_brace_label_y(dat,
    starting_points$upper_label_y,
    range_coords,
    gap_label = plot_settings$brace_label_gap_y
  )

  return(dat)
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

calc_brace_position <- function(dat, overlap) {
  ## Calculate the range between all year combinations:
  dat$range <- apply(dat[, c("year_start_axis", "year_end_axis")], 1, function(x) {
    diff(range(x))
  })

  if (overlap == TRUE) {
    if (all(dat$range == dat$range[1])) { ## All braces have the same range

      ## The first brace will get an indention to the left, the second one none
      dat$brace_position_x <- ifelse(dat$year_start_axis == min(dat$year_start_axis),
        yes = "left",
        no = "middle"
      )

      ## The first brace will get plotted on top, the second one on the bottom:
      dat$brace_position_y <- ifelse(dat$year_start_axis == min(dat$year_start_axis),
        yes = "top",
        no = "bottom"
      )
    } else {
      # The smaller brace won't get any indention.
      dat$brace_position_x <- ifelse(dat$range == max(dat$range),
        yes = ifelse(dat$year_start_axis == min(dat$year_start_axis),
          yes = "left",
          no = "right"
        ),
        no = "middle"
      )

      # The larger brace will be plotted on top
      dat$brace_position_y <- ifelse(dat$range == max(dat$range),
        yes = "top",
        no = "bottom"
      )
    }
  } else {
    dat$brace_position_x <- rep("middle", nrow(dat))
    dat$brace_position_y <- rep("middle", nrow(dat))
  }

  return(dat)
}


calc_brace_label_x <- function(dat,
                               range_total,
                               plot_settings){
  label_x <- dat$year_start_axis + dat$range * dat$mid + (range_total * plot_settings$brace_label_nudge_x) ## Position ist evtl noch was anderes, z.B. * 0 oder so
  return(label_x)
}

