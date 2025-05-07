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
calc_brace_coords <- function(grouping_var_lvls, coords, years_list, plot_settings = plotsettings_lineplot()) {
  if (nrow(years_list$years_braces) == 0) {
    return(list(coord_dat = data.frame(), group_labels = data.frame(label_pos_y = 0)))
  }
  years_braces <- years_list$years_braces


  ## Calculate the y coordinates for the braces and labels:
  ## Check if any braces overlap. If that's the case, they have to plotted below each other
  overlap <- calc_overlap(
    year_start = years_braces$year_start,
    year_end = years_braces$year_end
  )


  # calc starting points for braces -----------------------------------------
  ## Braces should start some distance below the lower x-axis. Labels below lowest brace
  range_coords <- diff(range(coords))
  if (overlap) {
    lower_brace_y_a <- coords[1] - (range_coords * plot_settings$brace_span_y)
    lower_brace_y_b <- lower_brace_y_a - range_coords * plot_settings$brace_span_y
    upper_label_y <- lower_brace_y_b - range_coords * plot_settings$brace_label_nudge_y

    starting_points <- list(
      "lower_brace_y_a" = lower_brace_y_a,
      "lower_brace_y_b" = lower_brace_y_b,
      "upper_label_y" = upper_label_y
    )
  } else {
    lower_brace_y <- coords[1] - (range_coords * plot_settings$brace_span_y)
    upper_label_y <- lower_brace_y - range_coords * plot_settings$brace_label_nudge_y

    starting_points <- list(
      "lower_brace_y" = lower_brace_y,
      "upper_label_y" = upper_label_y
    )
  }


  ## For each brace, save where it should be indented, and where it's position relative to the other braces is
  brace_positions <- calc_brace_position(years_list, overlap)

  if ("lower_brace_y_a" %in% names(starting_points)) { # in this case we have an overlap of braces
    ## larger brace on top, smaller one below. If equal, just put the one with the smallest start year on top:
    brace_positions$upper_y <- ifelse(brace_positions$brace_position_y == "top",
                                      yes = coords[1],
                                      no = starting_points$lower_brace_y_a
    )

    # Lower brace coordinates:
    brace_positions$lower_y <- ifelse(brace_positions$brace_position_y == "top",
                                      starting_points$lower_brace_y_a,
                                      starting_points$lower_brace_y_b
    )
  } else {
    brace_positions$upper_y <- coords[1]
    brace_positions$lower_y <- starting_points$lower_brace_y
  }


  range_coords <- diff(range(coords)) ## Take from above!

  # calc brace label y ------------------------------------------------------
  if(any(!is.na(brace_positions$range))){
    max_range <- max(brace_positions$range, na.rm = TRUE)
  }else{
    max_range <- NA
  }

  brace_positions$label_pos_x <- brace_positions$year_start + brace_positions$range * brace_positions$brace_position_x + (max_range * plot_settings$brace_label_nudge_x)

label_pos_y <- sapply(seq_along(grouping_var_lvls), function(x) {
    starting_points$upper_label_y - (range_coords * plot_settings$brace_label_gap_y * (x - 1))
  })

  df <- data.frame(
    grouping_lvls = grouping_var_lvls,
    label_pos_y = label_pos_y
  )


  return(list(coord_dat = brace_positions, group_labels = df))
}



calc_brace_position <- function(years_list, overlap) {
  ## Don't do this stuff df wise, only once for all year combinations.
  ## I only need one coordinate for each possible brace, that's it!
  years_braces_df <- years_list$years_braces
  ## Calculate the range between all year combinations:

  years_braces_df$range <- years_braces_df$year_end - years_braces_df$year_start


  if (overlap == TRUE) {
    if (all(years_braces_df$range == years_braces_df$range[1])) { ## All braces have the same range

      ## The first brace will get an indention to the left, the second one none
      years_braces_df$brace_position_x <- ifelse(years_braces_df$year_start == min(years_braces_df$year_start),
                                                 yes = "left",
                                                 no = "middle"
      )

      ## The first brace will get plotted on top, the second one on the bottom:
      years_braces_df$brace_position_y <- ifelse(years_braces_df$year_start == min(years_braces_df$year_start),
                                                 yes = "top",
                                                 no = "bottom"
      )
    } else {
      # The smaller brace won't get any indention.

      years_braces_df$brace_position_x <- ifelse(years_braces_df$range == max(years_braces_df$range),
                                                 yes = ifelse(years_braces_df$year_start == min(years_braces_df$year_start),
                                                              yes = "left",
                                                              no = "right"
                                                 ),
                                                 no = "middle"
      )

      # The larger brace will be plotted on top
      years_braces_df$brace_position_y <- ifelse(years_braces_df$range == max(years_braces_df$range),
                                                 yes = "top",
                                                 no = "bottom"
      )

      # If any bottom brace starts at the first year, the upper needs to go right

      middle_bottom <- ifelse(years_braces_df$year_start == min(years_braces_df$year_start) & years_braces_df$brace_position_y == "bottom",
                              yes = TRUE,
                              no = FALSE
      )

      if (any(middle_bottom)) {
        years_braces_df[years_braces_df$brace_position_y == "top", "brace_position_x"] <- "right"
      }
    }
  } else {
    years_braces_df$brace_position_x <- "middle"
    years_braces_df$brace_position_y <- rep("middle", nrow(years_braces_df))
  }

  years_braces_df$brace_position_x <- ifelse(years_braces_df$brace_position_x == "left",
                                             0.25,
                                             ifelse(years_braces_df$brace_position_x == "right",
                                                    0.75,
                                                    0.5
                                             )
  )


  return(years_braces_df)
}



# Plot_points -------------------------------------------------------------
calc_x_nudge <- function(dat, nudge_x, plot_settings) {
  x_value_range <- diff(dat$plot_lims$x_value_range)
  min_max_trend <- get_min_max(dat$dat_final)

  dat <- merge(dat$dat_final, min_max_trend,
               by = "trend",
               all.x = TRUE,
               all.y = FALSE
  )

  if (plot_settings$split_plot) {
    dat$x_coords <- ifelse(dat$year == dat$minimum,
                           yes = dat$year_axis + x_value_range * nudge_x,
                           no = ifelse(dat$year == dat$maximum,
                                       yes = dat$year_axis - x_value_range * nudge_x,
                                       no = dat$year_axis
                           )
    )
  } else {
    dat$x_coords <- ifelse(dat$year == min(dat$year, na.rm = TRUE),
                           yes = dat$year + x_value_range * nudge_x,
                           no = ifelse(dat$year == max(dat$year, na.rm = TRUE),
                                       yes = dat$year - x_value_range  * nudge_x,
                                       no = dat$year
                           )
    )
  }
  return(dat)
}


nudge_x_axis_labels <- function(dat, plot_settings) {
  range_years <- diff(range(dat$year))
  min_max_trend <- get_min_max(dat)

  dat <- merge(dat, min_max_trend,
               by = "trend",
               all.x = TRUE,
               all.y = FALSE
  )

  if (plot_settings$split_plot) {
    dat$x_coords <- ifelse(dat$year == dat$minimum,
                           yes = dat$year_axis + range_years * plot_settings$axis_x_label_centralize,
                           no = ifelse(dat$year == dat$maximum,
                                       yes = dat$year_axis - range_years * plot_settings$axis_x_label_centralize,
                                       no = dat$year_axis
                           )
    )
  } else {
    dat$x_coords <- ifelse(dat$year == min(dat$year, na.rm = TRUE),
                           yes = dat$year + range_years * plot_settings$axis_x_label_centralize,
                           no = ifelse(dat$year == max(dat$year, na.rm = TRUE),
                                       yes = dat$year - range_years * plot_settings$axis_x_label_centralize,
                                       no = dat$year
                           )
    )
  }

  return(dat)
}

calc_y_nudge <- function(plot_dat, plot_settings){
  nudge_val <- plot_dat$plot_lims$y_value_space_diff * plot_settings$point_label_nudge_y

  if (!is.null(plot_settings$point_label_nudge_direction)) {
    ## Checks
    stopifnot(all(names(plot_settings$point_label_nudge_direction) %in% levels(plot_dat$plot_dat$subgroup_var)))

    res_frame <- nudge_by_level(plot_dat$plot_dat, plot_settings = plot_settings, nudge_val = nudge_val)


  }else{

    res_frame <- within(plot_dat$plot_dat, {
      nudge_y <- ifelse(point_est == ave(point_est, year, FUN = min) & length(levels(subgroup_var)) > 1,
                        -nudge_val,
                        nudge_val
       )
})
  }
  return(res_frame)
}





nudge_by_level <- function(df, plot_settings, nudge_val) {
  for (i in levels(df$subgroup_var)) {
    df[df$subgroup_var == i, "nudge_y"] <- nudge_val * as.numeric(paste0(plot_settings$point_label_nudge_direction[[i]], "1"))
  }
  return(df)
}
