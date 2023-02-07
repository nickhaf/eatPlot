
# Plot_braces -------------------------------------------------------------
calc_brace_coords <- function(data, coords) {
  data$brace_upper_y <- ifelse(data$year_start == min(data$year_start),
                               coords[1],
                               coords[1] - coords[1] * 0.10
  )
  data$brace_lower_y <- ifelse(data$year_start == min(data$year_start),
                               coords[1] - coords[1] * 0.10,
                               coords[1] - coords[1] * 0.15
  )

  data$label_pos_y <- ifelse(data$grouping_var == 1,
                             coords[1] - coords[1] * 0.17, # Position upper brace label
                             coords[1] - coords[1] * 0.23  # Position lower brace label
  )
  data$label_pos_x <- ifelse(data$year_start == min(data$year_start),
                             calc_pos_label_x(data$year_start, data$year_end, 0.25),
                             calc_pos_label_x(data$year_start, data$year_end, 0.5)
  )

  return(data)
}



# Plot_points -------------------------------------------------------------
calc_y_nudge <- function(vec, n_groups){
  range_est <- range(vec, na.rm = TRUE)
  nudge_y_val <- (range_est[2]-range_est[1]) * 0.25
  nudge_y_vec <- rep(nudge_y_val, n_groups)
  nudge_y_vec[1] <- nudge_y_vec[1] * -1
  return(nudge_y_vec)
}
