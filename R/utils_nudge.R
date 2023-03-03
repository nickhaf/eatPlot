
# Plot_braces -------------------------------------------------------------
calc_brace_coords <- function(dat, coords) {

dat <- dat[, c("grouping_var", "state_var", "year_start", "year_end", "brace_label")]
dat$overlap <- calc_overlap(dat$year_start, dat$year_end)


if(any(dat$overlap == TRUE)){
# Upper and lower brace ---------------------------------------------------

  dat$upper_y <- ifelse(dat$year_start == min(dat$year_start),
                               coords[1],
                               coords[1] - coords[1] * 0.10
  )
  dat$lower_y <- ifelse(dat$year_start == min(dat$year_start),
                               coords[1] - coords[1] * 0.10,
                               coords[1] - coords[1] * 0.15
  )

  dat$label_pos_y <- ifelse(dat$grouping_var == levels(dat$grouping_var)[1],
                             coords[1] - coords[1] * 0.23, # Position upper brace label
                             coords[1] - coords[1] * 0.17  # Position lower brace label
  )
  dat$label_pos_x <- ifelse(dat$year_start == min(dat$year_start),
                             calc_pos_label_x(dat$year_start, dat$year_end, 0.25),
                             calc_pos_label_x(dat$year_start, dat$year_end, 0.5)
  )

  # indent the first brace
  dat$mid <-  ifelse(dat$year_start == min(dat$year_start), 0.25, 0.5)

}else{
  dat$upper_y <- coords[1]

  dat$lower_y <- coords[1] - coords[1] * 0.1

  dat$label_pos_y <- ifelse(dat$grouping_var == levels(dat$grouping_var)[1],
                            coords[1] - coords[1] * 0.16, # Position upper brace label
                            coords[1] - coords[1] * 0.10 # Position lower brace label
  )

  dat$label_pos_x <- calc_pos_label_x(dat$year_start, dat$year_end, 0.5)
  dat$mid <- rep(0.5, nrow(dat))

}

## Long oder wide format argument

dat$trend <- paste0(dat$year_start, dat$year_end)

dat_long <- reshape(
  dat,
  idvar = c("grouping_var", "trend"),
  varying = c("upper_y", "year_end", "lower_y", "year_start"),
  v.names = c("year", "value"),
  direction = "long")

  dat <- unique(dat_long[, c("grouping_var", "state_var", "overlap", "label_pos_y", "label_pos_x", "year", "value", "brace_label", "trend")])
  dat <- rename_column(dat, old = "value", new = "brace_y")

  return(dat)
}



# Plot_points -------------------------------------------------------------
calc_y_nudge <- function(vec, n_groups){
  range_est <- range(vec, na.rm = TRUE)
  nudge_y_val <- (range_est[2]-range_est[1]) * 0.25
  nudge_y_vec <- rep(nudge_y_val, n_groups)
  nudge_y_vec[1] <- nudge_y_vec[1] * -1
  return(nudge_y_vec)
}
