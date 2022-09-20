draw_brace <- function(dat_long, range_est, upper_label, lower_label, year_vec){

list(
  # small brace
  ggbrace::geom_brace(
    mapping = aes(
      x = c(year_vec[2], year_vec[3]),
      y = c(range_est[1] -62, range_est[1] - 92)
    ),
    inherit.data = F,
    rotate = 180,
    size = 0.8,
    npoints = 200
    ),
  # large brace
    ggbrace::geom_brace(
      mapping = aes(
        x = c(year_vec[1], year_vec[3]),
        y = c(range_est[1] - 60, range_est[1] - 30)
      ),
      mid = 0.25,
      inherit.data = F,
      rotate = 180,
      size = 0.8,
      npoints = 200),
  # labels small brace
  annotate("text",
           x = year_vec[2] + (year_vec[3] - year_vec[2])/2,
           y = 300,
           label = "-24",
           size = 3,
           fontface = "bold"),
  annotate("text",
           x = year_vec[2] + (year_vec[3] - year_vec[2])/2,
           y = 280,
           label = "-24",
           size = 3),
  # labels large brace
  annotate("text",
           x = year_vec[1] + (year_vec[2] - year_vec[1])/2,
           y = 300,
           size = 3,
           label = "-24",
           fontface = "bold"),
  annotate("text",
           x = year_vec[1] + (year_vec[2] - year_vec[1])/2,
           y = 280,
           size = 3,
           label = "-24")
  )
}


## annotate with the function which calculates label position?
