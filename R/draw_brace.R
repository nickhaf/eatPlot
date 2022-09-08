draw_brace_small <- function(dat_long, range_est){
  ggbrace::geom_brace(
    mapping = aes(
      x = c(2016, 2021),
      y = c(range_est[1] -62, range_est[1] - 92
            ),
      label = "my_label \n my_label2"),
    inherit.data = F,
    rotate = 180,
    size = 0.8,
    npoints = 200,
    labelsize = 2)
}

draw_brace_large <- function(dat_long, range_est){
  ggbrace::geom_brace(
    mapping = aes(
      x = c(2011, 2021),
      y = c(range_est[1] - 60, range_est[1] - 30),
      label = " \n  my_label \n my_label2"),
    mid = 0.25,
    inherit.data = F,
    rotate = 180,
    size = 0.8, npoints = 200,
    labelsize = 2)
}
