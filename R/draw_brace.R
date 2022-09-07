draw_brace_small <- function(dat_long, range_est){

# search for correct brace position - smallest est as reference

brace_res <- ggbrace::geom_brace(
    mapping = aes(
      x = c(2016, 2021),
      y = c(range_est[1] -62, range_est[1] - 92
            ),
      label = "my_label \n my_label2"),
    inherit.data = F,
    rotate = 180,
    size = 0.8,
    npoints = 200,
    labelsize = 2) #+
  # ggbrace::geom_brace(
  #   mapping = aes(
  #     x = c(2011, 2021),
  #     y = c(range_est[1] - 60, range_est[1] - 30),
  #     label = "my_label"
  #   ),
  #   mid = 0.25,
  #   inherit.data = F,
  #   rotate = 180,
  #   size = 0.8, npoints = 200) +
    # coord_cartesian(ylim = c(range_est[1] - 30, range_est[2] + 30), clip = "off") + # limits without droping observations
    # theme(plot.margin = unit(c(0, 0, 0.30, 0), units="npc")) +
  #NULL

 #   return(brace_res)
}


draw_brace_large <- function(dat_long, range_est){

  # search for correct brace position - smallest est as reference
  ggbrace::geom_brace(
    mapping = aes(
      x = c(2011, 2021),
      y = c(range_est[1] - 60, range_est[1] - 30),
      label = "my_label"
    ),
    mid = 0.25,
    inherit.data = F,
    rotate = 180,
    size = 0.8, npoints = 200)
}
