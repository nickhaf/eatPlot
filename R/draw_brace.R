draw_brace <- function(my_plot, dat_long){

# search for correct brace position

brace_res <- my_plot +
  ggbrace::geom_brace(
    mapping = aes(
      x = c(2016, 2021),
      y = c(min(dat_long$est) - 15, min(dat_long$est) - 12
            ),
      label = "my_label \n my_label2"),
    inherit.data = F,
    rotate = 180,
    size = 0.8,
    npoints = 200,
    labelsize = 3) +
  ggbrace::geom_brace(
    mapping = aes(
      x = c(2011, 2021),
      y = c(min(dat_long$est) - 10, min(dat_long$est) - 7),
      label = "my_label"
    ),
    mid = 0.25,
    inherit.data = F,
    rotate = 180,
    size = 0.8, npoints = 200) +

    coord_cartesian(ylim=range(dat_long$est), clip = "off") +
    theme(plot.margin = unit(c(0, 0, 0.40, 0), units="npc")) +
  NULL

    return(brace_res)
}
