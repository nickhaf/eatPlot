gsub_plot_dat <- function(plot_dat, whole_group = "wholeGroup", new_whole_group = "Deutschland") {
  if (inherits(plot_dat, "list")) {
    for (i in names(plot_dat)) {
      plot_dat[[i]][] <- lapply(plot_dat[[i]], function(x) {
        if(is.character(x)){gsub(whole_group, new_whole_group, x)
          }else{x}
      })
    }
  } else if (is.data.frame(plot_dat)) {
    plot_dat[] <- lapply(plot_dat, function(x) {
      if(is.character(x)){gsub(whole_group, new_whole_group, x)
        }else{x}
    })
  } else {
    stop("Please provide a list or a data.frame.")
  }
  return(plot_dat)
}
