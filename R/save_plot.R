#' Title
#'
#' @param p Plot that should be saved.
#' @param filename Filepath and -name for the new plot file.
#' @param width Width of the new plot file in mm. Defaults to `160`.
#' @param height Height of the new plot file in mm. Defaults to `226.2`
#' @param format Format of the new plot file. Currently, only PDF is needed.
#' @param scaling Proportional width and height changes, so the proportion between them is kept.
#'
#' @return A PDF file.
#' @export
#'
#' @examples # tbd
save_plot <- function(p, filename, width = 160, height = 226.2, format = "PDF", scaling = 1) {
  width_inch <- width / 25.4
  height_inch <- height / 25.4

  grDevices::pdf(file = filename, width = width_inch * scaling, height = height_inch * scaling)
  plot(p)
  grDevices::dev.off()
}
