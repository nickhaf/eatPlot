#' Title
#'
#' @param p Plot that should be saved.
#' @param filename Filepath and -name for the new plot file.
#' @param width Width of the new plot file. Defaults to `16`.
#' @param height Height of the new plot file. Defaults to `25.2`
#' @param format Format of the new plot file. Currently, only PDF is needed.
#' @param scaling Proportional width and height changes, so the proportion between them is kept.
#'
#' @return A PDF file.
#' @export
#'
#' @examples #tbd
save_plot <- function(p, filename, width = 16, height = 25.2, format = "PDF", scaling = 1) {
  grDevices::pdf(file = filename, width = width * scaling, height = height * scaling)
  plot(p)
  grDevices::dev.off()
}
