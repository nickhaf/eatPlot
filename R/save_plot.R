#' Title
#'
#' @param p Plot that should be saved.
#' @param filename Filepath and -name for the new plot file.
#' @param width Width of the new plot file. Defaults to `16`.
#' @param height Height of the new plot file. Defaults to `25.2`
#' @param format Format of the new plot file. Currently, only PDF is needed.
#'
#' @return
#' @export
#'
#' @examples
save_plot <- function(p, filename, width = 16, height = 25.2, format = "PDF") {
  pdf(file = filename, width = width, height = height)
  plot(p)
  dev.off()
}
