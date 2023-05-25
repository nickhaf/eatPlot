#' Wrapper for ggsave.
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
save_plot <- function(p, filename, width = 160, height = 226.2, scaling = 1) {
  ggplot2::ggsave(filename,
    p,
    height = height * scaling,
    width = width * scaling,
    units = "mm",
    device = cairo_pdf
  )
}
