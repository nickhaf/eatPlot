#' Wrapper for [ggplot2::ggsave()].
#'
#' @param p Plot that should be saved.
#' @param filename Filepath and -name for the new plot file.
#' @param width Width of the new plot file in mm. Defaults to `160`.
#' @param height Height of the new plot file in mm. Defaults to `226.2`
#' @param format Format of the new plot file. Currently, only PDF is needed.
#' @param device Device for saving the plot. Should be `cairo_pdf` for pdfs, so font is displayed correctly.
#' @param ... Other arguments passed on to [ggplot2::ggsave()].
#'
#' @return A PDF file.
#' @export
#'
#' @examples # tbd
save_plot <- function(p, filename, width = 160, height = 226.2, device = cairo_pdf, ...) {
  ggplot2::ggsave(filename,
    p,
    height = height,
    width = width,
    units = "mm",
    device = device,
    ...
  )
}
