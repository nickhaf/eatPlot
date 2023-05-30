#' Wrapper for [grDevices::pdf()].
#'
#' @param p Plot that should be saved.
#' @param filename Filepath and -name for the new plot file.
#' @param width Width of the new plot file in mm. Defaults to `160`.
#' @param height Height of the new plot file in mm. Defaults to `226.2`
#' @param colormodel Colormodel for saving the plot. See [grDevices::pdf()] for more information. Defaults to "cmyk".
#' @param scaling Scaling parameter for changing height in widht relationally.
#' @param ... Further arguments passed to [grDevices::pdf()].
#'
#' @return A PDF file.
#' @export
#'
#' @examples # tbd

## Cario.pdf device for correct dash representation:
# save_plot <- function(p, filename, width = 160, height = 226.2, device = grDevices::cairo_pdf, ...) {
#   ggplot2::ggsave(filename,
#     p,
#     height = height,
#     width = width,
#     units = "mm",
#     device = device,
#     colormodel = "cmyk"
#   )
# }

## pdf for the correct colomodel cmyk:
save_plot <- function(p, filename, width = 160, height = 226.2, colormodel = "cmyk", scaling = 1, ...) {
  width_inch <- width / 25.4
  height_inch <- height / 25.4

  grDevices::pdf(file = filename,
                 width = width_inch * scaling,
                 height = height_inch * scaling,
                 colormodel = colormodel,
                 ...
                 )
  plot(p)
  grDevices::dev.off()
}
