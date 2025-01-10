#' Filter specified rows from plot_dat objects.
#'
#' @param plot_dat A list of data.frames prepared by [prep_lineplot()].
#' @param filter_statement Character string containing a logical expression for filtering specific rows in all data.frames in the list when possible. Write `dat$column_name` to specify the wanted columns.
#' @param list_elements Character vector with the names of the list objects you want to filter in. Defaults to `c("plot_lines", "plot_points", "plot_background_lines", "plot_braces")`.
#'
#' @return The plot_dat list with rows filtered as specified.
#' @export
#'
#' @examples # tbd
filter_plot_dat <- function(plot_dat,
                            filter_statement,
                            list_elements = c("plot_lines", "plot_points", "plot_background_lines", "plot_braces")) {
  c(
    plot_dat[!(names(plot_dat) %in% list_elements)],
    lapply(plot_dat[list_elements], function(dat) {
      dat[eval(parse(text = filter_statement)), ]
    })
  )
}
