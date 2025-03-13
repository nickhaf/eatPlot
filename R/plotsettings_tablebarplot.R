# Checking
check_plotsettings_barplot <- function(settings_list) {
  stopifnot(
    "The object provided for the 'default_list' argument does not have the correct length. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      length(settings_list) == 41
  )
  stopifnot(
    "The object provided for the 'default_list' argument does not have the correct names. Please use the function 'plot_settings()' for constructing a list of the correct type." =
      names(settings_list) %in% c(
        "axis_x_label_size",
        "axis_x_lims",
        "background_stripes_border",
        "background_stripes_colour",
        "bar_background_lines",
        "bar_background_lines_linetype",
        "bar_background_lines_colour",
        "bar_background_0line_linetype",
        "bar_background_0line_colour",
        "bar_background_lines_spanners",
        "bar_fill_colour",
        "bar_frame_linetype",
        "bar_label_nudge_x",
        "bar_label_size",
        "bar_line_width",
        "bar_nudge_y",
        "bar_pattern_fill_colour",
        "bar_pattern_spacing",
        "bar_pattern_type",
        "bar_pattern_width",
        "bar_type",
        "bar_width",
        "column_spanners_nudge_y",
        "column_spanners_row_height",
        "column_spanners_2_nudge_y",
        "column_spanners_2_row_height",
        "columns_alignment",
        "columns_nudge_x",
        "columns_nudge_y",
        "columns_table_sig_superscript_letter",
        "columns_table_sig_superscript_letter_nudge_x",
        "columns_width",
        "headers_alignment",
        "headers_background_colour",
        "headers_font_size",
        "headers_ggtext",
        "headers_nudge_x",
        "headers_nudge_y",
        "headers_row_height",
        "font_size",
        "space_right"
      )
  )


  stopifnot(is.numeric(settings_list$axis_x_label_size))
  stopifnot(is.numeric(settings_list$axis_x_lims) & length(settings_list$axis_x_lims) == 2 | is.null(settings_list$axis_x_lims))
  stopifnot(settings_list$background_stripes_border %in% c("Inf", "background_line_both", "background_line_left", "background_line_right"))
  stopifnot(all(is_colour(settings_list$background_stripes_colour)))
  stopifnot(settings_list$bar_background_lines %in% c("border", "scale_breaks", "none"))
  stopifnot(is.character(settings_list$bar_background_lines_linetype))
  stopifnot(is.character(settings_list$bar_background_lines_colour))
  stopifnot(is.character(settings_list$bar_background_0line_linetype))
  stopifnot(is.character(settings_list$bar_background_0line_colour))
  stopifnot(is.numeric(unlist(settings_list$bar_background_lines_spanners)) | is.null(settings_list$bar_background_lines_spanners))
  stopifnot(all(is_colour(settings_list$bar_fill_colour)))
  stopifnot(is.character(settings_list$bar_frame_linetype))
  stopifnot(is.numeric(settings_list$bar_label_nudge_x))
  stopifnot(is.numeric(settings_list$bar_label_size))
  stopifnot(is.numeric(settings_list$bar_line_width))
  stopifnot(is.numeric(settings_list$bar_nudge_y))
  stopifnot(all(is_colour(settings_list$bar_pattern_fill_colour)))
  stopifnot(is.character(settings_list$bar_pattern_type))
  stopifnot(settings_list$bar_type %in% c("pattern", "frame", "stacked"))
  stopifnot(is.numeric(settings_list$bar_width))
  stopifnot(is.numeric(settings_list$column_spanners_nudge_y))
  stopifnot(is.numeric(settings_list$column_spanners_row_height))
  stopifnot(is.numeric(settings_list$column_spanners_2_nudge_y))
  stopifnot(is.numeric(settings_list$column_spanners_2_row_height))
  stopifnot(is.numeric(settings_list$columns_alignment))
  stopifnot(is.numeric(settings_list$columns_nudge_x))
  stopifnot(is.numeric(settings_list$columns_nudge_y))
  stopifnot(is.character(settings_list$columns_table_sig_superscript_letter))
  stopifnot(is.numeric(settings_list$columns_table_sig_superscript_letter_nudge_x))
  stopifnot(is.numeric(settings_list$columns_width) | is.null(settings_list$columns_width))
  stopifnot(is.numeric(settings_list$headers_alignment) | is.null(settings_list$headers_alignment))
  stopifnot(is_colour(settings_list$headers_background_colour) | is.null(settings_list$headers_background_colour))
  stopifnot(is.numeric(settings_list$headers_font_size))
  stopifnot(is.logical(settings_list$headers_ggtext))
  stopifnot(is.numeric(settings_list$headers_nudge_x))
  stopifnot(is.numeric(settings_list$headers_nudge_y))
  stopifnot(is.numeric(settings_list$headers_row_height))
  stopifnot(is.numeric(settings_list$font_size))
  stopifnot(is.numeric(settings_list$bar_pattern_spacing))
  stopifnot(is.numeric(settings_list$bar_pattern_width))
  stopifnot(is.numeric(settings_list$space_right))
}


#' Set parameters for the barplots.
#'
#' @param axis_x_label_size Numeric for the size of the x axis labels. Defaults to `5`.
#' @param axis_x_lims Numeric vector of length `2` for the x-axis limits. Will be set automatically, `NULL` (default).
#' @param background_stripes_border Character string of either `c("Inf", "background_line_both", "background_line_left", "background_line_right", "background_line_table")`. The background stripes will either be drawn over the whole plot (`"Inf"`), from the outer left background_line to the outer right background_line (`"background_line_both"`), from the outer left background line to the right of the plot (`"background_line_left"`), the outer right background line to the left of the plot (`"background_line_right`), or only over the table-part of the plot (`background_line_table"`).
#' @param background_stripes_colour Character vector containing the background colour of each row. Defaults to `NULL`.
#' @param bar_background_lines Character string of either `c("borders", "scale_breaks", "none")`, indicating whether the barplot should receive dotted lines on its borders, at every scale break or none at all.
#' @param bar_background_lines_linetype Character string indicating the linetype for the background lines of the barplot.
#' @param bar_background_lines_colour Character string indicating the colour for the background lines of the barplot.
#' @param bar_background_0line_linetype Character string indicating the linetype for the background line of the barplot at zero.
#' @param bar_background_0line_colour Character string indicating the colour for the background line of the barplot at zero.
#' @param bar_background_lines_spanners List containing of numeric vectors of two elements for indicating over which rows the background_lines in the barplot should span. Each vector contains the start and end row for the background line. Defaults to `NULL`, in which case The background_lines will be drawn from top to bottom.
#' @param bar_fill_colour Colour of the bar filling. Can be either one colour for all bars, or a (named) vector with the names of the groups specified in `bar_fill`. If no names are provided for the vector, the order of the factor levels of `bar_fill` will be used for determining the colour assignment.
#' @param bar_frame_linetype Named vector with the bar frame linetypes. Names have to be found in the column defined in the `bar_sig`-argument of`plot_tablebarplot()`. Defaults to `solid`.
#' @param bar_label_nudge_x Numeric for nudging the bar labels in x direction.
#' @param bar_label_size Numeric for the font size of the bar labels.
#' @param bar_line_width Numeric for the line-size around the bar.
#' @param bar_nudge_y Numeric vector for nudging the bar in y direction. Either of the same length as the data, to nudge each bar sepearatly, or of length 1 to nudge all bars the same. Defaults to `0`.
#' @param bar_pattern_fill_colour Named vector with the filling colours for the bar pattern. Names of the vector must be found in the column specified in `bar_pattern_fill`. Defaults to `white`.
#' @param bar_pattern_spacing Numeric for the gap between patterns.
#' @param bar_pattern_type Named vector with the pattern types for the barpattern.
#' @param bar_pattern_width Numeric for the width of the pattern stripes. Note that by default the pattern are the whit stripes, so an increase of the `bar_pattern_width` parameter will increase the thickness of the white stripes.
#' @param bar_type Character string indicating whether levels of the grouping variable should be visualized by pattern fill ("pattern"), line type ("frame") or stacked barplot ("stacked").
#' @param bar_width Numeric between `0` and `1` specifying the width of the bar. Defaults to `0.4`.
#' @param column_spanners_nudge_y Numeric vector to increase or decrease the space between column spanners text and line. Can be either of length 1, or provide a nudging parameter for each column spanner. Defaults to `-0.2`.
#' @param column_spanners_row_height Numeric for the row height of the row the first level of column spanners is written in. Defaults to `1`.
#' @param column_spanners_2_nudge_y Numeric vector to increase or decrease the space between column spanners level 2 text and line. Can be either of length 1, or provide a nudging parameter for each column spanner. Defaults to `-0.2`.
#' @param column_spanners_2_row_height Numeric for the row height of the row the second level of column spanners is written in. Defaults to `1`
#' @param columns_alignment Numeric vector with one element for each column, determining the text adjustement within the column. Can be `0` (left-aligned), `0.5` (central-aligned), or `1` (right-aligned). Defaults to `0.5`.
#' @param columns_nudge_x Numeric vector to nudge the column text in x direction. Defaults to `0`.
#' @param columns_nudge_y Numeric vector to nudge the column texts in y direction. Defaults to `0`.
#' @param columns_table_sig_superscript_letter Character, that will be added on significant values defined by `columns_table_sig_superscript`.
#' @param columns_table_sig_superscript_letter_nudge_x Numeric for nudging the superscript towards or away from a number.
#' @param columns_width Numeric vector with relative column widths. Has to be equal to the number of columns (including the bar chart, if a bar chart is plotted) that are plotted in the table. Defaults to `NULL`, in which case all collumns will get the same width.
#' @param headers_alignment Numeric vector with one element for each column, determining the text adjustement of the headers. Can be `0` (left-aligned), `0.5` (central-aligned), or `1` (right-aligned). Defaults to `NULL`, in which case the alignment of the columns will be adopted.
#' @param headers_background_colour Colour of the background of the headers.
#' @param headers_font_size Numeric for the font size that will be used for the headers and column_spanners. Defaults to `2.5`.
#' @param headers_ggtext Logical indicating whether the headers should be drawn with ggtext::geom_richtext. Defaults to `TRUE`. Only set to `FALSE` if you want to plot greek letters in the headers AND save the plot in cmyk color format afterwards.
#' @param headers_nudge_x Numeric to nudge the column_headers in x direction. Defaults to `0`.
#' @param headers_nudge_y Numeric to nudge the column_headers in y direction. Defaults to `0`.
#' @param headers_row_height Numeric for the row height of the row the headers are written in. Defaults to `1`.
#' @param font_size Numeric vector with as many elements as columns for the font sizes of the columns. Defaults to `2.5`.
#' @param space_right Numeric for the width of a white space that will be added on the right of the plotting pane. Has to be the same scale as the data. Defaults to `0`.
#' @param default_list Named list with predefined settings. Defaults to a list with all settings set to `0`.
#'
#' @return A named list with settings for a table/barplot.
#' @export
#'
#' @examples
#' ## Calling the function without any arguments will result in the default settings-list.
#' plotsettings_tablebarplot()
#'
#' ## Arguments will overvrite the respective list element:
#' plot_settings <- plotsettings_tablebarplot(font_size = 2.5)
#' plot_settings$font_size
#'
#' ## Colours can be defined like so:
#' plotsettings_tablebarplot(
#'   bar_fill_colour = c(
#'     "blue",
#'     grDevices::rgb(10, 13, 82, maxColorValue = 255)
#'   )
#' )
#'
#' ## Or, to get better control over the colour assignment
#' ## we can also directly name the colours with the different groups
#' ## defined in the bar_sig-argument of plot_tablebarplot()
#' ## (as these groups are used to distinguish between different bar colours):
#' plotsettings_tablebarplot(
#'   bar_fill_colour = c(
#'     fill_group_1 = "blue",
#'     fill_group_2 = grDevices::rgb(
#'       10, 13, 82,
#'       maxColorValue = 255
#'     )
#'   )
#' )
#'
plotsettings_tablebarplot <- function(axis_x_label_size = NULL,
                                      axis_x_lims = NULL,
                                      background_stripes_border = NULL,
                                      background_stripes_colour = NULL,
                                      bar_background_lines = NULL,
                                      bar_background_lines_linetype = NULL,
                                      bar_background_lines_colour = NULL,
                                      bar_background_0line_linetype = NULL,
                                      bar_background_0line_colour = NULL,
                                      bar_background_lines_spanners = NULL,
                                      bar_fill_colour = NULL,
                                      bar_frame_linetype = NULL,
                                      bar_label_nudge_x = NULL,
                                      bar_label_size = NULL,
                                      bar_line_width = NULL,
                                      bar_nudge_y = NULL,
                                      bar_pattern_fill_colour = NULL,
                                      bar_pattern_spacing = NULL,
                                      bar_pattern_type = NULL,
                                      bar_pattern_width = NULL,
                                      bar_type = NULL,
                                      bar_width = NULL,
                                      column_spanners_nudge_y = NULL,
                                      column_spanners_row_height = NULL,
                                      column_spanners_2_nudge_y = NULL,
                                      column_spanners_2_row_height = NULL,
                                      columns_alignment = NULL,
                                      columns_nudge_x = NULL,
                                      columns_nudge_y = NULL,
                                      columns_table_sig_superscript_letter = NULL,
                                      columns_table_sig_superscript_letter_nudge_x = NULL,
                                      columns_width = NULL,
                                      headers_alignment = NULL,
                                      headers_background_colour = NULL,
                                      headers_font_size = NULL,
                                      headers_ggtext = NULL,
                                      headers_nudge_x = NULL,
                                      headers_nudge_y = NULL,
                                      headers_row_height = NULL,
                                      font_size = NULL,
                                      space_right = NULL,
                                      default_list = NULL) {
  ## Build a list with sensible defaults if no default is provided
  if (is.null(default_list)) {
    plot_settings <- list(
      "axis_x_label_size" = 5,
      "axis_x_lims" = NULL,
      "background_stripes_border" = "Inf",
      "background_stripes_colour" = "white",
      "bar_background_lines" = "none",
      "bar_background_lines_linetype" = "dotted",
      "bar_background_lines_colour" = "darkgrey",
      "bar_background_0line_linetype" = "solid",
      "bar_background_0line_colour" = "black",
      "bar_background_lines_spanners" = NULL,
      "bar_fill_colour" = "white",
      "bar_frame_linetype" = "solid",
      "bar_label_size" = 2,
      "bar_label_nudge_x" = 0,
      "bar_line_width" = 0.5,
      "bar_nudge_y" = 0,
      "bar_pattern_fill_colour" = "white",
      "bar_pattern_type" = "none",
      "bar_type" = "frame",
      "bar_width" = 0.4,
      "column_spanners_nudge_y" = -0.2,
      "column_spanners_row_height" = 1,
      "column_spanners_2_nudge_y" = -0.2,
      "column_spanners_2_row_height" = 1,
      "columns_alignment" = 0.5,
      "columns_nudge_x" = 0,
      "columns_nudge_y" = 0,
      "columns_table_sig_superscript_letter" = "a",
      "columns_table_sig_superscript_letter_nudge_x" = 0,
      "columns_width" = NULL,
      "headers_alignment" = NULL,
      "headers_background_colour" = "white",
      "headers_font_size" = 2.5,
      "headers_ggtext" = TRUE,
      "headers_nudge_x" = 0,
      "headers_nudge_y" = 0,
      "headers_row_height" = 1,
      "bar_pattern_spacing" = 0.1,
      "bar_pattern_width" = 0.5,
      "font_size" = 2.5,
      "space_right" = 0
    )
  } else {
    plot_settings <- default_list
  }

  check_plotsettings_barplot(plot_settings)

  ## Update the default list with all values that are != NULL
  for (i in names(plot_settings)) {
    value <- get(i)
    if (!is.null(value)) {
      plot_settings[[i]] <- value
    }
  }
  return(plot_settings)
}
