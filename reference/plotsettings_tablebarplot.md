# Set parameters for the barplots.

Set parameters for the barplots.

## Usage

``` r
plotsettings_tablebarplot(
  axis_x = NULL,
  axis_x_label_size = NULL,
  axis_x_lims = NULL,
  axis_x_stepsize = NULL,
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
  bar_label_colour = NULL,
  bar_label_nudge_x = NULL,
  bar_label_nudge_y = NULL,
  bar_label_nudge_x_out = NULL,
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
  default_list = NULL
)
```

## Arguments

- axis_x:

  Logical indicating whether the x-axis should be drawn. Defaults to
  `TRUE`.

- axis_x_label_size:

  Numeric for the size of the x axis labels. Defaults to `5`.

- axis_x_lims:

  Numeric vector of length `2` for the x-axis limits. Will be set
  automatically, `NULL` (default).

- axis_x_stepsize:

  Numeric for the distance between x-axis ticks. Default is `10`.

- background_stripes_border:

  Character string of either
  `c("Inf", "background_line_both", "background_line_left", "background_line_right", "background_line_table")`.
  The background stripes will either be drawn over the whole plot
  (`"Inf"`), from the outer left background_line to the outer right
  background_line (`"background_line_both"`), from the outer left
  background line to the right of the plot (`"background_line_left"`),
  the outer right background line to the left of the plot
  (`"background_line_right`), or only over the table-part of the plot
  (`background_line_table"`).

- background_stripes_colour:

  Character vector containing the background colour of each row.
  Defaults to `NULL`.

- bar_background_lines:

  Either a character string of either
  `c("borders", "scale_breaks", "none")` or a numeric vector, indicating
  whether the barplot should receive lines on its borders, at every
  scale break, none at all, or at some manually defined spots.

- bar_background_lines_linetype:

  Character string indicating the linetype for the background lines of
  the barplot.

- bar_background_lines_colour:

  Character string indicating the colour for the background lines of the
  barplot.

- bar_background_0line_linetype:

  Character string indicating the linetype for the background line of
  the barplot at zero.

- bar_background_0line_colour:

  Character string indicating the colour for the background line of the
  barplot at zero.

- bar_background_lines_spanners:

  List containing of numeric vectors of two elements for indicating over
  which rows the background_lines in the barplot should span. Each
  vector contains the start and end row for the background line.
  Defaults to `NULL`, in which case The background_lines will be drawn
  from top to bottom.

- bar_fill_colour:

  Colour of the bar filling. Can be either one colour for all bars, or a
  (named) vector with the names of the groups specified in `bar_fill`.
  If no names are provided for the vector, the order of the factor
  levels of `bar_fill` will be used for determining the colour
  assignment.

- bar_frame_linetype:

  Named vector with the bar frame linetypes. Names have to be found in
  the column defined in the `bar_sig`-argument
  of[`plot_tablebarplot()`](https://nickhaf.github.io/eatPlot/reference/plot_tablebarplot.md).
  Defaults to `solid`.

- bar_label_colour:

  Colour of the bar labels. Can either be a single colour, or a named
  vector that contains the colour for each group defined in `bar_fill`.
  Defaults to `"black"`.

- bar_label_nudge_x:

  Numeric for nudging the bar labels in x direction.

- bar_label_nudge_y:

  Numeric for nudging the bar labels in y direction.

- bar_label_nudge_x_out:

  Numeric. Necessary for stacked barplots. If the percentage of a
  subgroup lies below that value, the label will be nudged to the left
  or right of the stacked bar. Defaults to `0`.

- bar_label_size:

  Numeric for the font size of the bar labels.

- bar_line_width:

  Numeric for the line-size around the bar.

- bar_nudge_y:

  Numeric vector for nudging the bar in y direction. Either of the same
  length as the data, to nudge each bar sepearatly, or of length 1 to
  nudge all bars the same. Defaults to `0`.

- bar_pattern_fill_colour:

  Named vector with the filling colours for the bar pattern. Names of
  the vector must be found in the column specified in
  `bar_pattern_fill`. Defaults to `white`.

- bar_pattern_spacing:

  Numeric for the gap between patterns.

- bar_pattern_type:

  Named vector with the pattern types for the barpattern.

- bar_pattern_width:

  Numeric for the width of the pattern stripes. Note that by default the
  pattern are the whit stripes, so an increase of the
  `bar_pattern_width` parameter will increase the thickness of the white
  stripes.

- bar_type:

  Character string defining the bar type. There are "default", pattern
  fill ("pattern"), "frame" or "stacked" barplot.

- bar_width:

  Numeric between `0` and `1` specifying the width of the bar. Defaults
  to `0.4`.

- column_spanners_nudge_y:

  Numeric vector to increase or decrease the space between column
  spanners text and line. Can be either of length 1, or provide a
  nudging parameter for each column spanner. Defaults to `-0.2`.

- column_spanners_row_height:

  Numeric for the row height of the row the first level of column
  spanners is written in. Defaults to `1`.

- column_spanners_2_nudge_y:

  Numeric vector to increase or decrease the space between column
  spanners level 2 text and line. Can be either of length 1, or provide
  a nudging parameter for each column spanner. Defaults to `-0.2`.

- column_spanners_2_row_height:

  Numeric for the row height of the row the second level of column
  spanners is written in. Defaults to `1`

- columns_alignment:

  Numeric vector with one element for each column, determining the text
  adjustement within the column. Can be `0` (left-aligned), `0.5`
  (central-aligned), `1` (right-aligned), or `2` (right-aligned, but in
  the middle of the column). Defaults to `0.5`.

- columns_nudge_x:

  Numeric vector to nudge the column text in x direction. Negativ to
  nudge left, positive to nudge right. Defaults to `0`.

- columns_nudge_y:

  Either a numeric vector or a list of numeric vectors of the same
  length as columns in the table (including the bar chart). Nudges the
  column texts in y direction, either for all rows in the column the
  same amount (vector), or each row in each column specifically (list).
  Negative values to nudge down, positive values to nudge up. Defaults
  to `0`.

- columns_table_sig_superscript_letter:

  Character, that will be added on significant values defined by
  `columns_table_sig_superscript`.

- columns_table_sig_superscript_letter_nudge_x:

  Numeric for nudging the superscript towards or away from a number.

- columns_width:

  Numeric vector with relative column widths. Has to be equal to the
  number of columns (including the bar chart, if a bar chart is plotted)
  that are plotted in the table. Defaults to `NULL`, in which case all
  collumns will get the same width.

- headers_alignment:

  Numeric vector with one element for each column, determining the text
  adjustement of the headers. Can be `0` (left-aligned), `0.5`
  (central-aligned), or `1` (right-aligned). Defaults to `NULL`, in
  which case the alignment of the columns will be adopted.

- headers_background_colour:

  Colour of the background of the headers.

- headers_font_size:

  Numeric for the font size that will be used for the headers and
  column_spanners. Defaults to `2.5`.

- headers_ggtext:

  Logical indicating whether the headers should be drawn with
  [`ggtext::geom_richtext()`](https://wilkelab.org/ggtext/reference/geom_richtext.html).
  Defaults to `TRUE`. Only set to `FALSE` if you want to plot greek
  letters in the headers AND save the plot in cmyk color format
  afterwards. When set to
  FALSE,[`grDevices::plotmath()`](https://rdrr.io/r/grDevices/plotmath.html)
  will be used for plotting the special characters, so refer to the
  documentation of the
  [`grDevices::plotmath()`](https://rdrr.io/r/grDevices/plotmath.html)-function
  for the syntax.

- headers_nudge_x:

  Numeric to nudge the column_headers in x direction. Defaults to `0`.

- headers_nudge_y:

  Numeric to nudge the column_headers in y direction. Defaults to `0`.

- headers_row_height:

  Numeric for the row height of the row the headers are written in.
  Defaults to `1`.

- font_size:

  Numeric vector with as many elements as columns for the font sizes of
  the columns. Defaults to `2.5`.

- space_right:

  Numeric for the width of a white space that will be added on the right
  of the plotting pane. Has to be the same scale as the data. Defaults
  to `0`.

- default_list:

  Named list with predefined settings. Defaults to a list with all
  settings set to `0`.

## Value

A named list with settings for a table/barplot.

## Examples

``` r
## Calling the function without any arguments will result in the default settings-list.
plotsettings_tablebarplot()
#> $axis_x
#> [1] TRUE
#> 
#> $axis_x_label_size
#> [1] 5
#> 
#> $axis_x_lims
#> NULL
#> 
#> $axis_x_stepsize
#> [1] 10
#> 
#> $background_stripes_border
#> [1] "Inf"
#> 
#> $background_stripes_colour
#> [1] "white"
#> 
#> $bar_background_lines
#> [1] "none"
#> 
#> $bar_background_lines_linetype
#> [1] "dotted"
#> 
#> $bar_background_lines_colour
#> [1] "darkgrey"
#> 
#> $bar_background_0line_linetype
#> [1] "solid"
#> 
#> $bar_background_0line_colour
#> [1] "black"
#> 
#> $bar_background_lines_spanners
#> NULL
#> 
#> $bar_fill_colour
#> [1] "white"
#> 
#> $bar_frame_linetype
#> [1] "solid"
#> 
#> $bar_label_colour
#> [1] "black"
#> 
#> $bar_label_size
#> [1] 2
#> 
#> $bar_label_nudge_x
#> [1] 0
#> 
#> $bar_label_nudge_y
#> [1] 0
#> 
#> $bar_label_nudge_x_out
#> [1] 0
#> 
#> $bar_line_width
#> [1] 0.5
#> 
#> $bar_nudge_y
#> [1] 0
#> 
#> $bar_pattern_fill_colour
#> [1] "white"
#> 
#> $bar_pattern_type
#> [1] "none"
#> 
#> $bar_type
#> [1] "default"
#> 
#> $bar_width
#> [1] 0.4
#> 
#> $column_spanners_nudge_y
#> [1] -0.2
#> 
#> $column_spanners_row_height
#> [1] 1
#> 
#> $column_spanners_2_nudge_y
#> [1] -0.2
#> 
#> $column_spanners_2_row_height
#> [1] 1
#> 
#> $columns_alignment
#> [1] 0.5
#> 
#> $columns_nudge_x
#> [1] 0
#> 
#> $columns_nudge_y
#> [1] 0
#> 
#> $columns_table_sig_superscript_letter
#> [1] "a"
#> 
#> $columns_table_sig_superscript_letter_nudge_x
#> [1] 0
#> 
#> $columns_width
#> NULL
#> 
#> $headers_alignment
#> NULL
#> 
#> $headers_background_colour
#> [1] "white"
#> 
#> $headers_font_size
#> [1] 2.5
#> 
#> $headers_ggtext
#> [1] TRUE
#> 
#> $headers_nudge_x
#> [1] 0
#> 
#> $headers_nudge_y
#> [1] 0
#> 
#> $headers_row_height
#> [1] 1
#> 
#> $bar_pattern_spacing
#> [1] 0.1
#> 
#> $bar_pattern_width
#> [1] 0.5
#> 
#> $font_size
#> [1] 2.5
#> 
#> $space_right
#> [1] 0
#> 

## Arguments will overvrite the respective list element:
plot_settings <- plotsettings_tablebarplot(font_size = 2.5)
plot_settings$font_size
#> [1] 2.5

## Colours can be defined like so:
plotsettings_tablebarplot(
  bar_fill_colour = c(
    "blue",
    grDevices::rgb(10, 13, 82, maxColorValue = 255)
  )
)
#> $axis_x
#> [1] TRUE
#> 
#> $axis_x_label_size
#> [1] 5
#> 
#> $axis_x_lims
#> NULL
#> 
#> $axis_x_stepsize
#> [1] 10
#> 
#> $background_stripes_border
#> [1] "Inf"
#> 
#> $background_stripes_colour
#> [1] "white"
#> 
#> $bar_background_lines
#> [1] "none"
#> 
#> $bar_background_lines_linetype
#> [1] "dotted"
#> 
#> $bar_background_lines_colour
#> [1] "darkgrey"
#> 
#> $bar_background_0line_linetype
#> [1] "solid"
#> 
#> $bar_background_0line_colour
#> [1] "black"
#> 
#> $bar_background_lines_spanners
#> NULL
#> 
#> $bar_fill_colour
#> [1] "blue"    "#0A0D52"
#> 
#> $bar_frame_linetype
#> [1] "solid"
#> 
#> $bar_label_colour
#> [1] "black"
#> 
#> $bar_label_size
#> [1] 2
#> 
#> $bar_label_nudge_x
#> [1] 0
#> 
#> $bar_label_nudge_y
#> [1] 0
#> 
#> $bar_label_nudge_x_out
#> [1] 0
#> 
#> $bar_line_width
#> [1] 0.5
#> 
#> $bar_nudge_y
#> [1] 0
#> 
#> $bar_pattern_fill_colour
#> [1] "white"
#> 
#> $bar_pattern_type
#> [1] "none"
#> 
#> $bar_type
#> [1] "default"
#> 
#> $bar_width
#> [1] 0.4
#> 
#> $column_spanners_nudge_y
#> [1] -0.2
#> 
#> $column_spanners_row_height
#> [1] 1
#> 
#> $column_spanners_2_nudge_y
#> [1] -0.2
#> 
#> $column_spanners_2_row_height
#> [1] 1
#> 
#> $columns_alignment
#> [1] 0.5
#> 
#> $columns_nudge_x
#> [1] 0
#> 
#> $columns_nudge_y
#> [1] 0
#> 
#> $columns_table_sig_superscript_letter
#> [1] "a"
#> 
#> $columns_table_sig_superscript_letter_nudge_x
#> [1] 0
#> 
#> $columns_width
#> NULL
#> 
#> $headers_alignment
#> NULL
#> 
#> $headers_background_colour
#> [1] "white"
#> 
#> $headers_font_size
#> [1] 2.5
#> 
#> $headers_ggtext
#> [1] TRUE
#> 
#> $headers_nudge_x
#> [1] 0
#> 
#> $headers_nudge_y
#> [1] 0
#> 
#> $headers_row_height
#> [1] 1
#> 
#> $bar_pattern_spacing
#> [1] 0.1
#> 
#> $bar_pattern_width
#> [1] 0.5
#> 
#> $font_size
#> [1] 2.5
#> 
#> $space_right
#> [1] 0
#> 

## Or, to get better control over the colour assignment
## we can also directly name the colours with the different groups
## defined in the bar_sig-argument of plot_tablebarplot()
## (as these groups are used to distinguish between different bar colours):
plotsettings_tablebarplot(
  bar_fill_colour = c(
    fill_group_1 = "blue",
    fill_group_2 = grDevices::rgb(
      10, 13, 82,
      maxColorValue = 255
    )
  )
)
#> $axis_x
#> [1] TRUE
#> 
#> $axis_x_label_size
#> [1] 5
#> 
#> $axis_x_lims
#> NULL
#> 
#> $axis_x_stepsize
#> [1] 10
#> 
#> $background_stripes_border
#> [1] "Inf"
#> 
#> $background_stripes_colour
#> [1] "white"
#> 
#> $bar_background_lines
#> [1] "none"
#> 
#> $bar_background_lines_linetype
#> [1] "dotted"
#> 
#> $bar_background_lines_colour
#> [1] "darkgrey"
#> 
#> $bar_background_0line_linetype
#> [1] "solid"
#> 
#> $bar_background_0line_colour
#> [1] "black"
#> 
#> $bar_background_lines_spanners
#> NULL
#> 
#> $bar_fill_colour
#> fill_group_1 fill_group_2 
#>       "blue"    "#0A0D52" 
#> 
#> $bar_frame_linetype
#> [1] "solid"
#> 
#> $bar_label_colour
#> [1] "black"
#> 
#> $bar_label_size
#> [1] 2
#> 
#> $bar_label_nudge_x
#> [1] 0
#> 
#> $bar_label_nudge_y
#> [1] 0
#> 
#> $bar_label_nudge_x_out
#> [1] 0
#> 
#> $bar_line_width
#> [1] 0.5
#> 
#> $bar_nudge_y
#> [1] 0
#> 
#> $bar_pattern_fill_colour
#> [1] "white"
#> 
#> $bar_pattern_type
#> [1] "none"
#> 
#> $bar_type
#> [1] "default"
#> 
#> $bar_width
#> [1] 0.4
#> 
#> $column_spanners_nudge_y
#> [1] -0.2
#> 
#> $column_spanners_row_height
#> [1] 1
#> 
#> $column_spanners_2_nudge_y
#> [1] -0.2
#> 
#> $column_spanners_2_row_height
#> [1] 1
#> 
#> $columns_alignment
#> [1] 0.5
#> 
#> $columns_nudge_x
#> [1] 0
#> 
#> $columns_nudge_y
#> [1] 0
#> 
#> $columns_table_sig_superscript_letter
#> [1] "a"
#> 
#> $columns_table_sig_superscript_letter_nudge_x
#> [1] 0
#> 
#> $columns_width
#> NULL
#> 
#> $headers_alignment
#> NULL
#> 
#> $headers_background_colour
#> [1] "white"
#> 
#> $headers_font_size
#> [1] 2.5
#> 
#> $headers_ggtext
#> [1] TRUE
#> 
#> $headers_nudge_x
#> [1] 0
#> 
#> $headers_nudge_y
#> [1] 0
#> 
#> $headers_row_height
#> [1] 1
#> 
#> $bar_pattern_spacing
#> [1] 0.1
#> 
#> $bar_pattern_width
#> [1] 0.5
#> 
#> $font_size
#> [1] 2.5
#> 
#> $space_right
#> [1] 0
#> 
```
