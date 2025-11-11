# Set parameters for the lineplots.

Set parameters for the lineplots.

## Usage

``` r
plotsettings_lineplot(
  axis_x_background_colour = NULL,
  axis_x_background_width_x = NULL,
  axis_x_background_width_y = NULL,
  axis_x_label_centralize = NULL,
  axis_x_label_nudge_y = NULL,
  axis_x_label_size = NULL,
  axis_y = NULL,
  axis_y_tick_distance = NULL,
  axis_y_lims = NULL,
  background_facet_remove = NULL,
  background_subgroup_remove = NULL,
  background_lines = NULL,
  background_line_colour = NULL,
  box_facet_linewidth = NULL,
  brace_label_gap_y = NULL,
  brace_label_nudge_x = NULL,
  brace_label_size = NULL,
  brace_label_nudge_y = NULL,
  brace_line_width = NULL,
  brace_span_y = NULL,
  equal_trend_line_length = NULL,
  line_type = NULL,
  line_width = NULL,
  margin_below_y_axis = NULL,
  margin_above_y_axis = NULL,
  margin_bottom = NULL,
  margin_left = NULL,
  margin_right = NULL,
  margin_top = NULL,
  n_cols = NULL,
  point_label_nudge = NULL,
  point_shapes = NULL,
  point_label_size = NULL,
  point_label_nudge_direction = NULL,
  point_label_nudge_x = NULL,
  point_label_nudge_y = NULL,
  point_size = NULL,
  split_plot = NULL,
  split_plot_gap_width = NULL,
  subgroup_colours = NULL,
  default_list = NULL
)
```

## Arguments

- axis_x_background_colour:

  Colour value of the x-axis background.

- axis_x_background_width_x, axis_x_background_width_y:

  Numeric. The background space will be increased in x- or y-direction,
  if this parameter is increased.

- axis_x_label_centralize:

  Numeric. The x-axis labels will be nudged into the center by this
  amount, if the plot is a split lineplot.

- axis_x_label_nudge_y:

  Numeric for shifting the x-axis labels vertically. Increase to lower
  the x-axis labels.

- axis_x_label_size:

  Numeric for the font size of the x-axis labels.

- axis_y:

  Logical, indicating whether a y-axis should be plotted to the left of
  each row or not.

- axis_y_tick_distance:

  Numeric, for which distance should lie between tick marks. The first
  tick will start at the lower end of `axis_y_lims` or be calculated
  automatically. The following ticks will be added in distances defined
  by `axis_y_tick_distance`. Defaults to `20`.

- axis_y_lims:

  Numeric for the y-axis limits. Defaults to `NULL`, in which case the
  limits will be set automatically.

- background_facet_remove:

  Logical, indicating whether the `background_facet` should be removed.
  If not, it will be plotted as extra facet additionally to the
  background line. Defaults to `TRUE`.

- background_subgroup_remove:

  Logical, indicating whether the `background_subgroup` should be
  removed. If not, it will be plotted as extra facet additionally to the
  background line. Defaults to `TRUE`.

- background_lines:

  Logical, indicating whether the whole group trend should be plotted in
  the background. Defaults to `TRUE`.

- background_line_colour:

  Colour of the background line.

- box_facet_linewidth:

  Numeric for the linewidth of the box around chosen seperate_plot-vars
  (e.g., states).

- brace_label_gap_y:

  Numeric for the size of the vertical gap between brace labels.

- brace_label_nudge_x:

  Numeric. The brace labels will be shifted along the x-axis by this
  amount. Increase to shift the labels further to the right.

- brace_label_size:

  Numeric fontsize of the bracelabels.

- brace_label_nudge_y:

  Numeric. The brace labels will be shifted along the y-axis by this
  amount. Increase to let the labels start further below.

- brace_line_width:

  Numeric for the thickness of the brace.

- brace_span_y:

  Numeric for the width of the brace on the y-axis.

- equal_trend_line_length:

  Logical. If true, all trend lines will have the same length, even if
  the actual distance between the year is differently. E.g., the
  distance between the years of the trends 2010 - 2011 and 2011 - 2020
  would be equal, even though the first trend only takes 1 year, and the
  second one 9 years. Please use with care, as it will distort the
  x-axis and results in misleading line lengths. Defaults to `FALSE`.

- line_type:

  Named vector containing the linetypes for the plots. The names must be
  either `"TRUE"` or `"FALSE"`, as the linetpyes represent
  significances. See
  [ggplot2::scale_linetype](https://ggplot2.tidyverse.org/reference/scale_linetype.html).

- line_width:

  Numeric for the thicknes of the plotted lines.

- margin_below_y_axis, margin_above_y_axis:

  Numeric to scale the area between the y-axis and the braces/the
  x-axis-header.

- margin_bottom, margin_left, margin_right, margin_top:

  Numeric for the area around the plot. See
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

- n_cols:

  Numeric, indicating how many columns of smaller plots the final
  lineplot should have.

- point_label_nudge:

  Logical. If `TRUE`, the point labels will be nudged automatically by
  [ggrepel::geom_text_repel](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
  to try and avoid them overlapping with lines and/or points. Might
  interfere with other point-label settings, so use at your own risk.
  Alternatively, you could try to use `point_label_nudge_direction` to
  get some controll over the point labels.

- point_shapes:

  Named numeric vector for the shape of the points. The vectornames must
  refer to the levels in the point_sig column. See
  [ggplot2::scale_shape](https://ggplot2.tidyverse.org/reference/scale_shape.html).

- point_label_size:

  Numeric for the fontsize of the pointlabels.

- point_label_nudge_direction:

  Named list with contents of either "+" or "-". The names have to be
  the factorlevels of the grouping_var. For "+" the point lables will be
  printed above the point, for "-" below. If `NULL` is provided, the
  labels will be printed below the points for the lowest group, and
  above the points for all others.

- point_label_nudge_x, point_label_nudge_y:

  Numeric for the amount the pointlabel is nudged in x- or y-direction.

- point_size:

  Numeric for the size of plotted points.

- split_plot:

  Logical, indicating whether the different trends should be split or
  not.

- split_plot_gap_width:

  Numeric for the width of the gap in a split plot in npc.

- subgroup_colours:

  Named vector with the colours for different grouping_var groups. The
  names of the vector have to be equivalent to the factorlevels of your
  grouping_var.

- default_list:

  Named list with predefined settings. Defaults to a list with all
  settings set to `0`.

## Value

A named list with settings for the lineplots.

## Examples

``` r
plotsettings_lineplot(n_cols = 2, axis_x_label_centralize = 0.1)
#> $axis_x_background_colour
#> [1] "lightgrey"
#> 
#> $axis_x_background_width_x
#> [1] 0
#> 
#> $axis_x_background_width_y
#> [1] 0.04
#> 
#> $axis_x_label_centralize
#> [1] 0.1
#> 
#> $axis_x_label_nudge_y
#> [1] 0
#> 
#> $axis_x_label_size
#> [1] 2
#> 
#> $axis_y
#> [1] FALSE
#> 
#> $axis_y_tick_distance
#> [1] 20
#> 
#> $axis_y_lims
#> NULL
#> 
#> $background_facet_remove
#> [1] TRUE
#> 
#> $background_subgroup_remove
#> [1] TRUE
#> 
#> $background_lines
#> [1] TRUE
#> 
#> $background_line_colour
#> [1] "black"
#> 
#> $brace_label_gap_y
#> [1] 0.08
#> 
#> $brace_label_nudge_x
#> [1] 0
#> 
#> $brace_label_nudge_y
#> [1] 0.05
#> 
#> $brace_label_size
#> [1] 2
#> 
#> $brace_line_width
#> [1] 0.5
#> 
#> $brace_span_y
#> [1] 0.1
#> 
#> $equal_trend_line_length
#> [1] FALSE
#> 
#> $subgroup_colours
#> NULL
#> 
#> $line_type
#> NULL
#> 
#> $line_width
#> [1] 1
#> 
#> $margin_below_y_axis
#> [1] 0.1
#> 
#> $margin_above_y_axis
#> [1] 0.1
#> 
#> $margin_bottom
#> [1] 0
#> 
#> $margin_left
#> [1] 0
#> 
#> $margin_right
#> [1] 0
#> 
#> $margin_top
#> [1] 0
#> 
#> $n_cols
#> [1] 2
#> 
#> $point_label_nudge
#> [1] FALSE
#> 
#> $point_label_nudge_direction
#> NULL
#> 
#> $point_label_nudge_x
#> [1] 0
#> 
#> $point_label_nudge_y
#> [1] 0
#> 
#> $point_label_size
#> [1] 2
#> 
#> $point_shapes
#> NULL
#> 
#> $point_size
#> [1] 1
#> 
#> $box_facet_linewidth
#> [1] 0.5
#> 
#> $split_plot
#> [1] FALSE
#> 
#> $split_plot_gap_width
#> [1] 0
#> 
```
