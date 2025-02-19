library(gtable)
library(tidyverse)

plot_layout <- gtable(
  widths = unit(c(0.3, 0.7), c('npc', 'npc')),
  heights = unit(c(0.9, 0, 0.1), c('npc', 'cm', 'npc'))
)

gtable_show_layout(plot_layout)
xtick <- 0:8
ytick <- seq(0, 50, 10)

points <- pointsGrob(
  x = mpg$displ / xtick[length(xtick)],
  y = mpg$hwy / ytick[length(ytick)],
  default.units = 'npc',
  size = unit(6, 'pt')
)

xaxis <- xaxisGrob(
  at = seq(0, 1, length.out = length(xtick)),
  label = xtick
)

yaxis <- yaxisGrob(
  at = seq(0, 1, length.out = length(ytick)),
  label = ytick
)

grid.newpage()
grid.draw(points)

plot_layout <- gtable_add_grob(
  plot_layout,
  grobs = list(points, xaxis, xaxis),
  t = c(1, 2, 2), # the row defining the top extent of each grob
  l = c(2, 1, 2), # the column defining the left extent of each grob
  clip = 'off'
)

grid.newpage()
grid.draw(plot_layout)

## Okay, so this might help me to split the data up into rows and columns.
## Might get very complex though ...
