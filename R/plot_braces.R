#' Plot braces below plot.
#'
#' @param data_trend_point Prepared Trend data.
#'
#' @return ggplot2 object.
#' @export
#'
#' @examples ##
plot_braces <- function(data_trend_point){


  # Get range for y-axis
  range_est <- range(c(data_trend_point$est_point_start, data_trend_point$est_point_end))

  coords <- calc_coords(range_est)

  brace_coords <- calc_brace_coords(data_trend_point, coords)

  c(
    # Clip Coordinate system. Necessary, so the brace can be drawn outside of the plot
    ggplot2::coord_cartesian(clip = "off", ylim = c(plyr::round_any(coords[1], accuracy = 10), plyr::round_any(coords[2], accuracy = 10))),

    ## Loop to draw a brace for every year_start
    lapply(unique(data_trend_point$year_start), function(x) {

      coordinates <- brace_coords[brace_coords$year_start == x, ]
      coordiantes <- unique(brace_coords[,c("year_start", "year_end", "brace_upper_y", "brace_lower_y")])

      ggbrace::geom_brace(
        mapping = ggplot2::aes(
          x = c(coordinates$year_start, coordinates$year_end),
          y = c(coordinates$brace_upper_y, coordinates$brace_lower_y)
        ),
        mid = ifelse(coordinates$year_start == min(brace_coords$year_start), 0.25, 0.5),
        inherit.data = F,
        rotate = 180,
        size = 0.8,
        npoints = 200
      )
    })
    )
}



# ggplot2::ggplot() +
#   plot_lines(data_lines = data[["trend_point"]]) +
#   plot_braces(data_trend_point) +
#   ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc"))
#
#




# ## Utils
#
# ## Calc coordinate system borders. Programmatisch lösen.
# calc_coords <- function(range_vec){
#   coords <- c(plyr::round_any(range_vec[1] - 40, accuracy = 10, f = floor),
#               plyr::round_any(range_vec[2] + 20, accuracy = 10, f = ceiling)
#               )
#   return(coords)
# }
#
#
# ## Calc the coordinates for drawing the braces.
# calc_brace_coords <- function(data, coords){
#   # Calculate brace coordinates
#   data$brace_upper_y <- ifelse(data$year_start == min(data$year_start), coords[1], coords[1] - 72) ## Programmatisch lösen
#   data$brace_lower_y <- ifelse(data$year_start == min(data$year_start), coords[1] - 70, coords[1] - 102)
#   data$label_pos <- ifelse(data$grouping_var == 1, range_est[1] - 113, range_est[1] - 133)
#   # data$estTrend_within_label <- ifelse(data$sigTrend_within == "bold", paste0("**", round(data$estTrend_within, 0), "**"), round(data$estTrend_within, 0))
#   #   data$sigTrend_vsGermany <- ifelse(data$pTrend_vsGermany < 0.05, "<sup>a</sup>", "")
#
#   return(data)
# }
#
#
# plot_brace_label <- function(data_trend_point){
#
# }
#
#
#
#
# ######################################### Working example
#
#
# data <- prep_lineplot(trend_books, grouping_var = "KBuecher_imp3", competence = "GL")
# data_trend_point <- data[["trend_point"]]
# data_trend_point <- data_trend_point[data_trend_point$TR_BUNDESLAND == "Berlin", ]
# dat_trend <- data_trend_point
#
# ## Old stuff
# draw_brace <- function(dat_trend) {
#
#   # Get range for y-axis
#   range_est <- range(c(dat_trend$est_point_start, dat_trend$est_point_end))
#
#   # Calculate brace coordinates
#   brace_coordinates <- dat_trend
#   brace_coordinates$brace_upper_y <- ifelse(dat_trend$year_start == min(dat_trend$year_start), range_est[1] - 40, range_est[1] - 72)
#   brace_coordinates$brace_lower_y <- ifelse(dat_trend$year_start == min(dat_trend$year_start), range_est[1] - 70, range_est[1] - 102)
#   brace_coordinates$label_pos <- ifelse(dat_trend$grouping_var == 1, range_est[1] - 113, range_est[1] - 133)
#   #brace_coordinates$sigTrend_vsGermany <- ifelse(dat_trend$pTrend_vsGermany < 0.05, "<sup>a</sup>", "")
#   #brace_coordinates$estTrend_within_label <- ifelse(dat_trend$sig_trend_within == "bold", paste0("**", round(dat_trend$estTrend_within, 0), "**"), round(dat_trend$estTrend_within, 0))
#
#   # Combine different ggplot functions
#   c(
#     # Clip Coordinate system. Necessary, so the brace can be drawn inside the plot
#     ggplot2::coord_cartesian(clip = "off", ylim = c(range_est[1] - 40, range_est[2] + 20)),
#     lapply(unique(dat_trend$year_start), function(x) {
#
#       coordinates <- brace_coordinates[brace_coordinates$year_start == x, ]
#       coordiantes <- brace_coordinates[,c("year_start", "year_end", "brace_upper_y", "brace_lower_y")]
#       coordinates <- unique(coordinates)
#
#       ggbrace::geom_brace(
#         mapping = ggplot2::aes(
#           x = c(coordinates$year_start, coordinates$year_end),
#           y = c(coordinates$brace_upper_y, coordinates$brace_lower_y)
#         ),
#         mid = ifelse(coordinates$year_start == min(brace_coordinates$year_start), 0.25, 0.5),
#         inherit.data = F,
#         rotate = 180,
#         size = 0.8,
#         npoints = 200
#       )
#     })
#   )
# }
# ggplot2::ggplot() +
#   plot_lines(data_lines = data[["trend_point"]]) +
#   draw_brace(dat_trend) +
#   ggplot2::theme(plot.margin = ggplot2::margin(0.05, 0.03, 0.25, 0.03, "npc")) +
#   NULL
#     ggtext::geom_richtext(
#       data = brace_coordinates[!(brace_coordinates$year_start == 2011 &
#                                      brace_coordinates$year_end == 2016), ],
#       mapping = ggplot2::aes(
#         x = year_start + (year_end - max(year_start)) / 2,
#         y = label_pos,
#         label = c("test1", "test2")
#       ),
#       size = 3,
#       label.padding = grid::unit(rep(0, 4), "pt"),
#       fill = NA,
#       label.color = NA
#     )
#
#
# ## coord_cartesian: ylims, also wo ist die y-Achse? Ohne diese Limits werden die braces noch im Plot gezeichnet, sie sollen ja aber Unter der x-Achse losgehen.
# ## Plot margin: Wie viel weiße Fläche (auf die auch z.B. die Braces passen), bleibt neben den Achsen frei?
#
#



