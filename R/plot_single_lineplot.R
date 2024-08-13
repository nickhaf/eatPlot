#' Plot a single lineplot. Multiple of this lineplots are combined as tiles to the output of [plot_lineplot()].
#'
#' @inheritParams plot_lineplot
#' @inheritParams plotsettings_lineplot
#' @param plot_lims List obtained by [calc_plot_lims()], containing different coordinate values.
#'
#' @return ggplot2 Object.
#' @export
#'
#' @examples # tbd
plot_single_lineplot <- function(plot_dat,
                                 brace_coords,
                                 plot_lims = NULL,
                                 # point_values = "est_noTrend_noComp",
                                 # point_sig = "sig_noTrend_noComp",
                                 # line_values = c("est_noTrendStart_noComp", "est_noTrendEnd_noComp"),
                                 # line_se = NULL,
                                 # line_sig = "sig_Trend_CompWithin",
                                 label_est = "trend",
                                 # label_se = "se_Trend_noComp",
                                 # label_sig_high = "sig_Trend_CompCrossDiffWhole",
                                 # label_sig_bold = "sig_Trend_noComp",
                                 # background_lines = TRUE,
                                 plot_settings = plotsettings_lineplot()) {

  # Assemble a single lineplot (one "tile" in the whole lineplot).

  ## Pro trend nur 2 versch. Werte jeweils in der Spalte.

  brace_coords$coord_dat_test <- brace_coords$coord_dat %>%
  dplyr::mutate(trend = paste0(.$year_start, .$year_end)) %>%
  tidyr::pivot_longer(cols = c("upper_y", "lower_y"),
                 values_to = "y") %>%
  tidyr::pivot_longer(cols = c("year_start", "year_end"),
                 values_to = "year",
                 names_to = "year_type")

  ggplot2::ggplot(plot_dat,
         mapping = ggplot2::aes(
           x = year,
           y = est_point,
           group = id,
           linetype = line_sig
         )) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::coord_cartesian(y=range(plot_dat$est_point), clip = "off") + #for the range just use the data for the respective axis
    ggplot2::theme(plot.margin = ggplot2::unit(c(0.25, 0.11, 0.11, 0.11), units="npc")) +
    theme_line(plot_settings) +

    ## Später eventuell ohen coords manuell zu berechnen? Nur wenns Sinn macht.
    ggbrace::stat_brace(
      data = brace_coords$coord_dat_test,
      mapping = ggplot2::aes(
        x = year,
        y = y,
        group = trend
      ))


    plot_braces(
            plot_dat,
            brace_coords = brace_coords,
            plot_lims = plot_lims,
            label_est = label_est,
            # label_se = label_se,
            # label_sig_high = label_sig_high,
            # label_sig_bold = label_sig_bold,
            plot_settings = plot_settings
          )
#
#   list(
#     theme_line(plot_settings),
#     set_scales(plot_settings),
#     plot_braces(
#       plot_dat[["plot_braces"]],
#       plot_lims = plot_lims,
#       label_est = label_est,
#       label_se = label_se,
#       label_sig_high = label_sig_high,
#       label_sig_bold = label_sig_bold,
#       plot_settings = plot_settings
#     ),
#     if (background_lines == TRUE) {
#       plot_background_lines(
#         dat = plot_dat[["plot_background_lines"]],
#         line_values = line_values,
#         line_se = line_se,
#         plot_settings = plot_settings
#       )
#     },
#     if (!is.null(line_values)) {
#       plot_lines(
#         data_plot_lines = plot_dat[["plot_lines"]],
#         line_values = line_values,
#         line_sig = line_sig,
#         plot_settings = plot_settings
#       )
#     },
#     if (!is.null(point_values)) {
#       plot_points(
#         data_plot_points = plot_dat[["plot_points"]],
#         point_values = point_values,
#         point_sig = point_sig,
#         plot_lims = plot_lims,
#         plot_settings = plot_settings
#       )
#     },
#     plot_x_axis(plot_dat[["plot_points"]],
#       plot_lims = plot_lims,
#       plot_settings = plot_settings
#     ),
#     if (plot_settings$split_plot == TRUE) {
#       if (plot_settings$equal_trend_line_length == TRUE) {
#         ggplot2::facet_grid(. ~ years_Trend, scales = "free_x")
#       } else {
#         ggplot2::facet_grid(. ~ years_Trend, scales = "free_x", space = "free_x")
#       }
#     },
#     set_plot_coords(
#       plot_dat,
#       plot_lims = plot_lims,
#       plot_settings = plot_settings
#     )
#   )
}
