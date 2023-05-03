#' Plot background lines.
#'
#' @param dat Data.
#' @inheritParams plot_lineplot
#'
#' @return ggplot2 object.
#' @export
#'
#' @examples #tbd
plot_background_lines <- function(dat, line_values, line_se) {

  #colnames(dat)[colnames(dat) %in% paste0(unlist(line_values), "_wholeGroup")] <- gsub("_wholeGroup", "", colnames(dat)[colnames(dat) %in% paste0(unlist(line_values), "_wholeGroup")])

  colnames(dat) <- gsub("Start", ".Start", colnames(dat))
  line_values <- gsub("Start", ".Start", line_values)
  line_se <- gsub("Start", ".Start", line_se)
  colnames(dat) <- gsub("End", ".End", colnames(dat))
  line_values <- gsub("End", ".End", line_values)
  line_se <- gsub("End", ".End", line_se)

  dat_long <- stats::reshape(
    dat,
    direction = "long",
    varying = c(line_values, line_se),
    sep = "."
  )

  dat_long$y_pos <- dat_long$est_noTrend + dat_long$se_noTrend
  dat_long$y_neg <- dat_long$est_noTrend - dat_long$se_noTrend
  # dat_long$id <- 1:nrow(dat_long)
  #
  # dat_coords <- stats::reshape(
  #   dat_long,
  #   direction = "long",
  #   varying = c("y_pos", "y_neg"),
  #   sep = "_"
  # )

  # dat_coords <- dat_coords[order(dat_coords$y), ]
  # dat_coords <- dat_coords[order(dat_coords$est_noTrend), ]
  # dat_coords <- dat_coords[dat_coords$years_Trend == "20112013",]


  ## Kleiner Test:
  # ggplot2::ggplot() +
  ggplot2::geom_ribbon(data = dat_long,
                       ggplot2::aes(
                         x = est_noTrend,
                         ymin = y_neg,
                         ymax = y_pos,
                         group = .data$years_Trend
                       ))
  ## Alte Linien:
  # ggplot2::geom_segment(
  #   data = dat,
  #   ggplot2::aes(
  #     x = .data$year_start,
  #     xend = .data$year_end,
  #     y = .data[[line_values[1]]],
  #     yend = .data[[line_values[2]]],
  #     group = .data$years_Trend
  #   ),
  #   linewidth = 1,
  #   color = grDevices::rgb(147, 205, 221,
  #     maxColorValue = 255
  #   )
  # )
}
