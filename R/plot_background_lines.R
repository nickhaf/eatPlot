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

  line_values <- paste0(line_values, "_wholeGroup")
  colnames_se <- if (is.null(line_se)) gsub("est_", "se_", line_values) else line_se

  if (all(colnames_se %in% colnames(dat))) {

    colnames(dat) <- .trimNames(colnames(dat))
    line_values <- .trimNames(line_values)
    colnames_se <- .trimNames(colnames_se)

    colnames(dat) <- gsub("_start", ".Start", colnames(dat))
    colnames(dat) <- gsub("_end", ".End", colnames(dat))

    dat_long <- stats::reshape(
      dat,
      direction = "long",
      varying = c(line_values, colnames_se, "year.Start", "year.End"),
      sep = "."
    )

    dat_long <- within(dat_long, {
      y_pos <- est_noTrend + se_noTrend
      y_neg <- est_noTrend - se_noTrend
    })

    # ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = dat_long,
                         ggplot2::aes(
                           x = year,
                           ymin = y_neg,
                           ymax = y_pos,
                           group = .data$years_Trend
                         ),
                         fill = grDevices::rgb(147, 205, 221,
                                               maxColorValue = 255),
                         linewidth = 1)
  } else {

    message("Plotting only the lines for the estimates as no SE column was found. Please check if this was not intended.")

    # ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = dat,
      ggplot2::aes(
        x = .data$year_start,
        xend = .data$year_end,
        y = .data[[line_values[1]]],
        yend = .data[[line_values[2]]],
        group = .data$years_Trend
      ),
      linewidth = 1,
      color = grDevices::rgb(147, 205, 221,
        maxColorValue = 255
      )
    )
  }
}

.trimNames <- function(name_vector) {
  name_vector <- gsub("Start.*", ".Start", name_vector)
  name_vector <- gsub("End.*", ".End", name_vector)
  name_vector
}

# df_backgroundlines <- data.frame(
#   TR_BUNDESLAND = rep("wholeGroup", 2),
#   year_start = c(2011, 2013),
#   year_end = c(2013, 2016),
#   est_noTrendStart_noComp_wholeGroup = c(10:11),
#   est_noTrendEnd_noComp_wholeGroup = c(11:12),
#   se_noTrendStart_noComp_wholeGroup = c(0.5, 1.2),
#   se_noTrendEnd_noComp_wholeGroup = c(0.1, 0.9),
#   years_Trend = c("20112013", "20132016")
# )
# dat = df_backgroundlines
# line_values = c("est_noTrendStart_noComp_wholeGroup", "est_noTrendEnd_noComp_wholeGroup")
# line_se = c("se_noTrendStart_noComp_wholeGroup", "se_noTrendEnd_noComp_wholeGroup")

