#' Plot background lines.
#'
#' @param dat Data.
#' @inheritParams plot_lineplot
#'
#' @return ggplot2 object.
#' @export
#'
#' @examples # tbd
plot_background_lines <- function(dat, line_values, line_se, plot_settings = plotsettings_lineplot()) {
  line_values <- paste0(line_values, "_wholeGroup")
  colnames_se <- if (is.null(line_se)){gsub("est_", "se_", line_values)
    }else{line_se}

  if (all(colnames_se %in% colnames(dat))) {
    colnames(dat) <- .trimNames(colnames(dat))
    line_values <- .trimNames(line_values)
    colnames_se <- .trimNames(colnames_se)

    colnames(dat) <- gsub("_start", ".Start", colnames(dat))
    colnames(dat) <- gsub("_end", ".End", colnames(dat))

    est_col <- gsub("\\..*", "", line_values[1])
    se_col <- gsub("\\..*", "", colnames_se[1])


    dat_long <- stats::reshape(
      dat,
      direction = "long",
      varying = c(line_values, colnames_se, "year.Start", "year.End"),
      sep = "."
    )

    dat_long$y_pos <- dat_long[, est_col] + dat_long[, se_col]
    dat_long$y_neg <- dat_long[, est_col] - dat_long[, se_col]


    # ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = dat_long,
      ggplot2::aes(
        x = .data$year,
        ymin = .data$y_neg,
        ymax = .data$y_pos,
        group = .data$years_Trend
      ),
      fill = plot_settings$background_line_colour,
      linewidth = 0.1
    )
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
      color =  plot_settings$background_line_colour
    )
  }
}

.trimNames <- function(name_vector) {
  name_vector <- gsub("Start.*", ".Start", name_vector)
  name_vector <- gsub("End.*", ".End", name_vector)
  name_vector
}
