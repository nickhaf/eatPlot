#' Construct labels
#'
#' This function creates a new column or label, merging estimates and standard errors with significant estimates represented in bold or as superscript (via `brace_label_est`), and standard errors in brackets, if needed (via `brace_label_se`). NAs are converted to empty strings. Main usage is for plotting tables and brace labels.
#'
#' @inheritParams plot_lineplot
#' @param dat Data frame with the columns that should be merged into labels.
#' @param brace_label_sig_high_extra_column Logical, if set 'FALSE' the superscript for significant values is added directly into the label (necessary for line plots), if set 'TRUE' the superscript for significant values is written into an extra column with the ending '_sig_superscript' (necessary for tables).
#' @param new_name Character string for the new column that is added to `dat`. Defaults to `'label'`.
#' @param round_est Rounding of brace_label_est.
#' @param round_se Rounding of brace_label_se.
#'
#' @return The data frame with an added column for the constructed label.
#' @export
#'
#' @examples
#' # example data frame
#' dat <- data.frame(
#'   names = c("Berlin", "Hamburg", "Hessen", "Niedersachsen", "Saarland"),
#'   estimate = c(400, 650, 380, 500, 600),
#'   se = c(0.1, 0.45, 1, 0.27, 0.9),
#'   p_estimate = c(FALSE, FALSE, TRUE, TRUE, FALSE)
#'   )
construct_label <- function(dat,
                            column_est = NULL,
                            column_se = NULL,
                            column_sig_bold = NULL,
                            column_sig_superscript = NULL,
                            sig_superscript_letter = NULL,
                            round_est = 0,
                            round_se = 1,
                            plot_settings = plotsettings_tablebarplot()) {

  dat <- fill_column(dat, column_name = column_est, filling = "")
  if(is.null(column_est)){column_est <- "column_est"}
  dat <- fill_column(dat, column_name = column_se, filling = NA)
  if(is.null(column_se)){column_se <- "column_se"}
  dat <- fill_column(dat, column_name = column_sig_bold, filling = FALSE)
  if(is.null(column_sig_bold)){column_sig_bold <- "column_sig_bold"}
  dat <- fill_column(dat, column_name = column_sig_superscript, filling = FALSE)
  if(is.null(column_sig_superscript)){column_sig_superscript <- "column_sig_superscript"}


  if (any(is.na(dat[, c(column_sig_bold, column_sig_superscript)]))) {
    for (i in c(column_sig_bold, column_sig_superscript)) {
      dat[is.na(dat[, i]), i] <- FALSE
    }
  }

  if (is.numeric(dat[, column_est])) {
    dat[, column_est] <- format(round(dat[, column_est], round_est),
                            trim = TRUE,
                            nsmall = round_est)
    dat[, column_est][dat[, column_est] == "NA"] <- ""
  }

  if (is.numeric(dat[, column_se])) {
    dat[, column_se] <- format(round(dat[, column_se], round_se),
                           trim = TRUE,
                           nsmall = round_se)
    dat[, column_se][dat[, column_se] == "NA"] <- ""
  }

  label_est <- ifelse(dat[, column_sig_bold] == TRUE & dat[, column_est] != "",
    paste0("**", dat[, column_est], "**"),
    dat[, column_est]
  )

  label_superscript <- ifelse(dat[, column_sig_superscript] == TRUE & dat[, column_est] != "",
                          paste0("<sup>", sig_superscript_letter, "</sup>"),
                          "")

  label_se <- ifelse(
    !is.na(dat[, column_se]),
    paste0(" (", dat[, column_se], ")"),
    ""
    )


    label_out <- paste0(
      label_est,
      label_superscript,
      label_se
    )
  # } else {
  #   ## For alignment it is necessary to first plot the numbers and then add the superscript. Therefore it is saved in an additional column.
  #   if (any(dat$label_sig != "")) {
  #     dat[, paste0(new_name, "_sig_superscript")] <- dat$label_sig
  #   }
  #   dat[, new_name] <- paste0(
  #     dat[, column_est],
  #     dat[, column_se]
  #   )
  # }
  #
  # dat <- remove_columns(dat, cols = c("brace_label_est", "label_sig", "brace_label_se", "brace_label_sig_bold", "brace_label_sig_high"))

  return(label_out)
}


#' Construct labels
#'
#' This function creates a new column or label, merging estimates and standard errors with significant estimates represented in bold or as superscript (via `label_est`), and standard errors in brackets, if needed (via `label_se`). NAs are converted to empty strings. Main usage is for plotting tables and brace labels.
#'
#' @inheritParams plot_lineplot
#' @param dat Data frame with the columns that should be merged into labels.
#' @param label_sig_high_extra_column Logical, if set 'FALSE' the superscript for significant values is added directly into the label (necessary for line plots), if set 'TRUE' the superscript for significant values is written into an extra column with the ending '_sig_superscript' (necessary for tables).
#' @param new_name Character string for the new column that is added to `dat`. Defaults to `'label'`.
#' @param round_est Rounding of label_est.
#' @param round_se Rounding of label_se.
#'
#' @return The data frame with an added column for the constructed label.
#' @export
#'
#' @examples
#' # example data frame
#' dat <- data.frame(
#'   names = c("Berlin", "Hamburg", "Hessen", "Niedersachsen", "Saarland"),
#'   estimate = c(400, 650, 380, 500, 600),
#'   se = c(0.1, 0.45, 1, 0.27, 0.9),
#'   p_estimate = c(FALSE, FALSE, TRUE, TRUE, FALSE)
#'   )
construct_label_2 <- function(dat,
                            new_name = "label",
                            label_est = NULL,
                            label_se = NULL,
                            label_sig_bold = NULL,
                            label_sig_high = NULL,
                            label_sig_high_extra_column = FALSE,
                            round_est = 0,
                            round_se = 1,
                            plot_settings = plotsettings_tablebarplot()) {

  dat <- fill_column(dat, column_name = label_est, filling = "")
  dat <- fill_column(dat, column_name = label_se, filling = NA)
  dat <- fill_column(dat, column_name = label_sig_high, filling = FALSE)
  dat <- fill_column(dat, column_name = label_sig_bold, filling = FALSE)


  if (any(is.na(dat[, c("label_sig_high", "label_sig_bold")]))) {
    for (i in c("label_sig_high", "label_sig_bold")) {
      dat[is.na(dat[, i]), i] <- FALSE
    }
  }

  if (is.numeric(dat$label_est) & !is.null(round_est)) {
    dat$label_est <- format(round(dat$label_est, round_est),
                            trim = TRUE,
                            nsmall = round_est)
    dat$label_est[dat$label_est == "NA"] <- ""
  }
  if (is.numeric(dat$label_se) & !is.null(round_se)) {
    dat$label_se <- format(round(dat$label_se, round_se),
                           trim = TRUE,
                           nsmall = round_se)
    dat$label_se[dat$label_se == "NA"] <- ""

  }

  dat$label_est <- ifelse(dat$label_sig_bold == TRUE & dat$label_est != "",
                          paste0("**", dat$label_est, "**"),
                          dat$label_est
  )

  dat$label_sig <- ifelse(dat$label_sig_high == TRUE & dat$label_est != "",
                          paste0("<sup>", plot_settings$columns_table_sig_high_letter, "</sup>"), "")

  dat$label_se <- ifelse(!is.na(dat$label_se) & dat$label_se != "",
                         paste0(" (", dat$label_se, ")"),
                         ""
  )

  dat[, c("label_est", "label_sig", "label_se")][is.na(dat[, c("label_est", "label_sig", "label_se")]) | dat[, c("label_est", "label_sig", "label_se")] == "NA"] <- ""

  if (label_sig_high_extra_column == FALSE) {
    dat[, new_name] <- paste0(
      dat$label_est,
      dat$label_sig,
      dat$label_se
    )
  } else {
    ## For alignment it is necessary to first plot the numbers and then add the superscript. Therefore it is saved in an additional column.
    if (any(dat$label_sig != "")) {
      dat[, paste0(new_name, "_sig_superscript")] <- dat$label_sig
    }
    dat[, new_name] <- paste0(
      dat$label_est,
      dat$label_se
    )
  }

  dat <- remove_columns(dat, cols = c("label_est", "label_sig", "label_se", "label_sig_bold", "label_sig_high"))

  return(dat)
}
