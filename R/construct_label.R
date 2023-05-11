#' Build labels with significances represented in bold or as raised 'a', and, if wanted, standard errors in brackets. Main usage for plotting tables and brace labels.
#'
#' @inheritParams plot_lineplot
#' @param dat Dataframe with the columns that should be merged into labels.
#' @param new_name Character string for the new column that is added to `dat`. Defaults to `'label'`.
#' @param round_est Rounding of label_est.
#' @param round_se Rounding of label_se.
#'
#' @return The data.frame with an added column for the constructed label.
#' @export
#'
#' @examples #tbd
construct_label <- function(dat,
                            new_name = "label",
                            label_est = NULL,
                            label_se = NULL,
                            label_sig_bold = NULL,
                            label_sig_high = NULL,
                            round_est = 0,
                            round_se = 1) {
  dat <- fill_column(dat, column_name = label_est, filling = "")
  dat <- fill_column(dat, column_name = label_se, filling = NA)
  dat <- fill_column(dat, column_name = label_sig_high, filling = FALSE)
  dat <- fill_column(dat, column_name = label_sig_bold, filling = FALSE)


if(any(is.na(dat[, c("label_sig_high", "label_sig_bold")]))){
  for (i in c("label_sig_high", "label_sig_bold")) {
    dat[is.na(dat[, i]), i] <- FALSE
  }
}

  if (is.numeric(dat$label_est) & !is.null(round_est)) {
    dat$label_est <- format(round(dat$label_est, round_est), trim = TRUE)
  }
  if (is.numeric(dat$label_se) & !is.null(round_se)) {
    dat$label_se <- format(round(dat$label_se, round_se), trim = TRUE)
  }

  dat$label_est <- ifelse(dat$label_sig_bold == TRUE,
    paste0("**", dat$label_est, "**"),
    dat$label_est
  )

  dat$label_sig <- ifelse(dat$label_sig_high == TRUE, "<sup>a</sup>", "")

  dat$label_se <- ifelse(!is.na(dat$label_se),
    paste0(" (", dat$label_se, ")"),
    ""
  )

  dat[, c("label_est", "label_sig", "label_se")][is.na(dat[, c("label_est", "label_sig", "label_se")])] <- ""

  dat[, new_name] <- paste0(
    dat$label_est,
    dat$label_sig,
    dat$label_se
  )

  dat <- remove_columns(dat, cols = c("label_est", "label_sig", "label_se", "label_sig_bold", "label_sig_high"))

  return(dat)
}
