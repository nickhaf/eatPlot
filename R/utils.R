is_colour <- function(x) {
  vapply(x, function(X) {
    tryCatch(is.matrix(grDevices::col2rgb(X)),
             error = function(e) FALSE)
  },
  USE.NAMES = FALSE,
  FUN.VALUE = logical(1))
}


calc_sig <- function(p_vec, sig_niveau) {
  res <- ifelse(is.na(p_vec), FALSE, # NA
    ifelse(p_vec < sig_niveau & !is.na(p_vec), TRUE, FALSE)
  )

  return(res)
}


filter_strings <- function(identifier, paste_vec, val_vec) {
  if (any(duplicated(val_vec))) {
    stop("Duplicated groups. For example, there might be two groups of the same type within the same Bundesland.")
  } else {
    res <- sapply(identifier, function(x) {
      grep(paste0(x, paste_vec), val_vec)
    },
    USE.NAMES = FALSE
    )
  }
  return(res)
}

build_column <- function(dat, old, new) {
  check_column(dat, old)
  if (is.null(old)) {
    dat[, new] <- NA
    return(dat)
  } else {
    #colnames(dat)[colnames(dat) == old] <- new
    dat[, new] <- dat[ , old]

    return(dat)
  }
}

remove_columns <- function(dat, cols) {
  dat <- dat[, !(colnames(dat) %in% cols), drop = FALSE]
  return(dat)
}



## Find the years that can be plotted as trend. Returns all unique consecutive year combinations.
consecutive_numbers <- function(vec) {
  vec_ordered <- vec[order(vec)]
  res <- list()
  i <- 1
  while (i < length(vec_ordered)) {
    res_numbers <- c(vec_ordered[i], vec_ordered[i + 1])
    if (res_numbers[[1]] != res_numbers[[2]]) {
      res[[i]] <- res_numbers
      i <- i + 1
    } else {
      i <- i + 1
    }
  }

  res <- unique(Filter(Negate(is.null), res))
  return(res)
}



# extractor for specific types of rows ------------------------------------

get_group <- function(val_vec, groups, starts_with = "", ends_with = "", log_res = TRUE) {
  if (log_res == TRUE) {
    grepl(
      paste0(
        paste0(starts_with, groups, ends_with),
        collapse = "|"
      ),
      val_vec
    )
  } else {
    grep(
      paste0(
        paste0(starts_with, groups, ends_with),
        collapse = "|"
      ),
      val_vec,
      value = TRUE
    )
  }
}

get_wholeGroup <- function(val_vec) {
  grepl("wholeGroup", val_vec)
}


# Extract group membership from group column. Splits String by "." and extracts the first value that is found in the group_vector
write_group <- function(val_vec, groups) {
  val_vec <- gsub("_", "\\.", val_vec)
  group_vec <- strsplit(val_vec, split = "\\.")

res_vec <- unlist(
    lapply(group_vec, function(x) {
      log_vec <- x %in% groups
      if (all(log_vec == FALSE)) {
        res <- NA
      } else {
        res <- x[log_vec][1]
      }
      return(res)
    })
  )

return(res_vec)
}


# Helper functions for reshaping to long format ---------------------------
prep_long <- function(data, include_pattern, remove_pattern = NULL, suffix = "") {
  if (!is.null(remove_pattern)) {
    cols_removed <- grep(remove_pattern, colnames(data), invert = TRUE, value = TRUE)
    data <- data[, cols_removed]
  }

  col_pos <- grep(include_pattern, colnames(data))
  colnames(data)[col_pos] <- gsub("\\.|_", "", colnames(data)[col_pos])

  ## before first number, insert ".". Needed by reshape() for automatically building the new columns.
  colnames(data)[col_pos] <- sapply(colnames(data)[col_pos], function(x) insert_first_number(x, insertion = "\\."))

  data_long <- stats::reshape(data, direction = "long", varying = colnames(data)[col_pos])
  data_long$id <- NULL

  # put suffix on all new columns containing the values:
  new_colnames <- colnames(data_long)[!(colnames(data_long) %in% colnames(data))]
  for (i in new_colnames) {
    data_long <- build_column(data_long, old = i, new = paste0(i, suffix))
  }

  colnames(data_long) <- gsub("\\.", "_", colnames(data_long))
  colnames(data_long) <- gsub("trend", "_trend", colnames(data_long))
  data_long <- build_column(data_long, old = paste0("time", suffix), new = "year")

  return(data_long)
}

## Split the time column with the two comparisons years into two columns, so start- and endyear both have a seperate column
split_years <- function(dat) {
  years <- regmatches(dat$year, gregexpr("[[:digit:]]+", dat$year))

  # extract the years and add them to the long data frame
  year_cols <- data.frame()

  for (i in 1:length(years)) {
    year_cols <- rbind(year_cols, as.numeric(years[[i]]))
  }
  colnames(year_cols) <- c("year_start", "year_end")
  dat <- cbind(dat, year_cols)
  dat$trend <- paste0(dat$year_start, dat$year_end)
  dat <- build_column(dat, "year", "trend_years")

  return(dat)
}


calc_plot_borders <- function(x, accuracy = 10) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)

  borders <- vapply(c(min_x, max_x), function(y) {
    if (y < 0) {
      plyr::round_any(y, accuracy = accuracy, floor)
    } else {
      plyr::round_any(y, accuracy = accuracy, ceiling)
    }
  },
  FUN.VALUE = numeric(1)
  )

  max_border <- max(abs(borders[1]), abs(borders[2]))

  return(c(-1 * max_border, max_border))
}



insert_first_number <- function(char_string, insertion) {
  string_number <- unique(unlist(regmatches(char_string, gregexpr("[[:digit:]]+", char_string))))[[1]]
  res <- sub(string_number, paste0(insertion, string_number), char_string)
  return(res)
}



## Split all groups containing "vs" to get the respective comparisons
get_comparisons <- function(dat, states, sub_groups) {
  comparisons_log <- grepl("\\.vs\\.", dat$group_var)

  dat[comparisons_log, "compare_1"] <- sapply(strsplit(dat[comparisons_log, "group_var"], split = "\\.vs\\."), function(x) {
    x[[1]]
  })
  dat[comparisons_log, "compare_2"] <- sapply(strsplit(dat[comparisons_log, "group_var"], split = "\\.vs\\."), function(x) {
    x[[2]]
  })

  for (i in c("compare_1", "compare_2")) {
    dat[, i] <- gsub(paste0(states, collapse = "|"), "BL", dat[, i])
    if (!is.null(sub_groups)) {
      dat[, i] <- gsub(paste0(sub_groups, collapse = "|"), "_groupingVar", dat[, i])
    }
    dat[, i] <- gsub("__|___", "_", dat[, i])
  }

  return(dat)
}


# Overlap occurs, when one start point lies between a start and end point, or an end point lies between a start and an end point

calc_overlap <- function(year_start, year_end) {
  years <- c()
  for (i in 1:length(year_start)) {
    years[i] <- any((year_start[i] > year_start[-i]) & (year_start[i] < year_end[-i]))
  }
  return(years)
}



## Function for checking which arguments are in the colnames, and returning those which are not
check_missing_colnames <- function(x, colnames_vec) {
  names(x[!x %in% colnames_vec])
}


get_min_max <- function(dat) {
  min_max_trend <- by(dat, dat$trend, function(x) {
    data.frame(
      trend = unique(x$trend),
      minimum = min(x$year),
      maximum = max(x$year)
    )
  })

  min_max_dat <- do.call(rbind, min_max_trend)

  return(min_max_dat)
}

check_column <- function(dat, column) {
  if (!is.null(column)) {
    if (!(column %in% colnames(dat))) {
      stop(paste0("Variable '", column, "' not found in data."))
    }
  }
}


fill_null <- function(dat, column_name, filling){
    dat[[column_name]] <- rep(filling, nrow(dat))
    return(dat)
}



build_column_2 <- function(df, column_name, filling = NA){

  if(!is.null(column_name)){
    df[[deparse(substitute(column_name))]] <- df[[column_name]]
  }else{
    df[[deparse(substitute(column_name))]] <- rep(filling, nrow(df))
  }
return(df)
  }

