is_colour <- function(x) {
  vapply(x, function(X) {
    tryCatch(is.matrix(grDevices::col2rgb(X)),
      error = function(e) FALSE
    )
  },
  USE.NAMES = FALSE,
  FUN.VALUE = logical(1)
  )
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
  ## Umwandeln aller "_" in groups in "-"
  if (any(grepl("_", groups))) {
    stop("Your grouping_var or state_var contains '_', please use '-' instead.")
  }

  val_vec <- gsub("TR_BUNDESLAND=", "", val_vec)
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
  ## Sometimes it's necessary to remove some columns before reforming to long format:
  if (!is.null(remove_pattern)) {
    cols_removed <- grep(remove_pattern,
      colnames(data),
      invert = TRUE,
      value = TRUE
    )
    data <- data[, cols_removed]
  }

  col_pos <- grep(
    include_pattern,
    colnames(data)
  )
  colnames(data)[col_pos] <- gsub("\\.|_", "", colnames(data)[col_pos])


  ## before the first number of the year columns, insert ".". Needed by reshape() for automatically building the new columns.
  year_cols <- unlist(sapply(colnames(data)[col_pos], insert_first_number, "\\."))


  if (!is.null(year_cols)) {
    colnames(data)[col_pos] <- year_cols

    data_long <- stats::reshape(data,
      direction = "long",
      varying = colnames(data)[col_pos]
    )
    data_long$id <- NULL
  } else { ## In this case, there is no trend:
    colnames(data)[col_pos] <- paste0(colnames(data)[col_pos], "_noTrend")
    data_long <- data
    data_long$time <- "noTrend"

  }
  # put suffix on all new columns containing the values:
  new_colnames <- colnames(data_long)[!(colnames(data_long) %in% colnames(data))]

  data_long <- rename_columns(data_long,
    old_names = new_colnames,
    new_names = paste0(new_colnames, suffix)
  )

  data_long <- rename_columns(data_long, old_names = paste0("time", suffix), new_names = "year")
  colnames(data_long) <- gsub("\\.", "_", colnames(data_long))
  colnames(data_long) <- gsub("trend", "_trend", colnames(data_long))

  return(data_long)
}

## Split the time column with the two comparisons years into two columns, so start- and endyear both have a seperate column
split_years <- function(dat, year_col = "year") {
  years <- regmatches(dat[, year_col], gregexpr("[[:digit:]]+", dat[, year_col]))

  # extract the years and add them to the long data frame
  year_cols <- data.frame()

  for (i in 1:length(years)) {
    year_cols <- rbind(year_cols, as.numeric(years[[i]]))
  }
  colnames(year_cols) <- c("year_start", "year_end")
  dat <- cbind(dat, year_cols)
  dat[, year_col] <- paste0(dat$year_start, dat$year_end)

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
  string_number <- unique(unlist(regmatches(char_string, gregexpr("[[:digit:]]+", char_string))))

  if (length(string_number != 0)) {
    res <- sub(string_number[[1]], paste0(insertion, string_number[[1]]), char_string)
  } else {
    res <- NULL
  }
  return(res)
}



## Split all groups containing "vs" to get the respective comparisons

## Problem: sub_groups mit vs-grouping_var

get_comparisons <- function(dat, states, sub_groups) {
  if(!is.null(sub_groups)){
  sub_groups <- unique(unlist(strsplit(levels(sub_groups), split = "\\.vs\\.")))
}
  dat$group_var <- gsub("TR_BUNDESLAND=", "", dat$group_var)

  comparisons_log <- grepl("\\.vs\\.", dat$group_var)

  dat$group_var <- replace_VS(dat$group_var)

  dat[comparisons_log, "compare_1"] <- sapply(strsplit(dat[comparisons_log, "group_var"], split = "\\.VS\\."), function(x) {
    x[[1]]
  })
  dat[comparisons_log, "compare_2"] <- sapply(strsplit(dat[comparisons_log, "group_var"], split = "\\.VS\\."), function(x) {
    x[[2]]
  })

  for (i in c("compare_1", "compare_2")) {
    if (!is.null(sub_groups)) {
    for (j in seq_along(sub_groups)) {
      group_j <- sub_groups[j]
      dat[, i] <- gsub(
        pattern = paste0(
          paste0("_", group_j),
          paste0("*", group_j),
          collapse = "|"
          ),
       replacement = paste0(
          "_grouping_var_",
          letters[j]),
        x = dat[, i])
    }
}
    dat[, i] <- gsub(paste0(states, collapse = "|"), "BL", dat[, i])
    dat[, i] <- gsub("__|___", "_", dat[, i])

    dat[is.na(dat[, i]), i] <- "no_comp"
  }

    ## Check if comparison is one of group in state vs. the group in wholeGroup

  dat$compare_2 <- ifelse(dat$grouping_var == dat$compare_2 & !is.na(dat$grouping_var) & !is.na(dat$compare_2) & dat$grouping_var != "no_comp" & dat$compare_2 != "no_comp",
    "wholeGroupSameGroup",
    dat$compare_2
  )

  return(dat)
}


# x = character string
replace_VS <- function(x) {
  if (!inherits(x, "character")) {
    stop("'x' must be a character vector.")
  }
  y <- strsplit(x, split = ".vs.")

  len2 <- which(sapply(y, length) == 2)
  if (length(len2) > 0) {
    for (i in len2) {
      x[i] <- paste0(y[[i]][1], ".VS.", y[[i]][2])
    }
  }

  len4 <- which(sapply(y, length) == 4)
  if (length(len4) > 0) {
    for (i in len4) {
      x[i] <- paste0(y[[i]][1], ".vs.", y[[i]][2], ".VS.", y[[i]][3], ".vs.", y[[i]][4])
    }
  }
  return(x)
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
  min_max_trend <- by(dat, dat$years_Trend, function(x) {
    data.frame(
      years_Trend = unique(x$years_Trend),
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


fill_null <- function(dat, column_name, filling) {
  dat[[column_name]] <- rep(filling, nrow(dat))
  return(dat)
}


## Add a new column that is derived from an old one. Takes characters as input.
build_column <- function(dat, old, new) {
  check_column(dat, old)
  if (is.null(old)) {
    dat[, new] <- NA
    return(dat)
  } else {
    # colnames(dat)[colnames(dat) == old] <- new
    dat[, new] <- dat[, old]

    return(dat)
  }
}




## Add a new column that is derived from an old one. Takes an object as input.
fill_column <- function(df, column_name, filling = NA) {
  if (is.null(column_name)) {
    df[[deparse(substitute(column_name))]] <- rep(filling, nrow(df))
  } else if (column_name %in% colnames(df)) {
    df[[deparse(substitute(column_name))]] <- df[[column_name]]
  } else if ((!column_name %in% colnames(df))) {
    warning(paste0("Your column '", column_name, "' is not part of your data. Trying to set it automatically."))
    df[[deparse(substitute(column_name))]] <- rep(filling, nrow(df))
  }
  return(df)
}

fill_na <- function(df, column_name, filling) {
  if (is.null(column_name)) {
    return(df)
  } else {
    df[is.na(df[, column_name]), column_name] <- filling
    return(df)
  }
}

check_columns <- function(dat, column) {
  if (column %in% colnames(dat)) {
    return(column)
  } else {
    warning(paste0("The column '", column, "' was not found in data and will not be considered for the plot."))
    return(NULL)
  }
}

check_factor <- function(dat, column, variable_type) {
  if (!is.factor(dat[, column]) & !is.null(column)) {
    message("Your ", variable_type, " '", column, "' is not a factor. It will be sorted alphabetically, which might result in an unwanted factor order.")
    dat[, column] <- as.factor(dat[, column])
  }
  return(dat)
}

rename_columns <- function(dat, old_names, new_names) {
  colnames(dat)[colnames(dat) %in% old_names] <- new_names

  return(dat)
}


merge_2 <- function(dat_1, dat_2, ...) {
  if (is.null(dat_1) | is.null(dat_2)) {
    return(data.frame())
  }
  if (nrow(dat_1) == 0 & nrow(dat_2) == 0) {
    return(data.frame())
  }
  if (sum(nrow(dat_1) == 0 | nrow(dat_2) == 0) == 1) {
    if (nrow(dat_1) > nrow(dat_2)) {
      return(dat_1)
    } else {
      return(dat_2)
    }
  }

  dat_merged <- merge(dat_1, dat_2, ...)
}

get_plot_coords <- function(plot) {
  diff(ggplot2::layer_scales(plot)$x$get_limits())
}

sub_dash <- function(vec){
  if(is.character(vec)){
  vec <- gsub("-", "\uad", vec)
  }
  return(vec)
}

# Colours should be displayed in the order they are put in:
construct_colour_scale <- function(colours, dat, colname){
  if(is.null(names(colours)) & colname %in% colnames(dat)){
    names(colours) <- unique(dat[, colname])
  }
return(colours)
  }

