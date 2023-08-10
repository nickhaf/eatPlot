#' Check if character string is a colour.
#'
#' @keywords internal
#' @noRd
#' @param x Character string.
#'
#' @return Logical.
#'
#' @examples is_colour("black")
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

#' Calculate if a p-value is significant.
#'
#' @keywords internal
#' @noRd
#'
#' @param p_vec Numeric vector of p-values.
#' @param sig_niveau Significance niveau. All values smaller will be considered significant.
#'
#' @return Logical vector.
#'
#' @examples calc_sig(c(0.05, 0.01, 0.1, NA), 0.05)
calc_sig <- function(p_vec, sig_niveau) {
  res <- ifelse(is.na(p_vec),
    yes = FALSE,
    no = ifelse(p_vec < sig_niveau & !is.na(p_vec),
      yes = TRUE,
      no = FALSE
    )
  )
  return(res)
}

#' Remove columns from a data.frame.
#'
#' @keywords internal
#' @noRd
#'
#' @param dat Data.frame.
#' @param cols Character vector of columns that should be removed.
#'
#' @return Data.frame without the columns specified in cols.
#'
#' @examples # tbd
remove_columns <- function(dat, cols) {
  dat <- dat[, !(colnames(dat) %in% cols), drop = FALSE]
  return(dat)
}


#' Find all consecutive numbers in a vector. Needed for automatically setting the Trend years.
#'
#' @keywords internal
#' @noRd
#'
#' @param vec Numeric vector.
#'
#' @return List containing all consecutive number combinations.
#'
#' @examples # tbd
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


#' Extract group membership from group column.
#'
#' Splits vec by "." and extracts the first value that is found in groups.
#'
#' @keywords internal
#' @noRd
#'
#' @param vec Character vector, usually the group-column in a data.frame returned by [eatRep::report()].
#' @param groups Character vector of groups that should be extracted from vec.
#'
#' @return Returns the first group found in vec.
#'
#' @examples # tbd
write_group <- function(vec, groups) {
  ## "_" in groups is used as divider, so
  if (any(grepl("_", groups))) {
    stop("Your grouping_var or state_var contains '_', please use '-' instead.")
  }

  vec <- gsub("TR_BUNDESLAND=", "", vec)
  vec <- gsub("_", "\\.", vec)
  group_vec <- strsplit(vec, split = "\\.")

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


#' Reshape prepared data to long format.
#'
#' @keywords internal
#' @noRd
#'
#' @param data Dataframe that should be reshaped.
#' @param include_pattern Character string of patterns in column names with values that should be reshaped into long format.
#' @param remove_pattern Character string of patterns in column names that should be removed before reshaping. E.g., all trend columns.
#' @param suffix Character string to put at the end of all new (reshaped) columns.
#'
#' @return Data.frame in long format.
#'
#' @examples # tbd
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

  data_long <- rename_columns(data_long,
    old_names = paste0("time", suffix),
    new_names = "year"
  )

  colnames(data_long) <- gsub("\\.", "_", colnames(data_long))
  colnames(data_long) <- gsub("trend", "_trend", colnames(data_long))

  return(data_long)
}

## Split the time column with the two comparisons years into two columns, so start- and endyear both have a seperate column
#' Split Trend column in two seperate year columns.
#'
#' @keywords internal
#' @noRd
#'
#' @param dat Input data.frame.
#' @param year_col Trend column with two years in it.
#'
#' @return The data.frame with a `year_start` and a `year_end` column, derived from the `year_col`.
#'
#' @examples # tbd
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


#' Calculate the x-axis range for bar-plots.
#'
#' @keywords internal
#' @noRd
#'
#' @param x Numeric vector that is plotted on the x-axis.
#' @param accuracy Numeric for rounding the borders. Defaults to `10`, so the plot borders will be divisible by `10`.
#'
#' @return Numeric vector with the min and max values of the x axis.
#'
#' @examples # tbd
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


#' Insert a character string in front of the first number in a character string.
#'
#' @keywords internal
#' @noRd
#'
#' @param char_string Character string that gets the insertion.
#' @param insertion Character to insert before the first number in `char_string`.
#'
#' @return A character string with an insertion in front of the first number.
#'
#' @examples insert_first_number("test2013b", "\\.")
insert_first_number <- function(char_string, insertion) {
  string_number <- unique(unlist(regmatches(char_string, gregexpr("[[:digit:]]+", char_string))))

  if (length(string_number != 0)) {
    res <- sub(string_number[[1]], paste0(insertion, string_number[[1]]), char_string)
  } else {
    res <- NULL
  }
  return(res)
}


get_comparisons <- function(dat, states, sub_groups) {
  if (!is.null(sub_groups)) {
    sub_groups <- unique(unlist(strsplit(levels(sub_groups), split = "\\.vs\\.")))
  }
  dat$group_var <- gsub("TR_BUNDESLAND=|___", "", dat$group_var)

  comparisons_log <- grepl("\\.vs\\.", dat$group_var)

  dat$group_var <- replace_VS(dat$group_var)

  ## if .vs. in the string, delete the second comparison part, it is redundant, as the next comparison is always of these groups against these groups in the wholeGroup
  dat$group_var <- ifelse(grepl("\\.vs\\.", dat$group_var),
                          gsub("\\.VS\\..*", "", dat$group_var),
                          dat$group_var)

dat$group_var <- gsub("\\.vs\\.", "\\.VS\\.", dat$group_var)

  dat[comparisons_log, "compare_1"] <- sapply(strsplit(dat[comparisons_log, "group_var"], split = "\\.VS\\."), function(x) {
    x[[1]]
  })
  dat[comparisons_log, "compare_2"] <- sapply(strsplit(dat[comparisons_log, "group_var"], split = "\\.VS\\."), function(x) {
    x[[2]]
  })

  for (i in c("compare_1", "compare_2")) {
    dat[, i] <- gsub(paste0(states, collapse = "|"), "BL", dat[, i])
    dat[, i] <- gsub("__|___|", "_", dat[, i], fixed = TRUE)
    dat[, i] <- gsub(".vs.", "vs", dat[, i])

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

#' Calculate, if year columns are overlapping.
#'
#' They overlap, if one of the start or end years lies between another start and end year.
#'
#' @keywords internal
#' @noRd
#'
#' @param year_start Numeric vector.
#' @param year_end Numeric vector.
#'
#' @return Logical vector the same length as `year_start` with a `TRUE` if the respective year is overlapping.
#'
#' @examples calc_overlap(c(2010, 2012, 2013), c(2015, 2016, 2015))
calc_overlap <- function(year_start, year_end) {
  overlap <- c()
  for (i in 1:length(year_start)) {
    overlap[i] <- any((year_start[i] > year_start[-i]) & (year_start[i] < year_end[-i]))
  }
  return(overlap)
}


#' Get smallest and largest year of each Trend.
#'
#' @keywords internal
#' @noRd
#'
#' @param dat Data.frame with a `year` and a `years_Trend` column.
#'
#' @return Data.frame with minimal and maximal year for each Trend.
#'
#' @examples # tbd
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

#' Check if column is part of data.frame and return error if not.
#'
#' @keywords internal
#' @noRd
#'
#' @param dat Data.frame.
#' @param column Character string of a column name that should be checked.
#'
#' @return Error if column is not part of `dat`.
#'
#' @examples # tbd
check_column <- function(dat, column) {
  if (!is.null(column)) {
    if (!(column %in% colnames(dat))) {
      stop(paste0("Variable '", column, "' not found in data."))
    }
  }
}


#' Check if column is part of the data.frame and return warning and `NULL` if not.
#'
#' @keywords internal
#' @noRd
#'
#' @param dat Data.frame.
#' @param column Character string of a column name that should be checked.
#'
#' @return The column if it is part of the data, or `NULL` if not.
#'
#' @examples # tbd
check_column_warn <- function(dat, column) {
  if (column %in% colnames(dat)) {
    return(column)
  } else {
    warning(paste0("The column '", column, "' was not found in data and will not be considered for the plot."))
    return(NULL)
  }
}

#' Add a new column that is derived from an old one.
#'
#' Copy a column or insert a new `NA` column.
#'
#' @keywords internal
#' @noRd
#'
#' @param dat Data.frame.
#' @param old Character string of column name in dat.
#' @param new Character string of new column.
#'
#' @return Data.frame with a new column, either copyed or `NA`.
#'
#' @examples # tbd
build_column <- function(dat, old, new) {
  check_column(dat, old)
  if (is.null(old)) {
    dat[, new] <- NA
    return(dat)
  } else {
    dat[, new] <- dat[, old]

    return(dat)
  }
}


#' Build new column with predefined values.
#'
#' @keywords internal
#' @noRd
#'
#' @param df Data.frame.
#' @param column_name Character string of the column name.
#' @param filling Character string that will fill the column if it is not part of `df` originally.
#'
#' @return Data.frame with a new column.
#'
#' @examples # tbd
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


#' Replace NAs in a data.frame with specified values.
#'
#' @keywords internal
#' @noRd
#'
#' @param df Data.frame.
#' @param column_name Character string of a column in `df`.
#' @param filling Character string or numeric for replacing `NAs`.
#'
#' @return Data.frame with altered column.
#'
#' @examples # tbd
fill_na <- function(df, column_name, filling) {
  if (is.null(column_name)) {
    return(df)
  } else {
    df[is.na(df[, column_name]), column_name] <- filling
    return(df)
  }
}



#' Check if data.frame column is a factor and shape into factor if not.
#'
#' @keywords internal
#' @noRd
#'
#' @param dat Data.frame.
#' @param column Character string for a column name in `dat`.
#' @param variable_type Character string of the function argument this column was given to (necessary for message).
#'
#' @return Data.frame with the column as factor.
#'
#' @examples # tbd
check_factor <- function(dat, column, variable_type) {
  if (!is.factor(dat[, column]) & !is.null(column)) {
    message("Your ", variable_type, " '", column, "' is not a factor. It will be sorted alphabetically, which might result in an unwanted factor order.")
    dat[, column] <- as.factor(dat[, column])
  }
  return(dat)
}

#' Rename Columns
#'
#' @keywords internal
#' @noRd
#'
#' @param dat Data.frame with columns to be renamed.
#' @param old_names Character vector of column names that should be renamed.
#' @param new_names Character vector of the new column names.
#'
#' @return dataframe
#'
#' @examples #tbd
rename_columns <- function(dat, old_names, new_names) {
  if(!is.data.frame(dat)){
    stop("dat has to be a data.frame")
  }else{
  colnames(dat)[colnames(dat) %in% old_names] <- new_names
  }
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

#' Takes a [ggplot]-plot as input and extracts the range of the x-axis coordinates.
#'
#' @keywords internal
#' @noRd
#'
#' @param plot A [ggplot]-plot from which to extract the range of the x-axis coordinates.
#'
#' @return Numeric value of the range of the x-axis coordinates of the given `plot`.
#'
#' @examples
#' example_plot <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
#' get_x_range(example_plot)
get_x_range <- function(plot) {
  diff(ggplot2::layer_scales(plot)$x$get_limits())
}

#' Changes dashes in character vectors for correct plotting: en-dashes will be subbed by "\uad", so they are displayed correctly in the plotted PDF.
#'
#' @keywords internal
#' @noRd
#'
#' @param vec Character vector from which all dashes are to be changed.
#'
#' @return Character vector subbed by "\uad".
#'
#' @examples sub_dash(c("a-b-c", "e-f-d"))
sub_dash <- function(vec) {
  if (is.character(vec)) {
    vec <- gsub("-", "\uad", vec)
  }
  return(vec)
}

# Colours should be displayed in the order they are put in:
construct_colour_scale <- function(colours, dat, colname) {
  if (is.null(names(colours)) & colname %in% colnames(dat)) {
    names(colours) <- unique(dat[, colname])
  }
  return(colours)
}



#' Get the maximum occurence of a word in a vector of character strings.
#'
#' @keywords internal
#' @noRd
#'
#' @param string Vector of character strings. If a named list is provided, the list names are taken instead of the string.
#' @param word Character string that is counted.
#'
#' @return Maximum occurence of the word that is searched for.
#'
#' @examples count_words(c("Test word", "word word, Test word"), "word")
count_words <- function(string, word){

  if(inherits(string, "list") & !is.null(names(string))){
    string <- names(string)
  }

max(unlist(lapply(string, function(x){
  lengths(regmatches(x, gregexpr(word, x)))
})))
}


#' Define the scale breaks from plot borders.
#'
#' @keywords internal
#' @noRd
#'
#' @param plot_borders Numeric vector containing the outer limits of the plot.
#'
#' @return Numeric vector with the scale breaks.
#'
#' @examples set_scale_breaks(c(-100, 50))
set_scale_breaks <- function(plot_borders){
  scale_breaks <- unique(c(
    seq(0, plot_borders[1], by = -10),
    seq(0, plot_borders[2], by = 10)
  ))
  return(scale_breaks)
}
