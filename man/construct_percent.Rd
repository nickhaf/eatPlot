% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct_percent.R
\name{construct_percent}
\alias{construct_percent}
\title{Multiply Values in Columns by 100 to build percentages.}
\usage{
construct_percent(df, columns)
}
\arguments{
\item{df}{Input data.frame.}

\item{columns}{Character vector with the column names of the columns you want to multiply by 100.}
}
\value{
The input data.frame with the new columns multiplied by 100. New columns have the suffix "_percent" in their name.
}
\description{
Multiply Values in Columns by 100 to build percentages.
}
\examples{
df <- data.frame(col_1 = c(1, 2), col_2 = c(3, 4), col_3 = c("a", "b"))
construct_percent(df, columns = c("col_1", "col_2"))
}
