% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functionpackage.R
\name{impute_missing_values}
\alias{impute_missing_values}
\title{Replace Missing Values with Column Mean}
\usage{
impute_missing_values(data)
}
\arguments{
\item{data}{A data frame. Numeric columns are analyzed for missing values.}
}
\value{
A data frame where NA values in numeric columns are replaced with the column mean.
}
\description{
This function replaces missing values (NA) in numeric columns of a data frame
with the mean of the respective column.
}
\examples{
sample_data <- data.frame(
print(sample_data)
}
