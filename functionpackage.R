#' Replace Missing Values with Column Mean
#'
#' This function replaces missing values (NA) in numeric columns of a data frame
#' with the mean of the respective column.
#'
#' @param data A data frame. Numeric columns are analyzed for missing values.
#' @return A data frame where NA values in numeric columns are replaced with the column mean.
#' @examples
#' sample_data <- data.frame(
#  age = c(25, 30, NA, 45, 50),
#  income = c(50000, NA, 45000, NA, 60000),
#  gender = c("M", "F", "M", NA, "F")
#' print(sample_data)
#' @export
impute_missing_values <- function(data) {
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      mean_value <- mean(data[[col]], na.rm = TRUE)
      data[[col]][is.na(data[[col]])] <- mean_value
    }
  }
  return(data)
}

sample_data <- data.frame(
  age = c(25, 30, NA, 45, 50),
  income = c(50000, NA, 45000, NA, 60000),
  gender = c("M", "F", "M", NA, "F")
)


print("Original data:")
print(sample_data)

imputed_data <- impute_missing_values(sample_data)

print("Data after imputation:")
print(imputed_data)



