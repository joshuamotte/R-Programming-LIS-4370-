\name{replace_na}
\alias{replace_na}
\title{Replace Missing Values in a Data Frame}
\description{
  This function handles missing values in a data frame by either counting them or replacing them using these methods (mean, median, or imputation).
}
\usage{
replace_na(data, method = "count")
}
\arguments{
  \item{data}{A data frame with missing values.}
  \item{method}{A character string indicating the method to handle missing values.
  Possible values are:
  - "count" (default) to count the number of missing values,
  - "mean" to replace missing values with the mean of the column,
  - "median" to replace missing values with the median of the column,
  - "impute" to impute missing values with random sampling from non-missing values.}
}
\value{
  If `method` is "count", returns the total number of missing values.
  If `method` is "mean", "median", or "impute", returns a data frame with missing values replaced.
}
\examples{
# Example 1: Count the number of missing values in a data frame
replace_na(airquality, method = "count")

# Example 2: Replace missing values with the column mean
replace_na(airquality, method = "mean")

# Example 3: Replace missing values with the column median
replace_na(airquality, method = "median")

# Example 4: Impute missing values
replace_na(airquality, method = "impute")
}
