\name{replace_outliers}
\alias{replace_outliers}
\title{Replace or Show Outliers in a Data Frame}
\description{
  This function identifies outliers in the numeric columns of a data frame,
  and depending on the chosen method, either shows the outliers or replaces them with `NA`.
}
\usage{
replace_outliers(data, method = "show", threshold = 3)
}
\arguments{
  \item{data}{A data frame with numeric columns.}
  \item{method}{A character string indicating the method to handle outliers.
  Possible values are:
  - "show" (default) to show the outliers,
  - "remove" to replace outliers with `NA`.}
  \item{threshold}{A numeric value indicating the number of standard deviations away
  from the mean to consider a value as an outlier (default is 3).}
}
\value{
  If `method` is "show", returns a list of outliers for each column.
  If `method` is "remove", returns a data frame with outliers replaced by `NA`.
}
\examples{
# Example 1: Show outliers in a data frame
replace_outliers(airquality, method = "show")

# Example 2: Replace outliers with NA
replace_outliers(airquality, method = "remove")
}
