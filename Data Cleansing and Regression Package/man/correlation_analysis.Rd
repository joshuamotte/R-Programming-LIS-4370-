\name{correlation_analysis}
\alias{correlation_analysis}
\title{Compute and Visualize Correlations Between Variables}
\description{
  This function computes the correlation matrix for all numeric variables in the data
  frame and visualizes the results as a heatmap. It can also provide correlations
  with a specific target variable.
}
\usage{
correlation_analysis(data, target_var = NULL)
}
\arguments{
  \item{data}{A data frame containing numeric variables.}
  \item{target_var}{An optional string specifying a column name. If provided, the function
  will return the correlations between `target_var` and all other numeric variables in the data frame.
  If `NULL`, it returns a full correlation matrix.}
}
\value{
  A sorted correlation matrix or a sorted vector of correlations with the target variable, printed as a heatmap.
}
\examples{
# Example 1: Get full correlation matrix
correlation_analysis(airquality)

# Example 2: Get correlation with target variable "temperature"
correlation_analysis(airquality, target_var = "Wind")
}
