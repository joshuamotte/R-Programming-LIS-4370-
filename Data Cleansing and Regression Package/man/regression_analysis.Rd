\name{regression_analysis}
\alias{regression_analysis}
\title{Train and Compare Multiple Regression Models}
\description{
  This function trains multiple regression models (Linear Regression, SVM, KSVM,
  Random Forest) on the provided data and compares their performance based on
  RMSE, MAE, and R-squared.
}
\usage{
regression_analysis(data, target_var)
}
\arguments{
  \item{data}{A data frame containing the data for modeling.}
  \item{target_var}{The name of the target variable (dependent variable) in the data frame.
  It should be a column in the data.}
}
\value{
  A data frame containing the performance comparison (RMSE, MAE, and R-squared) for each model
  (Linear Regression, SVM, KSVM, Random Forest), ordered by RMSE.
}
\examples{
# Example: Regression analysis on the 'airquality' data frame with 'temperature' as the target variable
regression_analysis(airquality, target_var = "Wind")
}
