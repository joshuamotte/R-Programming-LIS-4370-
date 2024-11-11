#original code
tukey_multiple <- function(x) {
  outliers <- array(TRUE,dim=dim(x))
  for (j in 1:ncol(x))
  {
    outliers[,j] <- outliers[,j] && tukey.outlier(x[,j])
  }
  outlier.vec <- vector(length=nrow(x))
  for (i in 1:nrow(x))
  { outlier.vec[i] <- all(outliers[i,]) } return(outlier.vec) }


#how I ordered it to improve syntax
tukey_multiple <- function(x) {
  outliers <- array(TRUE,dim=dim(x))
  for (j in 1:ncol(x)) {
    outliers[,j] <- outliers[,j] && tukey.outlier(x[,j])
  }
  outlier.vec <- vector(length=nrow(x))
  for (i in 1:nrow(x)) { 
    outlier.vec[i] <- all(outliers[i,]) 
    } 
  return(outlier.vec) 
}

debug(tukey_multiple)
undebug(tukey_multiple)


#adding in the tukey.outlier function
tukey.outlier <- function(values) {
  q1 <- quantile(values, 0.25)
  q3 <- quantile(values, 0.75)
  iqr <- q3 - q1
  return(values < (q1 - 1.5 * iqr) | values > (q3 + 1.5 * iqr))
}

#idk so I asked chatgpt and it told me to get rid of the & symbol
# I would have never guessed that

tukey_multiple <- function(x) {
  outliers <- array(TRUE,dim=dim(x))
  for (j in 1:ncol(x)) {
    outliers[,j] <- outliers[,j] & tukey.outlier(x[,j])
  }
  outlier.vec <- vector(length=nrow(x))
  for (i in 1:nrow(x)) { 
    outlier.vec[i] <- all(outliers[i,]) 
  } 
  return(outlier.vec) 
}

cars <- mtcars

tukey_multiple(cars)
        
