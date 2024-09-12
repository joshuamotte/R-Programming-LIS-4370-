#formatting data into csv
hospital_data_csv <- 
'"freq","bloodp","first","second","finaldecision"
"0.6","103","bad","low","low"
"0.3","87","bad","low","high"
"0.4","32","bad","high","low"
"0.4","42","bad","high","high"
"0.2","59","good","low","low"
"0.6","109","good","low","high"
"0.3","78","good","high","low"
"0.4","205","good","high","high"
"0.9","135","NA","high","high"
"0.2","176","bad","high","high"'


#turning data into data frame
hospital_data <- read.csv(text = hospital_data_csv, header = TRUE)

hospital_data

#first column
cleaning_first_fn <- function(column){
  if (is.na(column)) {
    return(NA)
} else if (column == "good"){
    return(0)
} else if (column == "bad"){
    return(1)
}
}
hospital_data$first <- sapply(hospital_data$first, cleaning_first_fn)

#second/final column
cleaning_sf_fn <- function(column){
  if (is.na(column)) {
    return(NA)
  } else if (column == "low"){
    return(0)
  } else if (column == "high"){
    return(1)
  }
}
hospital_data$second <- sapply(hospital_data$second, cleaning_sf_fn)
hospital_data$finaldecision <- sapply(hospital_data$finaldecision, cleaning_sf_fn)

hospital_data

#box plots
boxplot(hospital_data)

boxplot(hospital_data$bloodp,
        ylab = "Blood Pressure")

boxplot(hospital_data[ ,-2],
        ylab = "Score")

#histograms

hist(hospital_data$first,
     breaks = 2,)
hist(hospital_data$second,
     breaks = 2)
hist(hospital_data$finaldecision,
     breaks = 2)

