library(tidyverse)
library(lubridate)
library(stats)
library(dplyr)
source("create_temp_matrix.R")

model_pred <- function(model, time, temp, temp_knots, dataframe){
  
  minute_of_week <- (lubridate::wday(time) - 1) * 24 * 60 + 
    lubridate::hour(time) * 60 + lubridate::minute(time)
  
  interval_of_week <- 1 + floor(minute_of_week / 15)
  
  ftow <- factor(interval_of_week)
  
  temp_mat <- create_temp_matrix(temp, temp_knots)
  
  dframe <- data.frame(dataframe, ftow, temp_mat)
  
  towt <- stats::predict(model, dplyr::select(dframe, -eload))
  
  results <- data.frame(dframe$time, dframe$eload, towt)
  
  return(results)
}