library(tidyverse)

resample <- function(data, l){
  
  data <- data %>%
    mutate(wday = wday(time, week_start = 1, label = FALSE)) 
  
  post_samp <- tibble()

  for (s in 1:3){
    for (i in 1:7){
      pre_samp <- data %>%
        filter(strategy == s, 
               wday == i)
      len <- nrow(pre_samp)
      
      # print(len)
      
      if (len <= l){
        sampled_rows <- pre_samp[sample(nrow(pre_samp), l - len, replace = TRUE), ]
      } else {
        sampled_rows <- pre_samp[sample(nrow(pre_samp), l, replace = TRUE), ]
      }
      
      # sampled_rows <- pre_samp[sample(nrow(pre_samp), max(0, l - len), replace = TRUE),]
  
      # print(nrow(sampled_rows))
      
      # post_samp <- rbind(pre_samp, sampled_rows, post_samp)
      
      if (len <= l){
        post_samp <- rbind(pre_samp, sampled_rows, post_samp)
      } else {
        post_samp <- rbind(sampled_rows, post_samp)
      }
      
      # print(nrow(post_samp))
      
      
    }
  } 
  post_samp <- post_samp %>%
    dplyr::select(-wday)
  
return(post_samp)
}
