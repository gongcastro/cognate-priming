# filter_dilation_speed: Calculate dilation speed
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Based on Kret & Sjak-Shie (2019)
# https://link.springer.com/article/10.3758/s13428-018-1075-y
# Center for Brain and Cognition, Pompeu Fabra University

filter_dilation_speed <- function(
  data,
  dilation_speed = dilation_speed,
  sd = 2.5
){
  
  require(dplyr)
  
  median_absolute_deviation <- data %>%
    median(
      dilation_speed,
      median(. %$% {{ dilation_speed }})
    )
  
  threshold <- median(.$dilation_speed) + (sd*median_absolute_deviation)
  
  data %>%
    mutate(speed_outlier = median_absolute_deviation > threshold) 
  
}
