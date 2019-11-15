# get_dilation_speed: Calculate dilation speed
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Based on Kret & Sjak-Shie (2019)
# https://link.springer.com/article/10.3758/s13428-018-1075-y
# Center for Brain and Cognition, Pompeu Fabra University

get_dilation_speed <- function(data, pupil, time){
  
  require(rlang)

  data %>%
    mutate(
      diff_pre       = abs({{ pupil }} - lag({{ pupil }})),
      diff_time_pre  = abs({{ time }}  - lag({{ time }})),
      diff_post      = abs({{ pupil }} - lead({{ pupil }})),
      diff_time_post = abs({{ time }}  - lead({{ time }})),
      pot_pre        = diff_pre/diff_time_pre,
      pot_post       = diff_post/diff_time_post
    )
  }
    


