# reset_time: Set time onset at 0 for each participant
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

reset_time <- function(data = ., time, participant){
  
  data %>%
    group_by({{ participant }}) %>%
    summarise(min_time = min({{ time }}, na.rm = TRUE)) %>%
    select(min_time, {{ participant }}) %>%
    left_join(data, .)
}
