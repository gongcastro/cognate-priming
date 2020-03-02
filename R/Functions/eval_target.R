# eval_target: Evaluate if sample is in target AOI
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

eval_target <- function(
  
  data,
  x_gaze,
  y_gaze,
  target_location,
  l_coords = c(280, 780, 290, 790),
  r_coords = c(1140, 1640, 290, 790)
  
){
  
  # load packages
  require(rlang, quietly = TRUE, warn.conflicts = FALSE)
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(tidyr, quietly = TRUE, warn.conflicts = FALSE)
  
 {{ data }} %>%
    mutate(
      t_xmin = case_when(({{ target_location }} == "r") ~ r_coords[1],
                         ({{ target_location }} == "l")  ~ l_coords[1],
                         TRUE                               ~ NA_real_),
      t_xmax = case_when(({{ target_location }} == "r") ~ r_coords[2],
                         ({{ target_location }} == "l")  ~ l_coords[2],
                         TRUE                               ~ NA_real_),
      t_ymin = case_when(({{ target_location }} == "r") ~ r_coords[3],
                         ({{ target_location }} == "l")  ~ l_coords[3],
                         TRUE                               ~ NA_real_),
      t_ymax = case_when(({{ target_location }} == "r") ~ r_coords[4],
                         ({{ target_location }} == "l")  ~ l_coords[4],
                         TRUE                               ~ NA_real_),
      gazeT = (({{x_gaze}} >= t_xmin) & ({{x_gaze}} <= t_xmax) & ({{y_gaze}} >= t_ymin) & ({{y_gaze}} <= t_ymax)),
    ) %>%
    pull(gazeT)
  
}
