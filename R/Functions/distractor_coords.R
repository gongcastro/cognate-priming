# distractor_coords: Create cordinates of images base on location in screen
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

distractor_coords <- function(
  
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
      d_xmin = case_when(({{ target_location }} == "l")  ~ r_coords[1],
                         ({{ target_location }} == "r") ~ l_coords[1],
                         TRUE                               ~ NA_real_),
      d_xmax = case_when(({{ target_location }} == "l")  ~ r_coords[2],
                         ({{ target_location }} == "r") ~ l_coords[2],
                         TRUE                               ~ NA_real_),
      d_ymin = case_when(({{ target_location }} == "l")  ~ r_coords[3],
                         ({{ target_location }} == "r") ~ l_coords[3],
                         TRUE                               ~ NA_real_),
      d_ymax = case_when(({{ target_location }} == "l")  ~ r_coords[4],
                         ({{ target_location }} == "r") ~ l_coords[4],
                         TRUE                               ~ NA_real_),
      gazeD = (({{x_gaze}} >= d_xmin) & ({{x_gaze}} <= d_xmax) & ({{y_gaze}} >= d_ymin) & ({{y_gaze}} <= d_ymax))
    ) %>%
    pull(gazeD)
  
}
