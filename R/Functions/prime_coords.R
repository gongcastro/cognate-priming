# prime_coords: Create cordinates of images base on location in screen
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

prime_coords <- function(
  
  data,
  x_gaze,
  y_gaze,
  p_coords = c(710, 1210, 290, 790)
  
){
  
  # load packages
  require(rlang, quietly = TRUE, warn.conflicts = FALSE)
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(tidyr, quietly = TRUE, warn.conflicts = FALSE)
  
  {{ data }} %>%
    mutate(
      p_xmin = p_coords[1],
      p_xmax = p_coords[2],
      p_ymin = p_coords[3],
      p_ymax = p_coords[4],
      gazeP = (({{x_gaze}} >= p_xmin) & ({{x_gaze}} <= p_xmax) & ({{y_gaze}} >= p_ymin) & ({{y_gaze}} <= p_ymax)),
    ) %>%
    pull(gazeP)
  
}
