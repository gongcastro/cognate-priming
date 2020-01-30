# check_aoi: Evaluate whether gaze is in AOI
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Based on Kret & Sjak-Shie (2019)
# https://link.springer.com/article/10.3758/s13428-018-1075-y
# Center for Brain and Cognition, Pompeu Fabra University

check_aoi <- function(
  data,
  aoi = c(1140, 1640, 290, 790),
  gazeX, gazeY
){
  require(rlang)
  data %>%
    {{ gazeX }}>=aoi[1] &
    {{ gazeX }}<=aoi[2] &
    {{ gazeY }}>=aoi[3] &
    {{ gazeY }}<aoi[4]
}
