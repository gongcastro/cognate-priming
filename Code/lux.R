library(tibble)
library(dplyr)

data <- tibble(
  center = c(63.76, 65.96, 65.44, 65.79, 66.14, NA, NA),
  left.upp = c(49.85, 49.90, 48.63, 47.48, 45.45, NA, NA),
  left.low = c(44.78, 55.03, 57.33, 57.96, NA, NA, NA),
  right.low = c(59.91, 58.78, 59.28, 58.48, 58.93, NA, NA),
  right.upp = c(52.40, 52.28, 52.15, 56.53, 57.11, NA, NA)
  ) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  t() %>%
  as.vector() %>%
  mean()

  data <- tibble(
    center = c(63.76, 65.96, 65.44, 65.79, 66.14, NA, NA),
    left.upp = c(49.85, 49.90, 48.63, 47.48, 45.45, NA, NA),
    left.low = c(44.78, 55.03, 57.33, 57.96, NA, NA, NA),
    right.low = c(59.91, 58.78, 59.28, 58.48, 58.93, NA, NA),
    right.upp = c(52.40, 52.28, 52.15, 56.53, 57.11, NA, NA)
  ) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  t() %>%
  as.vector() %>%
  sd()
  


  rbind(
    tibble(
  upp.right = c(1119.1, 106.5, 73.72, 101.4, 103.8, 97.7, NA),
  upp.center = c(130.7, 131.7, 147.2, 98.33, 131.8, 121.2, NA),
  upp.left = c(114, 101.5, 77.79, 65.14, 71.27, 70.46, 128.8),
  mid.left = c(96.78, 76.57, 73.74, 113.9, 81.31, 51.18, NA),
  low.left = c(46.43, 88.7, 47.60, 46.38, 72.02, 40.40, NA),
  low.center = c(98.45, 70.97, 92.18, 50.00, 58.73, NA, NA),
  low.right = c(59.08, 45.18, 57.56, 47.40, 65.99, NA, NA),
  mid.right = c(64.14, 70.59, 81.74, 64.76, 56.91, 90.58, NA)
    )
  )
