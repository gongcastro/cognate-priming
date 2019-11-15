# 01_filter: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(here)         # for locating files
library(magrittr)     # for using pipes
library(tibble)       # for tidy data presentation
library(dplyr)        # for manipulating data
library(tidyr)        # for rehsaping datasets
library(eyetrackingR) # for processing eye-tracking data

# set parameters
sampling_rate <- 120

#### import data #########################################################
data <- read.table(here::here("Data", "02_filtered.txt"), sep = "\t", header = TRUE) %>%
  as_tibble()

#### filter data #########################################################

# filter trials with less than 75% looking time to prime in prime phase
exclude_prime <- data %>%
  group_by(ID, Trial) %>%
  filter(phase == "prime", gazeP == 1) %>%
  summarise(n = n()) %>%
  mutate(valid = (n >= ((1500/1000)*(sampling_rate)*0.75))) %>%
  filter(valid == FALSE) %>%
  ungroup()

exclude_prime_trials <- summarise(exclude_prime, n = n()) %$% n
exclude_prime_ID <- exclude_prime %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  mutate(include = (n < (32*0.50))) %>%
  filter(include == FALSE) %>%
  ungroup()

# filter data
data.clean <- anti_join(data, exclude_prime, by = c("ID", "Trial")) %>%
  anti_join(., exclude_prime_ID, by = "ID") %>%
  filter(phase == "target_distractor", time >= 0, time <= 2000) %>%
  make_eyetrackingr_data(data               = .,
                         participant_column = "ID",
                         trackloss_column   = "meanValidity",
                         time_column        = "time",
                         trial_column       = "Trial",
                         aoi_columns        = c("gazeT", "gazeD"),
                         treat_non_aoi_looks_as_missing = FALSE
  ) %>%
  clean_by_trackloss(data                    = .,
                     trial_prop_thresh       = 0.25,
                     participant_prop_thresh = 0.50)

#### analyse trackloss ###################################################
trackloss <- trackloss_analysis(data.clean)

#### export data #########################################################
write.table(data.clean, "Data/02_filtered-gaze.txt", sep = "\t")
write.table(trackloss, "Data/02_filtered-trackloss.txt", sep = "\t")

