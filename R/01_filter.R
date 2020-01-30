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
library(stringr)      # for working with character strings
library(forcats)      # for dealing with NAs
library(eyetrackingR) # for processing eye-tracking data

# set parameters
sampling_rate <- 120

#### import data #########################################################
data.processed <- read.table(here::here("Data", "01_processed.txt"),
                             sep = "\t",
                             header = TRUE,
                             stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  mutate(Trial = as.numeric(Trial))

#### filter data #########################################################

# list of trials with <75% looking time to prime in prime phase
exclude_prime <- data.processed %>%
  group_by(ID, Trial) %>%
  filter(phase == "prime", gazeP == 1) %>%
  summarise(n = n()) %>%
  mutate(valid = (n >= ((1500/1000)*(sampling_rate)*0.75))) %>%
  filter(valid == FALSE) %>%
  ungroup()

# list of participants with <50% valid trials regaring prime looking time
exclude_prime_ID <- exclude_prime %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  mutate(include = (n < (32*0.50))) %>%
  filter(include == FALSE) %>%
  ungroup()

#### filter data  ############################################

data.filtered <- data.processed %>%
  # remove participants labelled as non-valid
  filter(valid) %>%
  # remove trials with <75% looking time to prime in prime phase
  anti_join(., exclude_prime, by = c("ID", "Trial")) %>%
  # remove participants with <50% valid trials regaring prime looking time
  anti_join(., exclude_prime_ID, by = "ID") %>%
  # remove trials where participants were unfamiliar with any of the words
  filter(
    !str_detect(prime, unfamiliarCat),
    !str_detect(prime, unfamiliarSpa),
    !str_detect(target, unfamiliarCat),
    !str_detect(target, unfamiliarSpa),
    !str_detect(distractor, unfamiliarCat),
    !str_detect(distractor, unfamiliarSpa)
  ) %>%
  # remove trial with <75% valid samples during target and distractor
  # remove participants with <50% valid trials during
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
trackloss <- trackloss_analysis(data.filtered)

#### export data #########################################################
write.table(data.filtered, file = here::here("Data", "02_filtered.txt"), sep = "\t", row.names = FALSE)
write.table(trackloss, "Data/02_filtered-trackloss.txt", sep = "\t")

