# 02_fixations: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(magrittr)     # for using pipes
library(readxl)       # for importing Excel files
library(tibble)       # for tidy data presentation
library(dplyr)        # for manipulating data
library(purrr)        # for working with lists
library(fuzzyjoin)    # for joining data and fixations
library(eyetrackingR) # for processing eye-tracking data
library(gazepath)     # for identiying fixations
library(ggplot2)      # for data visualisation

# set parameters
time_bin_duration <- 25  # how long should time bins be in ms?
sampling_rate     <- 120
screenX           <- 1920
screenY           <- 1080

#### import data ###########################################################
data <- read.table("Data/01_filtered.txt", sep = "\t") %>%
  make_eyetrackingr_data(data               = .,
                         participant_column = "ID",
                         trackloss_column   = "meanValidity",
                         time_column        = "time",
                         trial_column       = "Trial",
                         aoi_columns        = c("gazeT", "gazeD"),
                         treat_non_aoi_looks_as_missing = FALSE) %>%
  subset_by_window(rezero = TRUE, remove = TRUE, window_start_time = 233, window_end_time = 2000) %>%
  mutate(Trial = as.character(Trial),
         meanX = meanX,
         meanY = meanY,
         ID = as.character(ID)) %>%
  rename(trial = Trial) %>%
  as.data.frame()

# import trial data
trials <- as.list(c('Stimuli/list_bcn_cat1.xlsx',
                    'Stimuli/list_bcn_cat2.xlsx',
                    'Stimuli/list_bcn_cat3.xlsx',
                    'Stimuli/list_bcn_spa1.xlsx',
                    'Stimuli/list_bcn_spa2.xlsx',
                    'Stimuli/list_bcn_spa3.xlsx')) %>%
  map(~read_xlsx(.)) %>%
  set_names(c('bcn_cat1', 'bcn_cat2', "bcn_cat3", "bcn_spa1", "bcn_spa2", "bcn_spa3")) %>%
  bind_rows(., .id = 'list') %>%
  mutate(
    trialID   = as.character(trialID),
    height_px = 500,
    width_px  = 500,
    height_mm = 800,
    width_mm  = 800
  ) %>%
  filter(list == "bcn_spa1") %>%
  as.data.frame()

#### identify fixations ####################################################
ID_trials <- data %>%
  group_by(ID, trial) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  select(ID, trial) %>%
  split(f = list(.$ID)) %>%
  map(~select(., trial)) %>%
  map(~pull(., trial))


fixations <- gazepath(
  data       = data.split[[1]],
  x1         = "meanX",
  y1         = "meanY",
  d1         = "distance1",
  trial      = "trial",
  height_mm  = trials[, 10],
  width_mm   = trials[, 11],
  height_px  = trials[, 8],
  width_px   = trials[, 9],
  samplerate = 120,
  method     = "gazepath",
  extra_var = c("ID", "trialType", "trial")
) %$%
  fixations %>%
  map(~filter(., Value == "f")) 
  

  right_join(., data, by = c("ID", "trial")) %>%
  mutate(fixation = (time >= Start & time <= End)) %>%
  select(ID, time, fixation, trial, stimulus, trialType) %>%
 
map(., ~set_names(., data %>% group_by(ID, trial) %>% summarise(n = n()) %$% trial))
set_names(unique(data$ID)) 
bind_rows(., .id = "names")


plot(fixations[[1]], trial_index = 2)

