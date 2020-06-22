# 01_process: process data 
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up data ###########################################################
library(magrittr)     # for using pipes
library(tibble)       # for tidy data presentation
library(dplyr)        # for manipulating data
library(tidyr)        # for rehsaping datasets
library(data.table)   # for importing data
library(readxl)       # for importing Excel files
library(stringr)      # for working with character strings
library(janitor)      # for cleaning variable names
library(purrr)        # for working with lists
library(here)         # for locating files

# load functions
source(here("R", "functions.R"))

# set experimental parameters
sampling_rate  <- 120  # how many samples does the eye-tracker take per second?
screen_x       <- 1920 # width of the screen in pixels 
screen_y       <- 1080 # height of the screen in pixels

#### import data #######################################

# import participant-level data
participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	clean_names() %>%
	rename(participant_id = id, valid_participant = valid)

# import gaze data
dat_raw_exp   <- fread(here("Data", "00_raw.csv"), sep = ",", dec = ".")       # import data from experiment
dat_raw_pilot <- fread(here("Data", "00_raw-pilot.txt"), sep = "\t", dec = ".") # import data from pilot

dat_raw <- dat_raw_exp
#data.raw <- rbind(data.raw.exp, data.raw.pilot)

# import trial-level data
trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>% clean_names()

#### merge data #############################################################
dat_merged <- left_join(dat_raw, participants, by = "participant_id") %>%
	left_join(., trials, by = c("trial_id", "language", "version", "list")) %>%
	as_tibble() %>%
	mutate(trial_id = as.character(trial_id)) %>%
	select(participant_id, trial_id, phase, timebin, timestamp, l_x, l_y, l_dist, r_x, r_y, r_dist, trackloss, target_location, trial_type)

#### process data ###########################################################
# evaluate if gaze is in AOI
dat <- dat_merged %>%
	mutate(gaze_prime = eval_prime(data = ., x_gaze = l_x, y_gaze = l_y),
		   gaze_target = eval_target(data = ., x_gaze = l_x, y_gaze = l_y, target_location = target_location),
		   gaze_distractor = eval_distractor(data = ., x_gaze = l_x, y_gaze = l_y, target_location = target_location))

#### export data ###############################################
fwrite(dat, file = here("Data", "01_processed.csv"), sep = ",", row.names = FALSE)
