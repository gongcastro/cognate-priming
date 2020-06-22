# 00_import: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(dplyr)        # for manipulating data
library(tidyr)        # for rehsaping datasets
library(data.table)   # for importing data
library(readxl)       # for importing Excel files
library(tibble)       # for more informative data frames
library(stringr)      # for working with character strings
library(purrr)        # for working with lists
library(janitor)      # for cleaning variable names
library(ggplot2)
library(here)         # for locating files

# load functions
source(here("R", "functions.R"))

# set experimental parameters
sampling_rate      <- 120  # how many samples does the eye-tracker take per second?
screen_x           <- 1920 # width of the screen in pixels
screen_y           <- 1080 # height of the screen in pixels
relevant_variables <- c("participant", "trial", "phase", "system_time_stamp", "l_x", "l_y", "l_v", "r_x", "r_y", "r_v", "l_origin_user_coord_z", "r_origin_user_coord_z", "trial_num")

#### import data #######################################

# import participant-level data
participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	clean_names() %>%
	filter(!pilot) %>%
	drop_na(id) 

# import trial-level data
trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	clean_names() %>%
	mutate(trial_id = as.character(trial_id))

# import eye-tracking data
dat_raw <- paste0(here("Data", "Gaze data", "Barcelona/"), participants$filename) %>% # locate files with gaze data
	map(., fread, sep = ",", dec = '.', header = TRUE, stringsAsFactors = FALSE, na.strings = c("NaN", "NA", "Na", "-", ""))  # import files in the folder

#### process data #######################################
dat <- dat_raw %>%
	map(clean_names) %>%
	map(select, one_of(relevant_variables)) %>%
	map(mutate_at, vars(system_time_stamp, l_x, l_y, l_v, r_x, r_y, r_v, contains("origin")), as.numeric) %>%
	bind_rows() %>%
	as_tibble() %>%
	rename(participant_id = participant,
		   timestamp = system_time_stamp,
		   trial_id = trial,
		   l_dist = l_origin_user_coord_z,
		   r_dist = r_origin_user_coord_z) %>%
	mutate(participant_id = paste0("cognatepriming", participant_id),
		   trial_id = as.character(trial_id),
		   l_x = l_x*screen_x,
		   r_x = r_x*screen_x,
		   l_y = l_y*screen_y,
		   r_y = r_y*screen_y,
		   l_dist = ifelse(l_dist < 0, NA_real_, l_dist),
		   r_dist = ifelse(r_dist < 0, NA_real_, r_dist),
		   trackloss = !(r_v | l_v)) %>%
	group_by(participant_id, trial_id, phase) %>%
	mutate(timestamp = seq(from = 0,
						   to = ((1000/sampling_rate)*n())-(1000/sampling_rate),
						   by = 1000/sampling_rate),
		   timebin  = as.numeric(as.factor(cut_width(timestamp, width = 200, closed = "left")))) %>%
	ungroup() %>%
	filter(phase %in% c("Prime", "Target-Distractor")) %>%
	select(participant_id, trial_id, phase, timebin, timestamp, l_x, l_y, l_dist, r_x, r_y, r_dist, trackloss)

#### export data ###############################################
fwrite(dat, file = here("Data", "00_raw.csv"), sep = ",", row.names = FALSE)

