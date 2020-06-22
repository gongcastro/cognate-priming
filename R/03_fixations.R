# 03_fixations: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(magrittr)     # for using pipes
library(readxl)       # for importing Excel files
library(tibble)       # for tidy data presentation
library(dplyr)        # for manipulating data
library(purrr)        # for working with lists
library(data.table)   # for importing data
library(janitor)      # for cleaning variable names
library(gazepath)     # for identiying fixations
library(ggplot2)
library(here)         # for locating files

# set parameters
time_bin_duration <- 100  # how long should time bins be in ms?
sampling_rate     <- 120  # how many samples are takien per second?
screen_x          <- 1920 # screen width in pixels
screen_y          <- 1080 # screen heigh in pixels
resolution        <- 23   # screen size in inches

# load functions
source(here("R", "functions.R")) 

#### import data ###########################################################
# import participant-level data
participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	clean_names() %>%
	rename(participant_id = id,
		   valid_participant = valid) %>%
	filter(!pilot)

# import trial-leve data
trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	clean_names() %>%
	select(-c(target_location, trial_type)) %>%
	mutate(trial_id = as.character(trial_id))

# import filtered data
dat <- fread(here("Data", "02_filtered.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE) %>%
	select(-c(phase, gaze_prime)) %>%
	mutate(trial_id = as.character(trial_id),
		   participant_id = as.character(participant_id)) %>%
	as.data.frame()

#### identify fixations ####################################################
fixation_info <- split(dat, f = dat$participant_id) %>% # separate each participant's dataset
	# get fixations in each dataset
	map(., ~gazepath(data       = .,
					 x1         = "l_x",             # horizontal position
					 y1         = "l_y",             # vertical position
					 d1         = "l_dist",          # distance
					 x2         = "r_x",
					 y2         = "r_y",
					 d2         = "r_dist",
					 trial      = "trial_id",      # trial index in dataset
					 height_mm  = 285.75,        # AOI height in mm
					 width_mm   = 508,        # AOI width in mm
					 height_px  = screen_y,             # AOI height in px 
					 width_px   = screen_x,             # AOI width in px
					 samplerate = sampling_rate,   # eye-tracker sampling rate
					 method     = "gazepath",         # select algorithm
					 res_x = screen_x,
					 res_y = screen_y,
					 thres_dur = 100,
					 posthoc    = TRUE,            # merge consecutive fixations?
					 extra_var  = c("participant_id", "trial_id"))) %>%
	# keep these variables
	map("fixations") %>%          # extract fixation info
	map(bind_rows) %>%
	bind_rows() %>%        # merge fixations within participant
	clean_names() %>%
	filter(value == "f") %>%      # exclude saccades
	select(participant_id, trial_id, start, end) %>% # select variables of interest
	as_tibble() %>%               # for more informative datasets
	mutate_if(.predicate = is.factor, as.character) %>%
	group_by(participant_id, trial_id) %>% # whatever we do next, do it spearately for each combination of these variables
	mutate(fixnum = row_number()) %>% # index all fixations within each trial 
	ungroup()

	# reconstruct fixations across time domain
fixations <- fixation_info %>%
	right_join(., dat, by = c("participant_id", "trial_id")) %>% # merge fixation info with time data
	left_join(., participants, by = "participant_id") %>%
	left_join(., trials, by = c("location", "language", "version", "list", "trial_id")) %>%
	rename(time = timestamp) %>%
	mutate(fixation = (time >= start) & (time <= end),  # is time point within fixation boundaries?
		   dist = ifelse(is.na(l_dist), r_dist, l_dist),
		   x = ifelse(is.na(l_x), r_x, l_x),
		   y = ifelse(is.na(l_y), r_y, l_y)) %>%    
	group_by(participant_id, trial_id, time, trial_type, list, target_location, language, lang_profile, dist) %>%      # prepare to agreggate by time point
	summarise(fixation = any(fixation), # is this time point within any of the fixation boundaries?
			  x = first(x), # preserve gaze location in X-axis                       
			  y = first(y), # preserve gaze location in Y-axis
			  .groups = "drop") %>%              
	ungroup() %>%
	group_by(participant_id, trial_id) %>%
	mutate(timebin = as.numeric(as.factor(cut_width(time, width = 200, closed = "left")))) %>%
	ungroup() %>%
	mutate_if(.predicate = is.factor, as.character) %>%
	mutate(fix_target = eval_target(data = ., x_gaze = x, y_gaze = y, target_location = target_location),
		   fix_distractor = eval_distractor(data = ., x_gaze = x, y_gaze = y, target_location = target_location)) %>%
	relocate(participant_id, trial_id, timebin, time)
	

#### export data ##############################################
fwrite(fixation_info, here("Data", "03_fixation-info.csv"), sep = ",", dec = ".", row.names = FALSE)
fwrite(fixations, here("Data", "03_fixations.csv"), sep = ",", dec = ".", row.names = FALSE)
