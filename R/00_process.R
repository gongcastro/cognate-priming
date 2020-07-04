# 00_process: Prepare data for analysis ################
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################


# load packages
library(tidyverse)      
library(data.table)   # for importing data
library(readxl)       # for importing Excel files
library(stringr)      # for working with character strings
library(purrr)        # for working with lists
library(janitor)      # for cleaning variable names
library(here)         # for locating files

# load functions
source(here("R", "functions.R"))

# set experimental parameters
sampling_rate      <- 120  # how many samples does the eye-tracker take per second?
screen_x           <- 1920 # width of the screen in pixels
screen_y           <- 1080 # height of the screen in pixels
relevant_variables <- c("participant", "trial", "phase", "system_time_stamp", "l_x", "l_y", "l_v", "r_x", "r_y", "r_v", "l_origin_user_coord_z", "r_origin_user_coord_z", "trial_num")

#### import data #######################################

dat_participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	filter(!pilot) %>%
	drop_na(participant_id) %>%
	select(participant_id, id_db, location, test_language, list, filename)
dat_trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	mutate(trial_id = as.character(trial_id)) %>% 
	select(location, test_language, list, trial_id, target_location)
dat_gaze_raw_bcn <- paste0(here("Data", "Gaze data", "Barcelona/"), dat_participants$filename[!is.na(dat_participants$filename)]) %>% 
	map(., fread, sep = ",", dec = '.', header = TRUE, stringsAsFactors = FALSE, na.strings = c("NaN", "NA", "Na", "-", ""))
dat_gaze_raw_oxf <- fread(here("Data", "Gaze data", "Oxford", "CognatePriming_17Mar2020.csv"), stringsAsFactors = FALSE, na.strings = "")

#### process data #######################################

# import data from Barcelona
dat_gaze_bcn <- dat_gaze_raw_bcn %>%
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
		   trackloss = !(r_v | l_v),
		   location = "Barcelona") %>%
	group_by(participant_id, trial_id) %>%
	mutate(timestamp = seq(from = 0,
						   to = ((1000/sampling_rate)*n())-(1000/sampling_rate),
						   by = 1000/sampling_rate),
		   phase = case_when(between(timestamp, 0, 3000) ~ "Getter",
		   				  between(timestamp, 3000, 4500) ~ "Prime",
		   				  between(timestamp, 4500, 4700) ~ "Blank",
		   				  between(timestamp, 4700, 5500) ~ "Audio",
		   				  between(timestamp, 5500, 7500) ~ "Target-Distractor")) %>%
	ungroup() %>%
	left_join(dat_participants) %>%
	left_join(dat_trials) %>%
	select(participant_id, id_db, trial_id, phase, timestamp, target_location, l_x, l_y, l_dist, r_x, r_y, r_dist, trackloss) %>% 
	arrange(desc(participant_id), trial_id, timestamp, phase)

# import data from Oxford
dat_gaze_oxf <- dat_gaze_raw_oxf %>%
	as_tibble() %>%
	clean_names() %>%
	rename(id_db = id,
		   trialnum = trial,
		   timebin = timebin,
		   timestamp = timestamp,
		   prime = vis_un_stm,
		   target = vis_target_stm,
		   distractor = vis_distr_stm,
		   audio = aud_stm,
		   l_x = gaze_raw_left_x,
		   l_y = gaze_raw_left_y,
		   r_x = gaze_raw_right_x,
		   r_y = gaze_raw_right_y,
		   l_v = gaze_left_validity,
		   r_v = gaze_right_validity) %>%
	mutate(l_x = ifelse(!between(l_x,  0, screen_x), NA_real_, l_x),
		   l_y = ifelse(!between(l_y,  0, screen_y), NA_real_, l_y),
		   l_dist = 650,
		   r_x = ifelse(!between(r_x,  0, screen_x), NA_real_, r_x),
		   r_y = ifelse(!between(r_y,  0, screen_y), NA_real_, r_y),
		   r_dist = 650,
		   trackloss = !(r_v | l_v),
		   target_location = ifelse(vis_target_stm_pos==2, "l", "r"),
		   location = "Oxford") %>% 
	mutate_at(vars(id_db, trialnum), as.character) %>%
	group_by(id_db, trialnum) %>%
	mutate(timestamp = timestamp-min(timestamp, na.rm = TRUE),
		   timebin  = as.numeric(as.factor(cut_width(timestamp, width = 200, closed = "left"))),
		   phase = case_when(between(timestamp, 0, 3000) ~ "Getter",
		   				  between(timestamp, 3000, 4500) ~ "Prime",
		   				  between(timestamp, 4500, 4550) ~ "Blank",
		   				  between(timestamp, 4550, 5250) ~ "Audio",
		   				  between(timestamp, 5250, 7250) ~ "Target-Distractor")) %>%
	ungroup() %>%
	select(-audio) %>%
	left_join(dat_participants) %>% 
	left_join(., select(dat_trials, -target_location)) %>% 
	select(participant_id, id_db, trial_id, phase, timestamp, target_location, l_x, l_y, l_dist, r_x, r_y, r_dist, trackloss)  

#### merge data ################################################
dat <- bind_rows(dat_gaze_bcn, dat_gaze_oxf)

#### export data ###############################################
fwrite(dat, file = here("Data", "00_processed.csv"), sep = ",", row.names = FALSE)

