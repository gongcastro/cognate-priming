#### 00_import-oxford: Import Oxford data

#### set up #############################

# load packages
library(dplyr)        # for manipulating data
library(tidyr)        # for rehsaping datasets
library(data.table)   # for importing data
library(readxl)       # for importing Excel files
library(tibble)       # for more informative data frames
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

#### import data ##########################
dat_trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	clean_names() %>%
	select(trial_id, language, list, prime, target, distractor, trial_type) 
dat <- fread(here("Data", "Gaze data", "Oxford", "CognatePriming_17Mar2020.csv"), stringsAsFactors = FALSE, na.strings = "") %>%
	as_tibble() %>%
	clean_names() %>%
	rename(trial_num = trial, time_bin = timebin, time_stamp = timestamp,
		   prime = vis_un_stm, target = vis_target_stm, distractor = vis_distr_stm, audio = aud_stm,
		   l_x = gaze_raw_left_x, l_y = gaze_raw_left_y, r_x = gaze_raw_right_x, r_y = gaze_raw_right_y,
		   l_v = gaze_left_validity, r_v = gaze_right_validity) %>%
	mutate(target_location = ifelse(vis_target_stm_pos==2, "l", "r"),
		   location = "Oxford") %>%
	select(id, trial_num, time_bin, time_stamp, starts_with("l_"), starts_with("r_"), prime, target, distractor) %>%
	left_join(., dat_trials) 
	relocate(id, trial_num, trial_id, time_bin, time_stamp, starts_with("l_"), starts_with("r_"))
	
