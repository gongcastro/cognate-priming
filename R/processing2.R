#### processing: Prepare data for analysis -------------------------------------

# set up -----------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)
library(readxl)
library(lubridate)
library(googlesheets4)
library(janitor)
library(here)

# load functions
source(here("R", "utils.R"))

# set params
sampling_rate <- 120  # how many samples does the eye-tracker take per second?
screen_resolution <- c(x = 1920, y = 1080)
left_coords <- c(280, 780, 290, 790)
right_coords <- c(1140, 1640, 290, 790)
relevant_variables <- c(
	"participant", "trial_num", "trial", "phase", "time",
	"l_1", "l_2", "l_v", "r_1", "r_2", "r_v",
	"l_user_coord_3", "l_origin_user_coord_3",
	"r_user_coord_3", "r_origin_user_coord_3"
)

# get sample data
sample <- sample_get("gonzalo.garciadecastro@upf.edu") %>% 
	filter(location=="BCN",
		   valid_participant,
		   date_test > as_date("2020-02-14")) %>% 
	drop_na(filename) %>% 
	rename(participant = participant_id) %>% 
	select(participant, id_db, location, age_group, lp, test_language, list, version, filename) 

trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	rename(trial = trial_id) %>% 
	mutate(trial = as.character(trial)) %>% 
	select(location, test_language, list, version, trial, trial_type, target_location, prime, target, distractor) 


# get gaze data
gaze <- here("Data", sample$filename) %>% 
	map(., function(x) {
		fread(x, sep = ",", na.strings = c("", "NaN", "NA")) %>% 
			clean_names() %>% 
			as_tibble() %>% 
			rename_all(~rename_cols(.)) %>% 
			mutate(
				participant = paste0("cognatepriming", participant),
				time = row_number()*(1000/120)
			) %>% 
			mutate_all(function(x) ifelse(is.nan(x), NA_real_, x)) %>% 
			mutate_at(vars(matches("l_|r_")), as.numeric) %>% 
			mutate_at(vars(contains("_v")), function(x) as.logical(as.integer(x))) %>% 
			select(starts_with(relevant_variables)) 
	}) %>% 
	bind_rows() %>% 
	filter(phase %in% "Target-Distractor") %>% 
	mutate(
		x = ifelse(is.na(l_1), r_1, l_1)*screen_resolution["x"],
		y = ifelse(is.na(l_2), r_2, l_2)*screen_resolution["y"],
		d = ifelse(
			is.na(l_origin_user_coord_3),
			r_origin_user_coord_3 ,
			l_origin_user_coord_3
		),
		trial = as.character(trial)
	) %>% 
	group_by(participant, trial) %>%
	mutate(
		time = as.double(time),
		time = as.double(time-min(time, na.rm = TRUE))/1000,
		time_bin = cut_interval(time, length = 0.1, labels = FALSE)
	) %>%
	ungroup()

# process gaze -----------------------------------------------------------------
processed <- gaze %>% 
	left_join(sample, by = c("participant")) %>% 
	left_join(trials) %>% 
	select(participant, age_group, trial, time_bin, time, x, y, target_location, d, lp, trial_type, target) %>% 
	mutate(
		trial = as.integer(trial),
		gaze_in_l_aoi = gaze_in_aoi(x, y, left_coords),
		gaze_in_r_aoi = gaze_in_aoi(x, y, right_coords),
		fix_target =  (target_location=="r" & gaze_in_r_aoi) | (target_location=="l" & gaze_in_l_aoi),
		fix_distractor =  (target_location=="r" & gaze_in_l_aoi) | (target_location=="l" & gaze_in_r_aoi)
	) %>%
	arrange(participant, trial, time) %>%
	select(-c(gaze_in_l_aoi, gaze_in_r_aoi))

# summarise by trial -----------------------------------------------------------
gaze_trial <- processed %>%
	group_by(participant, age_group, trial, lp, trial_type) %>%
	summarise(
		fixations = sum(fix_target, na.rm = TRUE),
		n = n(),
		.groups = "drop"
	)

# summarise by time bin --------------------------------------------------------
gaze_time_bin <- processed %>% 
	group_by(participant, age_group, lp, trial, trial_type, time_bin, target) %>% 
	summarise(
		fixations = sum(fix_target, na.rm = TRUE),
		n = n(),
		.groups = "drop"
	) %>% 
	arrange(participant, trial, time_bin)

write.csv(gaze_time_bin, "Results/gaze_time_bins.csv", row.names = FALSE)


	
	
	
	
	