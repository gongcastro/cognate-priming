# process Barcelona gaze data

# set up ----
library(tidyverse)
library(janitor)
library(multilex)
library(lubridate)
library(data.table)
library(here)

# set params
screen_resolution <- c(x = 1920, y = 1080) # screen size in pixels
center_coords <- c(xmin = 710, xmax = 1210, ymin = 290, ymax = 790) # coordinates of center picture
left_coords <- c(xmin = 180, xmax = 680, ymin = 290, ymax = 790) # coordinates of left picture
right_coords <- c(xmin = 1240, xmax = 1640, ymin = 290, ymax = 790) # coordinates of right picture
relevant.variables <- c("participant", "trial_num", "trial", "phase", "time",
						"l_x", "l_y", "l_v", "r_x", "r_y", "r_v")
colname.changes <- c("system_time_stamp" = "time", "l_1" = "l_x", "l_2" = "l_y",
					 "process" = "phase", "r_1" = "r_x", "r_2" = "r_y",
					 "l_user_coord_3" = "l_user_coord_z",
					 "r_user_coord_3" = "r_user_coord_z","suje_num" = "participant")

# participants ----
participants <- readRDS(here("Data", "Participants", "participants.rds")) # participant data

# trial data ----
stimuli <- readRDS(here("Data", "Stimuli", "stimuli.rds")) %>%
	filter(location=="Barcelona") %>% 
	select(-c("prime2", "target2", "prime2_cdi", "target2_cdi", "valid_trial"))

# import gaze data ----
raw <- list.files(here("Data", "Gaze", "Barcelona"), full.names = TRUE) %>% 
	map(
		# apply the following function to each file listed above
		function(x) {
			fread(x, sep = ",", na.strings = c("", "NaN", "NA", "<NA>")) %>%
				# rename all variables to snake case
				clean_names() %>% 
				# fix some values from outdates files
				rename_all(str_replace_all, colname.changes) %>% 
				mutate(phase = str_replace_all(
					phase,
					c("GETTER" = "Getter",
					  "PRIMEIMAGE" = "Prime",
					  "TARGET_DISTRACTOR" = "Target-Distractor",
					  "prime" = "Prime",
					  "primeimage" = "Prime",
					  "target_distractor" = "Target-Distractor"
					))) %>%
				# get gaze in prime and target-ditractor phases
				filter(phase %in% c("Target-Distractor", "Prime")) %>%
				mutate(
					participant = paste0("cognatepriming", participant),
					# fix the timestamp in some files
					time = row_number()*(1000/120)
				) %>%
				# change variables to the right class
				mutate_all(function(x) ifelse(is.nan(x), NA_real_, x)) %>% # NaNs to NAs
				mutate_at(vars(starts_with("l_"), starts_with("r_")), as.numeric) %>% # gaze coords to numeric
				mutate_at(vars(contains("_v")), as.logical) %>% # validity columns to logicals
				# get only variables of interest
				select(any_of(relevant.variables))
		}) %>% 
	# merge all data sets assigning them their file name
	set_names(list.files(here("Data", "Gaze", "Barcelona")))  %>% 
	bind_rows(.id = "filename") %>% 
	as_tibble() %>%
	# restart timestamp at each trial phase change, and express in seconds
	group_by(participant, trial, phase, filename) %>%
	mutate(time = as.double(time-min(time, na.rm = TRUE))/1000) %>%
	ungroup() %>%
	# trim timestamps outside the 2 seconds range
	filter(
		between(time, 0, 2),
		participant %in% participants$participant # get only valid participants
	) %>%
	mutate(
		# more detailed evaluation of sample validity
		valid_sample = l_v & !is.na(l_x) & !is.na(l_y) & between(l_x, 0, 1) & between(l_y, 0, 1),
		# if sample is not valid, change value to NA
		x = ifelse(valid_sample, l_x, NA),
		y = ifelse(valid_sample, l_y, NA),
		# change gaze coodinates from 0-1 scale to screen resolution scale [1920x1080]
		x = x*screen_resolution["x"],
		y = y*screen_resolution["y"]
	) %>% 
	# add paticipant and trial info
	left_join(participants) %>% 
	left_join(stimuli) %>% 
	# evaluate if gaze coordinates are inside any AOI, and which
	mutate(
		aoi_center = between(x, center_coords["xmin"], center_coords["xmax"]) & between(y, center_coords["ymin"], center_coords["ymax"]),
		aoi_right = between(x, right_coords["xmin"], right_coords["xmax"]) & between(y, right_coords["ymin"], right_coords["ymax"]),
		aoi_left = between(x, left_coords["xmin"], left_coords["xmax"]) & between(y, left_coords["ymin"], left_coords["ymax"]),
		aoi_prime = aoi_center,
		aoi_target = ifelse(target_location=="L", aoi_left, aoi_right),
		aoi_distractor = ifelse(target_location=="L", aoi_right, aoi_left)
	) %>% 
	arrange(participant, trial, trial_num, phase) %>% 
	select(participant, date_test, lp, age_group, trial_num, trial, test_language, phase,
		   time, x, y, trial_type, target_location, aoi_prime, aoi_target, aoi_distractor,
		   valid_sample, prime_cdi, target_cdi) %>% 
	rename_all(function(x) str_remove(x, "_cdi"))


# merge data ----
processed <- reduce(list(raw, participants), left_join) %>%
	select(
		participant, date_test, age_group, lp, trial, trial_type, test_language, phase,
		time, x, y, valid_sample, target_location, aoi_prime, aoi_target, aoi_distractor, prime, target,
	)

# export data ---
saveRDS(processed, here("Data", "Gaze", "processed_barcelona.rds"))
