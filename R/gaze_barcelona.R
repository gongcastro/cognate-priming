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
looking_threshold <- c(prime = 500, target = 500) # minimum looking time (ms)
missing_trials_threshold <- c(cognate = 2, noncognate = 2, unrelated = 2) # minimum n trials in each condition
relevant.variables <- c("participant", "trial_num", "trial", "phase", "time",
						"l_x", "l_y", "l_v", "r_x", "r_y", "r_v")
colname.changes <- c("system_time_stamp" = "time", "l_1" = "l_x", "l_2" = "l_y",
					 "process" = "phase", "r_1" = "r_x", "r_2" = "r_y",
					 "l_user_coord_3" = "l_user_coord_z",
					 "r_user_coord_3" = "r_user_coord_z","suje_num" = "participant")

# participants ----
participants <- readRDS(here("Data", "Participants", "participants.rds")) # participant data
vocab_norms <- readRDS(here("Data", "Vocabulary", "vocabulary_norms.rds")) # CDI itmes known by 50% participans at 21mo (for imputation)

# trial data ----
trials <- readRDS(here("Data", "Stimuli", "stimuli.rds")) %>%
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
	left_join(trials) %>% 
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

# valid gaze
valid_gaze <- raw %>% 
	group_by(participant, age_group, trial_type, trial_num) %>% 
	summarise(
		# proportion of prime looking time during prime phase
		valid_looking_prime = sum(aoi_prime[phase=="Prime"], na.rm = TRUE)*(1000/120),
		# proportion of target looking time during target-distractor phase
		valid_looking_target = sum(aoi_target[phase=="Target-Distractor"], na.rm = TRUE)*(1000/120), 
		# proportion of distractor looking time during target-distractor phase
		valid_looking_distractor = sum(aoi_distractor[phase=="Target-Distractor"], na.rm = TRUE)*(1000/120), 
		.groups = "drop"
	) %>% 
	mutate(
		# prime looking proportion during prime phase is higher or equal than minimum
		valid_gaze_prime = valid_looking_prime >= looking_threshold["prime"],
		# participant has looked at least once to target AND distractor and higher or equal than minimum to both
		valid_gaze_target = valid_looking_target>0 & valid_looking_distractor>0 &
			(valid_looking_target+valid_looking_distractor >= looking_threshold["target"]),
		# trial meets all gaze criteria
		valid_gaze = valid_gaze_prime & valid_gaze_target 
	)

# evaluate vocabulary
valid_vocabulary <- raw %>%
	distinct(participant, age_group, trial_num, prime, target) %>% 
	left_join(select(participants, participant, age_group, vocab_words)) %>% 
	rowwise() %>% 
	# evaluate if word in in participant's vocabulary or is understood by 50% or more of 19-22 aged children
	mutate(
		valid_vocab_prime = (prime %in% vocab_words) | (prime %in% vocab_norms),
		valid_vocab_target = target %in% vocab_words | (target %in% vocab_norms),
		valid_vocab = valid_vocab_prime & valid_vocab_target
	)

# valid trials
valid_trials <- left_join(valid_gaze, valid_vocabulary) %>% 
	# trial meets all gaze and vocabulary criteria
	mutate(valid_trial =  valid_gaze_prime & valid_gaze_target & valid_vocab) %>% 
	select(participant, age_group, trial_type, trial_num, valid_gaze, valid_vocab, valid_trial)

# valid participants
valid_participants <- valid_trials %>% 
	group_by(participant, age_group, trial_type) %>% 
	summarise(
		# number of valid trials by participant, age group and trial type
		valid_trial_sum = sum(valid_trial, na.rm = TRUE),
		# total number of trials by participant, age, group, and trial type
		trial_n = n(),
		.groups = "drop"
	) %>% 
	# to wide format
	pivot_wider(-trial_n, names_from = trial_type, values_from = valid_trial_sum) %>% 
	# rename columns
	clean_names() %>% 
	mutate(
		# minimum number of cognate trials
		valid_participant_cognate = cognate > missing_trials_threshold["cognate"],
		# has minimum number of noncognate trials
		valid_participant_non_cognate = non_cognate > missing_trials_threshold["noncognate"],
		# has minimum number of unrelated trials
		valid_participant_unrelated = unrelated > missing_trials_threshold["unrelated"],
		# participant fulfills and the conditions above
		valid_participant = valid_participant_cognate & valid_participant_non_cognate & valid_participant_unrelated
	)


# merge data ----
processed <- list(raw, participants, valid_trials, valid_participants) %>%
	reduce(left_join) %>% 
	filter(phase=="Target-Distractor") %>% 
	select(
		participant, date_test, lp, age_group, trial_num, test_language, phase,
		time, x, y, trial_type, target_location, aoi_target, aoi_distractor,
		valid_sample, valid_gaze, valid_vocab, valid_trial,
		valid_participant, prime, target, vocab_size
	)


# export data ---
saveRDS(processed, here("Data", "Gaze", "processed_barcelona.rds"))
