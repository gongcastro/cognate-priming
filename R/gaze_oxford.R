# process Oxford gaze data

# set up ----
library(tidyverse)
library(janitor) # for cleaning colnames
library(lubridate) # for dealing with dates
library(readxl) # for importing Excel files
library(data.table) # for importing CSV files

# set params
screen_resolution <- c(x = 1920, y = 1080)
center_coords <- c(xmin = 710, xmax = 1210, ymin = 290, ymax = 790)
left_coords <- c(xmin = 180, xmax = 680, ymin = 290, ymax = 790)
right_coords <- c(xmin = 1240, xmax = 1640, ymin = 290, ymax = 790)
looking_threshold <- c(prime = 500, target = 500) # minimum looking time
missing_trials_threshold <- c(cognate = 2, noncognate = 2, unrelated = 2) # minimum n trials in each condition

# import participant data
participants <- read_xlsx("Data/Participants/participant_oxford_Apr2021.xlsx") %>% 
	clean_names() %>% 
	rename(participant = id, lp = lang_group) %>% 
	mutate(id_db = participant, test_language = "English") %>% 
	relocate(id_db, .after = participant)

# import vocabulary data
vocabulary_raw <- excel_sheets("Data/Vocabulary/vocabulary_oxford_Apr2021.xlsx") %>%
	map(function(x){
		read_xlsx(
			"Data/Vocabulary/vocabulary_oxford_Apr2021.xlsx", 
			sheet = x, 
			na = c("", "NA", "?", "x")
		)
	}
	) %>%
	set_names(excel_sheets("Data/Vocabulary/vocabulary_oxford_Apr2021.xlsx")) %>%
	bind_rows(.id = "version") %>%
	mutate_at(vars(-c(item, version)), as.integer) %>%
	pivot_longer(-c(item, version), names_to = "participant", values_to = "response") %>%
	#separate(item, c("category", "item"), sep = " - ", fill = "left") %>% 
	mutate(
		understands = response %in% c(1, 2),
		says = response %in% 2,
		# define age group
		age_group = case_when(
			substr(participant, start = 1, stop = 2) %in% c(21) ~ 21,
			substr(participant, start = 1, stop = 2) %in% c(24, 25) ~ 25,
			substr(participant, start = 1, stop = 2) %in% c(27, 30) ~ 30
		),
		age_group = as.factor(paste0(age_group, " months"))
	) %>%
	drop_na(response) %>%
	arrange(participant, version)

vocabulary_size <- vocabulary_raw %>%
	group_by(participant, age_group) %>%
	summarise(
		vocab_size = mean(understands), # proportion of understood words
		.groups = "drop"
	)

vocabulary <- vocabulary_raw %>%
	filter(understands) %>%
	group_by(participant, age_group) %>%
	summarise(
		vocab_words = list(unique(item)), # list of understood words
		.groups = "drop"
	) %>%
	right_join(vocabulary_size)

# import gaze data ----
raw <- list.files("Data/Gaze/Oxford", full.names = TRUE) %>% 
	# import and merge all files
	map(fread, na = c("", "NA")) %>% 
	bind_rows() %>% 
	as_tibble() %>%
	clean_names() %>% # colnames to snake case
  mutate(trial = ifelse(block == 2, trial+16, trial)) %>% # because of block format, trial number for 17th trial is recorded as block 2 trial 1
  rename(
		id_db = id, trial_num = trial, time = timestamp,
		l_x = gaze_raw_left_x, l_y = gaze_raw_left_y,
		r_x = gaze_raw_right_x, r_y = gaze_raw_right_y,
		l_v = gaze_left_validity, r_v = gaze_right_validity,
		target_location = vis_target_stm_pos,
		prime_unrelated = vis_un_stm, prime_cognate = vis_cp_stm, prime_noncognate = vis_np_stm,
		target = vis_target_stm, distractor = vis_distr_stm
	) %>%
	mutate_at(vars(matches("_v")), as.logical) %>%
	mutate(
		id_db = as.character(id_db),
		trial_type = case_when(
			!is.na(prime_unrelated) ~ "Unrelated",
			!is.na(prime_cognate) ~ "Cognate",
			!is.na(prime_noncognate) ~ "Non-cognate"
		),
		phase = case_when(
			between(time, 0, 2500) ~ "Getter", # attention getter lasts shorter than in Barcelona (3s)
			between(time, 2500, 4000) ~ "Prime",
			between(time, 4000, 4050) ~ "Blank",
			between(time, 4050, 4750) ~ "Audio",
			between(time, 4750, 6750) ~ "Target-Distractor"
		),
		target_location = ifelse(target_location==2, "L", "R"),
		prime = coalesce(prime_unrelated, prime_cognate, prime_noncognate), # get prime picture
		date_test = as_date(NA) # dat test is missing in current file
	) %>%
	filter(phase %in% c("Target-Distractor", "Prime")) %>% 
	left_join(participants) %>% # add participant information
	# restart time at every trial onset for eah participant in each testing session
	group_by(participant, age_group, trial_num, phase) %>%
	mutate(time = as.double(time-min(time, na.rm = TRUE))/1000) %>%
	ungroup() %>%
	# check gaze validity and take left gaze
	# get left eye
	# gaze is valid if flagged as such by eye-tracker, is not NA, and coords are within screen resolution
	mutate(
		l_v = l_v & !is.na(l_x) & !is.na(l_y) & between(l_x, 0, screen_resolution["x"]) & between(l_y, 0, screen_resolution["y"]),
		r_v = r_v & !is.na(l_x) & !is.na(r_y) & between(r_x, 0, screen_resolution["x"]) & between(r_y, 0, screen_resolution["y"]),
		# get left eye
		x = l_x,
		y = l_y,
		valid_sample = l_v # overall validity
	) %>% 
	mutate(
		aoi_center = between(x, center_coords["xmin"], center_coords["xmax"]) & between(y, center_coords["ymin"], center_coords["ymax"]),
		aoi_right = between(x, right_coords["xmin"], right_coords["xmax"]) & between(y, right_coords["ymin"], right_coords["ymax"]),
		aoi_left = between(x, left_coords["xmin"], left_coords["xmax"]) & between(y, left_coords["ymin"], left_coords["ymax"]),
		aoi_prime = aoi_center,
		aoi_target = ifelse(target_location=="L", aoi_left, aoi_right),
		aoi_distractor = ifelse(target_location=="L", aoi_right, aoi_left)
	) %>% 
	select(
		participant, date_test, age_group, lp, trial_num, test_language, phase, time,
		x, y, target_location, aoi_prime, aoi_target, aoi_distractor, valid_sample, lp,
		trial_type, prime, target, distractor
	)

# valid gaze
valid_gaze <- raw %>% 
	group_by(participant, age_group, trial_type, trial_num) %>% 
	summarise(
		# proportion of prime looking time during prime phase
		valid_prop_prime = sum(aoi_prime[phase=="Prime"], na.rm = TRUE)*(1000/120),
		# proportion of target looking time during target-distractor phase
		valid_prop_target = sum(aoi_target[phase=="Target-Distractor"], na.rm = TRUE)*(1000/120), 
		# proportion of distractor looking time during target-distractor phase
		valid_prop_distractor = sum(aoi_distractor[phase=="Target-Distractor"], na.rm = TRUE)*(1000/120), 
		.groups = "drop"
	) %>% 
	mutate(
		# prime looking proportion during prime phase is higher or equal than minimum
		valid_gaze_prime = valid_prop_prime >= looking_threshold["prime"],
		# participant has looked at least once to target AND distractor and higher or equal than minimum to both
		valid_gaze_target = valid_prop_target>0 & valid_prop_distractor>0 &
			(valid_prop_target+valid_prop_distractor >= looking_threshold["target"]),
		# trial meets all gaze criteria
		valid_gaze = valid_gaze_prime & valid_gaze_target 
	)

# evaluate vocabulary
valid_vocabulary <- raw %>%
	distinct(participant, age_group, trial_num, prime, target, distractor) %>% 
	left_join(vocabulary) %>% 
	rowwise() %>% 
	# participants understands prime, and target words in the testing language
	# IMPORTANT: some CDI labels don´t match words in the gaze file
	# I recommend adding a new column in the vocabulary file with the name used in the gaze data for each item
	# then the code in line 40 should be changed to:
	# 	pivot_longer(-c(item, item_id, version), names_to = "participant", values_to = "response") %>%
	# where item_id is the label used for this item in the gaze data
	mutate(valid_vocab = (prime %in% vocab_words) & (target %in% vocab_words))

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
		valid_trial_sum = sum(valid_trial),
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


# processed data
processed <- list(raw, vocabulary, valid_trials, valid_participants) %>%
	reduce(left_join) %>% 
	filter(phase=="Target-Distractor") %>% 
	select(
		participant, date_test, age_group, lp,
		trial_num, trial_type, test_language, phase,
		time, x, y, target_location, aoi_target, aoi_distractor,
		valid_sample, valid_gaze, valid_vocab, valid_trial,
		valid_participant, prime, target, vocab_size
	)

# export data ----
saveRDS(processed, "Data/Gaze/processed_oxford.rds")