# attrition

# set up ----

# load packages
library(tidyverse)
library(janitor) # for cleaning column names
library(here) # for locating files

# set params
looking_threshold <- c(prime = 500, target = 500) # minimum looking time
missing_trials_threshold <- c(cognate = 2, noncognate = 2, unrelated = 2) # minimum n trials in each condition

# import data ----
participants <- readRDS(here("Data", "Participants", "participants.rds"))
vocab <- readRDS(here("Data", "Vocabulary", "vocabulary.rds"))
gaze <- list(
	Barcelona = readRDS(here("Data", "Gaze", "processed_barcelona.rds")),
	Oxford = readRDS(here("Data", "Gaze", "processed_oxford.rds"))
) %>% 
	bind_rows(.id = "location") %>% 
	# to avoid issues with column names
	rename(time_stamp = time)

processed <- reduce(list(participants, vocab, gaze), left_join)


# evaluate inclusion criteria ----
# valid gaze
valid_gaze <- processed %>% 
	group_by(participant, age_group, trial_type, trial) %>% 
	summarise(
		# proportion of prime looking time during prime phase
		valid_looking_prime = sum(aoi_prime[phase=="Prime"], na.rm = TRUE)*(1000/120),
		# proportion of target looking time during target-distractor phase
		valid_looking_target = sum(aoi_target[phase=="Target-Distractor"], na.rm = TRUE)*(1000/120), 
		.groups = "drop"
	) %>% 
	mutate(
		# prime looking proportion during prime phase is higher or equal than minimum
		valid_gaze_prime = valid_looking_prime >= looking_threshold["prime"],
		# participant has looked at least once to target AND distractor and higher or equal than minimum to both
		valid_gaze_target = valid_looking_target>0 &
			(valid_looking_target >= looking_threshold["target"]),
		# trial meets all gaze criteria
		valid_gaze = valid_gaze_prime & valid_gaze_target 
	)

# evaluate vocabulary
valid_vocabulary <- processed %>%
	distinct(participant, age_group, trial, prime, target, vocab_words) %>% 
	rowwise() %>% 
	# evaluate if word in in participant's vocabulary or is understood by 50% or more of 19-22 aged children
	mutate(
		valid_vocab_prime = prime %in% vocab_words,
		valid_vocab_target = target %in% vocab_words,
		valid_vocab = valid_vocab_prime & valid_vocab_target
	) %>% 
	ungroup()

# valid trials
valid_trials <- left_join(valid_gaze, valid_vocabulary) %>% 
	# trial meets all gaze and vocabulary criteria
	mutate(valid_trial =  valid_gaze_prime & valid_gaze_target & valid_vocab) %>% 
	select(participant, age_group, trial_type, trial, valid_gaze_prime, valid_gaze_target, valid_vocab, valid_trial)

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

# attrition ----
attrition <- reduce(list(processed, valid_trials, valid_participants), left_join) %>% 
	distinct(
		participant, age_group, lp, location, test_language,
		list, trial, trial_type, valid_gaze_prime, valid_gaze_target,
		valid_vocab, valid_trial, valid_participant
	)
saveRDS(attrition, here("Results", "attrition.rds"))

# merge data ----
clean <- reduce(list(processed, valid_gaze, valid_trials, valid_participants), left_join) %>%
	filter(phase=="Target-Distractor", valid_trial, valid_participant) %>% 
	select(
		participant, date_test, lp, location, list, age_group, trial, test_language, phase,
		time_stamp, x, y, valid_sample, trial_type, target_location, aoi_target, aoi_distractor, prime, target,
		vocab_size_total, vocab_size_l1, vocab_size_conceptual
		) %>% 
	drop_na(trial)
saveRDS(clean, here("Data", "Gaze", "clean.rds"))


