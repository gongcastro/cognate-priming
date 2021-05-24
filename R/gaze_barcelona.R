# set up ----
library(tidyverse)
library(janitor)
library(multilex)
library(googlesheets4)
library(multilex)
library(lubridate)
library(data.table)

# set params
ml_connect("gonzalo.garciadecastro@upf.edu")
screen_resolution <- c(x = 1920, y = 1080)
center_coords <- c(xmin = 710, xmax = 1210, ymin = 290, ymax = 790)
left_coords <- c(xmin = 180, xmax = 680, ymin = 290, ymax = 790)
right_coords <- c(xmin = 1240, xmax = 1640, ymin = 290, ymax = 790)
looking_threshold <- c(prime = 500, target = 500) # minimum looking time
missing_trials_threshold <- c(cognate = 2, noncognate = 2, unrelated = 2) # minimum n trials in each condition
relevant.variables <- c("participant", "trial_num", "trial", "phase", "time",
						"l_x", "l_y", "l_v", "r_x", "r_y", "r_v")
colname.changes <- c("system_time_stamp" = "time", "l_1" = "l_x", "l_2" = "l_y",
					 "process" = "phase", "r_1" = "r_x", "r_2" = "r_y",
					 "l_user_coord_3" = "l_user_coord_z",
					 "r_user_coord_3" = "r_user_coord_z","suje_num" = "participant")

# importvocabulary data ----
p <- ml_participants()
r <- ml_responses(p, update = FALSE)
vocab_responses <- r %>% 
	mutate(
		understands = response > 1,
		age_group =  as.factor(
			case_when(
				between(age, 20, 24) ~ 21,
				between(age, 24, 28) ~ 25,
				between(age, 28, 33) ~ 30
			))) %>%
	filter(
		study=="CognatePriming",
		understands
	) %>%
	select("id_exp", "time", "age_group", "item", "response") %>%
	group_by(id_exp, time, age_group) %>%
	summarise(vocab_words = list(unique(item)), .groups = "drop")

vocabulary <- ml_vocabulary(p, r, scale = "prop", by = c("id_exp")) %>%
	right_join(select(p, id_db, time, id_exp)) %>%
	filter(type=="understands") %>%
	right_join(vocab_responses) %>%
	select(id_db, age_group, vocab_size = vocab_prop_total, vocab_words)

vocab_norms <- r %>%
	filter(language==dominance, between(age, 19, 22)) %>% 
	mutate(understands = response > 1) %>% 
	group_by(item) %>% 
	summarise(prop = mean(understands, na.rm = TRUE), .groups = "drop") %>% 
	filter(prop > 0.5) %>% 
	pull(item)

# import participant data ----
participants <- range_read(ss = "1JkhN4iBh3bi6PSReGGk9jSrVgDhZNOUmve6vNS2eEqE", sheet = "barcelona", na = "") %>%
	mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>%
	mutate(age_group = as.factor(age_group),
		   date_test = as_date(date_test)) %>% 
	filter(!pilot) %>% 
	left_join(vocabulary) %>% 
	select(participant, date_test, age_group, lp, test_language, list, version, vocab_size, vocab_words, filename)

# import trial data ----
trials <- range_read(ss = "1urHhHRu672-mmAUqPqg8wGNw5j4GeE18Vrj2TNZU7ns") %>%
	filter(location=="Barcelona") %>% 
	select(trial, location, test_language, list, version, prime, target_location, trial_type, target, prime_cdi, target_cdi)

# import gaze data ----
raw <- list.files("Data/Gaze/Barcelona", full.names = TRUE) %>% 
	map(
		# apply the following function to each file listed above
		function(x) {
			fread(x, sep = ",", na.strings = c("", "NaN", "NA", "<NA>")) %>%
				clean_names() %>% # rename all variables to snake case
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
				filter(phase %in% c("Target-Distractor", "Prime")) %>%
				mutate(
					participant = paste0("cognatepriming", participant),
					time = row_number()*(1000/120) # to fix the timestamp in some files
				) %>%
				mutate_all(function(x) ifelse(is.nan(x), NA_real_, x)) %>%
				mutate_at(vars(starts_with("l_"), starts_with("r_")), as.numeric) %>% # gaze coords to numeric
				mutate_at(vars(contains("_v")), as.logical) %>% # validity to logicals
				select(one_of(relevant.variables))
		}) %>% 
	set_names(list.files("Data/Gaze/Barcelona"))  %>% 
	bind_rows(.id = "filename") %>% 
	as_tibble() %>% 
	group_by(participant, trial, phase, filename) %>%
	mutate(
		time = as.double(time-min(time, na.rm = TRUE))/1000,
		time_bin = cut_interval(time, length = 0.1, labels = FALSE)
	) %>%
	ungroup() %>%
	filter(between(time, 0, 2),
		   participant %in% participants$participant) %>%
	mutate(
		valid_sample = l_v & !is.na(l_x) & !is.na(l_y) & between(l_x, 0, 1) & between(l_y, 0, 1),
		x = ifelse(valid_sample, l_x, NA),
		y = ifelse(valid_sample, l_y, NA),
		x = x*screen_resolution["x"],
		y = y*screen_resolution["y"]
	) %>% 
	left_join(participants) %>% 
	left_join(trials) %>% 
	mutate(
		aoi_center = between(x, center_coords["xmin"], center_coords["xmax"]) & between(y, center_coords["ymin"], center_coords["ymax"]),
		aoi_right = between(x, right_coords["xmin"], right_coords["xmax"]) & between(y, right_coords["ymin"], right_coords["ymax"]),
		aoi_left = between(x, left_coords["xmin"], left_coords["xmax"]) & between(y, left_coords["ymin"], left_coords["ymax"]),
		aoi_prime = aoi_center,
		aoi_target = ifelse(target_location=="L", aoi_left, aoi_right),
		aoi_distractor = ifelse(target_location=="L", aoi_right, aoi_left)
	) %>% 
	relocate(time_bin, .before = time, .after = ) %>%
	arrange(participant, trial, trial_num, phase, time_bin) %>% 
	select(participant, date_test, lp, age_group, trial_num, trial, test_language, phase,
		   time, x, y, trial_type, target_location, aoi_prime, aoi_target, aoi_distractor,
		   valid_sample, prime_cdi, target_cdi) %>% 
	rename_all(function(x) str_remove(x, "_cdi"))

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
	mutate(age_group = paste0(age_group, " months")) %>% 
	select(
		participant, date_test, lp, age_group, trial_num, test_language, phase,
		time, x, y, trial_type, target_location, aoi_target, aoi_distractor,
		valid_sample, valid_gaze, valid_vocab, valid_trial,
		valid_participant, prime, target, vocab_size
	)

# export data ---
saveRDS(processed, "Data/Gaze/processed_barcelona.rds")

