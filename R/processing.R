#### processing: Prepare data for analysis -------------------------------------

# set up -----------------------------------------------------------------------

# load packages
library(tidyverse)
library(multilex)
library(bilingualr)
library(here)

# set params
sampling_rate <- 120  # how many samples does the eye-tracker take per second?
left_coords <- c(xmin = 280, xmax = 780, ymin = 290, ymax = 790)
right_coords <- c(xmin = 1140, xmax = 1640, ymin = 290, ymax = 790)
email <- "gonzalo.garciadecastro@upf.edu"

vocabulary <- import_vocabulary(
	location = c("barcelona", "oxford"),
	path_oxford = here("Data", "vocabulary_oxford.xlsx"),
	google_email = email
) %>% 
	distinct(id_db, age_group, .keep_all = TRUE) 

# participants -----------------------------------------------------------------
participants <- get_participants(google_email = email) %>% 
	rename(valid_other = valid_participant) %>% 
	left_join(vocabulary) %>% 
	filter(!pilot) %>% 
	select(participant, id_db, location, age_group, test_language, list, version, lp, vocab_size, valid_other, test_language, list, version, filename)

# trials -----------------------------------------------------------------------
trials <- stimuli %>%
	rename(trial = trial_id) %>% 
	mutate(trial = as.numeric(trial)) %>% 
	select(location, test_language, list, version, trial, trial_type, target_location,
		   prime, target, distractor, matches("_cdi")) %>% 
	filter(location=="Barcelona")

items_to_know <- trials %>% 
	select(prime_cdi, target_cdi, distractor_cdi) %>% 
	as.list() %>% 
	unlist() %>%
	unique()

# get gaze data
gaze <-	import_gaze(location = c("oxford", "barcelona"), participants = participants) %>% 
	mutate(
		gaze_in_l_aoi = gaze_in_aoi(x, y, left_coords),
		gaze_in_r_aoi = gaze_in_aoi(x, y, right_coords),
		fix_target = (target_location=="r" & gaze_in_r_aoi) | (target_location=="l" & gaze_in_l_aoi),
		fix_distractor = (target_location=="r" & gaze_in_l_aoi) | (target_location=="l" & gaze_in_r_aoi),
	) %>% 
	mutate_at(vars(age_group, lp, trial_type), as.factor) %>% 
	arrange(id_db, trial_num, time) %>% 
	select(-c(gaze_in_l_aoi, gaze_in_r_aoi)) %>% 
	relocate(id_db, lp, age_group, trial_num, time_bin, time, fix_target, fix_distractor, x, y, d, v, target_location)

# summarise by trial -----------------------------------------------------------
gaze_trial <- gaze %>%
	group_by(participant, location, age_group, trial_num, lp, trial_type) %>%
	summarise(
		fixations_target = sum(fix_target, na.rm = TRUE),
		fixations_distractor = sum(fix_distractor, na.rm = TRUE),
		fixations_missing = sum(is.na(fix_target) | is.na(fix_distractor)),
		.groups = "drop"
	) %>% 
	rowwise() %>% 
	mutate(
		fixations_n = fixations_target + fixations_distractor,
		prop = fixations_target/fixations_n
	) %>% 
	ungroup() %>% 
	left_join(select(participants, participant, age_group, vocab_size))

# missing data report ----------------------------------------------------------
attrition <- gaze_trial %>% 
	left_join(select(participants, participant, age_group, valid_other)) %>% 
	mutate(
		missing = fixations_missing/(fixations_n+fixations_missing),
		valid_trial_samples = missing < 0.25,
		valid_trial_both = (fixations_target > 0) & (fixations_distractor > 0),
		#valid_trial_vocab = (prime_cdi %in% unlist(vocab_words)) & (target_cdi %in% unlist(vocab_words)),
		valid_trial = valid_trial_samples & valid_trial_both
	) %>% 
	get_valid_participants() %>% 
	select(participant, age_group, trial_num, trial_type, matches("valid"), missing)

attrition_summary <- attrition %>%
	group_by(participant, age_group) %>%
	summarise(
		missing_n = sum(!valid_trial),
		missing_prop = sum(!valid_trial)/n(),
		valid_participant = unique(valid_participant),
		.groups = "drop"
	)

# summarise by time bin --------------------------------------------------------
gaze_time <- gaze %>% 
	group_by(participant, location, age_group, lp, trial_type, time_bin) %>% 
	summarise(
		fixations_target = sum(fix_target, na.rm = TRUE),
		fixations_distractor = sum(fix_distractor, na.rm = TRUE),
		fixations_missing = sum(is.na(fix_target) | is.na(fix_distractor)),
		n = n(),
		.groups = "drop"
	) %>% 
	rowwise() %>% 
	mutate(
		fixations_n = fixations_target + fixations_distractor,
		prop = fixations_target/n
	) %>% 
	ungroup() %>% 
	arrange(participant, time_bin) %>% 
	left_join(select(attrition, participant, valid_participant, valid_trial)) %>% 
	left_join(select(participants, participant, age_group, vocab_size)) %>% 
	filter(between(time_bin, 1, 20))

# export data ------------------------------------------------------------------
participants %>%
	left_join(attrition_summary) %>%
	saveRDS(here("Results", "participants.rds"))
saveRDS(trials, here("Results", "trials.rds"))
attrition %>% 
	left_join(attrition_summary) %>% 
	saveRDS(here("Results", "attrition.rds"))
gaze_trial %>% 
	left_join(select(attrition, participant, trial_num, valid_participant, valid_trial)) %>%
	saveRDS(here("Results", "gaze_trial.rds"))
saveRDS(gaze_time, here("Results", "gaze_time.rds"))











