#### processing: Prepare data for analysis -------------------------------------

# set up -----------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)
library(readxl)
library(lubridate)
library(multilex)
library(googlesheets4)
library(ggdist)
library(janitor)
library(scales)
library(here)

# load functions
source(here("R", "utils.R"))
source(here("R", "utils-gaze.R"))

# set params
sampling_rate <- 120  # how many samples does the eye-tracker take per second?
left_coords <- c(280, 780, 290, 790)
right_coords <- c(1140, 1640, 290, 790)

vocabulary <- import_vocabulary(location = c("bcn", "oxf")) %>% 
	distinct(id_db, age_group, .keep_all = TRUE) %>% 
	mutate(age_group = as.factor(age_group))

# participants -----------------------------------------------------------------
participants <- sample_get("gonzalo.garciadecastro@upf.edu") %>% 
	rename(valid_other = valid_participant) %>% 
	filter(
		(location=="BCN" & !is.na(filename)) | location=="OXF",
		# valid_participant
		# date_test > as_date("2020-02-14") # get participants with right timestamps
	) %>%
	rename(participant = participant_id) %>% 
	mutate(age_group = as.factor(age_group)) %>% 
	left_join(vocabulary) %>% 
	select(participant, id_db, location, age_group, test_language, list, version, lp, vocab_size, valid_other, test_language, list, version, filename)

# trials -----------------------------------------------------------------------
trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	rename(trial = trial_id) %>% 
	mutate(trial = as.numeric(trial)) %>% 
	select(location, test_language, list, version, trial, trial_type, target_location,
		   prime, target, distractor, matches("_cdi")) %>% 
	filter(location=="BCN")

items_to_know <- trials %>% 
	select(prime_cdi, target_cdi, distractor_cdi) %>% 
	as.list() %>% 
	unlist() %>%
	unique()

# get gaze data
gaze <-	import_gaze(location = c("oxf", "bcn"), participants = participants) %>% 
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











