# 02_filter: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(magrittr)     # for using pipes
library(tibble)       # for tidy data presentation
library(dplyr)        # for manipulating data
library(tidyr)        # for rehsaping datasets
library(data.table)   # for importing data
library(readxl)       # for importing Excel files
library(stringr)      # for working with character strings
library(forcats)      # for dealing with NAs
library(janitor)      # for cleaning variable names
library(here)         # for locating files

# load functions
source(here("R", "functions.R"))

# set parameters
sampling_rate   <- 120   # samples per second
prime_duration  <- 1500  # prime presentation duration (ms)
target_duration <- 2000 # target-distractor presentation duration (ms)

#### import data #########################################################
participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	clean_names() %>%
	rename(participant_id = id, valid_participant = valid)
# import processed gaze data
dat_processed <- fread(here("Data", "01_processed.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE) %>%
	as_tibble() %>%
	mutate(trial_id = as.numeric(trial_id))

# import vocabulary data
vocab <- fread("~/projects/BiLexicon/Data/03_vocabulary-wide.csv") %>%
	select(id_db, receptive_vocab, productive_vocab, language) %>%
	as_tibble() %>%
	rename(language_vocab = language)

# import participants
participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	clean_names() %>% 
	rename(participant_id = id,
		   valid_participant = valid) %>%
	filter(!pilot) %>%
	left_join(vocab, by = "id_db")

# import trials
trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	clean_names() %>% 
	select(-c(target_location, trial_type)) %>%
	drop_na(location)

#### identify trials to remove #############################################

# trials that are not valid because of how the words involved (constant across participants)
exclude_list_trials <- trials %>%
	filter(!valid_trial) %>%
	rename(valid_trial_list = valid_trial) %>%
	select(trial_id, location, language, list, valid_trial_list) %>%
	mutate(trial_id = as.character(trial_id))
message(paste0("Trials to exclude due to the properties of the words involved: ",
			   nrow(filter(exclude_list_trials, !valid_trial_list)),
			   " out of ",
			   nrow(exclude_list_trials),
			   " (", round(100*nrow(filter(exclude_list_trials, !valid_trial_list))/nrow(exclude_list_trials), digits = 2), "%)"))

# trials with <75% valid samples in prime AOI during prime phase
exclude_prime_aoi <- dat_processed %>%
	filter(phase == "Prime") %>%
	mutate(trial_id = as.character(trial_id)) %>%
	group_by(participant_id, trial_id) %>%
	summarise(samples_prime = sum(gaze_prime, na.rm = TRUE),
			  samples_prime_total = n(),
			  .groups = "drop") %>%
	rowwise() %>% 
	mutate(samples_prime_prop = prod(samples_prime, 1/samples_prime_total, na.rm = TRUE)) %>%
	ungroup() %>%
	mutate(valid_prime = samples_prime_prop>=0.50) 



message(paste0("Trials to exclude due to <75% valid samples in prime AOI: ",
			   nrow(filter(exclude_prime_aoi, !valid_prime)),
			   " out of ",
			   nrow(exclude_prime_aoi),
			   " (", round(100*nrow(filter(exclude_prime_aoi, !valid_prime))/nrow(exclude_prime_aoi), digits = 2), "%)"))
	
# trials with < 75% valid samples during target-distractor phase
exclude_target_trackloss <- dat_processed %>%
	filter(phase == "Target-Distractor") %>%
	group_by(participant_id, trial_id) %>%
	summarise(samples_trackloss = sum(trackloss, na.rm = TRUE),
			  samples_trackloss_total = n(),
			  samples_trackloss_prop = prod(samples_trackloss, 1/samples_trackloss_total, na.rm = TRUE),
			  .groups = "drop") %>%
	mutate(valid_target_trackloss = samples_trackloss_prop<0.25,
		   trial_id = as.character(trial_id)) %>%
	ungroup()
message(paste0("Trials to exclude due to <75% valid samples during Target-Distractor presentation: ",
			   nrow(filter(exclude_target_trackloss, !valid_target_trackloss)),
			   " out of ",
			   nrow(exclude_target_trackloss),
			   " (", round(100*nrow(filter(exclude_target_trackloss, !valid_target_trackloss))/nrow(exclude_target_trackloss), digits = 2), "%)"))

# trials where target and/or distractor where not fixated at any point
exclude_target_fixation <- dat_processed %>%
	filter(phase == "Target-Distractor") %>%
	group_by(participant_id, trial_id) %>%
	summarise(samples_total = n(),
			  samples_target_aoi = sum(gaze_target, na.rm = TRUE),
			  samples_distractor_aoi = sum(gaze_distractor, na.rm = TRUE),
			  samples_target_prop = prod(samples_target_aoi, 1/samples_total, na.rm = TRUE),
			  samples_distractor_prop = prod(samples_distractor_aoi, 1/samples_total, na.rm = TRUE),
			  .groups = "drop") %>%
	mutate(valid_target_fixtation = samples_target_prop >= 0.05,
		   valid_distractor_fixation = samples_target_prop >= 0.05,
		   valid_fixation = valid_target_fixtation & valid_distractor_fixation,
		   trial_id = as.character(trial_id)) %>%
	ungroup()
message(paste0("Trials to exclude due to <5% valid samples in target AOI and <5% samples in distractor AOI during Target-Distractor presentation: ",
			   nrow(filter(exclude_target_fixation, !valid_fixation)),
			   " out of ",
			   nrow(exclude_target_fixation),
			   " (", round(100*nrow(filter(exclude_target_fixation, !valid_fixation))/nrow(exclude_target_fixation), digits = 2), "%)"))

# trials where participant was unfamiliar with prime, target, or distractor

# join all excluded trials
exclude_trials <- exclude_list_trials %>%
	mutate(trial_id = as.character(trial_id)) %>%
	full_join(select(exclude_prime_aoi, -samples_prime_total), by = c("trial_id")) %>%
	left_join(select(exclude_target_trackloss, -samples_trackloss_total), by = c("participant_id", "trial_id")) %>%
	full_join(exclude_target_fixation, by = c("participant_id", "trial_id")) %>%
	mutate(valid_trial = valid_prime & valid_target_trackloss & valid_fixation) %>%
	select(participant_id, trial_id, valid_trial) 
message(paste0("Total trials excluded: ",
			   nrow(filter(exclude_trials, !valid_trial)),
			   " out of ",
			   nrow(exclude_trials),
			   " (", round(100*nrow(filter(exclude_trials, !valid_trial))/nrow(exclude_trials), digits = 2), "%)"))

# participants with less than 50% valid trials in each condition
exclude_ntrials_participant <- dat_processed %>%
	mutate(trial_id = as.character(trial_id)) %>%
	group_by(participant_id, trial_id, trial_type) %>%
	summarise(n = n(), .groups = "drop") %>%
	left_join(exclude_trials, by = c("participant_id", "trial_id")) %>%
	select(participant_id, trial_id, trial_type, valid_trial) %>%
	ungroup() %>%
	group_by(participant_id, trial_type) %>%
	summarise(
		valid_trials = sum(valid_trial),
		total_trials = n(),
		valid_trials_prop = mean(valid_trial, na.rm = TRUE),
		.groups = "drop") %>%
	mutate(valid_n_trial_participant = valid_trials_prop >=0.50,
		   .groups = "drop") %>%
	ungroup()

message(paste0("Participants excluded for having <50% valid trials in each condition: ",
			   nrow(filter(exclude_ntrials_participant, !valid_n_trial_participant)),
			   " out of ",
			   nrow(exclude_ntrials_participant),
			   " (", round(100*nrow(filter(exclude_ntrials_participant, !valid_n_trial_participant))/nrow(exclude_ntrials_participant), digits = 2), "%)"))

# participants to be excluded for other reasons
exclude_participants_other <- participants %>%
	select(participant_id, valid_participant)
message(paste0("Participants excluded for other reasons: ",
			   nrow(filter(exclude_participants_other, !valid_participant)),
			   " out of ",
			   nrow(exclude_participants_other),
			   " (", round(100*nrow(filter(exclude_participants_other, !valid_participant))/nrow(exclude_participants_other), digits = 2), "%)"))

# participants with very low vocabulary

# join all excluded participants
exclude_participants <- exclude_participants_other %>%
	left_join(exclude_ntrials_participant, by = "participant_id") %>%
	mutate(valid_participant = valid_participant & valid_n_trial_participant) %>%
	distinct(participant_id, valid_participant) %>%
	arrange(participant_id)
message(paste0("Participants excluded: ",
			   nrow(filter(exclude_participants, !valid_participant)),
			   " out of ",
			   nrow(exclude_participants),
			   " (", round(100*nrow(filter(exclude_participants, !valid_participant))/nrow(exclude_participants), digits = 2), "%)"))

#### merge all excluded trials ###############################
excluded_all <- left_join(exclude_prime_aoi, exclude_target_trackloss, by = c("participant_id", "trial_id")) %>%
	left_join(., exclude_target_fixation, by = c("participant_id", "trial_id")) %>%
	left_join(., exclude_ntrials_participant, by = c("participant_id")) %>%
	left_join(., exclude_participants_other, by = c("participant_id"))

#### filter data  ############################################
dat_filtered <- dat_processed %>%
	mutate(trial_id = as.character(trial_id)) %>%
	left_join(exclude_participants, by = "participant_id") %>%
	left_join(exclude_trials, by = c("participant_id", "trial_id")) %>%
	filter(valid_participant,
		   valid_trial,
		   phase == "Target-Distractor",
		   between(timestamp, 0, 2000)) %>%
	select(-c(valid_participant, valid_trial))


#### export data #########################################################
fwrite(dat_filtered, here("Data", "02_filtered.csv"), sep = ",", row.names = FALSE)
fwrite(excluded_all, here("Data", "02_filtered-excluded.csv"), sep = ",", row.names = FALSE)
fwrite(logs, here("Data", "Logs", "02_filtered-logs.csv"), sep = ",", row.names = FALSE)

dat_filtered %>%
	group_split(participant_id) %>%
	map(~fwrite(., here("Data", "Gaze data (clean)", paste0(unique(.$participant_id), "_clean.csv")), sep = ",", dec = ".", row.names = FALSE))
