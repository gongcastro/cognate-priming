# 02_filter: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(tidyverse)
library(tidylog)
library(magrittr)     # for using pipes
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
dat_gaze <- fread(here("Data", "02_merged.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "") %>%
	as_tibble() %>%
	mutate(trial_id = as.numeric(trial_id))
dat_vocab <- fread(here("Data", "Vocabulary data", "vocabulary.csv"), sep = ",", dec = ".", stringsAsFactors = FALSE, na.strings = "") %>%
	rename(item_language = language,
		   participant_id = id_exp) %>% 
	select(participant_id, item, item_language, response) %>%
	filter(participant_id %in% dat_participants$participant_id)
dat_participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	filter(!pilot) %>%
	drop_na(participant_id) %>%
	distinct(participant_id, test_language)
dat_trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	mutate(trial_id = as.character(trial_id))

#### filter data #########################################################
dat_filter_trials <- dat_gaze %>%
	group_by(participant_id, trial_id, phase, trial_type, test_language, valid_trial_vocab, vocab_comp_cat, vocab_comp_spa, vocab_prod_cat, vocab_prod_spa) %>%
	summarise(n = n(),
			  prop_fix_prime = mean(fix_prime, na.rm = TRUE),
			  prop_fix_target = mean(fix_target, na.rm = TRUE),
			  prop_fix_distractor = mean(fix_distractor, na.rm = TRUE),
			  prop_trackloss = mean(trackloss, na.rm = TRUE),
			  .groups = "drop") %>%
	rowwise() %>% 
	mutate_at(vars(prop_fix_prime, prop_fix_target, prop_fix_distractor, prop_trackloss), function(x) ifelse(is.na(x), FALSE, x)) %>% 
	mutate(prop_fix_tardis = sum(prop_fix_target, prop_fix_distractor, na.rm = TRUE)) %>% 
	ungroup() %>%
	mutate(valid_phase = ifelse(phase=="Prime",
								prop_fix_prime>=0.50 & prop_trackloss<0.25 & valid_trial_vocab,
								prop_fix_tardis>=0.50 & prop_trackloss<0.25 & valid_trial_vocab)) %>% 
	group_by(participant_id, trial_id, trial_type) %>% 
	summarise(valid_trial = all(valid_phase)) %>%
	select(participant_id, trial_id, trial_type, valid_trial)
			  
dat_filter_participants <- dat_filter_trials %>%
	group_by(participant_id, trial_type) %>%
	summarise(valid_trial = sum(valid_trial, na.rm = TRUE)) %>%  
	mutate(valid_condition = valid_trial > 1) %>% 
	group_by(participant_id) %>% 
	summarise(valid_condition = sum(valid_condition, na.rm = TRUE)) %>% 
	mutate(valid_participant = valid_condition>=3) %>%
	select(participant_id, valid_participant)

dat <- reduce(list(dat_gaze, dat_filter_trials, dat_filter_participants), left_join) %>%
	filter(valid_trial, valid_participant) %>%
	select(-c(valid_trial, valid_participant, valid_condition))

#### descriptives ########################################################
descriptives <- dat %>%
	distinct(participant_id, lp, trial_id, trial_type) %>%
	group_by(participant_id, lp, trial_type) %>% 
	summarise(n = n()) %>%
	ungroup()

#### export data #########################################################
fwrite(dat, here("Data", "03_filtered.csv"), sep = ",", row.names = FALSE)
fwrite(descriptives, here("Data", "03_filtered-descriptives.csv"), sep = ",", row.names = FALSE)
