# 01_merge: merge data #####################################################
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up data ###########################################################
library(tidyverse)
library(magrittr)     # for using pipes
library(data.table)   # for importing data
library(readxl)       # for importing Excel files
library(stringr)      # for working with character strings
library(purrr)        # for working with lists
library(here)         # for locating files

# load functions
source(here("R", "functions.R"))

# set experimental parameters
sampling_rate  <- 120  # how many samples does the eye-tracker take per second?
screen_x       <- 1920 # width of the screen in pixels 
screen_y       <- 1080 # height of the screen in pixels

#### import data #######################################
dat_participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	filter(!pilot) %>%
	drop_na(participant_id)
dat_trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	mutate(trial_id = as.character(trial_id))
dat_gaze <- fread(here("Data", "01_fixations.csv"), sep = ",", dec = ".", stringsAsFactors = FALSE, na.strings = "") %>%
	mutate(trial_id = as.character(trial_id))
dat_vocab <- fread(here("Data", "Vocabulary data", "vocabulary.csv"), sep = ",", dec = ".", stringsAsFactors = FALSE, na.strings = "") %>%
	rename(item_language = language,
		   participant_id = id_exp) %>% 
	select(participant_id, item, item_language, response) %>%
	filter(participant_id %in% dat_participants$participant_id)

dat_merged <- dat_vocab %>% 
	mutate(understands = response > 1,
		   produces = response > 2) %>%
	group_by(participant_id) %>% 
	summarise(n = n(),
			  n_spa = length(unique(item[item_language=="Spanish"])),
			  n_cat = length(unique(item[item_language=="Catalan"])),
			  understands_spa = sum(understands[item_language=="Spanish"], na.rm = TRUE),
			  produces_spa = sum(produces[item_language=="Spanish"], na.rm = TRUE),
			  understands_cat = sum(understands[item_language=="Catalan"], na.rm = TRUE),
			  produces_cat = sum(produces[item_language=="Catalan"], na.rm = TRUE),
			  understands_spa_list = list(item[understands & item_language=="Spanish"]),
			  produces_spa_list = list(item[produces & item_language=="Spanish"]),
			  understands_cat_list = list(item[understands & item_language=="Catalan"]),
			  produces_cat_list = list(item[produces & item_language=="Catalan"]),
			  .groups = "drop") %>% 
	filter(participant_id %in% unique(dat_gaze$participant_id)) %>%
	left_join(dat_participants) %>%
	left_join(dat_trials) %>%
	rowwise() %>%
	mutate(vocab_comp_spa = prod(understands_spa, 1/n_spa, na.rm = TRUE),
		   vocab_prod_spa = prod(produces_spa, 1/n_spa, na.rm = TRUE),
		   vocab_comp_cat = prod(understands_cat, 1/n_cat, na.rm = TRUE),
		   vocab_prod_cat = prod(produces_cat, 1/n_cat, na.rm = TRUE),
		   valid_vocab_prime = ifelse(test_language=="Spanish", prime_cdi %in% unlist(produces_spa_list), prime_cdi %in% unlist(produces_cat_list)),
		   valid_vocab_target = ifelse(test_language=="Spanish", target_cdi %in% unlist(understands_spa_list), target_cdi %in% unlist(understands_cat_list)),
		   valid_vocab_distractor = ifelse(test_language=="Spanish", target_cdi %in% unlist(understands_spa_list), target_cdi %in% unlist(understands_cat_list)),
		   valid_trial_vocab = valid_vocab_prime & valid_vocab_target) %>%
	ungroup() %>%
	select(participant_id, trial_id, vocab_comp_spa, vocab_prod_spa, vocab_comp_cat, vocab_prod_cat, valid_trial_vocab) 

#### merge data #############################################################
dat <- left_join(dat_gaze, dat_participants) %>%
	left_join(dat_trials) %>% 
	left_join(dat_merged) %>% 
	select(participant_id, age, sex, lp, trial_id, trial_type, phase, time, x, y, trackloss, fix_prime, fix_target, fix_distractor, location, test_language, list, prime, target, distractor, valid_trial_vocab, vocab_comp_spa, vocab_prod_spa, vocab_comp_cat, vocab_prod_cat)

#### export data ###############################################
fwrite(dat, file = here("Data", "02_merged.csv"), sep = ",", row.names = FALSE)

