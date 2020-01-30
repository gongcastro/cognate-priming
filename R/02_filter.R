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
library(stringr)      # for working with character strings
library(forcats)      # for dealing with NAs
library(eyetrackingR) # for processing eye-tracking data
library(here)         # for locating files

# set parameters
sampling_rate <- 120

#### import data #########################################################
data.processed <- fread(here("Data", "01_processed.txt"), sep = "\t", header = TRUE, stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  mutate(TrialID = as.numeric(TrialID))

#### filter data #########################################################

# list of trials with <75% looking time to prime in prime phase
exclude_prime <- data.processed %>%
  group_by(ParticipantID, TrialID) %>%
  filter(Phase == "Prime", GazePrime == 1) %>%
  summarise(n = n()) %>%
  mutate(Valid = (n >= ((1500/1000)*(sampling_rate)*0.75))) %>%
  filter(Valid == FALSE) %>%
  ungroup()

# list of participants with <50% valid trials regaring prime looking time
exclude_prime_ID <- exclude_prime %>%
  group_by(ParticipantID) %>%
  summarise(n = n()) %>%
  mutate(include = (n < (32*0.50))) %>%
  filter(include == FALSE) %>%
  ungroup()

#### filter data  ############################################

data.filtered <- data.processed %>%
	# remove participants labelled as non-valid
	filter(Valid) %>%
	# remove trials with <75% looking time to prime in prime phase
	anti_join(., exclude_prime, by = c("ParticipantID", "TrialID")) %>%
	# remove participants with <50% valid trials regaring prime looking time
	anti_join(., exclude_prime_ID, by = "ParticipantID") %>%
	# remove trials where participants were unfamiliar with any of the words
	# PENDING!!!!!
	# remove trial with <75% valid samples during target and distractor
	# remove participants with <50% valid trials during
	filter(Phase == "Target-Distractor", between(TimeStamp, 0, 2000)) %>%
	make_eyetrackingr_data(data               = .,
						   participant_column = "ParticipantID",
						   trackloss_column   = "Trackloss",
						   time_column        = "TimeStamp",
						   trial_column       = "TrialID",
						   aoi_columns        = c("GazeTarget", "GazeDistractor"),
						   treat_non_aoi_looks_as_missing = FALSE
  ) %>%
  clean_by_trackloss(data                    = .,
                     trial_prop_thresh       = 0.25,
                     participant_prop_thresh = 0.50)

#### analyse trackloss ###################################################
trackloss <- trackloss_analysis(data.filtered)

#### export data #########################################################
write.table(data.filtered, here("Data", "02_filtered.txt"), sep = "\t", row.names = FALSE)
write.table(trackloss, here("Data", "02_filtered-trackloss.txt"), sep = "\t", row.names = FALSE)

