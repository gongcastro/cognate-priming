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
library(eyetrackingR) # for processing eye-tracking data
library(here)         # for locating files

# set parameters
sampling_rate  <- 120   # samples per second
prime_duration <- 1500  # prime presentation duration (ms)
target_duration <- 2000 # target-distractor presentation duration (ms)

#### import data #########################################################

# import processed gaze data
data.processed <- fread(here("Data", "01_processed.txt"), sep = "\t", header = TRUE, stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  mutate(TrialID = as.numeric(TrialID))

# import participants
participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	rename(ParticipantID    = ID,
		   ValidParticipant = Valid) %>%
	filter(!Pilot)

# import trials
trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	select(-c(TargetLocation, TrialType))

# import vocabulary data
vocab <- fread("~/projects/BiLexicon/Data/02_merged.txt") %>% select(ID, Item, Response, LanguageItem = Language, Dominance)

trial.familiarity <- left_join(participants, trials, by = c("Language", "Version", "List", "Location")) %>%
	pivot_longer(c("PrimeCDI", "TargetCDI", "DistractorCDI"), names_to = "Role", values_to = "Item") %>%
	left_join(vocab, by = c("ParticipantID" = "ID", "Item", "Language" = "Dominance")) %>%
	select(ParticipantID, TrialID, ValidParticipant, Language, LanguageItem, Role, Item, Response) %>%
	pivot_wider(id_cols = c("ParticipantID", "TrialID", "Language", "ValidParticipant", "LanguageItem", "Item"),
				names_from = c("Role", "LanguageItem"),
				values_from = "Response") %>%
	mutate(AllKnown = case_when(Language=="Spanish" ~ PrimeCDI_Spanish & PrimeCDI_Catalan & TargetCDI_Spanish & DistractorCDI_Spanish,
								Language=="Catalan" ~ PrimeCDI_Catalan & PrimeCDI_Spanish & TargetCDI_Catalan & DistractorCDI_Catalan,
								TRUE                ~ NA)) %>%
	select(ParticipantID, TrialID, AllKnown, Pilot)

#### identify trials to remove #############################################

# trials that are not valid because of how the words involved (constant across participants)
exclude_list_trials <- trials %>%
	filter(!ValidTrial) %>%
	rename(ValidTrialList = ValidTrial) %>%
	select(TrialID, Location, Language, List, ValidTrialList)
message(paste0("Trials to exclude due to the properties of the words involved: ",
			   nrow(filter(exclude_list_trials, !ValidTrialList)),
			   " out of ",
			   nrow(exclude_list_trials),
			   " (", round(100*nrow(filter(exclude_list_trials, !ValidTrialList))/nrow(exclude_list_trials), digits = 2), "%)"))

# trials with <75% valid samples in prime AOI during prime phase
exclude_prime_aoi <- data.processed %>%
	filter(Phase == "Prime") %>%
	group_by(ParticipantID, TrialID) %>%
	summarise(SamplesPrime = sum(GazePrime, na.rm = TRUE),
			  SamplesTotal = n(),
			  SamplesPrimeProp = prod(SamplesPrime, 1/SamplesTotal, na.rm = TRUE)
	) %>%
	mutate(ValidPrime = SamplesPrimeProp>=0.75,
		   TrialID = as.character(TrialID) 
	) %>%
	ungroup()
message(paste0("Trials to exclude due to <75% valid samples in prime AOI: ",
			   nrow(filter(exclude_prime_aoi, !ValidPrime)),
			   " out of ",
			   nrow(exclude_prime_aoi),
			   " (", round(100*nrow(filter(exclude_prime_aoi, !ValidPrime))/nrow(exclude_prime_aoi), digits = 2), "%)"))
	
# trials with < 75% valid samples during target-distractor phase
exclude_target_trackloss <- data.processed %>%
	filter(Phase == "Target-Distractor") %>%
	group_by(ParticipantID, TrialID) %>%
	summarise(SamplesTrackloss = sum(Trackloss, na.rm = TRUE),
			  SamplesTotal = n(),
			  SamplesTracklossProp = prod(SamplesTrackloss, 1/SamplesTotal, na.rm = TRUE)
	) %>%
	mutate(ValidTargetTrackloss = SamplesTracklossProp<0.25,
		   TrialID = as.character(TrialID)
	) %>%
	ungroup()
message(paste0("Trials to exclude due to <75% valid samples during Target-Distractor presentation: ",
			   nrow(filter(exclude_target_trackloss, !ValidTargetTrackloss)),
			   " out of ",
			   nrow(exclude_target_trackloss),
			   " (", round(100*nrow(filter(exclude_target_trackloss, !ValidTargetTrackloss))/nrow(exclude_target_trackloss), digits = 2), "%)"))

# trials where target and/or distractor where not fixated at any point
exclude_target_fixation <- data.processed %>%
	filter(Phase == "Target-Distractor") %>%
	group_by(ParticipantID, TrialID) %>%
	summarise(
		SamplesTotal = n(),
		SamplesTargetAOI = sum(GazeTarget, na.rm = TRUE),
		SamplesDistractorAOI = sum(GazeDistractor, na.rm = TRUE),
		SamplesTargetProp = prod(SamplesTargetAOI, 1/SamplesTotal, na.rm = TRUE),
		SamplesDistractorProp = prod(SamplesDistractorAOI, 1/SamplesTotal, na.rm = TRUE),
	) %>%
	mutate(
		ValidTargetFixation = SamplesTargetProp >= 0.05,
		ValidDistractorFixation = SamplesDistractorProp >= 0.05,
		ValidFixation = ValidTargetFixation & ValidDistractorFixation,
		TrialID = as.character(TrialID)
		) %>%
	ungroup()
message(paste0("Trials to exclude due to <5% valid samples in target AOI and <5% samples in distractor AOI during Target-Distractor presentation: ",
			   nrow(filter(exclude_target_fixation, !ValidFixation)),
			   " out of ",
			   nrow(exclude_target_fixation),
			   " (", round(100*nrow(filter(exclude_target_fixation, !ValidFixation))/nrow(exclude_target_fixation), digits = 2), "%)"))

# trials where participant was unfamiliar with prime, target, or distractor

# join all excluded trials
exclude_trials <- exclude_list_trials %>%
	mutate(TrialID = as.character(TrialID)) %>%
	full_join(select(exclude_prime_aoi, -SamplesTotal), by = c("TrialID")) %>%
	left_join(select(exclude_target_trackloss, -SamplesTotal), by = c("ParticipantID", "TrialID")) %>%
	full_join(exclude_target_fixation, by = c("ParticipantID", "TrialID")) %>%
	mutate(ValidTrial = ValidPrime & ValidTargetTrackloss & ValidFixation) %>%
	select(ParticipantID, TrialID, ValidTrial) 
message(paste0("Total trials excluded: ",
			   nrow(filter(exclude_trials, !ValidTrial)),
			   " out of ",
			   nrow(exclude_trials),
			   " (", round(100*nrow(filter(exclude_trials, !ValidTrial))/nrow(exclude_trials), digits = 2), "%)"))

# participants with less than 50% valid trials in each condition
exclude_ntrials_participant <- data.processed %>%
	mutate(TrialID = as.character(TrialID)) %>%
	group_by(ParticipantID, TrialID, TrialType) %>%
	summarise(n = n()) %>%
	left_join(exclude_trials, by = c("ParticipantID", "TrialID")) %>%
	select(ParticipantID, TrialID, TrialType, ValidTrial) %>%
	ungroup() %>%
	group_by(ParticipantID, TrialType) %>%
	summarise(
		ValidTrials = sum(ValidTrial),
		TotalTrials = n(),
		ValidTrialsProp = mean(ValidTrial, na.rm = TRUE)) %>%
	mutate(ValidNTrialParticipant = ValidTrialsProp >=0.50)

message(paste0("Participants excluded for having <50% valid trials in each condition: ",
			   nrow(filter(exclude_ntrials_participant, !ValidNTrialParticipant)),
			   " out of ",
			   nrow(exclude_ntrials_participant),
			   " (", round(100*nrow(filter(exclude_ntrials_participant, !ValidNTrialParticipant))/nrow(exclude_ntrials_participant), digits = 2), "%)"))

# participants to be excluded for other reasons
exclude_participants_other <- participants %>%
	select(ParticipantID, ValidParticipant)
message(paste0("Participants excluded for other reasons: ",
			   nrow(filter(exclude_participants_other, !ValidParticipant)),
			   " out of ",
			   nrow(exclude_participants_other),
			   " (", round(100*nrow(filter(exclude_participants_other, !ValidParticipant))/nrow(exclude_participants_other), digits = 2), "%)"))

# participants with very low vocabulary

# join all excluded participants
exclude_participants <- exclude_participants_other %>%
	left_join(exclude_ntrials_participant, by = "ParticipantID") %>%
	mutate(ValidParticipant = ValidParticipant & ValidNTrialParticipant) %>%
	distinct(ParticipantID, ValidParticipant) %>%
	arrange(ParticipantID)
message(paste0("Participants excluded: ",
			   nrow(filter(exclude_participants, !ValidParticipant)),
			   " out of ",
			   nrow(exclude_participants),
			   " (", round(100*nrow(filter(exclude_participants, !ValidParticipant))/nrow(exclude_participants), digits = 2), "%)"))

#### filter data  ############################################

data.filtered <- data.processed %>%
	mutate(TrialID = as.character(TrialID)) %>%
	left_join(exclude_participants, by = "ParticipantID") %>%
	left_join(exclude_trials, by = c("ParticipantID", "TrialID")) %>%
	filter(ValidParticipant,
		   ValidTrial,
		   Phase == "Target-Distractor",
		   between(TimeStamp, 0, 2000)) %>%
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

#### register logs ##############################################
logs <- data.filtered %>%
	group_by(ParticipantID, TrialID) %>%
	summarise(
		TimeStart = min(TimeStamp, na.rm = TRUE),
		TimeStop  = max(TimeStamp, na.rm = TRUE),
		MinX      = min(meanX, na.rm = TRUE),
		MaxX      = min(meanX, na.rm = TRUE),
		MinY      = max(meanY, na.rm = TRUE),
		MaxY      = max(meanY, na.rm = TRUE),
		MinDist   = min(meanDistance, na.rm = TRUE),
		MaxDist   = max(meanDistance, na.rm = TRUE),
	) %>%
	ungroup()

#### export data #########################################################
fwrite(data.filtered, here("Data", "02_filtered.txt"), sep = "\t", row.names = FALSE)
fwrite(exclude_trials, here("Data", "02_filtered-excluded-trials.txt"), sep = "\t", row.names = FALSE)
fwrite(exclude_participants, here("Data", "02_filtered-excluded-trials.txt"), sep = "\t", row.names = FALSE)
fwrite(logs, here("Data", "02_filtered-logs.txt"), sep = "\t", row.names = FALSE)

