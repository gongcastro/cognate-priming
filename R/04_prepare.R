# 04_prepares: Prepare data for Growth Curve Analysis
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up #############################################2

# load packages
library(data.table) # for importing and exporting data
library(dplyr)      # for manipulating data
library(tidyr)      # for rehsaping datasets
library(readxl)     # for importing Excel files
library(tibble)     # for more informative data frames
library(here)       # for locating files

#### import data #########################################
# import fixation data
fixations <- fread(here("Data", "03_fixations.txt"),
				   sep = "\t", dec = ".", stringsAsFactors = FALSE) %>%
	select(-c(Pilot, TotalSamples, SamplesTarget, SamplesDistractor, -OddsFix)) %>%
	as_tibble()

# import participant data
participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	rename(ParticipantID = ID) %>%
	select(ParticipantID, LangProfile, Location, Language, List)

# import trial data
trials <- fread(here("Data", "Stimuli", "02_stimuli-stats.txt")) %>%
	as_tibble() %>%
	unite("TrialIDLabel", c("Prime", "Distractor"), sep = "_")  %>%
	select(TrialID, TargetID = Target, Location, Language, List, FreqPrime, FreqTarget, LevenshteinPrime)


#### merge data ##########################################
data <- fixations %>%
	left_join(., participants, by = c("ParticipantID", "LangProfile", "Language")) %>%
	left_join(., trials, by = c("TrialID", "Language", "Location", "List")) %>%
	drop_na(TimeBin) %>%
	mutate(
		TrialType = factor(
			case_when(
				TrialType == "Unrelated"   ~ 0,
				TrialType == "Non-cognate" ~ 1,
				TRUE                       ~ 2
			)
		),
		Language = factor(ifelse(Language=="Spanish", -0.5, 0.5)),
		LangProfile = factor(ifelse(LangProfile=="Monolingual", 0, 1))
	)
t <- poly((unique(data$TimeBin)), 3)
data[,paste("TimeBin", 1:3, sep = "")] <- t[data$TimeBin, 1:3]

#### define time window if interest
data <- data %>% filter(TimeBin > 2)

#### define contrasts ####################################
contrasts(data$TrialType) <- contr.poly(3)

#### reorder variables ###################################
data <- data %>%
	select(ParticipantID, TargetID, starts_with("Time"),
		   ElogitFix, ProbFix, Weights, TrialType, LangProfile, Language,
		   FreqPrime, FreqTarget, LevenshteinPrime)

#### register logs #######################################
logs.participants <- data %>%
	group_by(ParticipantID, TrialType, Language, LangProfile) %>%
	summarise(
		TimeBinN    = n(),
		TimeBinMin  = min(TimeBin, na.rm = TRUE),
		TimeBinMax  = max(TimeBin, na.rm = TRUE),
		ProbFixMean = mean(ProbFix, na.rm = TRUE),
		ProbFixSD   = sd(ProbFix, na.rm = TRUE),
		ProbFixSEM  = sd(ProbFix, na.rm = TRUE)/sqrt(TimeBinN)
	)

logs.trials <- data %>%
	group_by(TargetID, TrialType, Language, LangProfile) %>%
	summarise(
		TimeBinN    = n(),
		TimeBinMin  = min(TimeBin, na.rm = TRUE),
		ProbFixMean = median(ProbFix, na.rm = TRUE),
		ProbFixMean = mean(ProbFix, na.rm = TRUE),
		ProbFixSD   = sd(ProbFix, na.rm = TRUE),
		ProbFixSEM  = sd(ProbFix, na.rm = TRUE)/sqrt(TimeBinN)
	)


#### export data #########################################
fwrite(data, here("Data", "04_prepared.txt"), sep = "\t", dec = ".", row.names = FALSE)
fwrite(logs.participants, here("Data", "04_prepared-log-participants.txt"), sep = "\t", dec = ".", row.names = FALSE)
fwrite(logs.trials, here("Data", "04_prepared-log-trials.txt"), sep = "\t", dec = ".", row.names = FALSE)

