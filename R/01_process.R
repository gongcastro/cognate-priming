# 01_process: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up data ###########################################################
library(magrittr)     # for using pipes
library(tibble)       # for tidy data presentation
library(dplyr)        # for manipulating data
library(tidyr)        # for rehsaping datasets
library(data.table)   # for importing data
library(readxl)       # for importing Excel files
library(stringr)      # for working with character strings
library(purrr)        # for working with lists
library(here)         # for locating files

# load functions
source(here("R", "Functions", "prime_coords.R"))
source(here("R", "Functions", "target_coords.R"))
source(here("R", "Functions", "distractor_coords.R"))
"%!in%" <- function(x, y) !(x %in% y)

# set experimental parameters
sampling_rate <- 120  # how many samples does the eye-tracker take per second?
screenX       <- 1920 # width of the screen in pixels 
screenY       <- 1080 # height of the screen in pixels

#### import data #######################################

# import participant-level data
participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	drop_na(Version) %>%
	rename(ParticipantID    = ID,
		   ValidParticipant = Valid)

# import gaze data
data.raw.exp   <- fread(here("Data", "00_raw.txt"), sep = "\t", dec = ".")       # import data from experiment
data.raw.pilot <- fread(here("Data", "00_raw-pilot.txt"), sep = "\t", dec = ".") # import data from pilot
data.raw <- rbind(data.raw.exp, data.raw.pilot)

# import trial-level data
trials <- read_xlsx(here("Stimuli", "stimuli.xlsx"))

#### merge data #############################################################
data.merged <- left_join(data.raw, participants, by = "ParticipantID") %>%
	left_join(., trials, by = c("TrialID", "Language", "Version", "List")) %>%
	as_tibble() %>%
	select(ParticipantID, TrialID, Phase, TimeStamp, meanX, meanY, meanDistance, Trackloss, TargetLocation, TrialType, Language, List, Version, DateTest, DateBirth, Age, Sex, LangProfile, ValidParticipant, ValidTrial)

#### process data ###########################################################
data <- data.merged %>%
	mutate(
		TrialID = as.character(TrialID),
		# evaluate if gaze is in prime AOI
		GazePrime = prime_coords(data = .,
							 x_gaze = meanX,
							 y_gaze = meanY),
		# evaluate if gaze is in target AOI
		GazeTarget = target_coords(data = .,
							  x_gaze = meanX,
							  y_gaze = meanY,
							  target_location = TargetLocation),
		# evaluate if gaze is in distractor AOI
		GazeDistractor = distractor_coords(data = .,
								  x_gaze = meanX,
								  y_gaze = meanY,
								  target_location = TargetLocation),
	)

#### export data ###############################################
fwrite(data, file = here("Data", "01_processed.txt"), sep = "\t", row.names = FALSE)
