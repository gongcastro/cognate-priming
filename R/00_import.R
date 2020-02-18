# 00_import: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(dplyr)        # for manipulating data
library(tidyr)        # for rehsaping datasets
library(data.table)   # for importing data
library(readxl)       # for importing Excel files
library(tibble)       # for more informative data frames
library(stringr)      # for working with character strings
library(purrr)        # for working with lists
library(here)         # for locating files

# load functions
"%!in%" <- function(x, y) !(x %in% y)

# set experimental parameters
sampling_rate <- 120  # how many samples does the eye-tracker take per second?
screenX       <- 1920 # width of the screen in pixels
screenY       <- 1080 # height of the screen in pixels

#### import data #######################################

# import participant-level data
participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	filter(!Pilot) %>%
	drop_na(Version) 

# import eye-tracking data
data <- paste0(here("Data", "Gaze data", "Barcelona/"), participants$Filename) %>% # locate files with gaze data
	map(., ~fread(., sep = ",", dec = '.', header = TRUE, stringsAsFactors = FALSE, na.strings = c("NaN", "NA", "Na", "-", ""))) %>%  # import files in the folder
	set_names(participants$ID) %>% # name each list element with its file name 
	map(~select(., one_of("SystemTimeStamp", "lX", "lY", "lV", "rV", "rX", "rY", "lUserCoordZ", "rUserCoordZ", # select only relevant variables
						  "Participant", "TrialNum", "Trial", "Phase"))) %>%
	map(~mutate(., Participant = paste0("cognatepriming", Participant))) %>%
	map(~mutate(., SystemTimeStamp = as.numeric(SystemTimeStamp))) %>%
	map(~mutate(., lX = as.numeric(lX))) %>%
	map(~mutate(., lY = as.numeric(lY))) %>%
	map(~mutate(., rX = as.numeric(rX))) %>%
	map(~mutate(., rY = as.numeric(rY))) %>%
	map(~mutate(., lUserCoordZ = str_remove_all(lUserCoordZ, "\\.") %>% substr(start = 1, 6) %>% as.numeric(.))) %>%
	map(~mutate(., rUserCoordZ = str_remove_all(lUserCoordZ, "\\.") %>% substr(start = 1, 6) %>% as.numeric(.))) %>%
	bind_rows() %>%
	as_tibble() %>%
	rename(
		ParticipantID = Participant,
		TrialID = Trial
		) %>%
	mutate(
		TrialID       = as.character(TrialID),
		meanX         = rowMeans(cbind(lX, rX), na.rm = TRUE)*screenX,  # change relative coords to screen coords
		meanY         = rowMeans(cbind(lY, rY), na.rm = TRUE)*screenY,  # change relative coords to screen coords
		meanDistance  = rowMeans(cbind(lUserCoordZ, rUserCoordZ), na.rm = TRUE)/1000, # change distance from screen to centimeters
		Trackloss     = !(rV | lV) # is the sample not valid in both eyes?
	) %>%
	drop_na(ParticipantID) %>%
	group_by(ParticipantID, TrialID, Phase) %>%
	mutate(TimeStamp = seq(from = 0, to = (8*n())-8, by = 8)) %>%
	ungroup() %>%
	filter(ParticipantID %in% participants$ID,
		   Phase %in% c("Prime", "Target-Distractor")) %>%
	select(ParticipantID, TrialID, Phase, TimeStamp, meanX, meanY, meanDistance, Trackloss)

#### register logs ##############################################
logs <- data %>%
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
	)

#### export data ###############################################
fwrite(data, file = here("Data", "00_raw.txt"), sep = "\t", row.names = FALSE)
fwrite(logs, file = here("Data", "00_raw-logs.txt"), sep = "\t", row.names = FALSE)

