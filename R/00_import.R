# 00_import: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(osfr)         # for connecting to OSF
library(magrittr)     # for using pipes
library(tibble)       # for tidy data presentation
library(dplyr)        # for manipulating data
library(tidyr)        # for rehsaping datasets
library(data.table)   # for importing data
library(readxl)       # for importing Excel files
library(lubridate)    # for working with dates
library(stringr)      # for working with character strings
library(purrr)        # for working with lists
library(ggplot2)      # for using cut_interval
library(googledrive)  # for downloading participant-level information
library(here)         # for locating files

# load functions
source(here("R", "Functions", "osf_download_folder.R"))
"%!in%" <- function(x, y) !(x %in% y)

# retrieve data from OSF (takes around a minute, you can download it manually)
osf_auth("")
osf_download_folder(project = "wekda", component = "Data", folder = "Gaze data", local.path = here("Data", "Gaze data"))

# set experimental parameters
sampling_rate <- 120  # how many samples does the eye-tracker take per second?
screenX       <- 1920 # width of the screen in pixels 
screenY       <- 1080 # height of the screen in pixels

#### import data #######################################

# import participant-level data
participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	filter(!Pilot) %>% # take info from participants (exclude pilot)
	drop_na(Version)

# import eye-tracking data
data <- list.files(here("Data", "Gaze data", "Barcelona"), full.names = TRUE, recursive = TRUE, ) %>%          # list files in the folder
	map(~fread(., sep = ',', dec = '.', header = TRUE, stringsAsFactors = FALSE)) %>%                          # import files in the folder
	set_names(str_remove(list.files(here("Data", "Gaze data", "Barcelona")), ".csv")) %>%                      # name each list element with its file name
	map(~select(., one_of("SystemTimeStamp", "lX", "lY", "lV", "rV", "rX", "rY", "lUserCoordZ", "rUserCoordZ", # select only relevant variables
						  "Participant", "TrialNum", "Trial", "Phase"))) %>%
	bind_rows() %>% # merge all datasets
	as_tibble() %>% # for more informative data-sets
	mutate(
		ParticipantID = Participant,
		TrialID       = as.character(Trial),
		meanX         = rowMeans(cbind(lX, rX), na.rm = TRUE)*screenX,  # change relative coords to screen coords
		meanY         = rowMeans(cbind(lY, rY), na.rm = TRUE)*screenY,  # change relative coords to screen coords
		meanDistance  = rowMeans(cbind(lUserCoordZ, rUserCoordZ), na.rm = TRUE)*10, # change distance from screen to centimeters
		Trackloss     = !(rV | lV ) # is the sample not valid in both eyes?
	) %>%
	drop_na(ParticipantID) %>%
	group_by(ParticipantID, TrialID, Phase) %>%
	mutate(TimeStamp = seq(from = 1000/sampling_rate, to = (1000/sampling_rate)*length(SystemTimeStamp), by = 1000/sampling_rate)) %>%
	ungroup() %>%
	mutate(ParticipantID = paste0("cognatepriming", as.character(Participant))) %>%
	filter(ParticipantID %in% participants$ID,
		   Phase %in% c("Prime", "Target-Distractor")) %>%
	select(ParticipantID, TrialID, Phase, TimeStamp, meanX, meanY, meanDistance, Trackloss)

#### export data ###############################################
fwrite(data, file = here("Data", "00_raw.txt"), sep = "\t", row.names = FALSE)

