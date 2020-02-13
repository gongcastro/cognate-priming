# 00_import-pilot: Analyse gaze in Cognate Priming task (data from pilot)
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(osfr)         # for connecting to OSF
library(dplyr)        # for manipulating data
library(tidyr)        # for rehsaping datasets
library(data.table)   # for importing data
library(readxl)       # for importing Excel files
library(stringr)      # for working with character strings
library(purrr)        # for working with lists
library(here)         # for locating files
 
# load functions
source(here("R", "Functions", "osf_download_folder.R"))
"%!in%" <- function(x, y) !(x %in% y)

# retrieve data from OSF (takes around a minute, you can download it manually)
osf_auth("")
osf_download_folder(PAT = "", project = "wekda", component = "Data", folder = "Gaze data", local.path = here("Data", "Gaze data"))

# set experimental parameters
sampling_rate <- 120  # how many samples does the eye-tracker take per second?
screenX       <- 1920 # width of the screen in pixels 
screenY       <- 1080 # height of the screen in pixels

#### import data #######################################

# import participant-level data
participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	filter(Pilot, Valid) # take info from participants (exclude pilot)

# import eye-tracking data
data <- list.files(here("Data", "Gaze data", "Barcelona"), full.names = TRUE, recursive = TRUE) %>%            # list files in the folder
	.[5:(nrow(participants)-1)] %>%                                                                                # get data from Pilot participants only
	map(~fread(., sep = ',', dec = '.', header = TRUE, stringsAsFactors = FALSE)) %>%                          # import files in the folder
	set_names(str_remove(list.files(here("Data", "Gaze data", "Barcelona"))[5:(nrow(participants)-1)], ".csv")) %>%                      # name each list element with its file name
	map(~select(., one_of("SystemTimeStamp", "lX", "lY", "lV", "rX", "rY", "rV", # select only relevant variables
						  "participant", "trial", "trialID", "phase"))) %>%
	bind_rows() %>% # merge all datasets
	as_tibble() %>% # for more informative data-sets
	rename(ParticipantID = participant, TrialID = trialID, Trial = trial, Phase = phase) %>%
	mutate(
		TrialID       = as.character(Trial),
		meanX         = rowMeans(cbind(lX, rX), na.rm = TRUE)*screenX,  # change relative coords to screen coords
		meanY         = rowMeans(cbind(lY, rY), na.rm = TRUE)*screenY,  # change relative coords to screen coords
		meanDistance  = 650, # change distance from screen to centimeters
		Trackloss     = !(rV | lV ) # is the sample not valid in both eyes?
	) %>%
	group_by(ParticipantID, TrialID, Phase) %>%
	mutate(TimeStamp = seq(from = 0, to = (8*n())-8, by = 8)) %>%
	ungroup() %>%
	mutate(ParticipantID = paste0("cognatepriming", as.character(ParticipantID)),
		   Phase = case_when(
		   	Phase=="prime" ~ "Prime",
		   	Phase=="target_distractor" ~ "Target-Distractor"
		   )
	) %>%
	filter(ParticipantID %in% participants$ID,
		   Phase %in% c("Prime", "Target-Distractor")) %>%
	select(ParticipantID, TrialID, Phase, TimeStamp, meanX, meanY, meanDistance, Trackloss)

#### export data ###############################################
fwrite(data, file = here("Data", "00_raw-pilot.txt"), sep = "\t", row.names = FALSE)
