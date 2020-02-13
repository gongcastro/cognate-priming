# 03_fixations: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(magrittr)     # for using pipes
library(readxl)       # for importing Excel files
library(tibble)       # for tidy data presentation
library(dplyr)        # for manipulating data
library(purrr)        # for working with lists
library(eyetrackingR) # for processing eye-tracking data
library(data.table)   # for importing data
library(gazepath)     # for identiying fixations
library(ggplot2)      # for data visualisation
library(gganimate)    # for animating plots
library(here)         # for locating files

# set parameters
time_bin_duration <- 100  # how long should time bins be in ms?
sampling_rate     <- 120  # how many samples are takien per second?
screenX           <- 1920 # screen width in pixels
screenY           <- 1080 # screen heigh in pixels
resolution        <- 23   # screen size in inches

# load functions
source(here("R", "Functions", "target_coords.R"))     # for evaluating whether gaze is in target
source(here("R", "Functions", "distractor_coords.R")) # for evaluating whether gaze is in distractor

#### import data ###########################################################
data <- fread(here("Data", "02_filtered.txt"), sep = "\t", header = TRUE) %>%
	select(-c(Phase, GazePrime)) %>%
	make_eyetrackingr_data(data               = .,
						   participant_column = "ParticipantID",
						   trackloss_column   = "Trackloss",
						   time_column        = "TimeStamp",
						   trial_column       = "TrialID",
						   aoi_columns        = c("GazeTarget", "GazeDistractor"),
						   treat_non_aoi_looks_as_missing = FALSE) %>%
	# keep only time window of interest and re-center the time
	subset_by_window(rezero = TRUE,
					 remove = TRUE,
					 window_start_time = 0,
					 window_end_time = 2000) %>%
	mutate(TrialID = as.character(TrialID)) %>%
	as.data.frame()

#### identify fixations ####################################################
fixation.info <- split(data, f = data$ParticipantID) %>% # separate each participant's dataset
	map(., ~gazepath( # get fixations in each dataset
		data       = .,
		x1         = "meanX",         # gaze horizontal position
		y1         = "meanY",         # gaze vertical position 
		d1         = "meanDistance",  # variable containing distance from screen
		trial      = "TrialID",       # trial index in dataset
		height_mm  = 132.2917,        # AOI height in mm
		width_mm   = 132.2917,        # AOI width in mm
		height_px  = 500,             # AOI height in px 
		width_px   = 500,             # AOI width in px
		samplerate = 120,             # eye-tracker sampling rate
		method     = "gazepath",      # select algorithm
		posthoc    = TRUE,            # merge consecutive fixations?
		thres_dur  = 100,             # minimum fixation duration
		in_thres   = 200,
		extra_var  = c("ParticipantID", "TrialID") # keep these variables
	)) %>%
	map("fixations") %>%          # extract fixation info
	map(~bind_rows(.)) %>%        # merge fixations within participant
	bind_rows(.) %>%              # merge fixations across participants
	filter(Value == "f") %>%      # exclude saccades
	select(ParticipantID, TrialID, Start, End) %>% # select variables of interest
	as_tibble() %>%               # for more informative datasets
	mutate_if(.predicate = is.factor, as.character) %>%
	group_by(ParticipantID, TrialID) %>% # whatever we do next, do it spearately for each combination of these variables
	mutate(FixNum = row_number()) %>% # index all fixations within each trial
	ungroup()

# reconstruct fixations across time domain
fixations <- fixation.info %>%
	right_join(., data, by = c("ParticipantID", "TrialID")) %>% # merge fixation info with time data
	rename(Time = TimeStamp) %>%
	mutate(Fixation = (Time >= Start) & (Time <= End)) %>%      # is time point within fixation boundaries?
	group_by(ParticipantID, TrialID, Time, TrialType, List, TargetLocation, Language, LangProfile, Pilot) %>%      # prepare to agreggate by time point
	summarise(Fixation = any(Fixation),                        # is this time point within any of the fixation boundaries?
			  meanX    = first(meanX),                         # preserve gaze location in X-axis
			  meanY    = first(meanY)) %>%                     # preserve gaze location in Y-axis
	ungroup() %>%
	mutate_if(.predicate = is.factor, as.character) %>%
	mutate(
		# evaluate if gaze is in target of distractor using a custom script (in pilot, target location was generated online)
		FixTarget     = target_coords(data = ., x_gaze = meanX, y_gaze = meanY, target_location = TargetLocation),
		FixDistractor = distractor_coords(data = ., x_gaze = meanX, y_gaze = meanY, target_location = TargetLocation),
		# generate time bins
		TimeBin       = as.numeric(cut(Time, breaks = seq(0, 2000, by = time_bin_duration), labels = seq(1, 2000/time_bin_duration)))
  ) %>%
  # aggregate across trials
  group_by(ParticipantID, TrialID, TrialType, TimeBin, Language, LangProfile, Pilot) %>%
  summarise(
    TotalSamples      = n(),               # number of samples in this time bin for this participant
    SamplesTarget     = sum(FixTarget),    # number of samples in ta
    SamplesDistractor = sum(FixDistractor)
  ) %>%
  mutate(
    PFixTarget = SamplesTarget/TotalSamples,
    PFixDistractor = SamplesDistractor/TotalSamples
  ) %>%
  pivot_longer( # make long format
    cols      = c("PFixTarget", "PFixDistractor"),
    names_to  = "AOI",
    values_to = "ProbFix"
  ) %>%
  mutate(AOI  = ifelse(AOI == "PFixTarget", "Target", "Distractor"),
         Time = TimeBin*time_bin_duration)

#### register logs ##############################################
logs <- fixations %>%
	group_by(ParticipantID, TrialID, TrialType) %>%
	summarise(
		TimeStart         = min(Time, na.rm = TRUE),
		TimeStop          = max(Time, na.rm = TRUE),
		TimeBinStart      = min(TimeBin, na.rm = TRUE),
		TimeBinStop       = max(TimeBin, na.rm = TRUE),
		SamplesTarget     = mean(SamplesTarget, na.rm = TRUE),
		SamplesDistractor = mean(SamplesDistractor, na.rm = TRUE),
		ProbFix           = mean(ProbFix, na.rm = TRUE)
	)
  
#### visualise data ##########################################
# fixations-screen animation
fixation.info %>%
	right_join(., data, by = c("ParticipantID", "TrialID")) %>% # merge fixation info with time data
	rename(Time = TimeStamp) %>%
	mutate(Fixation = (Time >= Start) & (Time <= End)) %>%
	group_by(ParticipantID, TrialID, Time, TargetLocation, Start, End) %>%
	summarise(Fixation = any(Fixation),
			  FixNum   = first(FixNum),
			  meanX    = first(meanX),
			  meanY    = first(meanY)) %>%
	filter(Fixation) %>%
	ggplot(aes(x = meanX, y = meanY)) +
	geom_rect(xmin = 280, xmax = 780, ymin = 290, ymax = 790,
			  fill = "transparent", colour = "white") +
	geom_rect(xmin = 1140, xmax = 1640, ymin = 290, ymax = 790,
			  fill = "transparent", colour = "white") +
	stat_bin_2d(binwidth = 100, na.rm = TRUE) +
	labs(x = "X coordinates (pixels", y = "Y coordinates", colour = "Fixation",
		 subtitle = "Time (ms): {frame_time}") +
	scale_fill_viridis_c(option = "magma") +
	labs(x = "X coordinates (pixels)", y = "Y coordinates (pixels)",
		 fill = "Fixation samples") +
	scale_x_continuous(limits = c(0, screenX)) +
	scale_y_continuous(limits = c(0, screenY)) +
	coord_fixed() +
	theme(
		text             = element_text(size = 12),
		axis.ticks       = element_blank(),
		axis.text        = element_blank(),
		axis.title       = element_blank(),
		panel.background = element_rect(fill = "#7F7F7F"),
		panel.grid       =  element_blank(),
		panel.border     = element_rect(colour = "black", fill = "transparent"),
		legend.position  = "bottom"
	) +
	transition_time(time = as.integer(Time))

#### export data ##############################################
fwrite(fixation.info, here("Data", "03_fixation-info.txt"), sep = "\t", dec = ".", row.names = FALSE)
fwrite(fixations, here("Data", "03_fixations.txt"), sep = "\t", dec = ".", row.names = FALSE)
fwrite(fixations, here("Data", "03_fixations-logs.txt"), sep = "\t", dec = ".", row.names = FALSE)



