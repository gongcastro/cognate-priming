# 02_fixations: Analyse gaze in Cognate Priming task ####
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up #############################################

# load packages
library(tidyverse)
library(magrittr)     # for using pipes
library(readxl)       # for importing Excel files
library(purrr)        # for working with lists
library(data.table)   # for importing data
library(janitor)      # for cleaning variable names
library(gazepath)     # for identiying fixations
library(here)         # for locating files

# set parameters
time_bin_duration <- 100  # how long should time bins be in ms?
sampling_rate     <- 120  # how many samples are takien per second?
screen_x          <- 1920 # screen width in pixels
screen_y          <- 1080 # screen height in pixels
screen_dpi        <- 23   # screen size in inches

# load functions
source(here("R", "functions.R")) 

#### import data ###########################################################

# import filtered data
dat_gaze <- fread(here("Data", "00_processed.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "") %>%
	mutate_at(vars(trial_id), as.character) %>%
	as.data.frame() %>%
	filter(phase %in% c("Prime", "Target-Distractor")) 

#### extract fixations ####################################################
fixations <- dat_gaze %>% 
	split(., f = .$participant_id) %>% 
	map(gazepath,
		x1 = "l_x",
		y1 = "l_y",
		d1 = "l_dist",
		x2 = "r_x",
		y2 = "r_y",
		d2 = "r_dist",
		trial = "trial_id",   
		height_mm = 285.75, # AOI height in mm
		width_mm = 508, # AOI width in mm
		height_px = screen_y, # AOI height in px 
		width_px = screen_x, # AOI width in px
		samplerate = sampling_rate, # eye-tracker sampling rate
		method = "gazepath", # select algorithm
		res_x = screen_x,
		res_y = screen_y,
		thres_dur = 100,
		posthoc = TRUE, # merge consecutive fixations?
		extra_var = c("participant_id", "trial_id")) %>% 
	map("fixations") %>% # extract fixations
	map(~map(., mutate_if, is.logical, as.factor)) %>%
	map(bind_rows) %>% 
	bind_rows() %>%       # merge fixations within participant
	clean_names() %>%
	filter(value=="f") %>%      # exclude saccades
	select(participant_id, trial_id, start, end) %>% # select variables of interest
	as_tibble() %>%               # for more informative datasets
	mutate_if(.predicate = is.factor, as.character) %>%
	group_by(participant_id, trial_id) %>% # whatever we do next, do it spearately for each combination of these variables
	mutate(fixnum = row_number) %>% # index all fixations within each trial 
	ungroup() %>% 
	mutate(phase = ifelse(start < 1500, "Prime", "Target-Distractor"),
		   start = ifelse(phase=="Prime", start+3000, start+4000),
		   end = ifelse(phase=="Prime", end+3000, end+4000))

# reconstruct fixations across time domain
dat <- fixations %>%
	left_join(., dat_gaze) %>%
	rename(time = timestamp) %>%
	mutate(fixation = (time >= start) & (time <= end),  # is time point within fixation boundaries?
		   x = ifelse(is.na(l_x), r_x, l_x),
		   y = ifelse(is.na(l_y), r_y, l_y)) %>%    
	group_by(participant_id, trial_id, phase, time, target_location, l_x, l_y, r_x, r_y, trackloss) %>%      # prepare to agreggate by time point
	summarise(fixation = any(fixation), # is this time point within any of the fixation boundaries?
			  x = first(x), # preserve gaze location in X-axis                       
			  y = first(y), # preserve gaze location in Y-axis
			  .groups = "drop") %>%              
	ungroup() %>%
	mutate_if(.predicate = is.factor, as.character) %>%
	mutate(fix_prime = eval_prime(data = ., x_gaze = x, y_gaze = y),
		   fix_target = eval_target(data = ., x_gaze = x, y_gaze = y, target_location = target_location),
		   fix_distractor = eval_distractor(data = ., x_gaze = x, y_gaze = y, target_location = target_location)) %>% 
	select(participant_id, trial_id, phase, time, fix_prime, x, y, fix_target, fix_distractor, trackloss)  
	

#### export data ##############################################
fwrite(fixations, here("Data", "01_fixation-info.csv"), sep = ",", dec = ".", row.names = FALSE)
fwrite(dat, here("Data", "01_fixations.csv"), sep = ",", dec = ".", row.names = FALSE)

#### visualise data ###########################################
d <- dat %>%
	mutate(., fixation = ifelse(is.na(fixation), FALSE, fixation)) %>%
	group_split(participant_id) 

# heatmap
for (i in 1:length(d)){
	d[[i]] %>%
		filter(time >= 300) %>%
		ggplot(aes(x, y, colour = time)) +
		facet_wrap(~trial_id) +
		annotate("rect", xmin = l_coords[1], xmax = l_coords[2], ymin = l_coords[3], ymax = l_coords[4],
				 colour = NA, fill = "white", alpha = 0.1) +
		annotate("rect", xmin = r_coords[1], xmax = r_coords[2], ymin = r_coords[3], ymax = r_coords[4],
				 colour = NA, fill = "white", alpha = 0.1) +
		geom_text(aes(x = 200, y = 1000, label = paste0("Target: ", target_location)), colour = "white", size = 2) +
		geom_point(alpha = 0.25, size = 0.1) +
		labs(colour = "Time (ms)",
			 title = paste0("Participant: ", unique(d[[i]]$participant_id)),
			 subtitle = paste0(unique(d[[i]]$lp), ", ", unique(d[[i]]$test_language), ", list ", unique(d[[1]]$list))) +
		theme(legend.position = "top",
			  axis.text = element_blank(),
			  axis.title = element_blank(),
			  axis.ticks = element_blank(),
			  strip.text = element_text(size = 7),
			  panel.background = element_rect(fill = "#808080"),
			  panel.grid = element_blank(),
			  panel.border = element_rect(colour = "grey", fill = "transparent")) +
		coord_fixed(xlim = c(0, screen_x), ylim = c(0, screen_y)) +
		ggsave(here("Figures", "Fixations", paste0(unique(d[[i]]$participant_id), "_gaze.png")), height = 7)
}

# fixations
for (i in 1:length(d)){
	d[[i]] %>%
		filter(time >= 300) %>%
		pivot_longer(c(x, y), names_to = "measure", values_to = "value") %>%
		ggplot(aes(time, value, colour = fixation, shape = measure)) +
		facet_wrap(~trial_id) +
		geom_point(size = 0.25, alpha = 0.5) +
		labs(x = "Time (ms)",
			 y = "Gaze position",
			 alpha = "Fixation",
			 colour = "Coordinate",
			 title = paste0("Participant: ", unique(d[[i]]$participant_id)),
			 subtitle = paste0(unique(d[[i]]$lp), ", ", unique(d[[i]]$test_language), ", list ", unique(d[[1]]$list))) +
		theme(legend.position = "top",
			  strip.text = element_text(size = 7),
			  panel.background = element_rect(fill = "transparent"),
			  panel.grid = element_line(colour = "grey80", linetype = "solid", size = 0.1),
			  panel.border = element_rect(colour = "grey", fill = "transparent")) +
		ggsave(here("Figures", "Fixations", paste0(unique(d[[i]]$participant_id), "_fixations.png")), width = 8, height= 5)
}
