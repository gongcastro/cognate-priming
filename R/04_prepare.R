# 04_prepares: Prepare data for Growth Curve Analysis
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up #############################################2

# load packages
library(data.table) # for importing and exporting data
library(dplyr)      # for manipulating data
library(tidyr)      # for rehsaping datasets
library(readxl)     # for importing Excel files
library(scales)     # for centering variables
library(tibble)     # for more informative data frames
library(janitor)    # for cleaning variable names
library(here)       # for locating files

# set parameters
time_bin_duration <- 100  # how long should time bins be in ms?

#### import data #########################################
# import fixation data
dat_raw <- fread(here("Data", "03_filtered.csv"), sep = ",", dec = ".", stringsAsFactors = FALSE, na.strings = "") %>%
	filter(phase=="Target-Distractor") %>%
	group_by(participant_id, trial_id) %>% 
	mutate(time = time-min(time, na.rm = TRUE),
		   timebin = as.numeric(cut(time, breaks = seq(0, 2000, by = time_bin_duration), labels = seq(1, 20, by = 1), include.lowest = TRUE, right = FALSE)),
		   vocab_comp = ifelse(test_language=="Spanish", vocab_comp_spa, vocab_comp_cat))

#### polynomials of time domain #########################################
t <- poly(sort(as.vector(unique(dat_raw$timebin))), 3)
polynomials <- data.frame(timebin = unique(dat_raw$timebin),
						  timebin1 = t[,1],
						  timebin2 = t[,2],
						  timebin3 = t[,3])

#### prepare data for GCA ###############################################
dat <- dat_raw %>%
	group_by(participant_id, trial_id, trial_type, timebin, lp, vocab_comp) %>%
	summarise(total_samples = n(),
			  samples_target = sum(fix_target, na.rm = TRUE),
			  samples_distractor = sum(fix_distractor, na.rm = TRUE),
			  .groups = "drop") %>%
	ungroup() %>%
	mutate(time = timebin*time_bin_duration) %>%
	rowwise() %>%
	mutate(total = total_samples,
		   target = samples_target,
		   distractor = samples_distractor,
		   prop = target/total,
		   weights = 1/(target+0.5) + 1/(total-target+0.5)) %>%
	filter(timebin > 2) %>% # filter data
	left_join(polynomials) %>%
	relocate(participant_id, trial_id, timebin, time, target, distractor, total, prop, weights, timebin1, timebin2, timebin3, trial_type)


#### export data #########################################
fwrite(dat, here("Data", "04_prepared.csv"), sep = ",", dec = ".", row.names = FALSE)

