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
fixations <- fread(here("Data", "03_fixations.csv"), sep = ",", dec = ".", stringsAsFactors = FALSE) %>%
	as_tibble()

# import participant data
participants <- read_xlsx(here("Data", "Participant data", "data_participants.xlsx")) %>%
	clean_names() %>%
	rename(participant_id = id) %>%
	select(participant_id, lang_profile, location, language, list)

# import trial data
trials <- fread(here("Data", "Stimuli", "02_stimuli-stats.txt")) %>%
	clean_names() %>%
	as_tibble() %>%
	unite("trial_id_label", c("prime", "distractor"), sep = "_")  %>%
	select(trial_id, target_id = target, location, language, list, freq_prime, freq_target, levenshtein_prime)


#### merge data ##########################################
dat <- fixations %>%
	left_join(., participants) %>%
	#left_join(., trials) %>%
	drop_na(timebin)

#### aggregate across trials ###############################################
dat_binned <- dat %>%
	group_by(participant_id, trial_id, trial_type, timebin, language, lang_profile) %>%
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
	filter(timebin > 2)

dat_binned %>%
	ggplot(aes(timebin, prop, colour = trial_type)) +
	facet_wrap(~lang_profile) +
	geom_smooth() +
	ggsave(here("Figures", "test.png"))

#### code predictors ####################################
dat_coded <- dat_binned %>%
	mutate(trial_type = factor(case_when(trial_type == "Unrelated"   ~ 0,
										 trial_type == "Non-cognate" ~ 1,
										 TRUE                       ~ 2)),
		   language = ifelse(language=="Spanish", -0.50, 0.50),
		   lang_profile = ifelse(lang_profile=="Monolingual", -0.50, 0.50))

#### create orthogonal polynomials ######################
t <- poly(sort(as.vector(unique(dat$timebin))), 3)
polynomials <- data.frame(timebin = unique(dat$timebin),
						  timebin1 = t[,1],
						  timebin2 = t[,2],
						  timebin3 = t[,3])
dat_poly <- left_join(dat_coded, polynomials, by = "timebin")

#### reorder variables ###################################
dat_prepared <- dat_poly %>%
	select(participant_id, trial_id, starts_with("time"), target, distractor, total, weights, trial_type, lang_profile, language)


#### export data #########################################
fwrite(dat_prepared, here("Data", "04_prepared.csv"), sep = ",", dec = ".", row.names = FALSE)

