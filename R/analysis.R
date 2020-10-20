# processing: Prepare data for analysis ################

#### set up ############################################

# load packages
library(tidyverse)      
library(data.table)   # for importing data
library(readxl)       # for importing Excel files
library(janitor)      # for cleaning variable names
library(eyetrackingR) # for processing looking times
library(here)         # for locating files
library(lme4)
library(broom.mixed)

# load functions
source(here("R", "functions.R"))

# set experimental parameters
sampling_rate      <- 120  # how many samples does the eye-tracker take per second?
screen_x           <- 1920 # width of the screen in pixels
screen_y           <- 1080 # height of the screen in pixels
relevant_variables <- c("participant", "trial_num", "trial", "phase", "system_time_stamp",
						"l_x", "l_y", "l_v", "r_x", "r_y", "r_v",
						"l_user_coord_z", "l_origin_user_coord_z",
						"r_user_coord_z", "r_origin_user_coord_z")
participant <- 43

#### import data ####################################################################
participants <- read_xlsx(here("Data", "participants.xlsx")) %>%
	filter(!pilot) %>%
	drop_na(participant_id) %>% 
	select(participant_id, id_db, location, age_group, lp, test_language, list, version, filename)

trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	mutate(trial_id = as.character(trial_id)) %>% 
	select(location, test_language, list, version, trial_id, trial_type, target_location) 

files <- paste0(here("Data", "Raw", "Barcelona"),
				.Platform$file.sep,
				participants$filename[!is.na(participants$filename)])

raw <- map(files, function(x) fread(x, sep = ",", dec = '.', header = TRUE, stringsAsFactors = FALSE,
											na.strings = c("NaN", "<NA>", "NA", "Na", "-", ""))) %>%
	set_names(files) %>% 
	map(~clean_names(.)) %>% 
	map(~select(., any_of(relevant_variables))) %>% 
	map(~rename(.,	participant_id = participant,
				timestamp = system_time_stamp,
				trial_id = trial,
				l_dist = l_user_coord_z,
				r_dist = r_user_coord_z)) %>% 
	map(~mutate_at(., vars(l_x, l_y, l_dist, r_x, r_y, r_dist),
				   function(x) as.numeric(ifelse(str_count(x, "\\.")>1, NA_character_, x)))) %>% 
	map(~mutate(.,
				timestamp = as.numeric(timestamp),
				participant_id = paste0("cognatepriming", participant_id),
				trial_id = as.character(trial_id),
				l_x = l_x*screen_x,
				r_x = r_x*screen_x,
				l_y = l_y*screen_y,
				r_y = r_y*screen_y,
				l_dist = ifelse(l_dist < 0, NA_real_, l_dist),
				r_dist = ifelse(r_dist < 0, NA_real_, r_dist),
				trackloss = !(r_v | l_v),
				location = "Barcelona"))

#### process raw data ###############################################################
gaze <- raw %>% 
	bind_rows(.id = "file") %>% 
	as_tibble() %>% 
	mutate_at(vars(l_x, r_x), function(x) ifelse(!between(x, 0, screen_x), NA_real_, x)) %>% 
	mutate_at(vars(l_y, r_y), function(x) ifelse(!between(x, 0, screen_y), NA_real_, x)) %>% 
	group_by(participant_id, trial_id) %>% 
	mutate(time = row_number()*(1000/120),
		   phase = case_when(between(time, 0, 3000) ~ "Getter",
		   				  between(time, 3000, 4500) ~ "Prime",
		   				  between(time, 4500, 4700) ~ "Blank",
		   				  between(time, 4700, 5500) ~ "Audio",
		   				  between(time, 5500, 7500) ~ "Target-Distractor"),
		   x = ifelse(is.na(l_x), r_x, l_x),
		   y = ifelse(is.na(l_y), r_y, l_y)) %>% 
	ungroup() %>% 
	select(participant_id, trial_id, phase, time, x, y, trackloss, location) %>% 
	left_join(participants, c("participant_id", "location")) %>% 
	left_join(trials, by = c("trial_id", "location", "test_language", "list", "version")) %>% 
	mutate(target = eval_target(data = ., x_gaze = x, y_gaze = y, target_location = target_location),
		   distractor = eval_distractor(data = ., x_gaze = x, y_gaze = y, target_location = target_location)) %>%
	ungroup() %>% 
	select(participant_id, trial_id, phase, time, target, distractor, x, y, trackloss, lp, trial_type) %>% 
	arrange(desc(participant_id), trial_id, time, phase) 


#### looking times #############################################################
looking <- make_eyetrackingr_data(gaze,
								  participant_column = "participant_id",
								  trial_column = "trial_id",
								  time_column = "time",
								  trackloss_column = "trackloss",
								  aoi_columns = c("target", "distractor"),
								  treat_non_aoi_looks_as_missing = TRUE) %>% 
	subset_by_window(window_start_time = 5700, window_end_time = 7500, rezero = TRUE, remove = TRUE) %>% 
	clean_by_trackloss(participant_prop_thresh = 0.50, trial_prop_thresh = 0.75) %>% 
	make_time_sequence_data(time_bin_size = 100, predictor_columns = c("lp", "trial_type"), aois = "target") %>% 
	as_tibble() %>% 
	clean_names() %>% 
	mutate_at(vars(lp, trial_type), as.factor)

contrasts(looking$lp) <- c(0.5, -0.5)
contrasts(looking$trial_type) <- cbind(c(0.5, 0.5, -1), c(0.5, -0.5, 0))

#### fit models ###################################
fit0 <- lmer(elog ~ (ot1+ot2+ot3) + (ot1+ot2+ot3 | participant_id),
			 data = looking, na.action = na.exclude, REML = FALSE,
			 control = lmerControl(optimizer = "bobyqa"))
fit1 <- lmer(elog ~ (ot1+ot2+ot3)*lp + (ot1+ot2+ot3 | participant_id:lp),
			 data = looking, na.action = na.exclude, REML = FALSE,
			 control = lmerControl(optimizer = "bobyqa"))
fit2 <- lmer(elog ~ (ot1+ot2+ot3)*(lp + trial_type) +((ot1+ot2+ot3) +trial_type | participant_id:lp),
			 data = looking, na.action = na.exclude, REML = FALSE,
			 control = lmerControl(optimizer = "bobyqa"))
fit3 <- lmer(elog ~ (ot1+ot2+ot3)*lp*trial_type + ((ot1+ot2+ot3) + trial_type | participant_id:lp),
			 data = looking, na.action = na.exclude, REML = FALSE,
			 control = lmerControl(optimizer = "bobyqa"))

comp <- anova(fit0, fit1, fit2, fit3)
coefs <- tidy(fit3, conf.int = TRUE) %>%  clean_names()
#### export
fwrite(gaze, here("Data", "raw.csv"), sep = ",", na = "NA")
fwrite(looking, here("Data", "looking-times.csv"), sep = ",", na = "NA")

#### check gaze ################################################

# model predictions
ggplot(looking, aes(ot1, elog, shape = trial_type,
					colour = trial_type, fill = trial_type)) +
	facet_wrap(lp~participant_id) +
	geom_hline(yintercept = 0) +
	stat_summary(fun = "mean", geom = "point", alpha = 0.5, size = 1) +
	labs(x = "Time (100 ms bins)", y = "Empirical logit of looking time",
		 shape = "Trial type", colour = "Trial type", fill = "Trial type") +
	stat_summary(aes(y = fitted(fit3)), fun.data = "mean_se", geom = "ribbon", colour = NA, alpha = 0.5) +
	stat_summary(aes(y = fitted(fit3)), fun = "mean", geom = "line") +
	theme_bw() +
	theme(legend.position = "top") +
	ggsave(here("Figures", "looking.png"))



# coefficients
coefs %>% 
	filter(effect %in% "fixed") %>% 
	arrange(term) %>% 
	ggplot(aes(estimate, fct_inorder(term), fill = term, colour = term)) +
	geom_vline(xintercept = 0) +
	geom_errorbarh(aes(xmin = estimate-std_error, xmax = estimate+std_error), height = 0) +
	geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), size = 6, alpha = 0.5, height = 0) +
	geom_point(size = 3) +
	labs(x = "Estimate", y = "Term") +
	theme_bw() +
	theme(legend.position = "none",
		  axis.title.y = element_blank()) +
	ggsave(here("Figures", "coefs.png"))




