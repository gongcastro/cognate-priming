#### processing: Prepare data for analysis -------------------------------------

# set up -----------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)
library(readxl)
library(janitor)
library(here)

# load functions
source(here("R", "utils.R"))

# set params
sampling_rate <- 120  # how many samples does the eye-tracker take per second?
screen_x <- 1920 # width of the screen in pixels
screen_y <- 1080 # height of the screen in pixels
relevant_variables <- c(
	"participant", "trial_num", "trial", "phase", "process", "system_time_stamp",
	"l_1", "l_2", "l_v", "r_1", "r_2", "r_v",
	"l_user_coord_3", "l_origin_user_coord_3",
	"r_user_coord_3", "r_origin_user_coord_3"
)

#### import data ---------------------------------------------------------------
participants <- read_xlsx(here("Data", "participants.xlsx")) %>%
	filter(!pilot) %>%
	drop_na(participant_id) %>% 
	select(participant_id, id_db, location, age_group, lp, test_language, list, version)

trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	mutate(trial_id = as.character(trial_id)) %>% 
	select(location, test_language, list, version, trial_id, trial_type, target_location, prime, target, distractor) 

#### process gaze data ---------------------------------------------------------
raw <- list.files("Data", ".csv", full.names = TRUE) %>% 
	map(fread, na.string = "NaN") %>% 
	map(~mutate_at(., vars(matches("SystemTimeStamp")), bit64::as.integer64)) %>% 
	bind_rows() %>% 
	clean_names() %>% 
	select(any_of(relevant_variables)) %>% 
	as_tibble() %>% 
	filter(phase %in% "Target-Distractor") %>% 
	rowwise() %>% 
	mutate(
		x = ifelse(is.na(l_1), r_1, l_1),
		y = ifelse(is.na(l_2), r_2, l_2),
		d = ifelse(is.na(l_origin_user_coord_3 ), r_origin_user_coord_3 , l_origin_user_coord_3)
		#timestamp = as.double(system_time_stamp)
	) %>%
	rename(timestamp = system_time_stamp) %>% 
	group_by(participant, trial) %>% 
	mutate(
		timestamp = timestamp-min(.$timestamp, na.rm = TRUE),
		time_bin = cut_interval(as.double(timestamp), length = 100, labels = FALSE)
	) %>% 
	ungroup() %>% 
	mutate(
		participant = paste0("cognatepriming", participant),
		trial = as.character(trial),
		x = x*screen_x,
		y = y*screen_y
	) %>% 
	left_join(participants, by = c("participant" = "participant_id")) %>% 
	left_join(trials, by = c("location", "test_language", "list", "version", "trial" = "trial_id")) %>% 
	select(participant, age_group, trial, time_bin, timestamp, x, y, target_location, d, lp, trial_type) %>% 
	arrange(participant, trial) %>% 
	mutate(target = eval_target(., x, y, target_location),
		   distractor = eval_distractor(., x, y, target_location)) 

fwrite(raw, here("Results", "raw.csv"), sep = ",", dec = ".", na = "NA")

#### summarise by time_bin -----------------------------------------------------
by_time_bin <- raw %>% 
	group_by(participant, age_group, trial, time_bin, lp, trial_type) %>% 
	summarise(target = sum(target, na.rm = TRUE),
			  distractor = sum(distractor, na.rm = TRUE),
			  n = n(),
			  .groups = "drop") 

fwrite(by_time_bin, here("Results", "by_time_bin.csv"), sep = ",", dec = ".", na = "NA")

#### summarise by trial --------------------------------------------------------
by_trial <- raw %>% 
	group_by(participant, age_group, trial, lp, trial_type) %>% 
	summarise(target = sum(target, na.rm = TRUE),
			  distractor = sum(distractor, na.rm = TRUE),
			  n = n(),
			  .groups = "drop") %>% 
	rowwise() %>% 
	mutate(p_target = prod(target, 1/sum(target, distractor, na.rm = TRUE), na.rm = TRUE),
		   p_distractor = prod(distractor, 1/sum(target, distractor, na.rm = TRUE), na.rm = TRUE)) %>% 
	ungroup()

fwrite(by_trial, here("Results", "by_trial.csv"), sep = ",", dec = ".", na = "NA")

ggplot(by_trial, aes(trial_type, p_target, colour = participant)) +
	facet_wrap(~lp) +
	geom_violin(colour = NA, fill = "grey", alpha = 0.5) +
	geom_hline(yintercept = 0.5, linetype = "dashed") +
	geom_jitter(shape = 1, stroke = 1, width = 0.1) +
	stat_summary(fun.data = "mean_se", geom = "pointrange", colour = "black", size = 1) +
	labs(x = "Group", y = "Proportion of fixations") +
	theme_minimal() +
	theme(axis.title = element_text(face = "bold"),
		  axis.title.x = element_blank(),
		  legend.title = element_blank(),
		  strip.background = element_rect(fill = "grey", colour = NA),
		  legend.position = "none")

