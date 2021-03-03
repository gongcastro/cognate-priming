#### processing: Prepare data for analysis -------------------------------------

# set up -----------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)
library(readxl)
library(lubridate)
library(multilex)
library(googlesheets4)
library(ggdist)
library(janitor)
library(scales)
library(here)

# load functions
source(here("R", "utils.R"))

# set params
sampling_rate <- 120  # how many samples does the eye-tracker take per second?
screen_resolution <- c(x = 1920, y = 1080)
left_coords <- c(280, 780, 290, 790)
right_coords <- c(1140, 1640, 290, 790)
relevant_variables <- c(
	"participant", "trial_num", "trial", "phase", "time",
	"l_1", "l_2", "l_v", "r_1", "r_2", "r_v",
	"l_user_coord_3", "l_origin_user_coord_3",
	"r_user_coord_3", "r_origin_user_coord_3"
)


# vocabulary -------------------------------------------------------------------
ml_connect("gonzalo.garciadecastro@upf.edu")
p <- ml_participants() 
r <- ml_responses(p, update = TRUE)
vocab_responses <- r %>% 
	mutate(
		understands = response > 1, # participant understands the item
		age_group = case_when(
			between(age, 20, 24) ~ 21,
			between(age, 24, 28) ~ 25,
			between(age, 28, 33) ~ 30
		)
	) %>% 
	filter(understands, study %in% "CognatePriming") %>% 
	select(id_exp, time, age_group, item, response) %>% 
	group_by(id_exp, time, age_group) %>% 
	summarise(understands = list(unique(item)), .groups = "drop")

vocab_size <- ml_vocabulary(p, r, scale = "prop", by = c("id_exp")) %>% 
	right_join(select(p, id, time, id_exp)) %>% 
	filter(type=="understands") %>% 
	right_join(vocab_responses) %>% 
	select(
		participant = id_exp,
		vocab_size = vocab_prop_total,
		vocab_words = understands,
		age_group
	) 

# participants -----------------------------------------------------------------
participants <- sample_get("gonzalo.garciadecastro@upf.edu") %>% 
	rename(valid_other = valid_participant) %>% 
	filter(
		location=="BCN",
		#valid_participant
		#date_test > as_date("2020-02-14") # get participants with right timestamps
	) %>% 
	drop_na(filename) %>% 
	rename(participant = participant_id) %>% 
	left_join(vocab_size) %>% 
	mutate(age_group = as.factor(age_group)) %>% 
	select(participant, id_db, location, age_group, lp, vocab_size, vocab_words, valid_other, test_language, list, version, filename)

saveRDS(participants, here("Results", "participants.rds"))

# trials -----------------------------------------------------------------------
trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	rename(trial = trial_id) %>% 
	mutate(trial = as.numeric(trial)) %>% 
	select(location, test_language, list, version, trial, trial_type, target_location,
		   prime, target, distractor, matches("_cdi")) %>% 
	filter(location=="BCN")

items_to_know <- trials %>% 
	select(prime_cdi, target_cdi, distractor_cdi) %>% 
	as.list() %>% 
	unlist() %>%
	unique()

saveRDS(trials, here("Results", "trials.rds"))

# get gaze data
gaze <-	map(here("Data", participants$filename), ~import_gaze_data(., relevant_variables)) %>% 
	set_names(participants$filename) %>% 
	bind_rows(.id = "filename") %>% 
	filter(phase %in% "Target-Distractor") %>% 
	mutate(
		x = ifelse(is.na(l_1), r_1, l_1)*screen_resolution["x"],
		y = ifelse(is.na(l_2), r_2, l_2)*screen_resolution["y"],
		d = ifelse(is.na(l_origin_user_coord_3), r_origin_user_coord_3 , l_origin_user_coord_3),
		trial = as.numeric(trial),
		time = as.double(time)
	) %>% 
	left_join(participants) %>%  
	left_join(trials) %>% 
	group_by(participant, age_group, trial) %>%
	mutate(
		time = as.double(time-min(time, na.rm = TRUE))/1000,
		time_bin = cut_interval(time, length = 0.1, labels = FALSE)
	) %>% 
	ungroup() %>% 
	relocate(time_bin, .before = time) %>% 
	arrange(participant, age_group, trial, time_bin) %>% 
	select(participant, age_group, vocab_size, vocab_words, list, test_language, version, trial, time_bin, time, x, y, target_location, d, lp, trial_type, target) %>% 
	mutate(
		gaze_in_l_aoi = gaze_in_aoi(x, y, left_coords),
		gaze_in_r_aoi = gaze_in_aoi(x, y, right_coords),
		fix_target =  (target_location=="r" & gaze_in_r_aoi) | (target_location=="l" & gaze_in_l_aoi),
		fix_distractor =  (target_location=="r" & gaze_in_l_aoi) | (target_location=="l" & gaze_in_r_aoi),
	) %>%
	mutate_at(vars(age_group, lp, trial_type), as.factor) %>% 
	arrange(participant, trial, time) %>%
	select(-c(gaze_in_l_aoi, gaze_in_r_aoi)) %>% 
	relocate(participant, lp, age_group, trial, time_bin, time, fix_target, fix_distractor, x, y, d, target_location, target, test_language, list, version, vocab_size, vocab_words)

# summarise by trial -----------------------------------------------------------
gaze_trial <- gaze %>%
	group_by(participant, age_group, list, test_language, version, trial, lp, trial_type, vocab_size, vocab_words) %>%
	summarise(
		fixations_target = sum(fix_target, na.rm = TRUE),
		fixations_distractor = sum(fix_distractor, na.rm = TRUE),
		fixations_missing = sum(is.na(fix_target) | is.na(fix_distractor)),
		.groups = "drop"
	) %>% 
	rowwise() %>% 
	mutate(
		fixations_n = fixations_target + fixations_distractor,
		prop = fixations_target/fixations_n
	) %>% 
	ungroup()

# missing data report ----------------------------------------------------------
attrition <- gaze_trial %>% 
	left_join(trials) %>% 
	left_join(select(participants, participant, age_group, valid_other)) %>% 
	mutate(
		missing = fixations_missing/(fixations_n+fixations_missing),
		valid_trial_samples = missing < 0.25,
		valid_trial_both = (fixations_target > 0) & (fixations_distractor > 0),
		valid_trial_vocab = (prime_cdi %in% unlist(vocab_words)) & (target_cdi %in% unlist(vocab_words)),
		valid_trial = valid_trial_samples & valid_trial_both & valid_trial_vocab
	) %>% 
	get_valid_participants() %>% 
	select(participant, age_group, trial, trial_type, matches("valid"), missing)

attrition_summary <- attrition %>%
	group_by(participant, age_group) %>%
	summarise(
		missing_n = sum(!valid_trial),
		missing_prop = sum(!valid_trial)/n(),
		valid_participant = unique(valid_participant),
		.groups = "drop"
	)

saveRDS(attrition, here("Results", "attrition.rds"))

# summarise by participant -----------------------------------------------------
gaze_trial <- left_join(gaze_trial, select(attrition, participant, trial, valid_participant, valid_trial))

saveRDS(gaze_trial, here("Results", "gaze_trial.rds"))

gaze_trial %>% 
	filter(valid_participant, valid_trial) %>% 
	group_by(participant, lp, trial_type, age_group) %>% 
	summarise(prop = mean(prop), .groups = "drop") %>% 
	ggplot(aes(trial_type, prop, colour = trial_type, fill = trial_type)) +
	facet_grid(lp~age_group) +
	geom_hline(yintercept = 0.5, linetype = "dashed") +
	geom_line(aes(group = participant), colour = "grey", alpha = 0.5) +
	#stat_slab(position = position_nudge(x = 0.25)) +
	geom_boxplot(width = 0.1, colour = "black", fill = "white", position = position_nudge(x = 0.2), outlier.colour = NA) +
	geom_point(size = 2, shape = 1, stroke = 1) +
	labs(x = "Condition", y = "Target looking / Total looking", colour = "Condition", fill = "Condition") + 
	scale_colour_brewer(palette = "Set1") +
	scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
	theme_custom() +
	theme(legend.position = "none",
		  panel.grid.major.y = element_line(colour = "grey", linetype = "dotted")) +
	ggsave(here("Figures", "gaze_trial.png"))

gaze_trial %>% 
	filter(valid_participant, valid_trial) %>% 
	group_by(participant, lp, trial_type, age_group, vocab_size) %>% 
	summarise(prop = mean(prop), .groups = "drop") %>% 
	ggplot(aes(vocab_size, prop, colour = trial_type, fill = trial_type)) +
	facet_grid(lp~age_group) +
	geom_hline(yintercept = 0.5, linetype = "dashed") +
	geom_line(aes(group = participant), colour = "grey", alpha = 0.5) +
	#stat_slab(position = position_nudge(x = 0.25)) +
	geom_point(size = 2, shape = 1, stroke = 1) +
	geom_smooth(se = FALSE, method = "lm") +
	labs(x = "Condition", y = "Target looking / Total looking", colour = "Condition", fill = "Condition") + 
	scale_colour_brewer(palette = "Set1") +
	scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
	scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
	theme_custom() +
	theme(legend.position = "none",
		  panel.grid.major.y = element_line(colour = "grey", linetype = "dotted")) +
	ggsave(here("Figures", "gaze_trial_vocabulary.png"))


# summarise by time bin --------------------------------------------------------
gaze_time <- gaze %>% 
	group_by(participant, age_group, lp, trial_type, time_bin, target, vocab_size) %>% 
	summarise(
		fixations_target = sum(fix_target, na.rm = TRUE),
		fixations_distractor = sum(fix_distractor, na.rm = TRUE),
		fixations_missing = sum(is.na(fix_target) | is.na(fix_distractor)),
		n = n(),
		.groups = "drop"
	) %>% 
	rowwise() %>% 
	mutate(
		fixations_n = fixations_target + fixations_distractor,
		prop = fixations_target/n
	) %>% 
	ungroup() %>% 
	arrange(participant, time_bin) %>% 
	left_join(select(attrition, participant, valid_participant, valid_trial)) %>% 
	filter(between(time_bin, 1, 20))

saveRDS(gaze_time, here("Results", "gaze_time.rds"))

gaze_time %>% 
	filter(valid_participant, valid_trial) %>% 
	group_by(time_bin, trial_type, lp, age_group) %>% 
	summarise(
		prop = mean(prop, na.rm = TRUE),
		n = n(),
		.groups = "drop"
	) %>% 
	ungroup() %>% 
	mutate(
		se = sqrt((prop*(1-prop))/(n+4)),
		lower = prop-se,
		upper = prop+se
	) %>% 
	ggplot(aes(x = time_bin, y = prop, colour = trial_type, fill = trial_type)) +
	facet_grid(lp~age_group) +
	geom_hline(yintercept = 0.5, linetype = "dashed") +
	geom_smooth() +
	geom_line() +
	#geom_ribbon(aes(ymin = lower, ymax = upper), width = 0) 
	geom_point(shape = 1, stroke = 1, size = 1) +
	labs(x = "Time bin (100 ms)", y = "Fixations", colour = "Condition", fill = "Condition") +
	scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
	scale_colour_brewer(palette = "Set1") +
	scale_fill_brewer(palette = "Set1") +
	theme_custom() +
	theme(legend.title = element_blank(),
		  legend.position = "top",
		  panel.grid = element_line(colour = "grey", linetype = "dotted")) +
	ggsave(here("Figures", "gaze_time.png"))


gaze_time %>% 
	filter(valid_participant, valid_trial) %>% 
	group_by(participant, time_bin, trial_type, lp, age_group) %>% 
	summarise(
		prop = mean(prop, na.rm = TRUE),
		n = n(),
		.groups = "drop"
	) %>% 
	ungroup() %>% 
	mutate(
		se = sqrt((prop*(1-prop))/(n+4)),
		lower = prop-se,
		upper = prop+se
	) %>% 
	ggplot(aes(x = time_bin, y = prop, colour = trial_type, fill = trial_type)) +
	facet_wrap(participant~lp) +
	geom_hline(yintercept = 0.5, linetype = "dashed") +
	geom_smooth() +
	geom_line() +
	#geom_ribbon(aes(ymin = lower, ymax = upper), width = 0) 
	geom_point(shape = 1, stroke = 1, size = 1) +
	labs(x = "Time bin (100 ms)", y = "Fixations", colour = "Condition", fill = "Condition") +
	scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
	scale_colour_brewer(palette = "Set1") +
	scale_fill_brewer(palette = "Set1") +
	theme_custom() +
	theme(legend.title = element_blank(),
		  legend.position = "top",
		  panel.grid = element_line(colour = "grey", linetype = "dotted")) +
	ggsave(here("Figures", "gaze_time_participant.png"), height = 12, width = 12)

# plot participants
participants %>% 
	left_join(attrition_summary) %>% 
	ggplot(aes(lp, fill = valid_participant)) +
	facet_grid(~age_group) +
	geom_bar() +
	geom_hline(yintercept = 24, linetype = "dashed") +
	labs(x = "List", y = "N", fill = "Valid?") +
	scale_fill_brewer(palette = "Set1") +
	theme_custom() +
	ggsave(here("Figures", "sample.png"))


col <- ifelse(distinct(attrition, participant, valid_participant)$valid_participant, "#377EB8", "#E41A1C")

attrition %>% 
	left_join(attrition_summary) %>% 
	mutate(valid_trial = ifelse(valid_trial, "Valid", "Invalid"),
		   participant = paste0(participant, " (", missing_n, ", ", percent(missing_prop), ")")) %>% 
	ggplot(aes(missing, participant, colour = valid_trial)) +
	geom_point(alpha = 0.5) +
	geom_vline(xintercept = 0.25, linetype = "dashed") +
	
	labs(x = "Proportion of missing trials (out of 32)", y = "Participant") +
	scale_color_brewer(palette = "Set1") +
	theme_custom() +
	theme(axis.text.y = element_text(colour = col, size = 8),
		  axis.title.y = element_blank(),
		  axis.text.x = element_text(colour = "black"),
		  legend.title = element_blank(),
		  legend.position = "top",
		  text = element_text(size = 15)) +
	ggsave(here("Figures", "missing.png"), height = 9)

# vocabulary
vocab_size %>% 
	mutate(age_group = as.factor(age_group)) %>% 
	left_join(participants) %>% 
	drop_na(lp) %>% 
	ggplot(aes(as.factor(age_group), vocab_size, colour = lp, fill = lp)) +
	geom_point(shape = 1, size = 3, stroke = 1, position = position_jitter(width = 0.1, seed = 888)) +
	geom_boxplot(width = 0.1, colour = "black", outlier.colour = NA, position = position_dodge(width = 0.75)) +
	labs(x = "Age group (months)", y = "Receptive vocabulary size (L1)", colour = "Group", fill = "Group") +
	scale_colour_brewer(palette = "Set1") +
	scale_fill_brewer(palette = "Set1") +
	theme_custom() +
 	theme(legend.title = element_blank(),
 		  legend.position = "top",
 		  panel.grid.major.y = element_line(colour = "grey", linetype = "dotted")) +
	ggsave(here("Figures", "vocabulary.png"))


