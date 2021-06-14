# analysis

# set up ----

# load packages
library(tidyverse)
library(eyetrackingR) # for processing eye-tracking data
library(lme4) # for fitting multilevel models
library(lmerTest) # for performing F-tests on lmer coefficientes
library(patchwork) # for arranging plots together
library(janitor) # for cleaning colnames
library(here) # for locating files

# functions
logit_to_prob <- function(x) exp(x) / (1 + exp(x))

# import data ----
participants <- readRDS(here("Data", "Participants", "participants.rds")) %>% 
	mutate(participant_unique = as.character(row_number()))
processed <- list(
	Barcelona = readRDS(here("Data", "Gaze", "processed_barcelona.rds")),
	Oxford = readRDS(here("Data", "Gaze", "processed_oxford.rds"))
) %>% 
	bind_rows(.id = "location") %>% 
	# remove CDI labels from item names
	mutate_at(vars(prime, target), function(x) str_remove_all(x, "spa_|cat_")) %>% 
	mutate(
		# needed for eyetrackingR - TRUE means trackloss, FALSE means valid gaze
		trackloss_col = ifelse(valid_sample == TRUE, FALSE, TRUE)
	) %>% 
	# to avoid issues with column names
	rename(time_stamp = time) %>% 
	left_join(distinct(participants, participant, age_group, participant_unique, vocab_size_center))

# attrition
attrition <- processed %>% 
	mutate(valid_other = !(valid_gaze & !valid_trial)) %>% 
	distinct(participant, age_group, lp, trial_type, trial_num, valid_gaze, valid_vocab, valid_other, valid_trial, valid_participant)
saveRDS(attrition, here("Results", "attrition.rds"))

# to eyetrackingR format
d <- processed %>% 
	filter(valid_participant, valid_trial) %>% 
	make_eyetrackingr_data(
		participant_column = "participant_unique",
		trial_column = "trial_num",
		time_column = "time_stamp",
		trackloss_column = "trackloss_col",
		item_columns = c("prime", "target"),
		aoi_columns = c('aoi_target','aoi_distractor'), 
		treat_non_aoi_looks_as_missing = FALSE
	) %>% 
	subset_by_window(rezero = TRUE, window_start_time = 0.3, window_end_time = 1.8) %>% 
	make_time_sequence_data(
		time_bin_size = 0.1, 
		predictor_columns = c("test_language", "location", "trial_type", "age_group", "lp"),
		aois = "aoi_target"
	) %>% 
	as_tibble() %>% 
	# add previous ID to link longitudinal participants
	left_join(distinct(processed, participant, participant_unique, vocab_size_center)) %>% 
	clean_names() %>% 
	select(participant, location, age_group, lp, vocab_size_center, prime, target, trial_num, trial_type,
		   time_bin, ot1, ot2, ot3, prop, weights, elog, logit_adjusted) %>%
	mutate_at(vars(age_group, lp, prime, target, trial_num, trial_type), as.factor)

# set a prior contrasts and orthogonal polynomials
contrasts(d$lp) <- c(-0.5, 0.5)
contrasts(d$trial_type) <- cbind(c(-0.5, -0.5, 1), c(0.5, -0.5, 0))
contrasts(d$age_group) <- contr.poly(3, contrasts = TRUE)

saveRDS(d, here("Data", "Gaze", "processed.rds"))

# fit model ----
fit <- lmer(
	elog ~ age_group + vocab_size_center + trial_type*lp*(ot1+ot2+ot3) +
		(1+ot1+ot2+ot3+trial_type+age_group | participant),
	control = lmerControl(optimizer = "bobyqa"),
	data = d
)
saveRDS(fit, here("Results", "fit.rds"))

# sanity checks ----

# 21mo monolinguals only
d_21_mon <- filter(d, age_group=="21 months", lp=="Monolingual") %>% 
	mutate(vocab_cat = ifelse(
		vocab_size_center > median(participants$vocab_size_center, na.rm = TRUE), 
		"Above median", 
		"Below median"
	)) %>% 
	mutate_at(vars(vocab_cat, location), as.factor)

contrasts(d_21_mon$vocab_cat) <- c(0.5, -0.5)
contrasts(d_21_mon$location) <- c(0.5, -0.5)

fit_21_mon <- lmer(
	elog ~ vocab_size_center + trial_type*(ot1+ot2+ot3) +
		(1+ot1+ot2+ot3 | participant),
	control = lmerControl(optimizer = "bobyqa"),
	data = filter(d, age_group=="21 months", lp=="Monolingual")
)
saveRDS(fit_21_mon, here("Results", "fit_21_mon.rds"))

# by median-split vocab
fit_21_mon_vocab <- lmer(
	elog ~ trial_type*vocab_cat*(ot1+ot2+ot3) +
		(1+ot1+ot2 | participant),
	control = lmerControl(optimizer = "bobyqa"),
	data = d_21_mon
)
saveRDS(fit_21_mon_vocab, here("Results", "fit_21_mon_vocab.rds"))

# by vocab (balanced)
vocab_max_min <- participants %>%
	drop_na(vocab_size_center) %>% 
	filter(age_group=="21 months", lp=="Monolingual") %>% 
	arrange(location, desc(vocab_size_center)) %>% 
	group_by(location) %>% 
	mutate(vocab_index = row_number()) %>% 
	ungroup() %>% 
	filter(
		((location=="Barcelona") & (vocab_index %in% 1:15)) |
			((location=="Oxford")) & (vocab_index %in% seq(max(vocab_index[location=="Oxford"])-15, max(vocab_index[location=="Oxford"])))
	)

fit_21_mon_vocab_pct <- d_21_mon %>% 
	filter(participant %in% vocab_max_min$participant) %>% 
	lmer(
		elog ~ trial_type*location*(ot1+ot2+ot3) +
			(1+ot1+ot2 | participant),
		control = lmerControl(optimizer = "bobyqa"),
		data = .
	)
saveRDS(fit_21_mon_vocab_pct, here("Results", "fit_21_mon_vocab_pct.rds"))

# by location
fit_21_mon_location <- lmer(
	elog ~ trial_type*location*(ot1+ot2+ot3) +
		(1+ot1+ot2 | participant),
	control = lmerControl(optimizer = "bobyqa"),
	data = d_21_mon
)
saveRDS(fit_21_mon_location, here("Results", "fit_21_mon_location.rds"))



