# analysis

# set up ----

# load packages
library(tidyverse)
library(eyetrackingR) # for processing eye-tracking data
library(lme4) # for fitting multilevel models
library(lmerTest) # for performing F-tests on lmer coefficients
library(patchwork) # for arranging plots together
library(janitor) # for cleaning column names
library(job) # for running scripts in the background
library(here) # for locating files

# functions
logit_to_prob <- function(x) exp(x) / (1 + exp(x))

# import data ----
participants <- readRDS(here("Data", "Participants", "participants.rds")) %>% 
	mutate(participant_unique = as.character(row_number()))
vocab <- readRDS(here("Data", "Vocabulary", "vocabulary.rds")) %>% 
	mutate(
		vocab_size_total_center = scale(vocab_size_total)[,1],
		vocab_size_l1_center = scale(vocab_size_l1)[,1],
		vocab_size_conceptual_center = scale(vocab_size_conceptual)[,1]
	) 

clean <- readRDS(here("Data", "Gaze", "clean.rds")) %>% 
	left_join(select(participants, age_group, participant, participant_unique))

# to eyetrackingR format
d <- clean %>% 
	mutate(trackloss_col = !valid_sample) %>% # eyetracking R
	make_eyetrackingr_data(
		participant_column = "participant_unique",
		trial_column = "trial",
		time_column = "time_stamp",
		trackloss_column = "trackloss_col",
		item_columns = c("prime", "target"),
		aoi_columns = c("aoi_target","aoi_distractor"), 
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
	left_join(distinct(participants, participant, participant_unique)) %>% 
	left_join(vocab) %>% 
	clean_names() %>% 
	select(participant, location, age_group, lp,
		   vocab_size_total_center, vocab_size_l1_center, vocab_size_conceptual_center,
		   prime, target, trial, trial_type,
		   time_bin, ot1, ot2, ot3, prop, weights, elog, logit_adjusted) %>%
	mutate_at(vars(age_group, lp, prime, target, trial, trial_type), as.factor)

# set a prior contrasts and orthogonal polynomials
contrasts(d$lp) <- c(-0.5, 0.5)
contrasts(d$trial_type) <- cbind(c(-0.25, -0.25, 0.5), c(0.5, -0.5, 0))
contrasts(d$age_group) <- contr.poly(3, contrasts = TRUE)
saveRDS(d, here("Data", "Gaze", "d.rds"))

# fit models ----

job(
	title = "Fit models",
	import = c(d),
	lmer_result = {
		# main model
		fit = lmer(
			elog ~ age_group + vocab_size_l1_center*trial_type*lp*(ot1+ot2+ot3) +
				(1+ot1+ot2+ot3+trial_type+age_group | participant),
			control = lmerControl(optimizer = "bobyqa"),
			data = d
		)
		saveRDS(fit, here("Results", "fit.rds"))
		
		
		# 21mo monolinguals only
		d_21_mon <- filter(d, age_group=="21 months", lp=="Monolingual") %>% 
			mutate(vocab_cat = ifelse(
				vocab_size_l1_center > median(vocab$vocab_size_l1_center, na.rm = TRUE), 
				"Above median", 
				"Below median"
			)) %>% 
			mutate_at(vars(vocab_cat, location), as.factor)
		
		contrasts(d_21_mon$vocab_cat) <- c(0.5, -0.5)
		contrasts(d_21_mon$location) <- c(0.5, -0.5)
		
		fit_21_mon = lmer(
			elog ~ vocab_size_l1_center + trial_type*(ot1+ot2+ot3) +
				(1+ot1+ot2+ot3 | participant),
			control = lmerControl(optimizer = "bobyqa"),
			data = filter(d, age_group=="21 months", lp=="Monolingual")
		)
		saveRDS(fit_21_mon, here("Results", "fit_21_mon.rds"))
		
		# by median-split vocab
		fit_21_mon_vocab = lmer(
			elog ~ trial_type*vocab_cat*(ot1+ot2+ot3) +
				(1+ot1+ot2 | participant),
			control = lmerControl(optimizer = "bobyqa"),
			data = d_21_mon
		)
		saveRDS(fit_21_mon_vocab, here("Results", "fit_21_mon_vocab.rds"))
		
		# by vocab (balanced)
		vocab_max_min <- participants %>%
			left_join(vocab) %>% 
			drop_na(vocab_size_l1_center) %>% 
			filter(age_group=="21 months", lp=="Monolingual") %>% 
			arrange(location, desc(vocab_size_l1_center)) %>% 
			group_by(location) %>% 
			mutate(vocab_index = row_number()) %>% 
			ungroup() %>% 
			filter(
				((location=="Barcelona") & (vocab_index %in% 1:15)) |
					((location=="Oxford")) & (vocab_index %in% seq(max(vocab_index[location=="Oxford"])-15, max(vocab_index[location=="Oxford"])))
			)
		
		fit_21_mon_vocab_pct = d_21_mon %>% 
			filter(participant %in% vocab_max_min$participant) %>% 
			lmer(
				elog ~ trial_type*(ot1+ot2+ot3) +
					(1+ot1+ot2+ot3 | participant),
				control = lmerControl(optimizer = "bobyqa"),
				data = .
			)
		saveRDS(fit_21_mon_vocab_pct, here("Results", "fit_21_mon_vocab_pct.rds"))
		
		# by location
		fit_21_mon_location = lmer(
			elog ~ trial_type*location*(ot1+ot2+ot3) +
				(1+ot1+ot2 | participant),
			control = lmerControl(optimizer = "bobyqa"),
			data = d_21_mon
		)
		saveRDS(fit_21_mon_location, here("Results", "fit_21_mon_location.rds"))
		
		
		# export
		export(c(fit, fit_21_mon, fit_21_mon_location, fit_21_mon_vocab, fit_21_mon_vocab_pct))
	}
)






