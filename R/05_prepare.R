# prepare

prepare_data <- function(
	gaze_bcn, # Barcelona gaze data, get_gaze_bcn output
	gaze_oxf, # Oxford gaze data, get_gaze_oxf output
	participants, # participants dataset, get_participants output
	stimuli, # stimuli dataset, get_stimuli output
	vocabulary, # vocabulary dataset, get_vocabulary output
	attrition # attrition dataset , get_attrition output
){
	suppressMessages({
		participants <- mutate(participants, participant_unique = as.character(row_number()))
		
		vocabulary <- mutate(
			vocabulary,
			vocab_size_total_center = scale(vocab_size_total)[,1],
			vocab_size_l1_center = scale(vocab_size_l1)[,1],
			vocab_size_conceptual_center = scale(vocab_size_conceptual)[,1]
		) 
		
		clean <- list(Barcelona = gaze_bcn, Oxford = gaze_oxf) %>% 
			bind_rows(.id = "location") %>% 
			# to avoid issues with column names
			rename(time_stamp = time) %>% 
			left_join(attrition) %>%
			left_join(vocabulary) %>% 
			filter(phase=="Target-Distractor", valid_trial, valid_participant) %>% 
			select(
				participant, date_test, lp, location, list, age_group, trial, test_language, phase,
				time_stamp, x, y, valid_sample, trial_type, target_location, aoi_target, aoi_distractor, prime, target,
				vocab_size_total, vocab_size_l1, vocab_size_conceptual
			) %>% 
			drop_na(trial) %>% 
			left_join(select(participants, age_group, participant, participant_unique))
		
		# to eyetrackingR format
		gaze <- clean %>% 
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
			subset_by_window(
				rezero = TRUE,
				window_start_time = 0.3,
				window_end_time = 1.8
			) %>% 
			make_time_sequence_data(
				time_bin_size = 0.1, 
				predictor_columns = c("test_language", "location", "trial_type", "age_group", "lp"),
				aois = "aoi_target"
			) %>% 
			as_tibble() %>% 
			# add previous ID to link longitudinal participants
			left_join(distinct(participants, participant, participant_unique)) %>% 
			left_join(vocabulary) %>%  
			clean_names() %>% 
			select(
				participant, location, age_group, lp,
				vocab_size_total_center, vocab_size_l1_center, vocab_size_conceptual_center,
				prime, target, trial, trial_type,
				time_bin, ot1, ot2, ot3, prop, weights, elog, logit_adjusted
			) %>%
			mutate_at(vars(age_group, lp, location, prime, target, trial, trial_type), as.factor)
		
		# set a prior contrasts and orthogonal polynomials
		contrasts(gaze$lp) <- c(-0.5, 0.5)
		contrasts(gaze$trial_type) <- cbind(c(0.25, 0.25, -0.5), c(0.5, -0.5, 0))
		if (length(unique(gaze$age_group))==3) {
			contrasts(gaze$age_group) <- contr.poly(3, contrasts = TRUE)
		} else if (length(unique(gaze$age_group))==2){
			contrasts(gaze$age_group) <- c(-0.5, 0.5)
		}
		contrasts(gaze$location) <- c(0.5, -0.5)
	})
	return(gaze)
	
}