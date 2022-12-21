# attrition
get_attrition <- function(
		gaze_aoi, # Barcelona gaze data, get_gaze_bcn output
		participants, # participants dataset, get_participants output
		stimuli, # stimuli dataset, get_stimuli output
		vocabulary, # vocabulary dataset, get_vocabulary output
		aoi_coords = aoi_coords,
		filter_vocabulary = c("prime", "target"), # should the trial be excluded if no comprehension in any of these words? (prime, target, distractor)
		looking_threshold = c(prime = 0.25, target = 0.25, distractor = 0), # minimum looking time
		min_trials = c(cognate = 2, non_cognate = 2, unrelated = 2) # minimum n trials in each condition
){
	suppressWarnings({
		
		valid_gaze <- gaze_aoi %>% 
			group_by(filename) %>% 
			mutate(sampling_rate = ifelse(n() < 8e3, 60, 120)) %>% 
			ungroup() %>% 
			group_by(id, age_group, trial_type, filename, trial, sampling_rate) %>% 
			summarise(valid_looking_prime = sum(aoi_prime[phase=="Prime"], na.rm = TRUE), # % of prime looking time during prime
					  valid_looking_target = sum(aoi_target[phase=="Target-Distractor"], na.rm = TRUE), # % of target looking time during target
					  valid_looking_distractor = sum(aoi_distractor[phase=="Target-Distractor"], na.rm = TRUE), # % of distractor looking time during target
					  .groups = "drop") %>% 
			mutate(across(starts_with("valid_looking_"), ~./sampling_rate)) %>% 
			select(-sampling_rate) %>% 
			# evaluate criteria
			mutate(valid_gaze_prime = valid_looking_prime >= looking_threshold["prime"], 				
				   valid_gaze_target = valid_looking_target >= looking_threshold["target"],			
				   valid_gaze_distractor = valid_looking_distractor > looking_threshold["distractor"],
				   valid_gaze = (valid_gaze_prime & valid_gaze_target & valid_gaze_distractor), # trial meets all gaze criteria
				   across(starts_with("valid_"), ~ifelse(is.na(.), FALSE, .))) 
		
		# valid trials
		valid_trials <- valid_gaze %>% 
			# trial meets all gaze and vocabulary criteria
			mutate(valid_trial = valid_gaze_prime & valid_gaze_target & valid_gaze_distractor) %>% 
			select(id, age_group, trial_type, trial, starts_with("valid_gaze_"), valid_trial)
		
		# valid participants
		valid_participants <- valid_trials %>% 
			group_by(id, age_group, trial_type) %>% 
			summarise(valid_trial_sum = sum(valid_trial, na.rm = TRUE), # number of valid trials by participant, age group and trial type
					  trial_n = n(), # total number of trials by participant, age, group, and trial type
					  .groups = "drop") %>% 
			pivot_wider(-trial_n, names_from = trial_type, values_from = valid_trial_sum, values_fill = 0) %>% 
			clean_names() %>% 
			mutate(
				valid_participant_cognate = cognate >= min_trials["cognate"], # minimum number of cognate trials
				valid_participant_non_cognate = non_cognate >= min_trials["non_cognate"], # has minimum number of noncognate trials
				valid_participant_unrelated = unrelated >= min_trials["unrelated"], # has minimum number of unrelated trials
				valid_participant = valid_participant_cognate & valid_participant_non_cognate & valid_participant_unrelated) # participant fulfils and the conditions above
		
		# attrition
		attrition <- list(gaze_aoi, valid_trials, valid_participants) %>% 
			reduce(left_join) %>% 
			distinct(id, age_group, trial, trial_id, trial_type, 
					 valid_gaze_prime, valid_gaze_target, valid_gaze_distractor,
					 valid_trial, cognate, non_cognate, unrelated, valid_participant)
		
		return(attrition)
	})
	
}

