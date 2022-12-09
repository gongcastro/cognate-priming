# attrition
get_attrition <- function(
		gaze_imputed, # Barcelona gaze data, get_gaze_bcn output
		participants, # participants dataset, get_participants output
		stimuli, # stimuli dataset, get_stimuli output
		vocabulary, # vocabulary dataset, get_vocabulary output
		aoi_coords = aoi_coords,
		filter_vocabulary = c("prime", "target"), # should the trial be excluded if no comprehension in any of these words? (prime, target, distractor)
		filter_counterbalancing = FALSE, # should trials from same counterbalancing pair be excluded together?
		looking_threshold = c(prime = 0.25, target = 0.25, distractor = 0), # minimum looking time
		missing_trials_threshold = c(cognate = 2, non_cognate = 2, unrelated = 2) # minimum n trials in each condition
){
	suppressWarnings({
		
		processed <- list(
			select(participants, -filename),
			stimuli,
			gaze_imputed
			) %>% 
			reduce(full_join) %>%  
			drop_na(filename) %>% 
			select(
				participant, age_group, trial, phase, time, x, y, 
				target_location,
				valid_sample,
				trial_type, prime_cdi, target_cdi, distractor_cdi,
				filename
			) 
		
		# evaluate inclusion criteria ----
		# valid gaze
		valid_gaze <- processed %>% 
			# evaluate if gaze coordinates are inside any AOI, and which
			mutate(
				aoi_center = gaze_in_center(x, y, aoi_coords = aoi_coords),
				aoi_left = gaze_in_left(x, y, aoi_coords),
				aoi_right = gaze_in_right(x, y, aoi_coords)
			) %>% 
			replace_na(list(aoi_center = FALSE, aoi_right = FALSE, aoi_left = FALSE)) %>% 
			mutate(
				aoi_prime = aoi_center,
				aoi_target = ifelse(target_location=="r", aoi_right, aoi_left),
				aoi_distractor = ifelse(target_location=="l", aoi_right, aoi_left)
			) %>% 
			rename_all(function(x) str_remove(x, "_cdi")) %>% 
			group_by(participant, age_group, trial_type, filename, trial) %>% 
			summarise(
				# proportion of prime looking time during prime phase
				valid_looking_prime = sum(aoi_prime[phase=="Prime"], na.rm = TRUE)*(1/120),
				# proportion of target looking time during target-distractor phase
				valid_looking_target = sum(aoi_target[phase=="Target-Distractor"], na.rm = TRUE)*(1/120), 
				# proportion of distractor looking time during target-distractor phase
				valid_looking_distractor = sum(aoi_distractor[phase=="Target-Distractor"], na.rm = TRUE)*(1/120), 
				.groups = "drop"
			) %>% 
			mutate(
				# prime looking proportion during prime phase is higher or equal than minimum
				valid_gaze_prime = ifelse(looking_threshold["prime"] > 0, valid_looking_prime > looking_threshold["prime"], TRUE),					
				# participant has looked at least once to target AND distractor and higher or equal than minimum to both
				valid_gaze_target = ifelse(looking_threshold["target"] > 0, valid_looking_target > looking_threshold["target"], TRUE),				
				# participant has looked at least once to target AND distractor and higher or equal than minimum to both
				valid_gaze_distractor = ifelse(looking_threshold["distractor"] > 0 , valid_looking_distractor > looking_threshold["distractor"], TRUE),
				# trial meets all gaze criteria
				valid_gaze = (valid_gaze_prime & valid_gaze_target & valid_gaze_distractor),
				across(starts_with("valid_"), ~ifelse(is.na(.), FALSE, .))
			) 
		
		# 
		# # evaluate vocabulary
		# valid_vocabulary <- processed %>%
		# 	distinct(participant, age_group, trial, prime, target, distractor, vocab_words) %>% 
		# 	rowwise() %>% 
		# 	# evaluate if word in in participant's vocabulary or is understood by 50% or more of 19-22 aged children
		# 	mutate(
		# 		valid_vocab_prime = ifelse("prime" %in% filter_vocabulary, prime %in% vocab_words, TRUE),
		# 		valid_vocab_target = ifelse("target" %in% filter_vocabulary, target %in% vocab_words, TRUE),
		# 		valid_vocab_distractor = ifelse("distractor" %in% filter_vocabulary, distractor %in% vocab_words, TRUE),
		# 		valid_vocab = valid_vocab_prime & valid_vocab_target & valid_vocab_distractor
		# 	) %>% 
		# 	ungroup()
		
		# valid trials
		valid_trials <- valid_gaze %>% 
			# trial meets all gaze and vocabulary criteria
			mutate(valid_trial = valid_gaze_prime & valid_gaze_target & valid_gaze_distractor) %>% 
			select(
				participant, 
				age_group,
				trial_type, 
				trial,
				valid_gaze_prime,
				valid_gaze_target,
				valid_gaze_distractor, 
				valid_trial
			)
		
		# valid counterbalancing
		# pairs_id <- valid_trials %>% 
		# 	left_join(select(participants, participant, age_group, test_language)) %>% 
		# 	rowwise() %>% 
		# 	mutate(
		# 		target_distractor = strsplit(paste(target, distractor, sep = "."), split = "\\."),
		# 		target_distractor = list(sort(target_distractor))
		# 	) %>% 
		# 	ungroup() %>% 
		# 	distinct(test_language, target_distractor) %>% 
		# 	mutate(counter_pair = row_number())
		# 
		# pairs <- valid_trials %>% 
		# 	rowwise() %>% 
		# 	mutate(
		# 		target_distractor = strsplit(paste(target, distractor, sep = "."), "\\."),
		# 		target_distractor = list(sort(target_distractor))
		# 	) %>% 
		# 	ungroup() %>% 
		# 	left_join(pairs_id) %>% 
		# 	select(participant, age_group, trial,  target, distractor, counter_pair)
		# 
		# valid_counter <- left_join(valid_trials, pairs) %>%  
		# 	count(participant, age_group, counter_pair) %>% 
		# 	mutate(valid_counter = n > 1) %>% 
		# 	select(-n)
		# 
		# if (!filter_counterbalancing) {
		# 	valid_counter <- valid_counter %>% 
		# 		mutate(valid_counter = TRUE)
		# }
		# 
		# valid_trials <- reduce(list(valid_trials, pairs, valid_counter), left_join) %>% 
		# 	mutate(valid_trial = valid_trial & valid_counter)
		# 
		
		# valid participants
		valid_participants <- valid_trials %>% 
			group_by(participant, age_group, trial_type) %>% 
			summarise(
				# number of valid trials by participant, age group and trial type
				valid_trial_sum = sum(valid_trial, na.rm = TRUE),
				# total number of trials by participant, age, group, and trial type
				trial_n = n(),
				.groups = "drop"
			) %>% 
			# to wide format
			pivot_wider(-trial_n, names_from = trial_type, values_from = valid_trial_sum, values_fill = 0) %>% 
			# rename columns
			clean_names() %>% 
			mutate(
				# minimum number of cognate trials
				valid_participant_cognate = cognate >= missing_trials_threshold["cognate"],
				# has minimum number of noncognate trials
				valid_participant_non_cognate = non_cognate >= missing_trials_threshold["non_cognate"],
				# has minimum number of unrelated trials
				valid_participant_unrelated = unrelated >= missing_trials_threshold["unrelated"],
				# participant fulfils and the conditions above
				valid_participant = valid_participant_cognate & valid_participant_non_cognate & valid_participant_unrelated
			)
		
		# attrition ----
		attrition <- list(select(processed, -trial_type), valid_trials, valid_participants) %>% 
			reduce(left_join) %>% 
			distinct(
				participant, 
				age_group, 
				trial, 
				trial_type,
				valid_gaze_prime, 
				valid_gaze_target,
				valid_gaze_distractor,
				valid_trial,
				cognate,
				non_cognate,
				unrelated,
				valid_participant
			)
		
		return(attrition)
	})
	
}

