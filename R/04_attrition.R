# attrition
get_attrition <- function(
	gaze_bcn, # Barcelona gaze data, get_gaze_bcn output
	gaze_oxf, # Barcelona gaze data, get_gaze_bcn output
	participants, # participants dataset, get_participants output
	stimuli, # stimuli dataset, get_stimuli output
	vocabulary, # vocabulary dataset, get_vocabulary output
	filter_vocabulary = c("prime", "target"), # should the trial be excluded if no comprehension in any of these words? (prime, target, distractor)
	looking_threshold = c(prime = 250, target = 250), # minimum looking time
	missing_trials_threshold = c(cognate = 0, noncognate = 0, unrelated = 0) # minimum n trials in each condition
){
	suppressWarnings({
		# merge Barcelona and Oxford gaze data
		gaze <- list(Barcelona = gaze_bcn, Oxford = gaze_oxf) %>% 
			bind_rows(.id = "location") %>% 
			# to avoid issues with column names
			rename(time_stamp = time)
		
		processed <- reduce(list(participants, vocabulary, gaze), left_join)
		
		# evaluate inclusion criteria ----
		# valid gaze
		valid_gaze <- processed %>% 
			group_by(participant, age_group, trial_type, trial) %>% 
			summarise(
				# proportion of prime looking time during prime phase
				valid_looking_prime = sum(aoi_prime[phase=="Prime"], na.rm = TRUE)*(1000/120),
				# proportion of target looking time during target-distractor phase
				valid_looking_target = sum(aoi_target[phase=="Target-Distractor"], na.rm = TRUE)*(1000/120), 
				# proportion of distractor looking time during target-distractor phase
				valid_looking_distractor = sum(aoi_distractor[phase=="Target-Distractor"], na.rm = TRUE)*(1000/120), 
				.groups = "drop"
			) %>% 
			mutate(
				# prime looking proportion during prime phase is higher or equal than minimum
				valid_gaze_prime = valid_looking_prime >= looking_threshold["prime"],
				# participant has looked at least once to target AND distractor and higher or equal than minimum to both
				valid_gaze_target = valid_looking_target>0 & (valid_looking_target >= looking_threshold["target"]),
				# participant has looked at least once to target AND distractor and higher or equal than minimum to both
				valid_gaze_distractor = valid_looking_distractor>0 & (valid_looking_distractor >= looking_threshold["distractor"]),
				# trial meets all gaze criteria
				valid_gaze = valid_gaze_prime & valid_gaze_target 
			)
		
		# evaluate vocabulary
		valid_vocabulary <- processed %>%
			distinct(participant, age_group, trial, prime, target, vocab_words) %>% 
			rowwise() %>% 
			# evaluate if word in in participant's vocabulary or is understood by 50% or more of 19-22 aged children
			mutate(
				valid_vocab_prime = ifelse("prime" %in% filter_vocabulary, prime %in% vocab_words, TRUE),
				valid_vocab_target = ifelse("target" %in% filter_vocabulary, target %in% vocab_words, TRUE),
				valid_vocab_distractor = ifelse("distractor" %in% filter_vocabulary, distractor %in% vocab_words, TRUE),
				valid_vocab = valid_vocab_prime & valid_vocab_target & valid_vocab_distractor
			) %>% 
			ungroup()
		
		# valid trials
		valid_trials <- left_join(valid_gaze, valid_vocabulary) %>% 
			# trial meets all gaze and vocabulary criteria
			mutate(valid_trial =  valid_gaze_prime & valid_gaze_target & valid_gaze_distractor & valid_vocab) %>% 
			select(participant, age_group, trial_type, trial, valid_gaze_prime, valid_gaze_target, valid_gaze_distractor, valid_vocab, valid_trial)
		
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
			pivot_wider(-trial_n, names_from = trial_type, values_from = valid_trial_sum) %>% 
			# rename columns
			clean_names() %>% 
			mutate(
				# minimum number of cognate trials
				valid_participant_cognate = cognate>missing_trials_threshold["cognate"],
				# has minimum number of noncognate trials
				valid_participant_non_cognate = non_cognate>missing_trials_threshold["noncognate"],
				# has minimum number of unrelated trials
				valid_participant_unrelated = unrelated>missing_trials_threshold["unrelated"],
				# participant fulfils and the conditions above
				valid_participant = valid_participant_cognate & valid_participant_non_cognate & valid_participant_unrelated
			)
		
		# attrition ----
		attrition <- reduce(list(processed, valid_trials, valid_participants), left_join) %>% 
			distinct(
				participant, age_group, lp, location, test_language,
				list, trial, trial_type, valid_gaze_prime, valid_gaze_target, valid_gaze_distractor,
				valid_vocab, valid_trial, valid_participant
			)
		
		return(attrition)
	})
	
}

>>>>>>> targets
