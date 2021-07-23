# attrition
get_attrition <- function(
	gaze_bcn, # Barcelona gaze data, get_gaze_bcn output
	gaze_oxf, # Barcelona gaze data, get_gaze_bcn output
	participants, # participants dataset, get_participants output
	stimuli, # stimuli dataset, get_stimuli output
	vocabulary, # vocabulary dataset, get_vocabulary output
	filter_vocabulary = c("prime", "target"), # should the trial be excluded if no comprehension in any of these words? (prime, target, distractor)
	filter_counterbalancing = TRUE, # should trials from same counterbalancing pair be excluded together?
	looking_threshold = c(prime = 0, target = 0, distractor = 0), # minimum looking time
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
				valid_gaze_prime = case_when(
					looking_threshold["prime"]>0 ~ (valid_looking_prime > looking_threshold["prime"]),
					TRUE ~ TRUE
				),					
				# participant has looked at least once to target AND distractor and higher or equal than minimum to both
				valid_gaze_target = case_when(
					looking_threshold["target"]>0 ~ (valid_looking_target > looking_threshold["target"]),
					TRUE ~ TRUE
				),				
				# participant has looked at least once to target AND distractor and higher or equal than minimum to both
				valid_gaze_distractor = case_when(
					looking_threshold["distractor"]>0 ~ (valid_looking_distractor > looking_threshold["distractor"]),
					TRUE ~ TRUE
				),
				# trial meets all gaze criteria
				valid_gaze = (valid_gaze_prime & valid_gaze_target & valid_gaze_distractor)
			) %>% 
			mutate_at(vars(starts_with("valid_")), function(x) ifelse(is.na(x), FALSE, x))
		
		# evaluate vocabulary
		valid_vocabulary <- processed %>%
			distinct(participant, age_group, trial, prime, target, distractor, vocab_words) %>% 
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
			mutate(
				valid_trial =  valid_gaze_prime & valid_gaze_target & valid_gaze_distractor & valid_vocab
			) %>% 
			select(
				participant, age_group, trial_type, trial, prime, target, distractor,
				valid_gaze_prime, valid_gaze_target, valid_gaze_distractor,
				valid_vocab, valid_trial
			)
		
		# valid counterbalancing
		pairs_id <- valid_trials %>% 
			left_join(select(participants, participant, age_group, test_language)) %>% 
			rowwise() %>% 
			mutate(
				target_distractor = strsplit(paste(target, distractor, sep = "."), split = "\\."),
				target_distractor = list(sort(target_distractor))
			) %>% 
			ungroup() %>% 
			distinct(test_language, target_distractor) %>% 
			mutate(counter_pair = row_number())
		
		pairs <- valid_trials %>% 
			rowwise() %>% 
			mutate(
				target_distractor = strsplit(paste(target, distractor, sep = "."), "\\."),
				target_distractor = list(sort(target_distractor))
			) %>% 
			ungroup() %>% 
			left_join(pairs_id) %>% 
			select(participant, age_group, trial,  target, distractor, counter_pair)
		
		valid_counter <- left_join(valid_trials, pairs) %>%  
			count(participant, age_group, counter_pair) %>% 
			mutate(valid_counter = n > 1) %>% 
			select(-n)
		
		if (!filter_counterbalancing) {
			valid_counter <- valid_counter %>% 
				mutate(valid_counter = TRUE)
		}
		
		valid_trials <- reduce(list(valid_trials, pairs, valid_counter), left_join) %>% 
			mutate(valid_trial = valid_trial & valid_counter)

	
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
				participant, age_group, lp, location, test_language, list,
				prime, target, distractor, trial, trial_type,
				valid_gaze_prime, valid_gaze_target, valid_gaze_distractor,
				valid_vocab, valid_trial, valid_counter, valid_participant
			)
		
		return(attrition)
	})
	
}
