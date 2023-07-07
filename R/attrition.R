# attrition
get_attrition_trials <- function(
		gaze_aoi, 
		participants, 
		aoi_coords,
		# minimum looking time in seconds
		looking_threshold = c(prime = 0.75,
							  test = 1.00,
							  test_each = 0.10)
){
	
	attrition_trials <- gaze_aoi |>  
		fix_sampling_rate(filename, date_onset = "2022-05-26") |>
		summarise(
			# % of prime looking time during prime
			prime_time = sum(is_gaze_prime[phase=="Prime"], 
							 na.rm = TRUE), 
			# % of target looking time during target
			target_time = sum(is_gaze_target[phase=="Target-Distractor"],
							  na.rm = TRUE), 
			# % of distractor looking time during target
			distractor_time = sum(is_gaze_distractor[phase=="Target-Distractor"], 
								  na.rm = TRUE), 
			.by = c(id, age_group, trial_type, filename, trial, sampling_rate)) |> 
		mutate(across(ends_with("_time"), \(x) x / sampling_rate),
			   test_time = target_time + distractor_time
		) |> 
		select(-sampling_rate) |> 
		# evaluate criteria
		mutate(
			is_valid_gaze_prime = prime_time >= looking_threshold["prime"], 				
			is_valid_gaze_test = test_time >= looking_threshold["test"],
			is_valid_gaze_test_each =
				(target_time > looking_threshold["test_each"]) &
				(distractor_time > looking_threshold["test_each"]),
			# trial meets all gaze criteria
			is_valid_gaze = (is_valid_gaze_prime & is_valid_gaze_test & is_valid_gaze_test_each),
			across(starts_with("is_valid_"), \(x) ifelse(is.na(x), FALSE, x)),
			is_valid_trial = is_valid_gaze
		) |> 
		select(id, age_group, trial_type, trial, 
			   starts_with("is_valid_gaze_"),
			   is_valid_trial)
	
	return(attrition_trials)
	
}

get_attrition_participants <- function(attrition_trials,
									   participants,
									   # minimum n trials in each condition
									   min_trials = c(cognate = 2, 
									   			   noncognate = 2, 
									   			   unrelated = 2)) {
	# valid participants
	attrition_participants <- attrition_trials |> 
		summarise(is_valid_trial_sum = sum(is_valid_trial, na.rm = TRUE), # number of valid trials by participant, age group and trial type
				  trial_n = n(), # total number of trials by participant, age, group, and trial type
				  .by = c(id, age_group, trial_type)) |> 
		pivot_wider(id_cols = -trial_n, 
					names_from = trial_type,
					values_from = is_valid_trial_sum,
					values_fill = 0,
					names_repair = janitor::make_clean_names) |> 
		rename(noncognate = non_cognate) |> 
		relocate(noncognate, .after = cognate) |> 
		mutate(
			# minimum number of cognate trials
			is_valid_cognate = cognate >= min_trials["cognate"], 
			# has minimum number of noncognate trials
			is_valid_noncognate = noncognate >= min_trials["noncognate"],
			# has minimum number of unrelated trials
			is_valid_unrelated = unrelated >= min_trials["unrelated"], 
			# participant fulfils and the conditions above
			is_valid_participant = is_valid_cognate & is_valid_noncognate & is_valid_unrelated
		) 
	
	return(attrition_participants)
}

