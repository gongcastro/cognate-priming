#' Apply trial-level inclusion criteria
#' 
get_attrition_trials <- function(gaze, participants, vocabulary, aoi_coords,
								 vocabulary_by = c("prime", "target"),
								 # minimum looking time in seconds
								 min_looking = c(prime = 0.75,
								 				test = 1.00,
								 				test_each = 0.10,
								 				test_any = 0.00)) {
	
	check_args_attrition_trials() # check input
	
	remove_list_names <- function(x, pattern) {
		names(x) <- gsub(pattern, "", names(x))
		return(x)
	}
	
	attrition_trials <- gaze |> 
		left_join(distinct(participants, child_id, session_id, vocab_id, filename),
				  by = join_by(child_id, session_id)) |>  
		fix_sampling_rate(filename, date_onset = "2022-05-26") |>
		summarise(prime_samples = sum(is_gaze_prime[phase=="Prime"], na.rm = TRUE),
				  target_samples = sum(is_gaze_target[phase=="Target-Distractor"], na.rm = TRUE),
				  distractor_samples = sum(is_gaze_distractor[phase=="Target-Distractor"], na.rm = TRUE),
				  .by = c(child_id, session_id, vocab_id, trial_type,
				  		trial, sampling_rate, matches("_cdi"))) |>
		mutate(across(matches("_samples"), \(x) x / sampling_rate, .names = "{.col}_looking"),
			   test_time = target_samples + distractor_samples) |> 
		rename_with(\(x) gsub("_samples_looking", "_looking", x)) |> 
		select(-sampling_rate) |> 
		left_join(vocabulary, by = join_by(child_id, vocab_id, session_id)) |>   
		# evaluate criteria
		validate_gaze(min_looking) |> 
		mutate(across(starts_with("is_valid_"), \(x) ifelse(is.na(x), FALSE, x))) |> 
		validate_vocabulary(contents, vocabulary_by) |>
		mutate(is_valid_trial = is_valid_gaze & is_valid_vocab) |>
		rowwise() |> 
		mutate(is_valid_vocab_all = list(unlist(across(matches("is_valid_vocab_")))),
			   looking = list(unlist(across(matches("_looking")))),
			   samples = list(unlist(across(matches("_samples")))),
			   is_valid_gaze_all = list(unlist(across(matches("is_valid_gaze_"))))) |> 
		ungroup() |> 
		mutate(across(c(looking, samples), 
					  \(x) remove_list_names(x, "_looking|_samples")),
			   across(matches("all"), 
			   	   \(x) remove_list_names(x, "is_valid_vocab_|is_valid_gaze_"))) |> 
		left_join(participants, by = join_by(session_id, vocab_id, child_id)) |> 
		select(session_id, trial, trial_type,
			   looking, samples,  is_valid_trial,
			   is_valid_vocab, is_valid_vocab_all, 
			   is_valid_gaze, is_valid_gaze_all)
	
	# test_attrition_trials(attrition_trials)
	
	return(attrition_trials)
}

#' Check arguments for `get_attrition_trials()`
check_args_attrition_trials <- function() {
	
	pf <- parent.frame()
	
	if (!is.data.frame(pf$gaze)) {
		cli_abort("gaze must be a data.frame")
	}
	if (!is.data.frame(pf$participants)) {
		cli_abort("participants must be a data.frame")
	}
	if (!is.data.frame(pf$vocabulary)) {
		cli_abort("participants must be a data.frame")
	}
	if (!is.list(pf$aoi_coords) || length(pf$aoi_coords)!=3) {
		cli_abort("aoi_coords must be a list of length three")
	}
	if (!all(purrr::map_dbl(pf$aoi_coords, length)==4) || !all(purrr::map_chr(pf$aoi_coords, class)=="numeric")) {
		cli_abort("aoi_coords nested vectors should should be of length four")
	}
	if (!all(pf$vocabulary_by %in% c("prime", "target", "distractor", "none"))) {
		cli_abort("vocabulary_by must be one of 'prime', 'target', 'distractor', or 'none'")
	}
	if (("none" %in% pf$vocabulary_by) && (length(pf$vocabulary_by) > 1)) {
		cli_abort("vocabulary_by must be either 'none', or any combination of 'prime', 'target', and 'distractor', but not both")
	}
	if (!is.numeric(pf$min_looking)) {
		cli_abort("min_looking must be a numeric vector of length four")
	}
	if (!setequal(names(pf$min_looking), c("prime", "test", "test_each", "test_any"))) {
		cli_abort("min_looking must be a numeric vector of length four")
	}
}

#' Apply gaze validity inclusion criteria
#' 
validate_gaze <- function(data, min_looking) {
	
	# validate arg values
	if (!length(min_looking!=3) | !is.numeric(min_looking)) {
		cli_abort("min_looking must be a numeric vector of length 3")
	}
	
	aoi_names <- c("prime_looking", "target_looking", "distractor_looking")
	if (!all(aoi_names %in% colnames(data))) {
		which.missing <- aoi_names[!(aoi_names %in% colnames(data))]
		cli_abort("{which.missing} is not a variable in data")
	}
	
	validity.names <- c(prime = "is_valid_gaze_prime",
						test = "is_valid_gaze_test",
						test_each = "is_valid_gaze_test_each",
						test_any = "is_valid_gaze_test_any")
	
	# validate prime
	data[validity.names["prime"]] <- data$prime_looking >= min_looking["prime"]
	
	# validate target
	test.time <- rowSums(data[, c("target_looking", "distractor_looking")])
	data[validity.names["test"]] <- test.time >= min_looking["test"]
	
	# validate test-any
	any.target <- data$target_looking >= min_looking["test_any"]
	any.distractor <- data$distractor_looking >= min_looking["test_any"]
	data[validity.names["test_any"]] <- any.target | any.distractor
	
	# validate test-each
	each.target <- data$target_looking >= min_looking["test_each"]
	each.distractor <- data$distractor_looking >= min_looking["test_each"]
	data[validity.names["test_each"]] <- each.target & each.distractor
	
	# validate all
	data$is_valid_gaze <- rowSums(data[, c(validity.names)])==length(validity.names)
	
	return(data)
}

#' Apply vocabulary validity inclusion criteria
validate_vocabulary <- function(data,
								vocab_contents,
								vocabulary_by = c("prime", "target")) {
	# check args
	cdi_names <- c("prime_cdi", "target_cdi", "distractor_cdi")
	if (!all(cdi_names %in% colnames(data))) {
		which.missing <- cdi_names[!(cdi_names %in% colnames(data))]
		cli_abort("{which.missing} is not a variable in data")
	}
	
	# evaluate validity by vocabulary contents
	vocab_validity <- data |>
		rowwise() |>
		mutate(is_valid_vocab_prime = ifelse("prime" %in% vocabulary_by,
											 prime_cdi %in% {{ vocab_contents }},
											 TRUE),
			   is_valid_vocab_target = ifelse("target" %in% vocabulary_by,
			   							   target_cdi %in% {{ vocab_contents }},
			   							   TRUE),
			   is_valid_vocab_distractor = ifelse("distractor" %in% vocabulary_by,
			   								   distractor_cdi %in% {{ vocab_contents }},
			   								   TRUE)) |>
		ungroup() |>
		mutate(is_valid_vocab = rowSums(across(matches("valid_vocab")))==3)
	
	return(vocab_validity)
}


#' Apply participant-level inclusion criteria
get_attrition_participants <- function(attrition_trials,
									   # minimum n trials in each condition
									   min_trials = c(cognate = 2,
									   			   noncognate = 2,
									   			   unrelated = 2)) {
	# valid participants
	attrition_participants <- attrition_trials |>
		# get number of valid trials by participant, age, group, and trial type
		summarise(is_valid_trial_sum = sum(is_valid_trial, na.rm = TRUE),
				  trial_n = n(),
				  .by = c(session_id, trial_type)) |>
		pivot_wider(id_cols = session_id,
					names_from = trial_type,
					values_from = is_valid_trial_sum,
					values_fill = 0,
					names_repair = janitor::make_clean_names) |>
		rename(noncognate = non_cognate) |>
		relocate(noncognate, .after = cognate) |>
		mutate(is_valid_cognate = cognate >= min_trials["cognate"],
			   is_valid_noncognate = noncognate >= min_trials["noncognate"],
			   is_valid_unrelated = unrelated >= min_trials["unrelated"],
			   is_valid_participant = rowSums(cbind(is_valid_cognate,
			   									 is_valid_noncognate,
			   									 is_valid_unrelated))==3) |>
		rowwise() |> 
		mutate(.ntrials = list(unlist(across(c(cognate, noncognate, unrelated))))) |> 
		ungroup() |> 
		select(session_id, .ntrials, is_valid_participant)
	
	test_attrition_participants(attrition_participants)
	
	return(attrition_participants)
}



