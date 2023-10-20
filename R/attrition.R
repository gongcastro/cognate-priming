#' Apply trial-level inclusion criteria
get_attrition_trials <- function(gaze_aoi,
								 participants,
								 aoi_coords,
								 vocabulary,
								 vocabulary_by = c("prime", "target"),
								 # minimum looking time in seconds
								 min_looking = c(
								 	prime = 0.75,
								 	test = 1.00,
								 	test_each = 0.10
								 )) {
	
	# check input
	check_args_attrition_trials()
	
	attrition_trials <- gaze_aoi |>
		fix_sampling_rate(filename, date_onset = "2022-05-26") |>
		summarise(
			prime_time = sum(is_gaze_prime[phase == "Prime"], na.rm = TRUE),
			target_time = sum(is_gaze_target[phase == "Target-Distractor"], na.rm = TRUE),
			distractor_time = sum(is_gaze_distractor[phase == "Target-Distractor"], na.rm = TRUE),
			.by = c(
				filename, trial_type, filename, trial, sampling_rate,
				matches("_cdi")
			)
		) |>
		mutate(across(ends_with("_time"), \(x) x / sampling_rate),
			   test_time = target_time + distractor_time
		) |>
		select(-sampling_rate) |>
		left_join(vocabulary, by = join_by(filename)) |>
		# evaluate criteria
		validate_gaze(min_looking) |>
		mutate(across(
			starts_with("is_valid_"),
			\(x) ifelse(is.na(x), FALSE, x)
		)) |>
		validate_vocabulary(total_contents, vocabulary_by) |>
		mutate(is_valid_trial = is_valid_gaze & is_valid_vocab) |>
		select(
			filename, trial_type, trial,
			starts_with("is_valid_gaze"),
			is_valid_vocab,
			is_valid_trial
		)
	
	test_attrition_trials(attrition_trials)
	
	return(attrition_trials)
}

#' Check arguments for `get_attrition_trials()`
check_args_attrition_trials <- function() {
	
	pf <- parent.frame()
	
	if (!is.data.frame(pf$gaze_aoi)) {
		cli_abort("gaze_aoi must be a data.frame")
	}
	if (!is.data.frame(pf$participants)) {
		cli_abort("participants must be a data.frame")
	}
	if (!is.data.frame(pf$vocabulary)) {
		cli_abort("participants must be a data.frame")
	}
	if (!is.list(pf$aoi_coords) || length(pf$aoi_coords) != 3) {
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
		cli_abort("min_looking must be a numeric vector of length three")
	}
	if (!setequal(names(pf$min_looking), c("prime", "test", "test_each"))) {
		cli_abort("min_looking must be a numeric vector of length three")
	}
}

#' Apply gaze validity inclusion criteria
validate_gaze <- function(data, min_looking) {
	
	# validate arg values
	if (!length(min_looking != 3) | !is.numeric(min_looking)) {
		cli_abort("min_looking must be a numeric vector of length 3")
	}
	
	aoi_names <- c("prime_time", "target_time", "distractor_time")
	if (!all(aoi_names %in% colnames(data))) {
		which.missing <- aoi_names[!(aoi_names %in% colnames(data))]
		cli_abort("{which.missing} is not a variable in data")
	}
	
	validity.names <- c(
		prime = "is_valid_gaze_prime",
		test = "is_valid_gaze_test",
		test_each = "is_valid_gaze_test_each"
	)
	
	# validate prime
	data[validity.names["prime"]] <- data$prime_time >= min_looking["prime"]
	
	# validate target
	test.time <- rowSums(data[, c("target_time", "distractor_time")])
	data[validity.names["test"]] <- test.time >= min_looking["test"]
	
	# validate test-each
	each.target <- (data$target_time >= min_looking["test_each"])
	each.distractor <- (data$distractor_time >= min_looking["test_each"])
	data[validity.names["test_each"]] <- each.target & each.distractor
	
	# validate all
	data$is_valid_gaze <- rowSums(data[, c(validity.names)]) == 3
	
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
		mutate(
			is_valid_vocab_prime = ifelse(
				"prime" %in% vocabulary_by,
				prime_cdi %in% {{ vocab_contents }},
				TRUE
			),
			is_valid_vocab_target = ifelse(
				"target" %in% vocabulary_by,
				target_cdi %in% {{ vocab_contents }},
				TRUE
			),
			is_valid_vocab_distractor = ifelse(
				"distractor" %in% vocabulary_by,
				distractor_cdi %in% {{ vocab_contents }},
				TRUE
			)
		) |>
		ungroup() |>
		mutate(is_valid_vocab = rowSums(across(matches("valid_vocab"))) == 3)
	
	return(vocab_validity)
}


#' Apply participant-level inclusion criteria
get_attrition_participants <- function(attrition_trials,
									   # minimum n trials in each condition
									   min_trials = c(
									   	cognate = 2,
									   	noncognate = 2,
									   	unrelated = 2
									   )) {
	# valid participants
	attrition_participants <- attrition_trials |>
		# get number of valid trials by participant, age, group, and trial type
		summarise(
			is_valid_trial_sum = sum(is_valid_trial, na.rm = TRUE),
			trial_n = n(),
			.by = c(filename, trial_type)
		) |>
		pivot_wider(
			id_cols = filename,
			names_from = trial_type,
			values_from = is_valid_trial_sum,
			values_fill = 0,
			names_repair = janitor::make_clean_names
		) |>
		rename(noncognate = non_cognate) |>
		relocate(noncognate, .after = cognate) |>
		mutate(
			is_valid_cognate = cognate >= min_trials["cognate"],
			is_valid_noncognate = noncognate >= min_trials["noncognate"],
			is_valid_unrelated = unrelated >= min_trials["unrelated"],
			is_valid_participant = rowSums(cbind(
				is_valid_cognate,
				is_valid_noncognate,
				is_valid_unrelated
			)) == 3
		)
	
	test_attrition_participants(attrition_participants)
	
	return(attrition_participants)
}

# Oxford functions ---------------------------------------------------------------

#' Apply trial-level inclusion criteria
get_attrition_trials_oxf <- function(
		gaze_processed, 
		participants, 
		stimuli,
		vocabulary,
		vocabulary_by = c("prime", "target"),
		aoi_coords,
		# minimum looking time in seconds
		min_looking = c(prime = 0.75,
						test = 1.00,
						test_each = 0.10)
){
	
	sampling_rate <- 120
	
	attrition_trials <- gaze_processed |>  
		left_join(stimuli, by = join_by(prime_stm, target_stm, distractor_stm)) |> 
		filter(phase %in% c("Prime", "Target-Distractor")) |> 
		add_count(id, trial_type, trial, phase, name = "total") |> 
		summarise(
			prime_time = sum(is_gaze_prime[phase=="Prime"], na.rm = TRUE), 
			target_time = sum(is_gaze_target[phase=="Target-Distractor"], na.rm = TRUE), 
			distractor_time = sum(is_gaze_distractor[phase=="Target-Distractor"], na.rm = TRUE), 
			.by = c(id, trial_type, trial, matches("_supp"))) |> 
		rename_with(\(x) gsub("_supp", "_cdi", x)) |> 
		mutate(across(ends_with("_time"), \(x) x / sampling_rate),
			   test_time = target_time + distractor_time) |> 
		validate_gaze(min_looking = min_looking) |> 
		mutate(across(starts_with("is_valid_"),
					  \(x) ifelse(is.na(x), FALSE, x))) |> 
		left_join(vocabulary,
				  by = join_by(id)) |> 
		validate_vocabulary(vocab_contents_supp, vocabulary_by) |> 
		mutate(is_valid_trial = is_valid_gaze & is_valid_vocab) |> 
		select(id, trial_type, trial, 
			   starts_with("is_valid_gaze"),
			   is_valid_vocab,
			   is_valid_trial)
	
	return(attrition_trials)
}

#' Apply gaze validity inclusion criteria
validate_gaze_oxf <- function(data, min_looking) {
	
	# validate arg values
	if (!length(min_looking != 3) | !is.numeric(min_looking)) {
		cli::cli_abort("min_looking must be a numeric vector of length 3")
	}
	
	aoi_names <- c("prime_time", "target_time", "distractor_time")
	if(!all(aoi_names %in% colnames(data))) {
		which.missing <- aoi_names[!(aoi_names %in% colnames(data))]
		cli::cli_abort("{which.missing} is not a variable in data")
	}
	
	validity.names <- c(prime = "is_valid_gaze_prime", 
						test = "is_valid_gaze_test",
						test_each = "is_valid_gaze_test_each")
	
	# validate prime
	data[validity.names["prime"]] <- data$prime_time >= min_looking["prime"]
	
	# validate target
	test.time <- rowSums(data[, c("target_time", "distractor_time")])
	data[validity.names["test"]] <- test.time >= min_looking["test"]
	
	# validate test-each
	each.target <- (data$target_time >= min_looking["test_each"])
	each.distractor <- (data$distractor_time >= min_looking["test_each"])
	data[validity.names["test_each"]] <- each.target & each.distractor
	
	# validate all
	data$is_valid_gaze <- rowSums(data[, c(validity.names)])==3
	
	return(data)
}

#' Apply vocabulary validity inclusion criteria
validate_vocabulary_oxf <- function(data,
									vocab_contents,
									vocabulary_by = c("prime", "target")) {
	
	# check args
	cdi_names <- c("prime_cdi", "target_cdi", "distractor_cdi")
	if(!all(cdi_names %in% colnames(data))) {
		which.missing <- cdi_names[!(cdi_names %in% colnames(data))]
		cli::cli_abort("{which.missing} is not a variable in data")
	}
	
	# evaluate validity by vocabulary contents
	vocab_validity <- data |> 
		rowwise() |> 
		mutate(is_valid_vocab_prime = ifelse(
			"prime" %in% vocabulary_by, 
			prime_cdi %in% {{ vocab_contents }},
			TRUE),
			is_valid_vocab_target = ifelse(
				"target" %in% vocabulary_by, 
				target_cdi %in% {{ vocab_contents }},
				TRUE),
			is_valid_vocab_distractor = ifelse(
				"distractor" %in% vocabulary_by,
				distractor_cdi %in% {{ vocab_contents }},
				TRUE
			)) |> 
		ungroup() |> 
		mutate(is_valid_vocab = rowSums(across(matches("valid_vocab")))==3)
	
	return(vocab_validity)
}



#' Apply participant-level inclusion criteria
get_attrition_participants_oxf <- function(attrition_trials,
										   # minimum n trials in each condition
										   min_trials = c(cognate = 2, 
										   			   noncognate = 2, 
										   			   unrelated = 2)) {
	# valid participants
	attrition_participants <- attrition_trials |> 
		# get number of valid trials by participant, age, group, and trial type
		summarise(is_valid_trial_sum = sum(is_valid_trial, na.rm = TRUE), 
				  trial_n = n(), 
				  .by = c(id, trial_type)) |> 
		pivot_wider(id_cols = id, 
					names_from = trial_type,
					values_from = is_valid_trial_sum,
					values_fill = 0,
					names_repair = janitor::make_clean_names) |> 
		rename(noncognate = non_cognate) |> 
		relocate(noncognate, .after = cognate) |> 
		mutate(
			is_valid_cognate = cognate >= min_trials["cognate"], 
			is_valid_noncognate = noncognate >= min_trials["noncognate"],
			is_valid_unrelated = unrelated >= min_trials["unrelated"], 
			is_valid_participant = rowSums(cbind(is_valid_cognate,
												 is_valid_noncognate,
												 is_valid_unrelated))==3
		) 
	
	return(attrition_participants)
}


