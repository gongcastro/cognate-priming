#' Prepare time course data for modelling
get_data_time <- function(gaze, 
						  participants,
						  stimuli,
						  vocabulary,
						  attrition_trials,
						  attrition_participants,
						  time_subset = c(0.00, 2.00),
						  contrast = "related",
						  ...
){
	
	gaze_tmp <- filter(gaze, phase=="Target-Distractor")
	
	vocabulary_tmp <- rename_with(vocabulary, 
								  \(x) paste0("voc_", x),
								  matches("prop|count"))
	
	clean <- participants |> 
		inner_join(gaze_tmp, by = join_by(child_id, session_id)) |> 
		select(child_id, session_id, age, lp, trial, timestamp, x, y,
			   matches("is_gaze_")) |> 
		left_join(attrition_trials, by = join_by(session_id, trial)) |> 
		left_join(attrition_participants, by = join_by(session_id)) |> 
		left_join(vocabulary_tmp, by = join_by(child_id, session_id)) |> 
		filter(is_valid_gaze,
			   between(timestamp, time_subset[1], time_subset[2])) |>
		mutate(across(c(lp, trial_type), as.factor),
			   timebin = findInterval(timestamp, 
			   					   seq(time_subset[1], 
			   					   	time_subset[2], 0.1))-1) |> 
		select(child_id, session_id, voc_l1 = voc_l1_prop,
			   age, lp, trial, timebin, timestamp, is_valid_vocab_all,
			   trial_type, is_gaze_target, is_gaze_distractor) 
	
	data_time <- aggregate_timecourse(clean, contrast, ...)
	
	test_data_time(data_time)
	
	return(data_time)
	
}

#' Aggregate eye-tracking data into time bins
#' 
aggregate_timecourse <- function(x, contrast, ...) {
	
	conditions <- get_conditions(contrast)
	
	# aggregate data
	data_time <- x |> 
		filter(trial_type!=conditions$exclude_condition) |> 
		# aggregated by participant, see Chow et al. (2018)
		summarise(sum_target = sum(is_gaze_target, na.rm = TRUE),
				  sum_distractor = sum(is_gaze_distractor, na.rm = TRUE),
				  .nsamples = sum(is_gaze_target | is_gaze_distractor, na.rm = TRUE),
				  .ntrials = length(unique(trial)),
				  .by = c(child_id, session_id, age, timebin, lp, trial_type, ...)) |> 
		# elog is the empirical logit, see Barr et al. (2008)
		mutate(.prop = ifelse(.nsamples==0, 0, sum_target / .nsamples),
			   .elog = log((sum_target + .5)/(sum_distractor + .5)),
			   across(c(.nsamples, timebin), as.integer),
			   across(c(child_id, session_id, lp), as.factor), 
			   condition = factor(trial_type, 
			   				   levels = conditions$condition_levels,
			   				   labels = conditions$condition_labels)) |>
		rename(.sum = sum_target) |> 
		arrange(desc(child_id), age, condition, timebin) |> 
		select(child_id, session_id, age, lp, condition, timebin, 
			   .sum, .prop, .elog, .nsamples, ...) 
	
	# set a priori contrasts
	contrasts(data_time$condition) <- c(0.5, -0.5)
	contrasts(data_time$lp) <- cbind(c(-0.5, 0.25, 0.25),
									 c(0, -0.5, 0.5))
	
	# test_data_time(data_time)
	
	return(data_time)
}

#' Get experimental conditions
#' 
get_conditions <- function(contrast) {
	# check args
	if (!(contrast %in% c("related", "cognate"))) {
		cli_abort("contrast must be 'related' or 'cognate'")
	}
	
	# define vars
	if (contrast=="related") {
		exclude_condition <- "Cognate"
		condition_levels <- c("Non-cognate", "Unrelated")
		condition_labels <- c("Related", "Unrelated")
	} else {
		exclude_condition <- "Unrelated"
		condition_levels <- c("Cognate", "Non-cognate")
		condition_labels <- c("Cognate", "Non-cognate")
	}
	
	return(tibble::lst(exclude_condition, 
					   condition_levels,
					   condition_labels))
}

#' Prepare aggregated data for modelling
get_data_summary <- function(data_time){
	
	data_summary <- data_time |> 
		summarise(across(c(matches("prop_|logit_")),
						 \(x) mean(x, na.rm = TRUE)),
				  .nsamples = sum(.nsamples),
				  .by = c(id, age, doe2, voc2,
				  		cognateness, relatedness, matches("voc_"))) |> 
		relocate(matches("voc_"), .after = logit_d)
	
	return(data_summary)
}




#' Aggregated across the time course of the trial
#'
get_data <- function(gaze, 
					 participants,
					 stimuli,
					 vocabulary,
					 attrition_trials,
					 attrition_participants,
					 time_subset = c(0.00, 2.00),
					 contrast = "related",
					 ...
){
	
	gaze_tmp <- filter(gaze, phase=="Target-Distractor")
	
	vocabulary_tmp <- rename_with(vocabulary, 
								  \(x) paste0("voc_", x),
								  matches("prop|count"))
	
	clean <- participants |> 
		inner_join(gaze_tmp, by = join_by(child_id, session_id)) |> 
		select(child_id, session_id, age, lp, trial, timestamp, x, y,
			   matches("is_gaze_")) |> 
		left_join(attrition_trials, by = join_by(session_id, trial)) |> 
		left_join(attrition_participants, by = join_by(session_id)) |> 
		left_join(vocabulary_tmp, by = join_by(child_id, session_id)) |> 
		filter(between(timestamp, time_subset[1], time_subset[2]),
			   is_valid_gaze) |> 
		mutate(across(c(lp, trial_type), as.factor),
			   timebin = findInterval(timestamp, 
			   					   seq(time_subset[1], 
			   					   	time_subset[2], 0.1))-1) |> 
		select(child_id, session_id, voc_l1 = voc_l1_prop,
			   age, lp, trial, timebin, timestamp,
			   trial_type, is_gaze_target, is_gaze_distractor, ...) 
	
	data <- aggregate_trial(clean, contrast, ...)
	
	# test_data_time(data)
	
	return(data)
	
}

#' Aggregate eye-tracking data into time bins
aggregate_trial <- function(x, contrast, ...) {
	
	conditions <- get_conditions(contrast)
	
	# aggregate data
	data <- x |> 
		filter(trial_type!=conditions$exclude_condition) |> 
		# aggregated by participant, see Chow et al. (2018)
		summarise(sum_target = sum(is_gaze_target, na.rm = TRUE),
				  sum_distractor = sum(is_gaze_distractor, na.rm = TRUE),
				  .nsamples = sum(is_gaze_target | is_gaze_distractor, na.rm = TRUE),
				  .ntrials = length(unique(trial)),
				  .by = c(child_id, voc_l1, session_id, age, lp, trial_type, ...)) |> 
		# elog is the empirical logit, see Barr et al. (2008)
		mutate(.prop = ifelse(.nsamples==0, 0, sum_target / .nsamples),
			   .elog = log((sum_target + .5)/(sum_distractor + .5)),
			   across(c(.nsamples), as.integer),
			   across(c(child_id, session_id, lp), as.factor), 
			   condition = factor(trial_type, 
			   				   levels = conditions$condition_levels,
			   				   labels = conditions$condition_labels)) |>
		rename(.sum = sum_target) |> 
		arrange(desc(child_id), age, condition) |> 
		select(child_id, session_id, age, lp, condition, 
			   voc_l1 = voc_l1,
			   .sum, .prop, .elog, .nsamples, ...) 
	
	# set a priori contrasts
	contrasts(data$condition) <- c(0.5, -0.5)
	contrasts(data$lp) <- cbind(c(-0.5, 0.25, 0.25),
								c(0, -0.5, 0.5))
	
	# test_data_time(data)
	
	return(data)
}


