#' Prepare time course data for modelling
get_data_time <- function(gaze, 
						  participants,
						  stimuli,
						  vocabulary,
						  attrition_trials,
						  attrition_participants,
						  time_subset = c(0.30, 2.00),
						  return_clean = TRUE,
						  ...
){
	
	gaze_tmp <- filter(gaze, phase=="Target-Distractor")
	
	vocabulary_tmp <- rename_with(vocabulary, 
								  \(x) paste0("voc_", x),
								  matches("prop|count"))
	
	clean <- participants |> 
		inner_join(gaze_tmp, by = join_by(child_id, session_id)) |> 
		select(child_id, session_id, age_group, age, lp, trial, timestamp, x, y,
			   matches("is_gaze_")) |> 
		left_join(attrition_trials, by = join_by(session_id, trial)) |> 
		left_join(attrition_participants, by = join_by(session_id)) |> 
		left_join(vocabulary_tmp, by = join_by(child_id, session_id)) |> 
		filter(between(timestamp, time_subset[1], time_subset[2]),
			   if (return_clean) is_valid_participant,
			   if (return_clean) is_valid_trial) |> 
		mutate(across(c(lp, age_group), as.factor),
			   condition = recode_condition(trial_type),
			   timebin = findInterval(timestamp, 
			   					   seq(time_subset[1], 
			   					   	time_subset[2], 0.1))-1) |> 
		select(child_id, session_id, voc_l1 = voc_l1_prop,
			   age_group, age, lp, trial, timebin, timestamp, is_valid_vocab_all,
			   condition, is_gaze_target, is_gaze_distractor,
			   is_valid_trial, is_valid_participant) 
	
	data_time <- aggregate_timecourse(clean, ...)
	
	# set a priori contrasts
	contrasts(data_time$condition) <- cbind(c(-0.5, 0.5, 0),
											c(0, -0.5, 0.5))
	contrasts(data_time$lp) <- cbind(c(-0.5, 0.25, 0.25),
									 c(0, -0.5, 0.5))
	contrasts(data_time$age_group) <- cbind(c(-0.5, 0.25, 0.25),
									 c(0, -0.5, 0.5))
	
	test_data_time(data_time)
	
	return(data_time)
	
}

#' Aggregate eye-tracking data into time bins
#' 
aggregate_timecourse <- function(x, contrast, ...) {
	
	# aggregate data
	data_time <- x |> 
		# aggregated by participant, see Chow et al. (2018)
		summarise(sum_target = sum(is_gaze_target, na.rm = TRUE),
				  sum_distractor = sum(is_gaze_distractor, na.rm = TRUE),
				  .nsamples = sum(is_gaze_target | is_gaze_distractor, na.rm = TRUE),
				  .ntrials = length(unique(trial)),
				  .by = c(child_id, session_id, age_group, age, 
				  		voc_l1, timebin, lp, condition, ...)) |> 
		# elog is the empirical logit, see Barr et al. (2008)
		mutate(.prop = ifelse(.nsamples==0, 0, sum_target / .nsamples),
			   .elog = log((sum_target + .5)/(sum_distractor + .5)),
			   across(c(.nsamples, timebin), as.integer),
			   across(c(child_id, session_id, lp), as.factor)) |> 
		rename(.sum = sum_target) |> 
		arrange(desc(child_id), age, condition, timebin) |> 
		select(child_id, session_id, age_group, age, voc_l1, lp, condition, timebin, 
			   .sum, .prop, .elog, .nsamples, ...) 
	
	# test_data_time(data_time)
	
	return(data_time)
}

#' Recode condition levels
#' 
recode_condition <- function(x) {
	fct_levels <- list("Unrelated" = "Unrelated",
					   "Non-cognate" = "Related/Non-cognate",
					   "Cognate" = "Related/Cognate")
	x <- factor(x, levels = names(fct_levels), labels = fct_levels)
	return(x)
}

#' Aggregated across the time course of the trial
#'
get_data_aggr <- function(gaze, 
						  participants,
						  stimuli,
						  vocabulary,
						  attrition_trials,
						  attrition_participants,
						  time_subset = c(0.30, 2.00),
						  return_clean = TRUE,
						  ...
){
	
	gaze_tmp <- filter(gaze, phase=="Target-Distractor")
	
	vocabulary_tmp <- rename_with(vocabulary, 
								  \(x) paste0("voc_", x),
								  matches("prop|count"))
	
	clean <- participants |> 
		inner_join(gaze_tmp, by = join_by(child_id, session_id)) |> 
		select(child_id, session_id, age_group, age, lp,
			   trial, timestamp, x, y,
			   matches("is_gaze_"), ...) |> 
		left_join(attrition_trials, by = join_by(session_id, trial)) |> 
		left_join(attrition_participants, by = join_by(session_id)) |> 
		left_join(vocabulary_tmp, by = join_by(child_id, session_id)) |> 
		filter(between(timestamp, time_subset[1], time_subset[2]),
			   if (return_clean) is_valid_participant,
			   if (return_clean) is_valid_trial) |> 
		mutate(across(c(lp, age_group), as.factor),
			   condition = recode_condition(trial_type),
			   timebin = findInterval(timestamp, 
			   					   seq(time_subset[1], 
			   					   	time_subset[2], 0.1))-1) |> 
		select(child_id, session_id, voc_l1 = voc_l1_prop,
			   age_group, age_group, age, lp, trial, timebin, timestamp,
			   condition, is_gaze_target, is_gaze_distractor, ...) 
	
	data <- aggregate_trial(clean, contrast, ...)
	
	# set a priori contrasts
	contrasts(data$condition) <- cbind(c(-0.5, 0.5, 0),
									   c(0, -0.5, 0.5))
	contrasts(data$lp) <- cbind(c(-0.5, 0.25, 0.25),
								c(0, -0.5, 0.5))
	contrasts(data$age_group) <- cbind(c(-0.5, 0.5, 0),
								c(0, -0.5, 0.5))
	# test_data_time(data)
	
	return(data)
	
}

#' Aggregate eye-tracking data into time bins
aggregate_trial <- function(x, contrast, ...) {
	
	# aggregate data
	data <- x |> 
		# aggregated by participant, see Chow et al. (2018)
		summarise(sum_target = sum(is_gaze_target, na.rm = TRUE),
				  sum_distractor = sum(is_gaze_distractor, na.rm = TRUE),
				  .nsamples = sum(is_gaze_target | is_gaze_distractor, na.rm = TRUE),
				  .ntrials = length(unique(trial)),
				  .by = c(child_id, voc_l1, session_id, 
				  		age_group, age, lp, condition, ...)) |> 
		# elog is the empirical logit, see Barr et al. (2008)
		mutate(.prop = ifelse(.nsamples==0, 0, sum_target / .nsamples),
			   .elog = log((sum_target + .5)/(sum_distractor + .5)),
			   across(c(.nsamples), as.integer),
			   across(c(child_id, session_id, lp, age_group), as.factor)) |> 
		rename(.sum = sum_target) |> 
		arrange(desc(child_id), age, condition) |> 
		select(child_id, session_id, age_group, age, lp, condition, 
			   voc_l1 = voc_l1, .sum, .prop, .elog, .nsamples, ...) 
	
	# test_data(data)
	
	return(data)
}


