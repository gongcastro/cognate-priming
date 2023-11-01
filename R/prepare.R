#' Prepare time course data for modelling
get_data_time <- function(gaze, 
						  participants,
						  vocabulary,
						  attrition_trials,
						  attrition_participants,
						  time_subset = c(0.00, 2.00)){
	
	gaze_tmp <- filter(gaze, phase=="Target-Distractor",
					   timestamp >= time_subset[1],
					   timestamp < time_subset[2]) |> 
		select(session_id, trial, phase, timestamp,
			   is_gaze_target, is_gaze_distractor, trial_type) |> 
		mutate(condition = recode_condition(trial_type),
			   timebin = findInterval(timestamp, seq(time_subset[1], 
			   									  time_subset[2], 0.1))-1) 
	
	vocabulary_tmp <- rename_with(vocabulary,  
								  \(x) gsub("_prop", "", paste0("voc_", x)), 
								  matches("_prop"))
	
	participants_tmp <- participants |> 
		select(child_id, session_id, age_group, age, lp)
	
	attrition_trials_tmp <- attrition_trials |> 
		filter(is_valid_trial) |> 
		select(session_id, trial, samples, is_valid_trial) 
	
	attrition_participants_tmp <- attrition_participants |> 
		filter(is_valid_participant) |> 
		select(session_id)
	
	data_time <- gaze_tmp |>
		inner_join(attrition_trials_tmp, by = join_by(session_id, trial)) |> 
		inner_join(attrition_participants_tmp, by = join_by(session_id)) |> 
		mutate(condition = recode_condition(trial_type)) |> 
		select(session_id, trial, timebin, timestamp,
			   condition, is_gaze_target, is_gaze_distractor) |> 
		# aggregated across trials by participant, time bin and condition
		# see Chow et al. (2018)
		summarise(.sum_t = sum(is_gaze_target, na.rm = TRUE),
				  .sum_d = sum(is_gaze_distractor, na.rm = TRUE),
				  .ntrials = length(unique(trial)),
				  .by = c(session_id, timebin, condition)) |> 
		# empirical logit with adjustment
		# see Barr et al. (2008)
		mutate(.nsamples = .sum_t + .sum_d,
			   .prop = ifelse(.nsamples==0, NA_real_, .sum_t / .nsamples),
			   # impute a maximum of 2 consecutive time bins using LOCF 
			   # .prop = zoo::na.locf(.prop, na.rm = TRUE, maxgap = 2),
			   .elog = log((.sum_t + .5)/(.sum_d + .5))) |> 
		arrange(desc(session_id), condition, timebin) |> 
		inner_join(participants_tmp, by = join_by(session_id)) |> 
		inner_join(vocabulary_tmp, by = join_by(child_id, session_id)) |> 
		mutate(across(c(.nsamples, timebin), as.integer),
			   across(c(child_id, session_id, lp, condition, age_group), as.factor),
			   across(c(age, matches("voc_"), timebin),
			   	   \(x) scale(x, scale = TRUE)[, 1],
			   	   .names = "{.col}_std")) |> 
		select(child_id, session_id, age_group, age, voc_l1, voc_total, lp,
			   condition, timebin, .sum_t, .sum_d, .prop, .elog, .nsamples,
			   matches("_std")) 
	
	# set a priori contrasts
	contrasts(data_time$condition) <- cbind(c(-0.5, 0.5, 0),
											c(0, -0.5, 0.5))
	contrasts(data_time$lp) <- cbind(c(-0.5, 0.25, 0.25),
									 c(0, -0.5, 0.5))
	contrasts(data_time$age_group) <- cbind(c(-0.5, 0.25, 0.25),
											c(0, -0.5, 0.5))
	
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
						  vocabulary,
						  attrition_trials,
						  attrition_participants,
						  time_subset = c(0.30, 2.00),
						  return_clean = TRUE,
						  ...
){
	
	gaze_tmp <- filter(gaze, phase=="Target-Distractor") |> 
		select(session_id, trial, phase, timestamp,
			   is_gaze_target, is_gaze_distractor, trial_type) |> 
		mutate(condition = recode_condition(trial_type),
			   timebin = findInterval(timestamp, seq(time_subset[1], 
			   									  time_subset[2], 0.1))-1) |> 
		filter(between(timestamp, time_subset[1], time_subset[2])) 
	
	vocabulary_tmp <- rename_with(vocabulary,  
								  \(x) gsub("_prop", "", paste0("voc_", x)), 
								  matches("_prop"))
	
	participants_tmp <- participants |> 
		select(child_id, session_id, age_group, age, lp)
	
	attrition_trials_tmp <- attrition_trials |> 
		filter(if (return_clean) is_valid_trial) |> 
		select(session_id, trial, samples, is_valid_trial) 
	
	attrition_participants_tmp <- attrition_participants |> 
		filter(if (return_clean) is_valid_participant) |> 
		select(session_id)
	
	data <- gaze_tmp |>
		inner_join(attrition_trials_tmp, by = join_by(session_id, trial)) |> 
		inner_join(attrition_participants_tmp, by = join_by(session_id)) |> 
		mutate(condition = recode_condition(trial_type)) |> 
		select(session_id, trial, condition, is_gaze_target, is_gaze_distractor) |> 
		# aggregated across trials by participant, time bin and condition
		# see Chow et al. (2018)
		summarise(sum_target = sum(is_gaze_target, na.rm = TRUE),
				  sum_distractor = sum(is_gaze_distractor, na.rm = TRUE),
				  .nsamples = sum(is_gaze_target | is_gaze_distractor, na.rm = TRUE),
				  .ntrials = length(unique(trial)),
				  .by = c(session_id, condition)) |> 
		# empirical logit with adjustment
		# see Barr et al. (2008)
		mutate(.nsamples = sum_target + sum_distractor,
			   .prop = ifelse(.nsamples==0, 0, sum_target / .nsamples),
			   .elog = log((sum_target + .5)/(sum_distractor + .5))) |> 
		rename(.sum = sum_target) |> 
		arrange(desc(session_id), condition) |> 
		inner_join(participants_tmp, by = join_by(session_id)) |> 
		inner_join(vocabulary_tmp, by = join_by(child_id, session_id)) |> 
		mutate(across(c(.nsamples), as.integer),
			   across(c(child_id, session_id, lp, condition, age_group), as.factor),
			   across(c(age, voc_l1),
			   	   \(x) scale(x, scale = TRUE)[, 1],
			   	   .names = "{.col}_std")) |> 
		select(child_id, session_id, age_group, age, voc_l1, voc_total, lp,
			   condition,  .sum, .prop, .elog, .nsamples,
			   matches("std")) 
	
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
