#' Prepare time course data for modelling
get_data_time <- function(gaze_aoi, 
						  participants,
						  stimuli,
						  vocabulary,
						  attrition_trials,
						  attrition_participants,
						  time_subset = c(0.00, 2.00),
						  contrast = "cognate"
){
	
	gaze_aoi_tmp <- subset(gaze_aoi, phase=="Target-Distractor")
	
	vocabulary_tmp <- rename_with(vocabulary, 
								  \(x) paste0("voc_", x),
								  matches("prop|count"))
	
	clean <- participants |> 
		inner_join(gaze_aoi_tmp,
				   by = join_by(filename)) |> 
		select(id, id_db, age_group, age, lp, trial, phase, timestamp, x, y, 
			   doe_catalan, doe_spanish,
			   filename, is_valid_gaze, matches("is_gaze_")) |> 
		left_join(attrition_trials,
				  by = join_by(filename, trial)) |> 
		left_join(attrition_participants,
				  by = join_by(filename)) |>
		left_join(vocabulary_tmp, by = join_by(filename)) |> 
		filter(is_valid_trial, 
			   is_valid_participant,
			   between(timestamp, time_subset[1], time_subset[2])) |> 
		mutate(across(c(age_group, lp, trial_type), as.factor),
			   timebin = findInterval(timestamp, 
			   					   seq(time_subset[1], 
			   					   	time_subset[2], 0.1))-1) |> 
		select(id, age, lp, trial, trial_type, timebin, timestamp,
			   is_gaze_target, is_gaze_distractor) 
	
	data_time <- aggregate_timecourse(clean, contrast)
	
	test_data_time(data_time)
	
	return(data_time)
	
}

#' Aggregate eye-tracking data into time bins
aggregate_timecourse <- function(x, contrast) {
	
	conditions <- get_conditions(contrast)
	
	# aggregate data
	data_time <- x |> 
		filter(trial_type != conditions$exclude_condition) |> 
		# aggregated by participant, see Chow et al. (2018)
		summarise(sum_target = sum(is_gaze_target, na.rm = TRUE),
				  sum_distractor = sum(is_gaze_distractor, na.rm = TRUE),
				  .nsamples = sum(is_gaze_target | is_gaze_distractor, na.rm = TRUE),
				  .ntrials = length(unique(trial)),
				  .by = c(id, age, timebin, lp, trial_type)) |> 
		# elog is the empirical logit, see Barr et al. (2008)
		mutate(.prop = ifelse(.nsamples==0, 0, sum_target / .nsamples),
			   .elog = log((sum_target + .5)/(sum_distractor + .5)),
			   across(c(.nsamples, timebin), as.integer),
			   across(c(id, lp), as.factor), 
			   condition = factor(trial_type, 
			   				   levels = conditions$condition_levels,
			   				   labels = conditions$condition_labels)) |>
		rename(.sum = sum_target) |> 
		arrange(desc(id), age, timebin) |> 
		select(id, age, lp, condition, timebin, 
			   .sum, .prop, .elog, .nsamples, .ntrials) 
	
	# set a priori contrasts
	contrasts(data_time$condition) <- c(0.5, -0.5)
	contrasts(data_time$lp) <- c(0.5, -0.5)
	
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

#' Functions for Oxford data ---------------------------------------------------

#' Prepare time course data for modelling
get_data_time_oxf <- function(gaze_processed, 
						  participants,
						  stimuli,
						  vocabulary = vocabulary,
						  attrition_trials,
						  attrition_participants,
						  time_subset = c(0.30, 2.00),
						  contrast = "related"
){
	
	gaze_processed_tmp <- subset(gaze_processed, phase=="Target-Distractor")
	
	vocabulary_tmp <- rename_with(vocabulary,
								  \(x) paste0("voc_", x),
								  matches("prop")) |> 
		select(id, matches("voc_")) |> 
		arrange(desc(voc_total_prop))
	
	clean <- participants |> 
		inner_join(gaze_processed_tmp,
				   by = join_by(id)) |> 
		select(id, age_group, age, lp, trial, phase, timestamp, x, y, 
			   is_gaze_valid, matches("is_gaze_")) |> 
		left_join(attrition_trials,
				  by = join_by(id, trial)) |> 
		left_join(attrition_participants,
				  by = join_by(id)) |>
		left_join(vocabulary_tmp, by = join_by(id)) |>
		filter(is_valid_trial, 
			   is_valid_participant,
			   between(timestamp, time_subset[1], time_subset[2])) |> 
		mutate(across(c(age_group, lp, trial_type), as.factor),
			   timebin = findInterval(timestamp, 
			   					   seq(time_subset[1], 
			   					   	time_subset[2], 0.1))) |> 
		select(id, age, lp, trial, trial_type, timebin, timestamp,
			   is_gaze_target, is_gaze_distractor) 
	
	data_time <- aggregate_timecourse_oxf(clean, contrast)
	
	return(data_time)
	
}

#' Aggregate eye-tracking data into time bins
aggregate_timecourse_oxf <- function(x, contrast) {
	
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
	
	# aggregate data
	data_time <- x |> 
		filter(trial_type != exclude_condition) |> 
		# aggregated by participant, see Chow et al. (2018)
		summarise(sum_target = sum(is_gaze_target, na.rm = TRUE),
				  sum_distractor = sum(is_gaze_distractor, na.rm = TRUE),
				  .nsamples = sum(is_gaze_target | is_gaze_distractor, na.rm = TRUE),
				  .ntrials = length(unique(trial)),
				  .by = c(id, age, timebin, lp, trial_type)) |> 
		# elog is the empirical logit, see Barr et al. (2008)
		mutate(.prop = ifelse(.nsamples==0, 0, sum_target / .nsamples),
			   .elog = log((sum_target + .5)/(sum_distractor + .5)),
			   across(c(.nsamples, timebin), as.integer),
			   across(c(id, lp), as.factor), 
			   condition = factor(trial_type, 
			   				   levels = condition_levels,
			   				   labels = condition_labels)) |>
		rename(.sum = sum_target) |> 
		arrange(desc(id), age, timebin) |> 
		select(id, age, condition, timebin, 
			   .sum, .prop, .elog, .nsamples, .ntrials) 
	
	# set a priori contrasts
	contrasts(data_time$condition) <- c(0.5, -0.5)
	
	return(data_time)
}


