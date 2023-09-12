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
	
	return(data_time)
	
}

#' Agrgegate eye-tracking data into time bins
aggregate_timecourse <- function(x, contrast) {
	
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
		select(id, age, lp, condition, timebin, 
			   .sum, .prop, .elog, .nsamples, .ntrials) 
	
	# set a priori contrasts
	contrasts(data_time$condition) <- c(0.5, -0.5)
	contrasts(data_time$lp) <- c(0.5, -0.5)
	
	return(data_time)
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
prepare_data_oxf <- function(gaze_processed, 
							 participants, 
							 attrition, 
							 time_subset = c(0, 2) # subset time (s)
){
	suppressMessages({
		
		gaze_processed_tmp <- filter(gaze_processed, phase=="Target-Distractor")
		
		clean <- participants |> 
			inner_join(gaze_processed,
					   by = join_by(filename)) |>
			between(timestamp, time_subset[1], time_subset[2]) |> 
			mutate(timestamp = timestamp - min(timestamp),
				   timebin = cut(timestamp, 
				   			  breaks = seq(0, 7, 0.1),
				   			  labels = FALSE,
				   			  include.lowest = TRUE),
				   .by = c(id, trial, trial_type)) |> 
			inner_join(select(attrition$participant, id, valid_participant),
					   by = join_by(id)) |> 
			filter(valid_participant) |> 
			inner_join(select(attrition$trials, id, trial, trial_type, valid_trial),
					   by = join_by(id, trial, trial_type)) |> 
			drop_na(trial) |> 
			left_join(select(participants, id, age_group),
					  by = join_by(id)) |> 
			select(id, age_group, trial, timebin, timestamp,
				   is_gaze_target, trial_type) |> 
			mutate(cognateness = ifelse(trial_type=="Unrelated", 
										"None",
										trial_type),
				   relatedness = ifelse(trial_type!="Unrelated",
				   					 "Related",
				   					 trial_type))
		
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
			mutate(doe2 = ifelse(doe_catalan >= doe_spanish,
								 doe_spanish, doe_catalan),
				   voc2 = voc_l2_count / voc_total_count,
				   cognateness = ifelse(trial_type=="Unrelated", "None", trial_type),
				   relatedness = ifelse(trial_type!="Unrelated", "Related", trial_type),
				   across(c(age_group, lp, trial_type, cognateness, relatedness), as.factor)) |> 
			filter(is_valid_trial, is_valid_participant) |> 
			select(id, age, lp, doe2, voc2, trial, timestamp,
				   is_gaze_target, is_gaze_distractor,
				   cognateness, relatedness,
				   matches("_prop"),
				   is_valid_trial, is_valid_participant)
		
		df <- clean |> 
			summarise(.sum = sum(is_gaze_target, na.rm = TRUE),
					  .ntrials = length(unique(trial)),
					  .n = n(),
					  .by = c(id, age_group, timebin, trial_type)) |> 
			mutate(.sum = case_when(.sum == 0 ~ 1.0,
									.sum == .n ~ .n - 1.0,
									.default = as.double(.sum)),
				   .prop = .sum / .n,
				   # see Chow et al. (2018) 
				   .logit = log(.sum / (.n - .sum))) |> 
			mutate(across(c(trial_type), as.factor), 
				   across(c(.sum, .n, timebin, .ntrials), as.integer)) |> 
			arrange(id, timebin) |>
			mutate(related = as.character(trial_type),
				   related = ifelse(related != "Unrelated",
				   				 "Related", 
				   				 related),
				   related = factor(related, levels = c("Related", "Unrelated"))) |> 
			# make orthogonal polynomials (see Mirman, 2014)
			polypoly::poly_add_columns(timebin,
									   degree = 3,
									   prefix = ".ot",
									   scale_width = 1)
		
		# set a prior contrasts and orthogonal polynomials
		contrasts(df$trial_type) <- cbind("C_NC" = c(+0.5, -0.5, 0),
										  "NC_UN" = c(0, +0.5, -0.5))
		
		contrasts(df$related) <- cbind("related_unrelated" = c(-0.5 , +0.5))
		contrasts(df$age_group) <- cbind("21_25" = c(-0.5, 0.5, 0),
										 "25_30" = c(0, -0.5, 0.5))
		
		df <- select(df, id, age_group, timebin,
					 starts_with(".ot"),
					 .sum, .n, .prop, .logit, 
					 related, trial_type, .ntrials)
	})
	
	return(df)
	
}

#' Aggregate data across trials
aggregate_data_oxf <- function(df_timecourse){
	
	df_aggregated <- df_timecourse |> 
		reframe(across(c(.prop, .logit),
					   \(x) mean(x, na.rm = TRUE)),
				across(c(.sum, .n), \(x) sum(x, na.rm = TRUE)),
				.ntrials = unique(.ntrials),
				.by = c(id, age_group, related, trial_type))
	
	return(df_aggregated)
}




