# prepare time course data
get_data_time <- function(gaze_aoi, 
						  participants,
						  stimuli,
						  vocabulary,
						  attrition_trials,
						  attrition_participants,
						  aoi_coords,
						  time_subset = c(0.33, 2.00) # subset time (s)
){
	
	gaze_aoi_tmp <- filter(gaze_aoi, phase=="Target-Distractor")
	
	vocabulary_tmp <- vocabulary |> 
		mutate(across(starts_with("vocab_size"), \(x) scale(x)[, 1],
					  .names = "{.col}_center")) |> 
		right_join(select(participants, id, id_db, age_group),
				   by = join_by(id_db, age_group))
	
	clean <- participants |> 
		select(-filename) |> 
		full_join(stimuli,
				  by = join_by(test_language, list, version),
				  relationship = "many-to-many") |> 
		inner_join(gaze_aoi_tmp,
				   by = join_by(id, age_group, trial_id, trial_type)) |> 
		select(id, age, age_group, trial, phase, timestamp, x, y, 
			   target_location, filename, is_valid_gaze, matches("is_gaze")) |> 
		# evaluate if gaze coordinates are inside any AOI, and which
		drop_na(filename) |> 
		left_join(attrition_trials,
				  by = join_by(id, age_group, trial)) |> 
		left_join(attrition_participants,
				  by = join_by(id, age_group)) |>
		drop_na(trial) |> 
		left_join(select(participants, id, age_group, lp),
				  by = join_by(id, age_group)) |> 
		mutate(across(c(age_group, lp, trial_type), as.factor)) |> 
		select(id, age, age_group, lp, trial, timestamp,
			   is_gaze_target, is_gaze_distractor, trial_type,
			   cognate, noncognate, unrelated,
			   is_valid_trial, is_valid_participant) |>
		filter(is_valid_trial, is_valid_participant)
	
	data_time <- clean |> 
		filter(between(timestamp, time_subset[1], time_subset[2])) |> 
		mutate(timebin = findInterval(timestamp, 
									  seq(time_subset[1], 
									  	time_subset[2], 0.1))-1) |> 
		# see Chow et al. (2018) 
		summarise(.sum_target = sum(is_gaze_target, na.rm = TRUE),
				  .sum_distractor = sum(is_gaze_distractor, na.rm = TRUE),
				  .ntrials = length(unique(trial)),
				  .n = n(),
				  .by = c(id, age_group, timebin, trial_type, lp)) |> 
		mutate(.prop = .sum_target / .n,
			   .logit = log((.sum_target + .5)/(.sum_distractor + .5))) |> 
		left_join(vocabulary_tmp,
				  by = join_by(id, age_group, lp)) |>  
		mutate(across(c(age_group, lp, trial_type), 
					  as.factor), 
			   across(c(.n, timebin, .ntrials), as.integer),
			   across(ends_with("_prop"), \(x) scale(x)[, 1],
			   	   .names = "{.col}_std")) |> 
		drop_na(l1_prop_std, 
				total_prop_std,
				concept_prop_std) |> 
		rename(vocab = total_prop, 
			   vocab_std = total_prop_std) |> 
		# make orthogonal polynomials (see Mirman, 2014)
		polypoly::poly_add_columns(timebin,
								   degree = 3,
								   prefix = "ot",
								   scale_width = 1) |> 
		select(id, age_group, lp,  trial_type, timebin, .prop, .logit,
			   .ntrials, .n, vocab, ot1:ot3) |> 
		arrange(id, age_group, timebin, trial_type) 
	
	# set a prior contrasts and orthogonal polynomials
	contrasts(data_time$lp) <- cbind("mb" = c(0.5, -0.5))
	contrasts(data_time$trial_type) <- cbind("ru" = c(0.25, 0.25, -0.5),
											 "cn" = c(0.5, -0.5, 0))
	contrasts(data_time$age_group) <- cbind("2125" = c(-0.5, 0.5, 0),
											"2530" = c(0, -0.5, 0.5))
	return(data_time)
	
}

# prepare aggregated data
get_data_summary <- function(data_time){
	
	data_summary <- data_time |> 
		reframe(across(c(.prop, .logit),
					   \(x) mean(x, na.rm = TRUE)),
				.ntrials = unique(.ntrials),
				.by = c(id, age_group, lp, trial_type, vocab))
	
	return(data_summary)
}


