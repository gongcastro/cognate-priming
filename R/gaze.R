


#' Functions for Oxford data ---------------------------------------------------

get_gaze_raw_oxford <- function(gaze_files) {
	
	gaze_raw <- read_csv(gaze_files, 
						 show_col_types = FALSE,
						 progress = FALSE, id = "file",
						 name_repair = janitor::make_clean_names) |> 
		mutate(id = as.character(id),
			   prime_stm = coalesce(vis_cp_stm,
			   					 vis_np_stm, 
			   					 vis_un_stm),
			   trial_type = case_when(!is.na(vis_cp_stm) ~ "Cognate",
			   					   !is.na(vis_np_stm) ~ "Non-cognate",
			   					   !is.na(vis_un_stm) ~ "Unrelated"),
			   is_gaze_prime = coalesce(vis_cp_isfxt,
			   						 vis_np_isfxt,
			   						 vis_un_isfxt),
			   trial = ifelse(block==2, trial + 16, trial)) |> 
		select(id, trial, timestamp, trial_type,
			   x = gaze_filtered_x,
			   y = gaze_filtered_y,
			   prime_stm,
			   target_stm = vis_target_stm,
			   distractor_stm = vis_distr_stm,
			   is_gaze_prime,
			   is_gaze_target = vis_target_isfxt,
			   is_gaze_distractor = vis_distr_isfxt,
			   is_gaze_valid = overall_validity) |> 
		mutate(across(matches("is_gaze"),
					  function(x) {
					  	y <- as.logical(ifelse(x==-1, 1, x))
					  	ifelse(is.na(y), FALSE, y)
					  }))
	
	return(gaze_raw)
}

#' Process Oxford eye-tracking data
#' 
get_gaze_oxford <- function(gaze_files, participants) {
	
	gaze_raw <- get_gaze_raw_oxf(gaze_files)
	
	phase_onsets <- c(0, 2.50, 4.00, 4.05, 4.75, 6.75)
	phase_labels <- c("Getter", "Prime", "Blank", "Audio", "Target-Distractor")
	
	gaze <- gaze_raw |> 
		mutate(timestamp = ((timestamp * 1e-3) - min(timestamp)),
			   phase = cut(timestamp, 
			   			breaks = phase_onsets,
			   			labels = phase_labels,
			   			include.lowest = TRUE),
			   .by = c(id, trial)) |>
		filter(phase %in% c("Prime", "Target-Distractor"),
			   !is.na(phase),
			   id %in% participants$id) |> 
		mutate(timestamp = timestamp - min(timestamp),
			   timebin = cut(timestamp,
			   			  breaks = seq(0.00, 7.00, 0.1),
			   			  labels = FALSE,
			   			  include.lowest = TRUE),
			   timebin = as.integer(timebin)-1,
			   phase = factor(phase, levels = c("Prime", "Target-Distractor")),
			   .by = c(id, trial, phase)) |> 
		relocate(phase, timebin, .after = trial) |> 
		relocate(ends_with("_stm"), .after = everything()) 
	
	return(gaze)
}

#' Compute looking times from eye-tracking data
#'
#' Get looking times
#' 
get_looking_times <- function(gaze_aoi, 
							  participants,
							  stimuli
) {
	gaze_aoi_tmp <- gaze_aoi
	
	stimuli_tmp <- select(stimuli, test_language, list, version,
						  ends_with("_cdi"), duration)
	
	looking_times <- gaze_aoi_tmp |>
		fix_sampling_rate(filename, date_onset = "2022-05-26") |>
		summarise(prime_time = sum(is_gaze_prime[phase=="Prime"], na.rm = TRUE),
				  target_time = sum(is_gaze_target[phase=="Target-Distractor"], na.rm = TRUE),
				  distractor_time = sum(is_gaze_distractor[phase=="Target-Distractor"], na.rm = TRUE),
				  .by = c(filename, trial_type, filename, trial, sampling_rate, matches("_cdi"))) |> 
		mutate(across(ends_with("_time"), \(x) x / sampling_rate),
			   across(ends_with("_time"), \(x) ifelse(x > 1.5, 1.5, x)),
			   test_time = target_time + distractor_time) |>
		select(-sampling_rate) |> 
		left_join(select(participants, filename, id, age_group, lp,
						 test_language, version, list),
				  by = join_by(filename)) |> 
		left_join(stimuli_tmp,
				  by = join_by(prime_cdi, target_cdi, distractor_cdi, test_language,
				  			 version, list)) |> 
		select(id, age_group, lp, filename, trial, trial_type, 
			   prime_time, 
			   target_time, 
			   distractor_time,
			   duration)
	
	return(looking_times)
	
}