#' Get eye-tracker data
#'
get_gaze <- function(gaze_files_bcn,
					 gaze_files_oxf,
					 participants,
					 aoi_coords,
					 stimuli,
					 non_aoi_as_na = TRUE) {
	
	# get eye-tracking data in Barcelona
	gaze_bcn <- get_gaze_bcn(gaze_files_bcn,
							 participants,
							 stimuli, 
							 aoi_coords = aoi_coords,
							 non_aoi_as_na = non_aoi_as_na)
	
	# get eye-tracking data in Oxford
	gaze_oxf <- get_gaze_oxf(gaze_files_oxf, participants)
	
	# merge datasets
	gaze <- bind_rows(gaze_bcn, gaze_oxf) |> 
		mutate(trial = as.integer(trial))
	
	test_gaze(gaze)
	
	return(gaze)
}

# Barcelona --------------------------------------------------------------------

get_gaze_bcn <- function(gaze_files,
						 participants,
						 stimuli,
						 aoi_coords,
						 non_aoi_as_na = TRUE) {
	
	participants_tmp <- select(participants, filename, child_id, session_id,
							   test_language, list, version)
	
	stimuli_tmp <- select(stimuli, test_language, list, version, trial,
						  loc_t = target_location, trial_type, matches("_cdi")) |> 
		mutate(list = as.integer(list))
	
	gaze_processed <- get_gaze_processed_bcn(gaze_files)
	
	gaze_bcn <- gaze_processed |> 
		mutate(filename = gsub("\\.csv.csv", "\\.csv", filename))  |> 
		inner_join(participants_tmp, by = join_by(filename)) |> 
		left_join(stimuli_tmp, by = join_by(trial, test_language, list, version)) |>  
		make_non_aoi_as_false(non_aoi_as_na = non_aoi_as_na) |>
		mutate(is_gaze_prime = gaze_in_prime(x, y, aoi_coords),
			   is_gaze_target = gaze_in_target(x, y, loc_t, aoi_coords),
			   is_gaze_distractor = gaze_in_distractor(x, y, loc_t, aoi_coords)) |> 
		select(child_id, session_id, trial, phase, timestamp, x, y,
			   is_gaze_prime, is_gaze_target, is_gaze_distractor,
			   is_valid_gaze, is_imputed, trial_type, matches("_cdi"))
	
	return(gaze_bcn)
	
}


#' Process Barcelona gaze data
get_gaze_processed_bcn <- function(gaze_files){
	
	screen_resolution <- c(x = 1920, y = 1080) # screen size in pixels
	
	# import gaze data ----
	gaze_processed <- get_gaze_raw_bcn(gaze_files) |> 
		fix_sampling_rate(filename, date_onset = "2022-05-26") |> 
		add_count(filename, name = "n_samples") |> 
		# restart timestamps at each trial phase change, and express in seconds
		mutate(timestamp = ((1:n())-1) / sampling_rate,
			   timestamp = timestamp - min(timestamp),
			   .by = c(filename, trial, phase)) |> 
		# trim timestamps outside the 2 seconds range
		filter(between(trial, 1, 32),
			   !(phase=="Prime" & timestamp >= 1.5), 
			   !(phase=="Target-Distractor" & timestamp >= 2.0)) |> 
		# change gaze coordinates from 0-1 scale to screen resolution scale [1920x1080]
		mutate(x = x*screen_resolution["x"],
			   y = y*screen_resolution["y"]) |> 
		select(filename, trial, phase, timestamp, x, y, is_valid_gaze, filename) |> 
		arrange(filename, trial, phase, timestamp)
	
	# merge data
	expanded <- expand(gaze_processed, filename, trial, phase, timestamp)
	
	gaze_processed <- gaze_processed |>
		right_join(expanded,
				   by = join_by(trial, phase, timestamp, filename)) |> 
		select(filename, trial, phase, timestamp, x, y, is_valid_gaze) |> 
		mutate(is_valid_gaze = ifelse(is.na(is_valid_gaze), FALSE, is_valid_gaze)) |> 
		dplyr::filter(!(phase=="Prime" & timestamp > 1.5),
					  !(phase=="Target" & timestamp > 2.0)) |> 
		mutate(x_imputed = zoo::na.locf(x, maxgap = 20, na.rm = FALSE),
			   y_imputed = zoo::na.locf(y, maxgap = 20, na.rm = FALSE),
			   is_imputed = is.na(x) & !is.na(x_imputed),
			   x = x_imputed,
			   y = y_imputed,
			   .by = c(filename, trial, phase)) |> 
		select(-c(x_imputed, y_imputed)) |> 
		relocate(is_imputed, .after = is_valid_gaze)
	
	return(gaze_processed)
}

#' Get raw eye-tracker gaze
get_gaze_raw_bcn <- function(gaze_files){
	
	# name changes (see R/utils.R)
	cols_dict <- get_name_dictionary()$col_name_changes
	phase_dict <- get_name_dictionary()$phase_name_changes
	keep_dict <- get_name_dictionary()$relevant_variables
	
	# prepare for loop and pre-allocate list
	n_files <- length(gaze_files)
	db <- vector(mode = "list", length = n_files)
	names(db) <- basename(gaze_files)
	na_strings <- c("", "NaN", "NA", "<NA>")
	
	# prepare progress bar
	pb_text <- "{pb_spin} Reading {pb_current}/{pb_total} | {col_blue(pb_percent)}"
	cli_progress_bar("Processing", total = n_files, format = pb_text)
	
	for (i in 1:n_files){
		db[[i]] <- arrow::read_csv_arrow(gaze_files[i], na = na_strings) |>
			# rename all variables to snake case
			janitor::clean_names() |> 
			rename(any_of(cols_dict)) |> 
			fix_timestamps() |> 
			fix_validity() |> 
			# get gaze in prime and target-distractor phases
			mutate(
				phase = str_replace_all(phase, phase_dict),
				trial = as.integer(trial),
				across(c(l_v, r_v), \(x) as.logical(as.integer(x))),
				x = ifelse(is.na(l_x) | !l_v, r_x, l_x), 
				y = ifelse(is.na(l_y) | !l_v, r_y, l_y),
				across(c(x, y), \(x) ifelse(!between(x, 0, 1), NA_real_, x)),
				is_valid_gaze = as.logical((!(is.na(x) | is.na(y)) & (l_v | r_v)))
			) |>
			dplyr::filter(phase %in% c("Target-Distractor", "Prime")) |>
			add_missing_cols(keep_dict) |> 
			select(any_of(keep_dict))
		
		cli_progress_update()
	}
	cli_progress_done(result = "done")
	
	cli_progress_step("{n_files} datasets retrieved", spinner = FALSE)
	gaze_raw  <- bind_rows(db, .id = "filename")
	cli_progress_done(result = "done")
	
	test_gaze_raw(gaze_raw)
	
	return(gaze_raw)
}


#' Get name dictionaries
get_name_dictionary <- function(...) {
	
	col_name_changes <- c(
		id = "participant",
		id = "suje_num",
		timestamp = "time",
		timestamp = "system_time_stamp",
		l_x = "l_1",
		l_y = "l_2",
		phase = "process",
		phase = "baseline",
		r_x = "r_1",
		r_y = "r_2",
		l_user_coord_z = "l_user_coord_3",
		r_user_coord_z = "r_user_coord_3",
		is_valid_gaze = "mean_validity",
		is_valid_gaze = "is_valid_gaze",
		id = "sujenum",
		trial_num = "numtrial_lista"
		# trial = "trial_num"
	)
	
	phase_name_changes <- c(
		"GETTER" = "Getter",
		"baseline" = "Getter",
		"PRIMEIMAGE" = "Prime",
		"prime" = "Prime",
		"primeimage" = "Prime",
		"BLANK" = "Blank",
		"Blank_AUDIO" = "Audio",
		"BLANK_AUDIO" = "Audio",
		"blank" = "Blank",
		"audio" = "Audio",
		"TARGET_DISTRACTOR" = "Target-Distractor",
		"target_distractor" = "Target-Distractor"
	)
	
	relevant_variables <- c("trial", "phase", "x", "y", "is_valid_gaze")
	
	name_dict <- list(col_name_changes = col_name_changes, 
					  phase_name_changes = phase_name_changes, 
					  relevant_variables = relevant_variables)
	
	return(name_dict)
}

#' Process Oxford eye-tracking data
#' 
get_gaze_oxford <- function(gaze_files, participants) {
	
	gaze_raw <- get_gaze_raw_oxf(gaze_files)
	
	phase_name_changes <- c(
		"GETTER" = "Getter",
		"baseline" = "Getter",
		"PRIMEIMAGE" = "Prime",
		"prime" = "Prime",
		"primeimage" = "Prime",
		"BLANK" = "Blank",
		"Blank_AUDIO" = "Audio",
		"BLANK_AUDIO" = "Audio",
		"blank" = "Blank",
		"audio" = "Audio",
		"TARGET_DISTRACTOR" = "Target-Distractor",
		"target_distractor" = "Target-Distractor"
	)
	
	relevant_variables <- c("trial", "phase", "x", "y", "is_valid_gaze")
	
	name_dict <- list(col_name_changes = col_name_changes, 
					  phase_name_changes = phase_name_changes, 
					  relevant_variables = relevant_variables)
	
	return(name_dict)
}

#' Fix eye-tracker sampling rate
#' 
#' From 2022-05-26, the eye-tracker sampling rate was accidentally changed from 120Hz to 60Hz.
#' This function makes sure that the processing of the datasets corresponding to subsequent experimental sessions takes this change in sampling rate  into account. 
fix_sampling_rate <- function(x, filename, date_onset) {
	
	sp60_files <- unique(x$filename)
	regex_pattern <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"
	sp60_dates <- stringr::str_extract(sp60_files, pattern = regex_pattern)
	sp60_dates <- as.Date(sp60_dates, format = "%Y-%m-%d")
	sp60_files <- sp60_files[which(sp60_dates >= as.Date(date_onset))]
	
	x$sampling_rate <- ifelse(x$filename %in% sp60_files, 60, 120)
	
	return(x)
}

#' Fix eye-tracker time stamps
fix_timestamps <- function(x) {
	
	fix_periods <- function(x) {
		
		x <- gsub("\\.", "", x)
		x <- gsub("^(.{1})(.*)$", "\\1.\\2", x, perl = TRUE)
		x <- as.double(x)
		
		return(x)
	}
	x_cols <- select(x, l_x, r_x, l_y, r_y)
	is_any_char <- any(purrr::map_chr(x_cols, class)=="character")
	
	if (is_any_char) {
		x$l_x <- fix_periods(x$l_x)
		x$r_x <- fix_periods(x$r_x)
		x$l_y <- fix_periods(x$l_y)
		x$r_y <- fix_periods(x$r_y)
	}
	return(x)
}

#' Add missing columns (if any)
add_missing_cols <- function(x, variables) {
	
	cols <- colnames(x)
	missing_col <- which(!(variables %in% cols))
	missing_col <- variables[missing_col]
	
	if (length(missing_col) > 0) {
		new <- rep(NA, length(missing_col))
		names(new) <- missing_col
		x <- mutate(x, !!!new)
	}
	
	return(x)
}

#' Fix eye-tracker sample valitidy
fix_validity <- function(x) {
	
	fix_zero_one <- function(x) {
		ifelse(!(x %in% c("0", "1")), "0", "1")
	}
	
	x_cols <- select(x, l_v, r_v)
	is_any_char <- any(purrr::map_chr(x_cols, class)=="character")
	
	if (is_any_char) {
		x$l_v <- fix_zero_one(x$l_v)
		x$r_v <- fix_zero_one(x$r_v)
	}
	return(x)
}



# helper functions -------------------------------------------------------------

make_non_aoi_as_false <- function(x, non_aoi_as_na = FALSE) {
	
	if (non_aoi_as_na) {
		na_sub <- list(is_gaze_center = FALSE,
					   is_gaze_right = FALSE, 
					   is_gaze_left = FALSE)	
		x <- replace_na(x, na_sub)
	}
	
	return(x)
}

# evaluate if gaze is in prime
gaze_in_prime <- function(x, y, aoi_coords){
	
	x_eval <- between(x, aoi_coords$c["xmin"], aoi_coords$c["xmax"]) 
	y_eval <- between(y, aoi_coords$c["ymin"], aoi_coords$c["ymax"])
	
	return(x_eval & y_eval)
}

# evaluate if gaze is in target
gaze_in_target <- function(x, y, loc, coords){
	is_in <- case_when(
		loc=="r" ~ between(x, coords$r["xmin"], coords$r["xmax"]) &
			between(y, coords$r["ymin"], coords$r["ymax"]),
		loc=="l" ~ between(x, coords$l["xmin"], coords$l["xmax"]) &
			between(y, coords$l["ymin"], coords$l["ymax"]),
		.default = FALSE
	)
	return(is_in)
}

# evaluate if gaze is in distractor
gaze_in_distractor <- function(x, y, loc, coords){
	is_in <- case_when(
		loc=="l" ~ between(x, coords$r["xmin"], coords$r["xmax"]) &
			between(y, coords$r["ymin"], coords$r["ymax"]),
		loc=="r" ~ between(x, coords$l["xmin"], coords$l["xmax"]) &
			between(y, coords$l["ymin"], coords$l["ymax"]),
		.default = FALSE
	)
	return(is_in)
}

#' Functions for Oxford data ---------------------------------------------------

# import gaze
get_gaze_raw_oxf <- function(gaze_files) {
	
	gaze_raw <- read_csv(gaze_files, 
						 show_col_types = FALSE,
						 progress = FALSE, id = "file",
						 name_repair = janitor::make_clean_names) |> 
		mutate(session_id = as.character(id),
			   prime_stm = coalesce(vis_cp_stm,
			   					 vis_np_stm,
			   					 vis_un_stm),
			   trial_type = case_when(!is.na(vis_cp_stm) ~ "Cognate",
			   					   !is.na(vis_np_stm) ~ "Non-cognate",
			   					   !is.na(vis_un_stm) ~ "Unrelated",),
			   is_gaze_prime = coalesce(vis_cp_isfxt,
			   						 vis_np_isfxt,
			   						 vis_un_isfxt),
			   trial = ifelse(block==2, trial + 16, trial)) |> 
		select(session_id, trial, timestamp, trial_type,
			   x = gaze_filtered_x, y = gaze_filtered_y,
			   prime_stm, target_stm = vis_target_stm,
			   distractor_stm = vis_distr_stm,
			   is_gaze_prime, is_gaze_target = vis_target_isfxt,
			   is_gaze_distractor = vis_distr_isfxt,
			   is_gaze_valid = overall_validity) |> 
		mutate(across(matches("is_gaze"),
					  function(x) {
					  	y <- as.logical(ifelse(x==-1, 1, x))
					  	ifelse(is.na(y), FALSE, y)
					  }))
	
	return(gaze_raw)
}

# process gaze
get_gaze_oxf <- function(gaze_files, 
						 participants) {
	
	phase_onsets <- c(0, 2.50, 4.00, 4.05, 4.75, 6.75)
	phase_labels <- c("Getter", "Prime", "Blank", "Audio", "Target-Distractor")
	
	gaze_raw <- get_gaze_raw_oxf(gaze_files) 
	
	gaze_oxf <- gaze_raw |> 
		rename(is_valid_gaze = is_gaze_valid) |> 
		rename_with(\(x) gsub("_stm", "_cdi", x)) |> 
		mutate(timestamp = ((timestamp * 1e-3) - min(timestamp)),
			   is_imputed = FALSE,
			   phase = cut(timestamp, 
			   			breaks = phase_onsets,
			   			labels = phase_labels,
			   			include.lowest = TRUE),
			   .by = c(session_id, trial)) |>
		filter(phase %in% c("Prime", "Target-Distractor"),
			   !is.na(phase),
			   session_id %in% participants$session_id) |> 
		mutate(timestamp = timestamp - min(timestamp),
			   timebin = cut(timestamp,
			   			  breaks = seq(0.00, 7.00, 0.1),
			   			  labels = FALSE,
			   			  include.lowest = TRUE),
			   timebin = as.integer(timebin)-1,
			   phase = factor(phase, levels = c("Prime", "Target-Distractor")),
			   .by = c(session_id, trial, phase)) |> 
		relocate(phase, timebin, .after = trial) |> 
		relocate(ends_with("_cdi"), .after = everything()) |> 
		left_join(participants,
				  by = join_by(session_id)) |> 
		select(child_id, session_id, trial, phase, timestamp,
			   x, y, is_gaze_prime, is_gaze_target, is_gaze_distractor,
			   is_valid_gaze, is_imputed, trial_type,
			   prime_cdi, target_cdi, distractor_cdi)
	
	return(gaze_oxf)
}

