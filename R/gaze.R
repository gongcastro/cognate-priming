# normalise raw eye-tracking files ---------------------------------------------

get_gaze_normalised <- function(gaze_files) {
	
	# get file names and paths
	file_names <- gsub(".csv$", "", basename(gaze_files))
	
	# prepare for loop and pre-allocate list
	i <- 1
	n_files <- length(file_names)
	db <- vector(mode = "list", length = n_files)
	names(db) <- file_names
	
	# name changes (see R/utils.R)
	cols_dict <- get_name_dictionary()$col_name_changes
	phase_dict <- get_name_dictionary()$phase_name_changes
	keep_dict <- get_name_dictionary()$relevant_variables
	
	# prepare progress bar
	pb_text <- "{pb_spin} Reading {pb_current}/{pb_total} | {col_blue(pb_percent)}"
	cli_progress_bar("Processing", total = n_files, format = pb_text)
	
	# read CSV and write Feather
	for (i in 1:n_files){
		
		na_strings <- c("", "NaN", "NA", "<NA>")
		db[[i]] <- x <- arrow::read_csv_arrow(gaze_files[i],
											  na = na_strings) |> 
			# rename all variables to snake case
			janitor::clean_names() |>  
			rename(any_of(cols_dict)) |> 
			fix_timestamps() |> 
			fix_validity() |> 
			mutate(
				trial = as.integer(trial),
				phase = str_replace_all(phase, phase_dict),
				across(c(l_v, r_v), \(x) as.logical(as.integer(x))),
				x = ifelse(is.na(l_x) | !l_v, r_x, l_x), 
				y = ifelse(is.na(l_y) | !l_v, r_y, l_y),
				across(c(x, y), \(x) ifelse(!between(x, 0, 1), NA_real_, x)),
				is_valid_gaze = as.logical((!(is.na(x) | is.na(y)) & (l_v | r_v)))
			) |> 
			add_missing_cols(keep_dict) |> 
			mutate(trial_id = as.integer(trial_id)) |> 
			select(any_of(keep_dict), -timestamp)
		
		cli_progress_update()
	}
	
	cli_progress_done(result = "done")
	
	cli_progress_step("Binding {n_files} datasets", spinner = FALSE)
	gaze_normalised <- bind_rows(db, .id = "filename")
	
	return(gaze_normalised)
}

# helper functions

# get name dictionary 
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
		is_valid_gaze = "valid_sample",
		id = "sujenum",
		trial_num = "numtrial_lista",
		trial_id = "trial_num"
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
	
	relevant_variables <- c(
		"id", "trial", "trial_id",
		"phase", "x", "y", "is_valid_gaze"
	)
	
	name_dict <- list(
		col_name_changes = col_name_changes, 
		phase_name_changes = phase_name_changes, 
		relevant_variables = relevant_variables
	)
	
	return(name_dict)
}

# add missing columns, if any
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


# import eye-tracking data -----------------------------------------------------

get_gaze_raw <- function(gaze_normalised) {
	
	# name changes (see R/utils.R)
	names_dict <- get_name_dictionary()$col_name_changes 
	
	gaze_raw <- gaze_normalised |>
		fix_sampling_rate(filename, date_onset = "2022-05-26") |> 
		add_count(filename, name = "n_samples") |> 
		mutate(timestamp = ((1:n())-1) / sampling_rate,
			   timestamp = timestamp - min(timestamp),
			   .by = c(filename, trial, phase)) |> 
		filter(between(trial_id, 1, 32),
			   phase %in% c("Target-Distractor", "Prime"), # get prime and target phase
			   !(phase=="Prime" & timestamp > 1.5), 
			   !(phase=="Target-Distractor" & timestamp > 2.0)) |> 
		select(id, trial, trial_id, phase, timestamp,
			   x, y, is_valid_gaze, filename)
	
	return(gaze_raw)
}

# helper functions

fix_sampling_rate <- function(x, filename, date_onset) {
	
	sp60_files <- unique(x$filename)
	regex_pattern <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"
	sp60_dates <- stringr::str_extract(sp60_files, pattern = regex_pattern)
	sp60_dates <- as.Date(sp60_dates, format = "%Y-%m-%d")
	sp60_files <- sp60_files[which(sp60_dates >= as.Date(date_onset))]
	
	x$sampling_rate <- ifelse(x$filename %in% sp60_files, 60, 120)
	
	return(x)
}

# process gaze data ------------------------------------------------------------

get_gaze_processed <- function(gaze_raw,
							   participants,
							   screen_resolution = c(x = 1920, y = 1080),
							   ...) {
	
	participants_tmp <- select(participants, id, id_db, age_group, filename)
	
	gaze_processed <- gaze_raw  |>
		mutate(filename = paste0(filename, ".csv")) |>
		right_join(participants_tmp,
				   by = join_by(id, filename)) |> 
		mutate(x = x * screen_resolution["x"], # change gaze coordinates from 0-1 scale to screen resolution scale
			   y = y * screen_resolution["y"],
			   filename = paste0(filename, ".csv"),
			   age_group = as.character(age_group),
			   is_valid_gaze = ifelse(is.na(is_valid_gaze),
			   					   FALSE,
			   					   is_valid_gaze)) |>
		# impute eye-tracker samples
		mutate(across(c(x, y),
					  \(x) zoo::na.locf(x, maxgap = 120/3, na.rm = FALSE),
					  .names = "{.col}_imputed"),
			   .by = c(filename, trial, phase)) |> 
		mutate(is_imputed = is.na(x) & !is.na(x_imputed),
			   x = x_imputed, 
			   y = y_imputed) |> 
		relocate(x, y, is_imputed, .after = is_valid_gaze) |> 
		arrange(id, age_group, trial, phase, timestamp) |>
		select(id, age_group, trial, trial_id, phase, timestamp, 
			   x, y, is_valid_gaze, is_imputed, filename)
	
	return(gaze_processed)
}


# evaluate if gaze is in AOI ---------------------------------------------------

get_gaze_aoi <- function(gaze_processed,
						 participants,
						 stimuli,
						 aoi_coords,
						 non_aoi_as_na = FALSE) {
	
	participants_tmp <- select(participants, id, age_group, filename,
							   test_language, list, version)
	
	stimuli_tmp <- select(stimuli, test_language, list, version, trial_id,
						  target_location, trial_type)
	
	gaze_aoi <- gaze_processed |> 
		mutate(filename = gsub(".csv.csv", ".csv", filename)) |> 
		left_join(participants_tmp,
				  by = join_by(id, age_group, filename)) |> 
		left_join(stimuli_tmp,
				  by = join_by(trial_id, test_language, list, version)) |> 
		select(id, age_group, trial, trial_id, phase, timestamp,
			   x, y, is_valid_gaze, is_imputed,
			   target_location, trial_type, filename) |> 
		# evaluate if gaze coordinates are inside any AOI, and which
		mutate(is_gaze_center = gaze_in_center(x, y, aoi_coords = aoi_coords),
			   is_gaze_left = gaze_in_left(x, y, aoi_coords),
			   is_gaze_right = gaze_in_right(x, y, aoi_coords)) |> 
		make_non_aoi_as_false(non_aoi_as_na = TRUE) |> 
		mutate(
			is_gaze_prime = is_gaze_center,
			is_gaze_target = ifelse(target_location=="r",
									is_gaze_right, 
									is_gaze_left),
			is_gaze_distractor = ifelse(target_location=="l", 
										is_gaze_right,
										is_gaze_left)
		) |> 
		select(id, age_group,
			   trial, trial_id,
			   phase, timestamp, x, y,
			   is_gaze_prime,
			   is_gaze_target,
			   is_gaze_distractor,
			   is_valid_gaze,
			   is_imputed,
			   trial_type, filename)
	
	return(gaze_aoi)
	
}

# helper functions

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
gaze_in_center <- function(x, y, aoi_coords){
	
	x_in_range <- (x >= aoi_coords$center["xmin"] & x <= aoi_coords$center["xmax"]) 
	y_in_range <- (y >= aoi_coords$center["ymin"] & y <= aoi_coords$center["ymax"])
	gaze_in_range <- rowSums(data.frame(x_in_range, y_in_range))==2
	
	return(gaze_in_range)
}

# evaluate if gaze is in target
gaze_in_right <- function(x, y, aoi_coords){
	
	x_in_range <- (x >= aoi_coords$right["xmin"] & x <= aoi_coords$right["xmax"]) 
	y_in_range <- (y >= aoi_coords$right["ymin"] & y <= aoi_coords$right["ymax"])
	gaze_in_range <- rowSums(data.frame(x_in_range, y_in_range))==2
	
	return(gaze_in_range)
}

gaze_in_left <- function(x, y, aoi_coords){
	
	x_in_range <- (x >= aoi_coords$left["xmin"] & x <= aoi_coords$left["xmax"]) 
	y_in_range <- (y >= aoi_coords$left["ymin"] & y <= aoi_coords$left["ymax"])
	gaze_in_range <- rowSums(data.frame(x_in_range, y_in_range))==2
	
	return(gaze_in_range)
}
