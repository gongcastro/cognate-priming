get_gaze_raw <- function(gaze_files){
	
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
	cli::cli_progress_bar("Processing", total = n_files, format = pb_text)
	
	for (i in 1:n_files){
		db[[i]] <- arrow::read_csv_arrow(gaze_files[i], na = na_strings) %>%
			# rename all variables to snake case
			janitor::clean_names() %>% 
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
			) %>%
			dplyr::filter(phase %in% c("Target-Distractor", "Prime")) %>%
			add_missing_cols(keep_dict) |> 
			select(any_of(keep_dict))
		
		cli::cli_progress_update()
	}
	cli::cli_progress_done(result = "done")
	
	cli::cli_progress_step("Binding {n_files} datasets...", spinner = FALSE)
	gaze_raw  <- bind_rows(db, .id = "filename")
	cli::cli_progress_done(result = "done")
	
	return(gaze_raw)
}

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


fix_sampling_rate <- function(x, filename, date_onset) {
	
	sp60_files <- unique(x$filename)
	regex_pattern <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"
	sp60_dates <- stringr::str_extract(sp60_files, pattern = regex_pattern)
	sp60_dates <- as.Date(sp60_dates, format = "%Y-%m-%d")
	sp60_files <- sp60_files[which(sp60_dates >= as.Date(date_onset))]
	
	x$sampling_rate <- ifelse(x$filename %in% sp60_files, 60, 120)
	
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

# process Barcelona gaze data
get_gaze_processed <- function(
		gaze_raw
){
	
	screen_resolution <- c(x = 1920, y = 1080) # screen size in pixels
	
	# import gaze data ----
	processed <- gaze_raw |> 
		fix_sampling_rate(filename, date_onset = "2022-05-26") |> 
		add_count(filename, name = "n_samples") |> 
		# restart timestamps at each trial phase change, and express in seconds
		mutate(timestamp = ((1:n())-1) / sampling_rate,
			   timestamp = timestamp - min(timestamp),
			   .by = c(filename, trial, phase)) |> 
		# trim timestamps outside the 2 seconds range
		filter(between(trial, 1, 32),
			   !(phase=="Prime" & timestamp > 1.5), 
			   !(phase=="Target-Distractor" & timestamp > 2.0)) |> 
		mutate(
			# change gaze coordinates from 0-1 scale to screen resolution scale [1920x1080]
			x = x*screen_resolution["x"],
			y = y*screen_resolution["y"]
		) %>% 
		select(filename, trial, phase, timestamp, x, y, is_valid_gaze, filename) %>% 
		arrange(filename, trial, phase, timestamp)
	
	
	# merge data ----
	expanded <- expand(processed, filename, trial, phase, timestamp)
	
	gaze_processed <- processed %>% 
		right_join(expanded,
				   by = join_by(trial, phase, timestamp, filename)) %>% 
		select(filename, trial, phase, timestamp, x, y, is_valid_gaze) %>% 
		mutate(is_valid_gaze = ifelse(is.na(is_valid_gaze), FALSE, is_valid_gaze)) %>% 
		dplyr::filter(
			!(phase=="Prime" & timestamp > 1.5),
			!(phase=="Target" & timestamp > 2.0)
		) |> 
		mutate(x_imputed = zoo::na.locf(x, maxgap = 20, na.rm = FALSE),
			   y_imputed = zoo::na.locf(y, maxgap = 20, na.rm = FALSE),
			   is_imputed = is.na(x) & !is.na(x_imputed),
			   x = x_imputed,
			   y = y_imputed,
			   .by = c(filename, trial, phase)) %>% 
		select(-c(x_imputed, y_imputed)) %>% 
		relocate(is_imputed, .after = is_valid_gaze)
	
	return(gaze_processed)
}

# evaluate if gaze is in AOI ---------------------------------------------------

get_gaze_aoi <- function(gaze_processed,
						 participants,
						 stimuli,
						 aoi_coords,
						 non_aoi_as_na = FALSE) {
	
	participants_tmp <- select(participants, filename,
							   test_language, list, version)
	
	stimuli_tmp <- select(stimuli, test_language, list, version, trial,
						  target_location, trial_type)
	
	gaze_aoi <- gaze_processed |> 
		mutate(filename = gsub(".csv.csv", ".csv", filename)) |> 
		left_join(participants_tmp,
				  by = join_by(filename)) |> 
		left_join(stimuli_tmp,
				  by = join_by(trial, test_language, list, version)) |> 
		select(filename, trial, phase, timestamp,
			   x, y, is_valid_gaze, is_imputed,
			   target_location, trial_type) |> 
		make_non_aoi_as_false(non_aoi_as_na = non_aoi_as_na) |> 
		mutate(
			is_gaze_prime = gaze_in_prime(x, y, aoi_coords = aoi_coords),
			is_gaze_target = gaze_in_target(x, y, target_location, aoi_coords),
			is_gaze_distractor = gaze_in_distractor(x, y, target_location, aoi_coords)
		) |> 
		select(filename, trial, phase, timestamp, x, y,
			   is_gaze_prime, is_gaze_target, is_gaze_distractor,
			   is_valid_gaze, is_imputed, trial_type)
	
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
gaze_in_prime <- function(x, y, aoi_coords){
	
	is_gaze_in_aoi <- between(
		x, 
		aoi_coords$center["xmin"], 
		aoi_coords$center["xmax"]
	) & 
		between(
			y, 
			aoi_coords$center["ymin"], 
			aoi_coords$center["ymax"]
		)
	
	is_gaze_in_aoi <- ifelse(is.na(is_gaze_in_aoi), FALSE, TRUE)
	
	return(is_gaze_in_aoi)
}

# evaluate if gaze is in target
gaze_in_target <- function(x, y, target_location, aoi_coords){
	
	is_gaze_in_aoi <- 
		case_when(
			target_location=="r" ~ 
				between(
					x,
					aoi_coords$right["xmin"],
					aoi_coords$right["xmax"]
				) &
				between(
					y,
					aoi_coords$right["ymin"],
					aoi_coords$right["ymax"]
				),
			target_location=="l" ~ 
				between(
					x,
					aoi_coords$left["xmin"],
					aoi_coords$left["xmax"]
				) &
				between(
					y,
					aoi_coords$left["ymin"],
					aoi_coords$left["ymax"]
				),
			TRUE ~ FALSE
		)
	
	return(is_gaze_in_aoi)
}


# evaluate if gaze is in distractor
gaze_in_distractor <- function(x, y, target_location, aoi_coords){
	
	is_gaze_in_aoi <- 
		case_when(
			target_location=="l" ~
				between(
					x,
					aoi_coords$right["xmin"],
					aoi_coords$right["xmax"]
				) &
				between(
					y,
					aoi_coords$right["ymin"],
					aoi_coords$right["ymax"]
				),
			target_location=="r" ~
				between(
					x,
					aoi_coords$left["xmin"],
					aoi_coords$left["xmax"]
				) &
				between(
					y,
					aoi_coords$left["ymin"],
					aoi_coords$left["ymax"]
				),
			TRUE ~ FALSE
		)
	
	return(is_gaze_in_aoi)
}

