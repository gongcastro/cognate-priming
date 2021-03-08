# Eye-tracking utils


# replace column names ---------------------------------------------------------
rename_cols <- function(x){
	str_replace_all(
		x,
		c(
			"system_time_stamp" = "time",
			"l_1" = "l_x",
			"l_2" = "l_y",
			"r_1" = "r_x",
			"r_2" = "r_y",
			"l_user_coord_3" = "l_user_coord_z",
			"r_user_coord_3" = "r_user_coord_z"
		)
	)
}


# evaluate sample validity -----------------------------------------------------
is_valid <- function(){}

# import Barcelona data --------------------------------------------------------
import_gaze <- function(location = c("bcn", "oxf"), participants) {
	
	oxf <- NULL
	bcn <- NULL
	
	relevant.variables <- c(
		"participant", "trial_num", "trial", "phase", "time",
		"l_x", "l_y", "l_v", "r_x", "r_y", "r_v",
		"l_user_coord_z",
		"r_user_coord_z"
	)
	screen_resolution <- c(x = 1920, y = 1080)
	
	if ("bcn" %in% location) {
		# import Barcelona data
		bcn <- map(
			here("Data", pull(drop_na(participants, filename), filename)),
			function(x) {
				fread(x, sep = ",", na.strings = c("", "NaN", "NA")) %>% 
					clean_names() %>% 
					rename_all(~rename_cols(.)) %>% 
					as_tibble() %>% 
					filter(phase %in% "Target-Distractor") %>% 
					mutate(
						participant = paste0("cognatepriming", participant),
						time = row_number()*(1000/120)
					) %>% 
					mutate_all(function(x) ifelse(is.nan(x), NA_real_, x)) %>% 
					mutate_at(vars(matches("l_|r_")), as.numeric) %>% 
					mutate_at(vars(contains("_v")), function(x) as.logical(as.integer(x))) %>% 
					select(one_of(relevant.variables)) 
			}
		) %>% 
			set_names(pull(drop_na(participants, filename), filename)) %>% 
			bind_rows(.id = "filename") %>% 
			left_join(participants) %>%  
			left_join(trials) %>% 
			rename(
				d_l = l_user_coord_z,
				d_r = r_user_coord_z
			) %>% 
			group_by(participant, age_group, trial) %>%
			mutate(
				time = as.double(time-min(time, na.rm = TRUE))/1000,
				time_bin = cut_interval(time, length = 0.1, labels = FALSE)
			) %>% 
			ungroup() %>% 
			filter(between(time, 0, 2)) %>% 
			mutate(
				l_v = l_v &
					!is.na(l_x) &
					!is.na(l_y) &
					between(l_x, 0, 1) &
					between(l_y, 0, 1),
				r_v = r_v &
					!is.na(l_x) &
					!is.na(r_y) &
					between(r_x, 0, 1) &
					between(r_y, 0, 1),
				v = l_v & r_v,
				
				l_x = ifelse(l_v, l_x, NA),
				l_y = ifelse(l_v, l_y, NA),
				r_x = ifelse(r_v, r_x, NA),
				r_y = ifelse(r_v, r_y, NA),
				
				x = ifelse(l_v, l_x, r_x)*screen_resolution["x"],
				y = ifelse(l_v, l_y, r_y)*screen_resolution["y"],
				
				d = ifelse(l_v, d_l, d_r),
			) %>% 
			relocate(time_bin, .before = time) %>% 
			arrange(participant, age_group, trial, time_bin) %>% 
			select(participant, id_db, age_group, test_language, trial_num, time_bin, time, target_location, x, y, d, v, lp, trial_type)
	}
	
	if ("oxf" %in% location) {
		# import Oxford raw data -------------------------------------------------------
		oxf <- map(
			list.files(here("Data"), full.names = TRUE, pattern = "oxford"),
			function(x) {
				fread(x, na.strings = c("", "NA")) %>% 
					as_tibble() %>% 
					clean_names() 
			}) %>% 
			bind_rows() %>% 
			select(
				id_db = id,
				trial_num = trial,
				time = timestamp,
				l_x = gaze_raw_left_x,
				l_y = gaze_raw_left_y,
				r_x = gaze_raw_right_x,
				r_y = gaze_raw_right_y,
				l_v = gaze_left_validity,
				r_v = gaze_right_validity,
				target_location = vis_target_stm_pos,
				prime_unrelated = vis_un_stm,
				prime_cognate = vis_cp_stm,
				prime_noncognate = vis_np_stm
			) %>%
			mutate_at(vars(matches("_v")), as.logical) %>% 
			mutate(
				trial_type = case_when(
					!is.na(prime_unrelated) ~ "Unrelated",
					!is.na(prime_cognate) ~ "Cognate",
					!is.na(prime_noncognate) ~ "Non-cognate"
				),
				d_l = 650,
				d_r = 650,
				phase = case_when(
					between(time, 0, 2500) ~ "Getter",
					between(time, 2500, 4000) ~ "Prime",
					between(time, 4000, 4050) ~ "Blank",
					between(time, 4050, 4750) ~ "Audio",
					between(time, 4750, 6750) ~ "Target-Distractor"
				),
				id_db = as.character(id_db),
				target_location = ifelse(target_location==2, "l", "r")
			) %>% 
			filter(phase %in% "Target-Distractor") %>% 
			left_join(participants) %>% 
			group_by(participant, age_group, trial_num) %>%
			mutate(
				time = as.double(time-min(time, na.rm = TRUE))/1000,
				time_bin = cut_interval(time, length = 0.1, labels = FALSE)
			) %>% 
			ungroup() %>% 
			mutate(
				l_v = l_v &
					!is.na(l_x) &
					!is.na(l_y) &
					between(l_x, 0, screen_resolution["x"]) &
					between(l_y, 0, screen_resolution["y"]),
				r_v = r_v &
					!is.na(l_x) &
					!is.na(r_y) &
					between(r_x, 0, screen_resolution["x"]) &
					between(r_y, 0, screen_resolution["y"]),
				
				v = l_v & r_v,
				
				l_x = ifelse(l_v, l_x, NA),
				l_y = ifelse(l_v, l_y, NA),
				r_x = ifelse(r_v, r_x, NA),
				r_y = ifelse(r_v, r_y, NA),
				
				x = ifelse(l_v, l_x, r_x),
				y = ifelse(l_v, l_y, r_y),
				
				d = ifelse(l_v, d_l, d_r)
			) %>%
			mutate(
				l_x = ifelse(between(l_x, 0, screen_resolution["x"]), l_x, NA),
				r_x = ifelse(between(r_x, 0, screen_resolution["x"]), r_x, NA),
				l_y = ifelse(between(l_y, 0, screen_resolution["y"]), l_y, NA),
				r_y = ifelse(between(r_y, 0, screen_resolution["y"]), r_y, NA),
				r_v = !is.na(r_x) & !is.na(r_y),
				l_v = !is.na(l_x) & !is.na(l_y),
				x = ifelse(!l_v, r_x, l_x),
				y = ifelse(!l_v, r_y, l_y),
				v = any(l_v, r_v)
			) %>% 
			ungroup() %>%  
			select(participant, id_db, age_group, test_language, trial_num, time_bin, time, target_location, x, y, d, v, lp, trial_type) %>% 
			drop_na(age_group)
	}
	x <- list("Barcelona" = bcn, "Oxford" = oxf) %>%
		bind_rows(.id = "location")
	return(x)
}


# evaluate of gaze is in AOI ---------------------------------------------------
gaze_in_aoi <- function(x, y, coords = c(710, 1210, 290, 790)) {
	(x >= coords[1] & x <= coords[2]) & (y >= coords[3] & y <= coords[4])
}


# summarise gaze by trial ------------------------------------------------------
gaze_summarise_by_trial <- function(
	sample = NULL,
	processed = NULL,
	google_email = NULL
){
	# if not provided, generate sample and processed gaze data
	if (is.null(processed)){
		if (is.null(sample)) {
			if (is.null(google_email)){
				google_email <- readline(prompt = "Google email: ")
				sample <- sample_get(google_email)
			} else {
				sample <- sample_get(google_email) %>%
					select(participant_id, id_db, location, age_group, lp, test_language, list, version)
			}
		}
		processed <- gaze_process(sample)
	}
	
	# summarise data
	gaze_summary_trial <- processed %>%
		group_by(participant, age_group, trial, lp, trial_type) %>%
		summarise(
			target = sum(target, na.rm = TRUE),
			distractor = sum(distractor, na.rm = TRUE),
			n = n(),
			.groups = "drop"
		) %>%
		rowwise() %>%
		mutate(
			p_target = prod(target, 1/sum(target, distractor, na.rm = TRUE), na.rm = TRUE),
			p_distractor = prod(distractor, 1/sum(target, distractor, na.rm = TRUE), na.rm = TRUE)
		) %>%
		ungroup()
	return(gaze_summary_trial)
}



