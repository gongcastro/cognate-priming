# process Oxford gaze data

get_gaze_oxf <- function(
	file_paths, # gaze data file paths
	participants, # participants dataset, get_participants output
	stimuli, # stimuli dataset, get_stimuli output
	screen_resolution = c(x = 1920, y = 1080),
	center_coords = c(xmin = 710, xmax = 1210, ymin = 290, ymax = 790),
	left_coords = c(xmin = 180, xmax = 680, ymin = 290, ymax = 790),
	right_coords = c(xmin = 1240, xmax = 1640, ymin = 290, ymax = 790)
){
	
	suppressWarnings({
		# import gaze data ----
		raw <- map(file_paths, fread, na = c("", "NA")) %>% 
			bind_rows() %>% 
			as_tibble() %>%
			clean_names() %>% # colnames to snake case
			mutate(trial = ifelse(block==2, trial+16, trial)) %>% # because of block format, trial number for 17th trial is recorded as block 2 trial 1
			rename(
				id_db = id, trial_num = trial, time = timestamp,
				l_x = gaze_raw_left_x, l_y = gaze_raw_left_y,
				r_x = gaze_raw_right_x, r_y = gaze_raw_right_y,
				l_v = gaze_left_validity, r_v = gaze_right_validity,
				target_location = vis_target_stm_pos,
				prime_unrelated = vis_un_stm, prime_cognate = vis_cp_stm, prime_noncognate = vis_np_stm,
				target = vis_target_stm, distractor = vis_distr_stm
			) %>%
			mutate_at(vars(matches("_v")), as.logical) %>%
			mutate(
				id_db = as.character(id_db),
				trial_type = case_when(
					!is.na(prime_unrelated) ~ "Unrelated",
					!is.na(prime_cognate) ~ "Cognate",
					!is.na(prime_noncognate) ~ "Non-cognate"
				),
				phase = case_when(
					between(time, 0, 2500) ~ "Getter", # attention getter lasts shorter than in Barcelona (3s)
					between(time, 2500, 4000) ~ "Prime",
					between(time, 4000, 4050) ~ "Blank",
					between(time, 4050, 4750) ~ "Audio",
					between(time, 4750, 6750) ~ "Target-Distractor"
				),
				target_location = ifelse(target_location==2, "L", "R"),
				prime = coalesce(prime_unrelated, prime_cognate, prime_noncognate), # get prime picture
				date_test = as_date(NA) # dat test is missing in current file
			) %>%
			filter(phase %in% c("Target-Distractor", "Prime")) %>% 
			left_join(participants) %>% # add participant information
			# restart time at every trial onset for eah participant in each testing session
			group_by(participant, age_group, trial_num, phase) %>%
			mutate(time = as.double(time-min(time, na.rm = TRUE))/1000) %>%
			ungroup() %>%
			# check gaze validity and take left gaze
			# get left eye
			# gaze is valid if flagged as such by eye-tracker, is not NA, and coords are within screen resolution
			mutate(
				# negative values are invalid
				l_x = ifelse(l_x < 0, NA, l_x),
				l_y = ifelse(l_y < 0, NA, l_y),
				r_x = ifelse(r_x < 0, NA, r_x),
				r_y = ifelse(r_y < 0, NA, r_y),
				l_v = l_v & !is.na(l_x) & !is.na(l_y) & between(l_x, 0, screen_resolution["x"]) & between(l_y, 0, screen_resolution["y"]),
				r_v = r_v & !is.na(l_x) & !is.na(r_y) & between(r_x, 0, screen_resolution["x"]) & between(r_y, 0, screen_resolution["y"]),
				# get left eye
				x = l_x,
				y = l_y,
				valid_sample = l_v # overall validity
			) %>% 
			mutate(
				aoi_center = between(x, center_coords["xmin"], center_coords["xmax"]) & between(y, center_coords["ymin"], center_coords["ymax"]),
				aoi_right = between(x, right_coords["xmin"], right_coords["xmax"]) & between(y, right_coords["ymin"], right_coords["ymax"]),
				aoi_left = between(x, left_coords["xmin"], left_coords["xmax"]) & between(y, left_coords["ymin"], left_coords["ymax"]),
				aoi_prime = aoi_center,
				aoi_target = ifelse(target_location=="L", aoi_left, aoi_right),
				aoi_distractor = ifelse(target_location=="L", aoi_right, aoi_left)
			) %>% 
			rename(trial = trial_num) %>% 
			select(
				participant, date_test, age_group, lp, trial, test_language, phase, time,
				x, y, target_location, aoi_prime, aoi_target, aoi_distractor, valid_sample, lp,
				trial_type, prime, target, distractor
			)
	
		# merge data ----
		processed <- reduce(list(raw, participants), left_join) %>%
			select(
				participant, date_test, age_group, lp, trial, trial_type, test_language, phase,
				time, x, y, valid_sample, target_location, aoi_prime, aoi_target, aoi_distractor,
				prime, target, distractor
			)
		
		return(processed)
		
	})
}

