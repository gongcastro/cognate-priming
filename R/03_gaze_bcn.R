# process Barcelona gaze data
get_gaze_bcn <- function(
	file_paths, # gaze data file paths
	participants, # participants dataset, get_participants output
	stimuli, # stimuli dataset, get_stimuli output
	screen_resolution = c(x = 1920, y = 1080), # screen size in pixels
	center_coords = c(xmin = 710, xmax = 1210, ymin = 290, ymax = 790), # coordinates of centre picture
	left_coords = c(xmin = 180, xmax = 680, ymin = 290, ymax = 790), # coordinates of left picture
	right_coords = c(xmin = 1240, xmax = 1640, ymin = 290, ymax = 790) # coordinates of right picture
){
	suppressMessages({
		# trial data ----
		stimuli <- stimuli %>%
			filter(location=="Barcelona") %>% 
			select(-c("prime2", "target2", "prime2_cdi", "target2_cdi", "valid_trial"))
		
		# import gaze data ----
		raw <- map(file_paths, get_gaze_raw) %>% 
			# merge all data sets assigning them their file name
			set_names(list.files(here("Data", "Gaze", "Barcelona")))  %>% 
			bind_rows(.id = "filename") %>% 
			as_tibble() %>%
			# restart timestamps at each trial phase change, and express in seconds
			group_by(participant, trial, phase, filename) %>%
			mutate(time = as.double(time-min(time, na.rm = TRUE))/1000) %>%
			ungroup() %>%
			# trim timestamps outside the 2 seconds range
			filter(
				between(time, 0, 2),
				participant %in% participants$participant # get only valid participants
			) %>%
			mutate(
				# more detailed evaluation of sample validity
				valid_sample = l_v & !is.na(l_x) & !is.na(l_y) & between(l_x, 0, 1) & between(l_y, 0, 1),
				# if sample is not valid, change value to NA
				x = ifelse(valid_sample, l_x, NA),
				y = ifelse(valid_sample, l_y, NA),
				# change gaze coodinates from 0-1 scale to screen resolution scale [1920x1080]
				x = x*screen_resolution["x"],
				y = y*screen_resolution["y"]
			) %>% 
			# add paticipant and trial info
			left_join(participants) %>% 
			left_join(stimuli) %>% 
			# evaluate if gaze coordinates are inside any AOI, and which
			mutate(
				aoi_center = between(x, center_coords["xmin"], center_coords["xmax"]) & between(y, center_coords["ymin"], center_coords["ymax"]),
				aoi_right = between(x, right_coords["xmin"], right_coords["xmax"]) & between(y, right_coords["ymin"], right_coords["ymax"]),
				aoi_left = between(x, left_coords["xmin"], left_coords["xmax"]) & between(y, left_coords["ymin"], left_coords["ymax"]),
				aoi_prime = aoi_center,
				aoi_target = ifelse(target_location=="L", aoi_left, aoi_right),
				aoi_distractor = ifelse(target_location=="L", aoi_right, aoi_left)
			) %>% 
			arrange(participant, trial, trial_num, phase) %>% 
			select(
				participant, date_test, lp, age_group, trial_num, trial, test_language, phase,
				time, x, y, trial_type, target_location, aoi_prime, aoi_target, aoi_distractor,
				valid_sample, prime_cdi, target_cdi, distractor_cdi
			) %>% 
			rename_all(function(x) str_remove(x, "_cdi"))
		
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
