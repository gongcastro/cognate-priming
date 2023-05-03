# pass eye-tracking files to Arrow
get_gaze_normalised <- function() {
	
	# set directories
	path <- "data/gaze/00_raw"
	new_path <- "data/gaze/01_normalised"
	unlink(list.files(new_path, full.names = TRUE), recursive = TRUE)
	
	# validate paths
	stopifnot(dir.exists(path))
	stopifnot(dir.exists(new_path))
	
	# get file names and paths
	file_paths <- list.files(path, full.names = TRUE)
	file_names <- gsub(".csv", "", list.files(path))
	
	# prepare for loop and pre-allocate list
	i <- 1
	n_files <- length(file_paths)
	gaze_normalised <- vector(mode = "list", length = n_files)
	names(gaze_normalised) <- file_names
	
	names_dict <- get_name_dictionary() # name changes (see R/utils.R)
	
	# prepare progress bar
	pb_text <- "{pb_spin} Reading file {pb_current}/{pb_total} [{pb_percent}]"
	cli_progress_bar("Processing", total = n_files, format = pb_text)
	
	# read CSV and write Feather
	for (i in 1:n_files){
		
		gaze_normalised[[i]] <- read_csv_arrow(file_paths[i], na = c("", "NaN", "NA", "<NA>")) %>% 
			clean_names() %>% # rename all variables to snake case
			rename_with(~str_replace_all(., names_dict$col_name_changes), everything()) %>% # fix some values from outdates files
			mutate(phase =  str_replace_all(phase, names_dict$phase_name_changes),
				   across(c(l_x, l_y, r_x, r_y), as.double),
				   across(c(l_v, r_v), ~as.logical(as.integer(.))),
				   x = ifelse(is.na(l_x) | !l_v, r_x, l_x) %>% 
				   	ifelse(!between(., 0, 1), NA_real_, .),
				   y = ifelse(is.na(l_y) | !l_v, r_y, l_y) %>% 
				   	ifelse(!between(., 0, 1), NA_real_, .),
				   valid_sample = (!is.na(x) & !is.na(y) & (l_v | r_v))) %>% 
			select(one_of(names_dict$relevant_variables), -time)
		
		cli_progress_update()
	}
	
	cli_progress_done(result = "done")
	db <- bind_rows(gaze_normalised, .id = "filename")
	write_dataset(db, new_path, partitioning = "filename")
}

# pass eye-tracking files to Arrow
get_gaze_joint <- function() {
	
	# set and validate dirs
	path <- "data/gaze/01_normalised"
	new_path <- "data/gaze/02_joint"
	stopifnot(dir.exists(path))
	stopifnot(dir.exists(new_path))
	
	file.remove(list.files(new_path, full.names = TRUE))
	
	names_dict <- get_name_dictionary() # name changes (see R/utils.R)
	
	gaze_joint <- open_dataset(path) %>% # read Arrow dataset
		filter(between(trial_id, 1, 32),
			   phase %in% c("Target-Distractor", "Prime")) %>%  # get prime and target phase
		rename_with(~str_replace_all(., names_dict[["col_name_changes"]]), everything()) %>% # fix some values from outdates files
		group_by(filename) %>% 
		mutate(id = paste0("cognatepriming", id)) %>% 
		collect() %>% 
		mutate(row_n = row_number(),
			   sampling_rate = ifelse(max(row_n) < 8e3, 60, 120),
			   time = row_n*(1/sampling_rate)) %>%  # reconstruct time in seconds
		ungroup() %>% 
		group_by(filename, trial, phase) %>% 
		mutate(time = time-min(time)) %>% 
		ungroup() %>% 
		filter(!(phase=="Prime" & time > 1.5), 
			   !(phase=="Target-Distractor" & time > 2.0)) %>% 
		select(id, trial, trial_id, phase, time, x, y, valid_sample, filename)
	# mutate(across(everything(), ~ifelse(is.nan(.), NA_real_, .))) %>%  # restart timestamps at each trial phase change
	
	write_dataset(gaze_joint, path = new_path, partitioning = "filename") # export Arrow database
	return(gaze_joint)
}

# process Barcelona gaze data
get_gaze_processed <- function(participants, # participants dataset, get_participants output
							   stimuli, # stimuli dataset, get_stimuli output
							   aoi_coords,
							   screen_resolution = c(x = 1920, y = 1080)) {
	suppressMessages({
		
		participants_tmp <- select(participants, id, age_group, filename)
		
		# set and validate dirs
		path <- "data/gaze/02_joint"
		new_path <- "data/gaze/03_processed" # get path
		stopifnot(dir.exists(path)) # validate path
		stopifnot(dir.exists(new_path)) # validate path
		unlink(list.files(new_path, full.names = TRUE), recursive = TRUE)
		
		gaze_processed <- open_dataset(path) %>% 
			filter(id %in% participants$id) %>% # trim timestamps outside the 2 seconds range
			mutate(x = x*screen_resolution["x"], # change gaze coordinates from 0-1 scale to screen resolution scale
				   y = y*screen_resolution["y"],
				   filename = paste0(filename, ".csv")) %>% 
			left_join(participants_tmp) %>% 
			mutate(age_group = as.character(age_group)) %>% 
			select(id, age_group, trial, trial_id, phase, time, x, y, valid_sample, filename) %>%
			arrange(id, age_group, trial, phase, time) %>% 
			select(id, age_group, trial, trial_id, phase, time, x, y, valid_sample, filename) %>% 
			mutate(valid_sample = ifelse(is.na(valid_sample), FALSE, valid_sample)) %>% 
			collect()
		
		write_dataset(gaze_processed, new_path, partitioning = "filename")
		
	})
	return(gaze_processed)
}


get_gaze_imputed <- function(...){
	
	# set and validate dirs
	path <- "data/gaze/03_processed" # get path
	new_path <- "data/gaze/04_imputed" # get path
	stopifnot(dir.exists(path)) # validate path
	stopifnot(dir.exists(new_path)) # validate path
	unlink(list.files(new_path, full.names = TRUE), recursive = TRUE)
	
	# impute gaze NAs
	gaze_imputed <- open_dataset(path) %>% 
		group_by(filename, trial, phase) %>% 
		collect() %>% 
		mutate(across(c(x, y), ~na.locf(., na.rm = FALSE, ...), .names = "{.col}_imputed"),
			   is_imputed = is.na(x) & !is.na(x_imputed)) %>% 
		ungroup() %>% 
		select(-c(x_imputed, y_imputed)) %>% 
		relocate(is_imputed, .after = valid_sample)
	
	write_dataset(gaze_imputed, new_path, partitioning = "filename")
	
	return(gaze_imputed)
	
}

get_gaze_aoi <- function(participants,
						 stimuli,
						 aoi_coords,
						 ...) {
	
	# set and validate dirs
	path <- "data/gaze/04_imputed"
	new_path <- "data/gaze/05_aoi" 
	stopifnot(dir.exists(path)) 
	stopifnot(dir.exists(new_path))
	unlink(list.files(new_path, full.names = TRUE), recursive = TRUE)
	
	gaze_aoi <- open_dataset(path) %>% 
		left_join(select(participants, id, age_group, filename, test_language, list, version)) %>% 
		left_join(select(stimuli, test_language, list, version, trial_id, target_location, trial_type)) %>% 
		select(id, age_group, trial, trial_id, phase, time, x, y, valid_sample, target_location, trial_type, filename) %>% 
		collect() %>% 
		# evaluate if gaze coordinates are inside any AOI, and which
		mutate(aoi_center = gaze_in_center(x, y, aoi_coords = aoi_coords),
			   aoi_left = gaze_in_left(x, y, aoi_coords),
			   aoi_right = gaze_in_right(x, y, aoi_coords)) %>% 
		replace_na(list(aoi_center = FALSE, aoi_right = FALSE, aoi_left = FALSE)) %>% 
		mutate(aoi_prime = aoi_center,
			   aoi_target = ifelse(target_location=="r", aoi_right, aoi_left),
			   aoi_distractor = ifelse(target_location=="l", aoi_right, aoi_left))  %>% 
		select(id, age_group, trial, trial_id, phase, time, x, y, valid_sample, 
			   aoi_prime, aoi_target, aoi_distractor, trial_type, filename)
	
	write_dataset(gaze_aoi, new_path, partitioning = "filename")
	
	return(gaze_aoi)
	
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


make_plots_gaze_raw <- function(gaze_imputed, aoi_coords){
	
	files <- unique(gaze_imputed$filename[!is.na(gaze_imputed$filename)])
	n_files <- length(files)
	
	aois <- tibble(
		phase = rep(c("Prime", "Target", "Target"), each = 2),
		dim = rep(c("x", "y"), each = 1, times = 3),
		xmin = -Inf,
		xmax = -Inf,
		ymin = c(
			aoi_coords$center["xmin"], 
			aoi_coords$center["ymin"], 
			aoi_coords$right["xmin"], 
			aoi_coords$right["ymin"], 
			aoi_coords$left["xmin"], 
			aoi_coords$left["ymin"]
		),
		ymax = c(
			aoi_coords$center["xmax"],
			aoi_coords$center["ymax"],
			aoi_coords$right["xmax"],
			aoi_coords$right["ymax"],
			aoi_coords$left["xmax"],
			aoi_coords$left["ymax"]
		)
	)
	
	i <- 0
	cli_progress_bar("Plotting", total = n_files, format = "{pb_spin} Plotting {pb_current}/{pb_total} [{pb_percent}]")
	
	for (i in 1:n_files) {
		plot_data <- gaze_imputed %>% 
			filter(filename==files[i]) %>% 
			pivot_longer(c(x, y), names_to = "dim", values_to = "value") %>% 
			mutate(is_imputed = factor(is_imputed, 
									   levels = c(FALSE, TRUE),
									   labels = c("No", "Yes")),
				   phase = factor(phase,
				   			   levels = c("Prime", "Target-Distractor"), 
				   			   labels = c("Prime", "Target")))
		
		plot <- plot_data %>% 
			ggplot() +
			aes(x = time, y = value, colour = is_imputed) + 
			facet_wrap(trial+phase~dim, ncol = 4, labeller = label_wrap_gen(multi_line=FALSE)) +
			geom_vline(xintercept = 0.3, size = 0.25, colour = pal_d3()(3)[3]) +
			geom_rect(data = aois,
					  aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
					  alpha = 0.25, colour = NA, fill = pal_d3()(2)[2],
					  inherit.aes = FALSE) +
			geom_point(size = 0.1, shape = 20, alpha = 0.5, na.rm = TRUE) +
			labs(x = "Time (ms)",
				 y = "Gaze position in screen",
				 colour = "Imputed?",
				 title = gsub(".csv", "", files[i]),
				 caption = "Rectangles indicate the AOIs") +
			theme_custom() +
			scale_color_manual(values = pal_d3()(4)[c(1, 4)]) +
			scale_y_continuous(limits = c(0, 1920), breaks = seq(0, 1920, 480)) +
			scale_x_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5),
							   labels = ~format(.*1000, big.mark = ",", scientific = FALSE) ) +
			theme(legend.position = "top",
				  legend.key = element_rect(fill = NA),
				  legend.title = element_text(size = 10),
				  axis.text.x = element_text(size = 7),
				  axis.text.y = element_text(size = 7),
				  panel.grid.major.x = element_line(colour = "grey", 
				  								  linetype = "dotted",
				  								  size = 0.25),
				  plot.title = element_text(size = 10),
				  strip.text = element_text(size = 6))
		
		file_name <- gsub(".csv", ".png", files[i])
		plot_height <- length(unique(plot_data$trial))*0.75
		
		ggsave(here("img", "01_raw", file_name), plot = plot, height = plot_height, width = 6)
		
		cli_progress_update()
	}
	cli_progress_done(result = "done")
	
}



