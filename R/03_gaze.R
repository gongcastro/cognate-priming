# pass eye-tracking files to Arrow
gaze_csv_to_arrow <- function() {
	
	# set directories
	path <- "data/gaze/00_raw"
	new_path <- "data/gaze/01_arrow"
	
	# validate paths
	stopifnot(dir.exists(path))
	stopifnot(dir.exists(new_path))
	
	# get file names and paths
	file_paths <- list.files(path, full.names = TRUE)
	file_names <- gsub(".csv", "", list.files(path))
	
	# prepare progress bar
	i <- 1
	n_files <- length(file_paths)
	pb_text <- "{pb_spin} Reading file {pb_current}/{pb_total} [{pb_percent}]"
	cli_progress_bar("Processing", total = n_files, format = pb_text)
	
	# read CSV and write Feather
	for (i in 1:n_files){
		csv_file <- read_csv_arrow(file_paths[i], na = c("", "NaN", "NA", "<NA>"))
		write_ipc_stream(csv_file, file.path(new_path, file_names[i]))
		cli_progress_update()
	}
	cli_progress_done(result = "done")
}

# read and preprocess eye-tracking files
get_gaze_files <- function(){
	
	# set directories
	path <- "data/gaze/01_arrow"
	new_path <- "data/gaze/02_processed"
	
	# validate paths
	stopifnot(dir.exists(path))
	stopifnot(dir.exists(new_path))
	
	# get file names and paths
	file_paths <- list.files(path, full.names = TRUE)
	file_names <- list.files(path)
	
	names_dict <- get_name_dictionary() # name changes (see R/utils.R)
	
	# prepare progress bar
	i <- 1
	n_files <- length(file_paths)
	options(cli.spinner = "dots")
	pb_text <- "Reading file {pb_bar} {pb_current}/{pb_total} [{pb_percent}]"
	cli_progress_bar("Processing", total = n_files, format = pb_text)
	
	for (i in 1:n_files){
		raw_data <- read_ipc_stream(file_paths[i])
		sampling_rate <- ifelse(nrow(raw_data) > 20e3, 120, 60)
		
		raw <- raw_data %>% 
			clean_names() %>% # rename all variables to snake case
			rename_with(~str_replace_all(., names_dict[["col_name_changes"]]), everything()) %>% # fix some values from outdates files
			mutate(phase =  str_replace_all(phase, names_dict[["phase_name_changes"]])) %>%
			filter(phase %in% c("Target-Distractor", "Prime")) %>% 	# get gaze in prime and target-distractor phases
			mutate(
				id = paste0("cognatepriming", id), # fix the timestamp in some files
				time = row_number()*(1000/sampling_rate), # change variables to the right class
				across(everything(), ~ifelse(is.nan(.), NA_real_, .)), # gaze coords to numeric
				across(c(starts_with("l_"), starts_with("r_")), as.numeric),  # validity columns to logicals
				across(contains("_v"), as.logical) 
			) %>% 
			# get only variables of interest
			select(any_of(names_dict[["relevant_variables"]]))
		
		write_ipc_stream(raw, file.path(new_path, file_names[i])) # write feather file
		
		cli_progress_update() # update progress bar
	}
	cli_progress_done(result = "done")
}

# process Barcelona gaze data
get_gaze_raw <- function(participants, # participants dataset, get_participants output
						 stimuli, # stimuli dataset, get_stimuli output
						 aoi_coords){
	
	suppressMessages({
		
		screen_resolution <- c(x = 1920, y = 1080) # screen size in pixels
		
		path <- "data/gaze/02_processed" # get path
		stopifnot(dir.exists(path)) # validate path
		file_paths <- list.files(path, full.names = TRUE) # list files
		
		raw <- map(file_paths, read_ipc_stream) %>% 
			# merge all data sets assigning them their file name
			set_names(list.files(path))  %>% 
			bind_rows(.id = "filename") %>% 
			# restart timestamps at each trial phase change, and express in seconds
			group_by(id, trial, phase, filename) %>% 
			mutate(time = (time-min(time))/1000) %>% 
			ungroup() %>%
			# trim timestamps outside the 2 seconds range
			filter(id %in% participants$id,
				   time < 2) %>% # get only valid participants
			mutate(
				# more detailed evaluation of sample validity
				valid_sample = (l_v & !is.na(l_x) & !is.na(l_y)) & (between(l_x, 0, 1) & between(l_y, 0, 1)),
				valid_sample = ifelse(is.na(valid_sample), FALSE, valid_sample),
				# if sample is not valid, change value to NA
				x = ifelse(valid_sample, l_x, NA),
				y = ifelse(valid_sample, l_y, NA),
				# change gaze coordinates from 0-1 scale to screen resolution scale [1920x1080]
				x = x*screen_resolution["x"],
				y = y*screen_resolution["y"],
				filename = paste0(filename, ".csv")
			) %>% 
			left_join(select(participants, id, age_group, filename)) %>% 
			select(id, age_group, trial, phase, time, x, y, valid_sample, filename) %>% 
			arrange(id, age_group, trial, phase, time)
		
		# merge data
		gaze_raw <- raw %>% 
			select(id, age_group, trial, phase, time, x, y, valid_sample, filename) %>% 
			mutate(valid_sample = ifelse(is.na(valid_sample), FALSE, valid_sample)) %>% 
			filter(!(phase=="Prime" & time > 1.5), 
				   !(phase=="Target" & time > 2.0))
		
		write_csv_arrow(gaze_raw, here("data", "gaze", "03_merged.csv"))
		
		
	})
	return(gaze_raw)
}


impute_gaze <- function(gaze_raw, ...){
	gaze_imputed <- gaze_raw %>% 
		group_by(filename, trial, phase) %>% 
		mutate(across(c(x, y), ~na.locf(., ...), .names = "{.col}_imputed"),
			   is_imputed = is.na(x) & !is.na(x_imputed)) %>% 
		ungroup() %>% 
		select(-c(x_imputed, y_imputed)) %>% 
		relocate(is_imputed, .after = valid_sample)
	
	return(gaze_imputed)
	
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

