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
	db <- vector(mode = "list", length = n_files)
	names(db) <- file_names
	
	names_dict <- get_name_dictionary() # name changes (see R/utils.R)
	
	# prepare progress bar
	pb_text <- "{pb_spin} Reading {pb_current}/{pb_total} | {col_blue(pb_percent)}"
	cli_progress_bar("Processing", total = n_files, format = pb_text)
	
	# read CSV and write Feather
	for (i in 1:n_files){
		
		db[[i]] <- x <- read_csv_arrow(file_paths[i], 
									   na = c("", "NaN", "NA", "<NA>")) |> 
			clean_names() |>  # rename all variables to snake case
			rename_with(
				\(x) str_replace_all(x, names_dict$col_name_changes),
				everything()
			) |> 
			fix_timestamps() |> 
			fix_validity() |> 
			mutate(
				phase =  str_replace_all(phase, names_dict$phase_name_changes),
				across(c(l_v, r_v), \(x) as.logical(as.integer(x))),
				x = ifelse(is.na(l_x) | !l_v, r_x, l_x), 
				y = ifelse(is.na(l_y) | !l_v, r_y, l_y),
				across(c(x, y), \(x) ifelse(!between(x, 0, 1), NA_real_, x)),
				valid_sample = (!(is.na(x) | is.na(y)) & (l_v | r_v))
			) |> 
			select(any_of(names_dict$relevant_variables), -time)
		
		# add missing columns, if any
		cols <- colnames(db[[i]])
		missing_col <- which(!(names_dict$relevant_variables %in% cols))
		missing_col <- cols[missing_col]
		
		if (length(missing_col) > 0) {
			new <- rep(NA_character_, length(missing_col))
			names(new) <- missing_col
			db[[i]] <- mutate(db[[i]], !!!new)
		}
		cli_progress_update()
	}
	
	cli_progress_done(result = "done")
	
	cli_progress_step("Binding datasets", spinner = FALSE)
	gaze_normalised <- bind_rows(db, .id = "filename")
	
	cli_progress_step("Writing Arrow database", spinner = FALSE)
	write_dataset(as_arrow_table(gaze_normalised), 
				  new_path,
				  partitioning = "filename",
				  hive_style = FALSE)
	
	return(gaze_normalised)
}

# pass eye-tracking files to Arrow
get_gaze_joint <- function(gaze_normalised) {
	
	# set and validate dirs
	new_path <- "data/gaze/02_joint"
	stopifnot(dir.exists(new_path))
	
	names_dict <- get_name_dictionary() # name changes (see R/utils.R)
	
	gaze_joint <- gaze_normalised %>%
		filter(between(trial_id, 1, 32),
			   phase %in% c("Target-Distractor", "Prime")) %>%  # get prime and target phase
		rename_with(~str_replace_all(., names_dict[["col_name_changes"]]),
					everything()) |>  # fix some values from outdated files
		collect() |> 
		add_count(filename, name = "n_samples") |> 
		group_by(filename, trial, phase) |>
		mutate(sampling_rate = ifelse(n_samples < 8e3, 60, 120),
			   time = 1:n()/sampling_rate,
			   time = time-min(time),
			   id = paste0("cognatepriming", id)) |> 
		ungroup() |> 
		filter(!(phase=="Prime" & time > 1.5), 
			   !(phase=="Target-Distractor" & time > 2.0)) |> 
		select(id, trial, trial_id, phase, time, x, y, valid_sample, filename) |> 
		as_arrow_table()
	# mutate(across(everything(), ~ifelse(is.nan(.), NA_real_, .))) %>%  # restart timestamps at each trial phase change
	
	# export Arrow database
	write_dataset(gaze_joint,
				  path = new_path, 
				  hive_style = FALSE,
				  partitioning = "filename") 
	
	return(gaze_joint)
}

# process Barcelona gaze data
get_gaze_processed <- function(gaze_joint,
							   participants, # participants dataset, get_participants output
							   stimuli, # stimuli dataset, get_stimuli output
							   aoi_coords,
							   screen_resolution = c(x = 1920, y = 1080)) {
	suppressMessages({
		
		participants_tmp <- select(participants, id, id_exp, age_group, filename)
		
		# set and validate dirs
		new_path <- "data/gaze/03_processed" # get path
		stopifnot(dir.exists(new_path)) # validate path
		
		gaze_processed <- gaze_joint %>% 
			mutate(filename = paste0(filename, ".csv")) |> 
			rename(id_exp = id) |> 
			right_join(participants_tmp,
					   by = join_by(id_exp, filename)) |> 
			mutate(x = x*screen_resolution["x"], # change gaze coordinates from 0-1 scale to screen resolution scale
				   y = y*screen_resolution["y"],
				   filename = paste0(filename, ".csv"),
				   age_group = as.character(age_group),
				   valid_sample = ifelse(is.na(valid_sample), 
				   					  FALSE, 
				   					  valid_sample)) %>% 
			arrange(id, age_group, trial, phase, time) %>% 
			select(id, age_group, trial, trial_id, phase, time, x, y, 
				   valid_sample, filename) 
		
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
	unlink(list.files(new_path, full.names = TRUE),
		   recursive = TRUE)
	
	# impute gaze NAs
	gaze_imputed <- open_dataset(path) %>% 
		group_by(filename, trial, phase) %>% 
		collect() %>% 
		mutate(across(c(x, y),
					  \(x) na.locf(x, na.rm = FALSE),
					  .names = "{.col}_imputed")) |> 
		ungroup() %>% 
		mutate(is_imputed = is.na(x) & !is.na(x_imputed)) %>% 
		select(-c(x, y)) |> 
		rename(x = x_imputed, y = y_imputed) |> 
		relocate(x, y, is_imputed, 
				 .after = valid_sample) 
	
	write_dataset(gaze_imputed,
				  new_path,
				  partitioning = "filename",
				  hive_style = FALSE)
	
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
	
	participants_tmp <- select(participants, id, age_group, filename,
							   test_language, list, version)
	stimul_tmp <- select(stimuli, test_language, list, version, trial_id,
						 target_location, trial_type)
	
	gaze_aoi <- open_dataset(path) %>% 
		left_join(participants_tmp) %>% 
		left_join(stimuli_tmp) %>% 
		select(id, age_group, trial, trial_id, phase, time,
			   x, y, valid_sample, target_location, trial_type, filename) %>% 
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
	
	write_dataset(gaze_aoi,
				  new_path, 
				  partitioning = "filename",
				  hive_style = FALSE)
	
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
			facet_wrap(trial+phase~dim, ncol = 4,
					   labeller = label_wrap_gen(multi_line=FALSE)) +
			geom_vline(xintercept = 0.3, linewidth = 0.25) +
			geom_rect(data = aois,
					  aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
					  alpha = 0.25, colour = NA,
					  inherit.aes = FALSE) +
			geom_point(size = 0.1, shape = 20, 
					   alpha = 0.5, na.rm = TRUE) +
			labs(x = "Time (ms)",
				 y = "Gaze position in screen",
				 colour = "Imputed?",
				 title = gsub(".csv", "", files[i]),
				 caption = "Rectangles indicate the AOIs") +
			theme_ggdist() +
			# scale_color_manual(values = pal_d3()(4)[c(1, 4)]) +
			scale_y_continuous(limits = c(0, 1920),
							   breaks = seq(0, 1920, 480)) +
			scale_x_continuous(limits = c(0, 2),
							   breaks = seq(0, 2, 0.5),
							   labels = function(x) {
							   	format(x * 1000,
							   		   big.mark = ",",
							   		   scientific = FALSE)
							   }) +
			theme(legend.position = "top",
				  legend.key = element_rect(fill = NA),
				  legend.title = element_text(size = 10),
				  axis.text.x = element_text(size = 7),
				  axis.text.y = element_text(size = 7),
				  panel.grid.major.x = element_line(colour = "grey", 
				  								  linetype = "dotted",
				  								  linewidth = 0.25),
				  plot.title = element_text(size = 10),
				  strip.text = element_text(size = 6))
		
		file_name <- gsub(".csv", ".png", files[i])
		plot_height <- length(unique(plot_data$trial))*0.75
		
		ggsave(here("img", "01_raw", file_name),
			   plot = plot, 
			   height = plot_height,
			   width = 6)
		
		cli_progress_update()
	}
	cli_progress_done(result = "done")
}



