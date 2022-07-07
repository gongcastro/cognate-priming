get_gaze_files <- function(){
	
	file_paths <- list.files(here("data", "gaze", "00_raw"), full.names = TRUE)
	file_names <- list.files(here("data", "gaze", "00_raw"))
	n_files <- length(file_paths)
	
	relevant_variables <- c(
		"participant", "trial_num", "trial", "phase", "time",
		"l_x", "l_y", "l_v", "r_x", "r_y", "r_v"
	)
	
	col_name_changes <- c(
		"system_time_stamp" = "time",
		"l_1" = "l_x",
		"l_2" = "l_y",
		"process" = "phase",
		"r_1" = "r_x",
		"r_2" = "r_y",
		"l_user_coord_3" = "l_user_coord_z",
		"r_user_coord_3" = "r_user_coord_z",
		"suje_num" = "participant"
	)
	
	phase_name_changes <- c(
		"GETTER" = "Getter",
		"PRIMEIMAGE" = "Prime",
		"TARGET_DISTRACTOR" = "Target-Distractor",
		"prime" = "Prime",
		"primeimage" = "Prime",
		"target_distractor" = "Target-Distractor"
	)
	
	
	for (i in 1:n_files){
		raw <- read_csv_arrow(
			file_paths[i], 
			na = c("", "NaN", "NA", "<NA>")
		) %>%
			# rename all variables to snake case
			clean_names() %>% 
			# fix some values from outdates files
			rename_all(str_replace_all, col_name_changes) %>% 
			mutate(phase = str_replace_all(phase, phase_name_changes)) %>%
			# get gaze in prime and target-distractor phases
			filter(phase %in% c("Target-Distractor", "Prime")) %>%
			mutate(
				participant = paste0("cognatepriming", participant),
				# fix the timestamp in some files
				time = row_number()*(1000/120)
			) %>%
			# change variables to the right class
			mutate_all(function(x) ifelse(is.nan(x), NA_real_, x)) %>% # NaNs to NAs
			mutate_at(
				vars(
					starts_with("l_"),
					starts_with("r_")
				), 
				as.numeric
			) %>% # gaze coords to numeric
			mutate_at(
				vars(contains("_v")),
				as.logical
			) %>% # validity columns to logicals
			# get only variables of interest
			select(any_of(relevant_variables))
		
		write_csv_arrow(raw, here("data", "gaze", "01_processed", file_names[i]))
		
		message(paste0("[", i, "/", n_files, "] ", file_names[i], " processed"))
	}
}

# process Barcelona gaze data
get_gaze_raw <- function(
		participants, # participants dataset, get_participants output
		stimuli, # stimuli dataset, get_stimuli output
		aoi_coords
){
	suppressMessages({
		
		screen_resolution <- c(x = 1920, y = 1080) # screen size in pixels
		
		file_paths <- list.files(here("data", "gaze", "01_processed"), full.names = TRUE)
		
		# trial data ----
		
		# import gaze data ----
		raw <- map(file_paths, read_csv_arrow) %>% 
			# merge all data sets assigning them their file name
			set_names(list.files(here("data", "gaze", "01_processed")))  %>% 
			bind_rows(.id = "filename") %>% 
			# restart timestamps at each trial phase change, and express in seconds
			group_by(participant, trial, phase, filename) %>%
			mutate(time = (row_number()-1)*(1/120)) %>%
			ungroup() %>%
			# trim timestamps outside the 2 seconds range
			filter(
				participant %in% participants$participant,
				time < 2
			) %>% # get only valid participants
			mutate(
				# more detailed evaluation of sample validity
				valid_sample = (l_v & !is.na(l_x) & !is.na(l_y)) & (between(l_x, 0, 1) & between(l_y, 0, 1)),
				valid_sample = ifelse(is.na(valid_sample), FALSE, valid_sample),
				# if sample is not valid, change value to NA
				x = ifelse(valid_sample, l_x, NA),
				y = ifelse(valid_sample, l_y, NA),
				# change gaze coordinates from 0-1 scale to screen resolution scale [1920x1080]
				x = x*screen_resolution["x"],
				y = y*screen_resolution["y"]
			) %>% 
			left_join(select(participants, participant, age_group, filename)) %>% 
			select(participant, age_group, trial, phase, time, x, y, valid_sample, filename) %>% 
			arrange(participant, age_group, trial, phase, time)
		
		
		# merge data ----
		expanded <- expand(raw, participant, age_group, trial, phase, time)
		
		gaze_raw <- raw %>% 
			right_join(expanded) %>% 
			select(
				participant, age_group, trial, phase,
				time, x, y, valid_sample, filename
			) %>% 
			mutate(valid_sample = ifelse(is.na(valid_sample), FALSE, valid_sample)) %>% 
			filter(
				!(phase=="Prime" & time > 1.5),
				!(phase=="Target" & time > 2.0)
			)
		
		write_csv_arrow(gaze_raw, here("data", "gaze", "03_merged.csv"))
		
		
	})
	return(gaze_raw)
}


impute_gaze <- function(gaze_raw){
	
	gaze_imputed <- gaze_raw %>% 
		group_by(filename, trial, phase) %>% 
		mutate(
			x_imputed = na.locf(x, maxgap = 20, na.rm = FALSE),
			y_imputed = na.locf(y, maxgap = 20, na.rm = FALSE),
			is_imputed = is.na(x) & !is.na(x_imputed),
			x = x_imputed,
			y = y_imputed
		) %>% 
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
	
	for (i in 1:n_files) {
		
		
		plot_data <- gaze_imputed %>% 
			filter(filename==files[i]) %>% 
			pivot_longer(
				c(x, y),
				names_to = "dim",
				values_to = "value"
			) %>% 
			mutate(
				is_imputed = factor(
					is_imputed, 
					levels = c(FALSE, TRUE),
					labels = c("No", "Yes")
				),
				phase = factor(
					phase,
					levels = c("Prime", "Target-Distractor"),
					labels = c("Prime", "Target")
				)
			)
		plot <- plot_data %>% 
			ggplot() +
			aes(
				x = time,
				y = value,
				colour = is_imputed
			) + 
			facet_wrap(
				trial+phase~dim,
				ncol = 4,
				labeller = label_wrap_gen(multi_line=FALSE)
			) +
			geom_vline(
				xintercept = 0.3,
				size = 0.25,
				colour = pal_d3()(3)[3]
			) +
			geom_rect(
				data = aois,
				aes(
					xmin = -Inf,
					xmax = Inf,
					ymin = ymin,
					ymax = ymax
				),
				inherit.aes = FALSE,
				alpha = 0.25,
				colour = NA,
				fill = pal_d3()(2)[2]
			) +
			geom_point(
				size = 0.1,
				shape = 20,
				alpha = 0.5,
				na.rm = TRUE
			) +
			labs(
				x = "Time (ms)",
				y = "Gaze position in screen",
				colour = "Imputed?",
				title = gsub(".csv", "", files[i]),
				caption = "Rectangles indicate the AOIs",
			) +
			theme_custom() +
			scale_color_manual(
				values = pal_d3()(4)[c(1, 4)]
			) +
			scale_y_continuous(
				limits = c(0, 1920),
				breaks = seq(0, 1920, 480)
			) +
			scale_x_continuous(
				limits = c(0, 2),
				breaks = seq(0, 2, 0.5),
				labels = function(x) format(x*1000, big.mark = ",", scientific = FALSE) 
			) +
			theme(
				legend.position = "top",
				legend.key = element_rect(fill = NA),
				legend.title = element_text(size = 10),
				axis.text.x = element_text(size = 7),
				axis.text.y = element_text(size = 7),
				panel.grid.major.x = element_line(
					colour = "grey", 
					linetype = "dotted",
					size = 0.25
				),
				plot.title = element_text(size = 10),
				strip.text = element_text(size = 6)
			)
		
		file_name <- gsub(".csv", ".png", files[i])
		plot_height <- length(unique(plot_data$trial))*0.75
		
		ggsave(here("img", "01_raw", file_name), plot = plot, height = plot_height, width = 6)
		
		message(paste0("[", i, "/", n_files, "] ", file_name, " plot generated"))
	}
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

