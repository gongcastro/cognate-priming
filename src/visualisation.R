# functions
make_plots_gaze <- function(gaze_aoi, 
							aoi_coords,
							participants,
							stimuli,
							attrition_trials,
							attrition_participants){
	
	files <- unique(gaze_aoi$filename[!is.na(gaze_aoi$filename)])
	n_files <- length(files)
	
	i <- 0
	pb_msg <- "{pb_spin} Plotting {pb_current}/{pb_total} | {col_blue(pb_percent)}"
	cli_progress_bar("Plotting",
					 total = n_files, 
					 format = pb_msg)
	
	attrition_trials$is_valid_trial <- ifelse(
		is.na(attrition_trials$is_valid_vocab),
		attrition_trials$is_valid_gaze_test & 
			attrition_trials$is_valid_gaze_test_each,
		attrition_trials$is_valid_trial
	)
	
	plot_data <- get_plot_data(gaze_aoi,
							   attrition_trials,
							   attrition_participants)
	
	plot_data_split <- plot_data |> 
		group_split(filename) |> 
		set_names(files)
	
	for (i in 1:n_files) {
		
		plot <- plot_gaze_data(plot_data_split[[i]], files[i])
		
		file_name <- gsub(".csv", ".png", files[i])
		plot_height <- length(unique(plot_data_split[[i]]$trial))*0.75
		
		ggsave(paste0("img/01_raw/", file_name),
			   plot = plot, 
			   height = plot_height,
			   width = 6)
		
		cli_progress_update()
	}
	cli_progress_done(result = "done")
}

get_plot_data <- function(gaze_aoi,
						  attrition_trials, 
						  attrition_participant) {
	
	plot_data <- gaze_aoi |> 
		left_join(attrition_trials,
				  by = join_by(id, age_group, trial, trial_type)) |> 
		left_join(attrition_participants,
				  by = join_by(id, age_group)) |> 
		left_join(select(participants, id, filename, test_language, list, version),
				  by = join_by(id, filename)) |> 
		mutate(target_y = ifelse(target_location=="l", 
								 mean(c(aoi_coords$left[["xmin"]],
								 	   aoi_coords$left[["xmax"]])), 
								 mean(c(aoi_coords$right[["xmin"]],
								 	   aoi_coords$right[["xmax"]]))), 
			   target_y = ifelse(phase=="Prime", NA, target_y)) |> 
		pivot_longer(c(x, y), names_to = "dim", values_to = "value") |> 
		mutate(
			target_y = ifelse(dim=="y", NA, target_y),
			id_age_group = paste0(id, " (", age_group, ")"),
			is_imputed = factor(is_imputed, 
								levels = c(FALSE, TRUE),
								labels = c("Observed", "Imputed")),
			is_phase_valid = ifelse(phase=="Prime", 
									is_valid_gaze_prime,
									is_valid_gaze_test & is_valid_gaze_test_each) |> 
				factor(levels = c(FALSE, TRUE),
					   labels = c("Invalid phase", "Valid phase")),
			is_valid_trial = factor(is_valid_trial,
									levels = c(FALSE, TRUE),
									labels = c("Invalid trial",
											   "Valid trial")),
			is_valid_participant = factor(is_valid_participant,
										  levels = c(FALSE, TRUE),
										  labels = c("Excluded",
										  		   "Included")),
			phase = factor(phase,
						   levels = c("Prime", "Target-Distractor"), 
						   labels = c("Prime", "Target")),
			phase_dim = paste0(phase, " - ", dim)
		)
	
	return(plot_data)
}

plot_gaze_data <- function(plot_data, file) {
	
	id_label <- paste0("ID ", unique(plot_data$id), ", ", 
					   unique(plot_data$age_group),
					   " (", unique(plot_data$is_valid_participant), ")")
	
	aois <- get_aois_viz(aoi_coords)
	
	dummy_data <- tibble(dim = c("x", "y"),
						 ymin = c(0, 0),
						 ymax = c(1920, 1080)) |> 
		expand_grid(phase = c("Prime", "Target")) |> 
		mutate(phase_dim = paste0(phase, " - ", dim))
	
	plot <- plot_data |> 
		ggplot(aes(x = timestamp,
				   y = value, 
				   colour = is_imputed)) + 
		facet_wrap(trial ~ phase_dim,
				   ncol = 4,
				   scales = "free_y",
				   labeller = label_wrap_gen(multi_line = FALSE)) +
		geom_blank(data = dummy_data,
				   aes(xmin = 0,
				   	xmax = 2,
				   	ymin = ymin,
				   	ymax = ymax),
				   inherit.aes = FALSE) + 
		geom_vline(xintercept = 0.3, 
				   linewidth = 0.25,
				   colour = "grey") +
		geom_rug(data = filter(plot_data, 
							   is_gaze_target,
							   phase=="Target"),
				 length = unit(0.05, "cm"),
				 sides = "t",
				 colour = "mediumspringgreen") +
		geom_rug(data = filter(plot_data, 
							   is_gaze_distractor,
							   phase=="Target"),
				 length = unit(0.05, "cm"),
				 sides = "b",
				 colour = "mediumspringgreen") +
		geom_rect(data = aois,
				  aes(xmin = -Inf,
				  	xmax = Inf,
				  	ymin = ymin,
				  	ymax = ymax),
				  alpha = 0.25, 
				  colour = NA,
				  inherit.aes = FALSE) +
		geom_text(aes(x = 0, y = 0,
					  label = paste0(is_phase_valid, ", ",
					  			   is_valid_trial)),
				  colour = "black",
				  size = 1.5,
				  vjust = 0,
				  hjust = 0) +
		geom_text(aes(x = 0, 
					  y = target_y,
					  label = "T"),
				  na.rm = TRUE,
				  colour = "grey50",
				  size = 2,
				  hjust = 0.5) +
		geom_point(size = 0.1,
				   shape = 20, 
				   alpha = 0.5,
				   na.rm = TRUE) +
		labs(title = id_label,
			 x = "Time (ms)",
			 y = "Gaze position in screen",
			 colour = "Imputed?",
			 caption = gsub(".csv", "", file)) +
		theme_ggdist() +
		scale_colour_manual(values = c("black", "red")) +
		scale_y_continuous(breaks = c(aoi_coords$left[["xmin"]],
									  aoi_coords$left[["xmax"]],
									  aoi_coords$right[["xmin"]],
									  aoi_coords$right[["xmax"]])) +
		scale_x_continuous(limits = c(0, 2),
						   breaks = seq(0, 2, 0.5),
						   labels = function(x) {
						   	format(x * 1000,
						   		   big.mark = ",",
						   		   scientific = FALSE)
						   },
						   sec.axis = dup_axis()) +
		guides(colour = guide_legend(override.aes = list(alpha = 1,
														 size = 3))) +
		theme(legend.position = "top",
			  legend.key = element_rect(fill = NA),
			  legend.title = element_text(size = 10),
			  axis.text.x = element_text(size = 6.5),
			  axis.text.y = element_text(size = 4.5),
			  panel.border = element_rect(fill = NA, colour = "grey"),
			  panel.grid.major.x = element_line(colour = "grey", 
			  								  linetype = "dotted",
			  								  linewidth = 0.25),
			  plot.title = element_text(size = 10),
			  strip.text = element_text(size = 6),
			  strip.text.y = element_text(angle = 0))
	
	return(plot)
}

# utils
get_aois_viz <- function(aoi_coords) {
	ymin_coords <- c(
		aoi_coords$center["xmin"], 
		aoi_coords$center["ymin"], 
		aoi_coords$right["xmin"], 
		aoi_coords$right["ymin"], 
		aoi_coords$left["xmin"], 
		aoi_coords$left["ymin"]
	)
	
	ymax_coords <- c(
		aoi_coords$center["xmax"],
		aoi_coords$center["ymax"],
		aoi_coords$right["xmax"],
		aoi_coords$right["ymax"],
		aoi_coords$left["xmax"],
		aoi_coords$left["ymax"]
	)
	
	aois <- tibble(
		phase = rep(c("Prime", "Target", "Target"), each = 2),
		dim = rep(c("x", "y"), each = 1, times = 3),
		xmin = -Inf,
		xmax = -Inf,
		ymin = ymin_coords,
		ymax = ymax_coords
	) |> 
		distinct(pick(everything()))
	
	aois$phase_dim <- paste0(aois$phase, " - ", aois$dim)
	
	return(aois)
}


# plot screen ------------------------------------------------------------------

make_plots_heatmap <- function(gaze_aoi,
							   aoi_coords,
							   participants,
							   stimuli,
							   attrition_trials,
							   attrition_participants) {
	
	files <- unique(gaze_aoi$filename[!is.na(gaze_aoi$filename)])
	n_files <- length(files)
	
	aois <- get_aois_viz(aoi_coords)
	
	i <- 0
	pb_msg <- "{pb_spin} Plotting {pb_current}/{pb_total} | {col_blue(pb_percent)}"
	cli_progress_bar("Plotting",
					 total = n_files, 
					 format = pb_msg)
	
	plot_data <- get_plot_data(gaze_aoi, 
							   attrition_trials,
							   attrition_participants)
	
	plot_data_split <- plot_data |> 
		group_split(filename) |> 
		set_names(files)
	
	for (i in 1:n_files) {
		
		id_label <- paste0("ID ", unique(plot_data_split[[i]]$id), ", ", 
						   unique(plot_data_split[[i]]$age_group),
						   " (", unique(plot_data_split[[i]]$is_valid_participant), ")")
		
		plot <- plot_heatmap_data(plot_data_split[[i]], files[i])
		
		file_name <- gsub(".csv", ".png", files[i])
		plot_height <- (length(unique(plot_data_split[[i]]$trial))/4)
		
		ggsave(paste0("img/02_heatmap/", file_name),
			   plot = plot, 
			   height = plot_height,
			   width = 8,
			   dpi = 800)
		
		cli_progress_update()
	}
	cli_progress_done(result = "done")
}


plot_heatmap_data <- function(plot_data, file) {
	
	aois <- map_df(aoi_coords, bind_rows, .id = "aoi") |> 
		expand_grid(phase = c("Prime", "Target")) |> 
		filter((phase=="Prime" & aoi=="center") | 
			   	(phase=="Target" & aoi!="center")) |> 
		mutate(target_x = xmin,
			   target_y = ymax,
			   target_location = case_when(
			   	aoi=="left" ~ "l",
			   	aoi=="right" ~ "r",
			   	.default = "c")) 
	
	plot <- plot_data |> 
		pivot_wider(id_cols = c(id, trial, timestamp, phase,
								is_imputed,
								target_location, is_phase_valid,
								is_valid_participant, id_age_group),
					names_from = dim,
					values_from = value) |> 
		ggplot(aes(x = x, y = y,
				   colour = timestamp)) + 
		facet_wrap(trial ~ phase,
				   ncol = 8,
				   labeller = label_wrap_gen(multi_line = FALSE)) +
		geom_text(aes(x = 1920, y = 0,
					  label = paste0(is_phase_valid, ", ",
					  			   is_valid_participant)),
				  colour = "black",
				  size = 1.5,
				  vjust = 0,
				  hjust = 1) +
		geom_rect(data = aois,
				  aes(xmin = xmin, 
				  	xmax = xmax,
				  	ymin = ymin, 
				  	ymax = ymax,
				  ),
				  fill = "grey90",
				  na.rm = TRUE,
				  inherit.aes = FALSE) +
		geom_text(data = right_join(aois,
									distinct(plot_data, 
											 trial,
											 phase,
											 target_location,
											 phase),
									by = join_by(phase, target_location)) |> 
				  	filter(phase=="Target"),
				  aes(x = target_x,
				  	y = target_y),
				  label = "T",
				  na.rm = TRUE,
				  size = 1,
				  hjust = 0.2,
				  vjust = 1.2,
				  inherit.aes = FALSE) +
		geom_path(linewidth = 1/4, na.rm = TRUE) +
		# geom_point(size = 0.1, shape = 20, 
		# 		   alpha = 0.5, na.rm = TRUE) +
		labs(title = id_label,
			 x = "X dimension",
			 y = "Y dimension",
			 colour = "Time (ms)",
			 caption = gsub(".csv", "", file)) +
		theme_ggdist() +
		scale_x_continuous(limits = c(0, 1920)) +
		scale_y_continuous(limits = c(0, 1080)) +
		scale_colour_gradient2(low = "blue", mid = "yellow" , high = "red", 
							   midpoint = median(plot_data$timestamp)) +
		coord_fixed() +
		theme(legend.position = "top",
			  legend.key = element_rect(fill = NA),
			  legend.title = element_text(size = 10),
			  axis.text = element_blank(),
			  axis.title.x = element_blank(),
			  axis.title.y = element_blank(),
			  axis.ticks = element_blank(),
			  panel.background = element_rect(colour = NA, fill = "grey60"),
			  panel.border = element_rect(fill = NA, colour = "grey"),
			  panel.grid = element_blank(),
			  plot.title = element_text(size = 10),
			  strip.background = element_rect(colour = NA, fill = NA),
			  strip.text = element_text(size = 6),
			  strip.text.y = element_text(angle = 0))
	
	return(plot)
}



# plot processed data ----------------------------------------------------------

make_plots_gaze_processed <- function(data_time, 
									  attrition_trials,
									  attrition_participants){
	
	plot_data <- data_time |> 
		left_join(attrition_participants,
				  by = join_by(id, age_group)) |> 
		mutate(is_valid_participant = factor(is_valid_participant,
											 levels = c(FALSE, TRUE),
											 labels = c("Excluded",
											 		   "Included")),
			   trial_type_label = paste0(trial_type, " (n = ", .ntrials),
			   age_group_label = paste0(age_group, " (",
			   						 is_valid_participant, "), C: ",
			   						 cognate, ", NC: ", noncognate, 
			   						 ", U: ", unrelated)) |> 
		group_split(id) |> 
		set_names(unique(data_time$id))
	
	participants <- unique(data_time$id)
	n_files <- length(participants)
	n_participants <- length(participants)
	n_trials <- distinct(data_time, id, age_group, trial_type, .ntrials)
	
	i <- 0
	pb_msg <- "{pb_spin} Plotting {pb_current}/{pb_total} | {col_blue(pb_percent)}"
	cli_progress_bar("Plotting",
					 total = n_files, 
					 format = pb_msg)
	
	for (i in 1:n_participants) {
		
		plot <- plot_data[[i]] |> 
			filter(id==participants[i]) |> 
			ggplot(aes(x = timebin, 
					   y = .prop,
					   colour = trial_type, 
					   shape = trial_type)) + 
			facet_grid(~age_group_label) +
			geom_hline(yintercept = 0.5, 
					   colour = "grey",
					   linewidth = 1) +
			stat_summary(fun.data = mean_se,
						 geom = "line",
						 linewidth = 3/4) +
			stat_summary(fun = mean,
						 geom = "point",
						 size = 2.5) +
			labs(x = "Time since target-distractor onset (ms)",
				 y = "Proportion of target fixations",
				 colour = "Trial type",
				 title = paste0("Participant ", participants[i]),
				 caption = "Proportion of samples in target AOI across trials",
				 shape = "Trial type") +
			theme_ggdist() +
			scale_colour_manual(values = c("dodgerblue", "orange", "red")) +
			scale_x_continuous(
				limits = c(0, 20),
				breaks = seq(0, 20, 5),
				labels = \(x) format(x*100, big.mark = ",", scientific = FALSE) 
			) +
			scale_y_continuous(limits = c(0, 1)) +
			theme(legend.position = "top",
				  legend.title = element_blank(),
				  legend.key = element_rect(fill = NA),
				  axis.text.y = element_text(size = 9),
				  panel.grid.major.x = element_line(colour = "grey",
				  								  linetype = "dotted"))
		
		file_name <- paste0(participants[i], ".png")
		
		n_age_groups <- data_time |> 
			filter(id==participants[i]) |> 
			pull(age_group) |> 
			unique() |> 
			length()
		
		ggsave(paste0("img/03_processed/", file_name), 
			   plot = plot, 
			   height = 5,
			   width = 5 * n_age_groups)
		
		cli_progress_update()
	}
}

plot_data <- gaze_aoi |> 
	left_join(attrition_trials,
			  by = join_by(id, age_group, trial, trial_type)) |> 
	left_join(attrition_participants,
			  by = join_by(id, age_group)) |> 
	left_join(select(participants, id, filename, test_language, list, version),
			  by = join_by(id, filename)) |> 
	mutate(target_y = ifelse(target_location=="l", 
							 mean(c(aoi_coords$left[["xmin"]],
							 	   aoi_coords$left[["xmax"]])), 
							 mean(c(aoi_coords$right[["xmin"]],
							 	   aoi_coords$right[["xmax"]]))), 
		   target_y = ifelse(phase=="Prime", NA, target_y)) |> 
	pivot_longer(c(x, y), names_to = "dim", values_to = "value") |> 
	mutate(
		target_y = ifelse(dim=="y", NA, target_y),
		id_age_group = paste0(id, " (", age_group, ")"),
		is_imputed = factor(is_imputed, 
							levels = c(FALSE, TRUE),
							labels = c("Observed", "Imputed")),
		is_phase_valid = ifelse(phase=="Prime", 
								is_valid_gaze_prime,
								is_valid_gaze_test & is_valid_gaze_test_each),
		phase = factor(phase,
					   levels = c("Prime", "Target-Distractor"), 
					   labels = c("Prime", "Target")),
		phase_dim = paste0(phase, " - ", dim)
	) |> 
	arrange(id, age_group, trial, phase, timestamp)

saveRDS(plot_data, "docs/plot_data.rds")

job::job(
	title = "Gaze plots", 
	{
		make_plots_gaze(gaze_aoi, 
						aoi_coords,
						participants,
						stimuli,
						attrition_trials,
						attrition_participants)
		
		make_plots_heatmap(gaze_aoi, 
						   aoi_coords,
						   participants,
						   stimuli,
						   attrition_trials,
						   attrition_participants)
	})
