# prepare

prepare_data <- function(gaze_aoi, 
						 participants, # participants dataset, get_participants output
						 stimuli, # stimuli dataset, get_stimuli output
						 vocabulary, # vocabulary dataset, get_vocabulary output
						 attrition, # attrition dataset , get_attrition output
						 aoi_coords,
						 time_subset = c(0, 2) # subset time (s)
){
	suppressMessages({
		
		vocabulary <- mutate(vocabulary, across(starts_with("vocab_size"), ~scale(.)[,1], .names = "{.col}_center")) 
		
		clean <- list(select(participants, -filename), stimuli, gaze_aoi) %>% 
			reduce(full_join) %>% 
			select(id, age_group, trial, phase, time, x, y, 
				   target_location, filename, valid_sample) %>% 
			# evaluate if gaze coordinates are inside any AOI, and which
			mutate(aoi_center = gaze_in_center(x, y, aoi_coords = aoi_coords),
				   aoi_left = gaze_in_left(x, y, aoi_coords),
				   aoi_right = gaze_in_right(x, y, aoi_coords)) %>% 
			replace_na(list(aoi_center = FALSE, aoi_right = FALSE, aoi_left = FALSE)) %>%
			mutate(aoi_prime = aoi_center,
				   aoi_target = ifelse(target_location=="r", aoi_right, aoi_left),
				   aoi_distractor = ifelse(target_location=="l", aoi_right, aoi_left)) %>% 
			drop_na(filename) %>% 
			left_join(attrition) %>% 
			filter(phase=="Target-Distractor",
				   valid_trial,
				   valid_participant) %>% 
			drop_na(trial) %>% 
			left_join(select(participants, id, age_group, lp)) %>% 
			select(id, age_group, lp, trial, time,
				   aoi_target, aoi_distractor, trial_type,
				   cognate, non_cognate, unrelated,
				   valid_trial, valid_participant)
		
		# to eyetrackingR format
		df <- clean %>% 
			filter(between(time, time_subset[1], time_subset[2])) %>% 
			mutate(time_bin = cut(time, 
								  breaks = seq(time_subset[1], time_subset[2], 0.1),
								  labels = FALSE,
								  include.lowest = TRUE))  %>% 
			group_by(id, age_group, time_bin, trial_type, lp) %>% 
			summarise(sum_target = sum(aoi_target, na.rm = TRUE),
					  n_trials = length(unique(trial)),
					  n = n(),
					  .groups = "drop") %>% 
			mutate(sum_target = case_when(sum_target==0 ~ 1.0,
										  sum_target==n ~ n-1.0,
										  TRUE ~ as.double(sum_target)),
				   prop = sum_target/n,
				   logit = log(sum_target/(n-sum_target))) %>% # see Chow et al. (2018) 
			left_join(vocabulary) %>%  
			mutate(across(c(age_group, lp, trial_type), as.factor), 
				   across(c(sum_target, n, time_bin, n_trials), as.integer),
				   across(starts_with("vocab_size"), ~scale(.)[,1], .names = "{.col}_center")) %>% 
			drop_na(vocab_size_l1_center, vocab_size_total_center, vocab_size_conceptual_center) %>% 
			arrange(id, time_bin) %>% 
			rename(vocab = vocab_size_total, 
				   vocab_std = vocab_size_total_center) %>% 
			# make orthogonal polynomials (see Mirman, 2014)
			poly_add_columns(time_bin, degree = 3, prefix = "ot", scale_width = 1)

		# set a prior contrasts and orthogonal polynomials
		contrasts(df$lp) <- cbind("mon_bil" = c(0.5, -0.5))
		contrasts(df$trial_type) <- cbind("related_unrelated" = c(0.25, 0.25, -0.5),
										  "cognate_noncognate" = c(0.5, -0.5, 0))
		contrasts(df$age_group) <- cbind("21_25" = c(-0.5, 0.5, 0),
										 "25_30" = c(0, -0.5, 0.5))
		
		df <- select(df, id, age_group, time_bin, ot1:ot3, 
					 sum_target, n, prop, logit, 
					 trial_type, lp, n_trials, vocab, vocab_std)
	})
	
	# export data
	saveRDS(df, file.path("data", "gaze.rds"))
	
	return(df)
	
}



make_plots_gaze_processed <- function(df){
	
	participants <- unique(df$id)
	n_participants <- length(participants)
	n_trials <- distinct(df, id, age_group, trial_type, n_trials)
	
	for (i in 1:n_participants) {
		
		plot <- df %>% 
			filter(id==participants[i]) %>% 
			ggplot(aes(x = time_bin, y = prop, colour = trial_type, shape = trial_type)) + 
			facet_grid(~age_group) +
			geom_hline(yintercept = 0.5, colour = "grey", size = 1) +
			geom_line(size = 1) +
			geom_point(size = 3, na.rm = TRUE) +
			geom_text(
				data = filter(n_trials, id==participants[i]),
				aes(x = 4, y = 1, label = paste0("n = ", n_trials)),
				hjust = 0, vjust = 1, size = 5,
				position = position_dodge(width = 10),
				fontface = "bold", show.legend = FALSE) +
			labs(x = "Time since target-distractor onset (ms)",
				 y = "Proportion of target fixations",
				 colour = "Trial type",
				 title = paste0("Participant ", participants[i]),
				 caption = "Proportion of samples in target AOI across trials",
				 shape = "Trial type") +
			theme_custom() +
			scale_color_d3() +
			scale_x_continuous(
				limits = c(0, 20),
				breaks = seq(0, 20, 5),
				labels = ~format(.*100, big.mark = ",", scientific = FALSE) 
			) +
			scale_y_continuous(limits = c(0, 1), labels = percent) +
			theme(legend.position = "top",
				  legend.title = element_blank(),
				  legend.key = element_rect(fill = NA),
				  axis.text.y = element_text(size = 9),
				  panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"))
		
		file_name <- paste0(participants[i], ".png")
		
		n_age_groups <- df %>% 
			filter(id==participants[i]) %>% 
			pull(age_group) %>% 
			unique() %>% 
			length()
		
		ggsave(here("img", "02_processed", file_name), plot = plot, height = 5, width = 5*n_age_groups)
		
		message(paste0("[", i, "/", n_participants, "] ", file_name, " plot generated"))
	}
	
}

aggregate_data <- function(df, plot = TRUE){
	
	aggregated <- df %>% 
		group_by(id, age_group, lp, trial_type, vocab_std) %>% 
		summarise(across(c(prop, logit), ~mean(., na.rm = TRUE)),
				  sum_target = sum(sum_target, na.rm = TRUE),
				  n = sum(n),
				  n_trials = unique(n_trials),
				  .groups = "drop")
	
	if (plot){
		means <- aggregated %>% 
			group_by(trial_type) %>% 
			summarise(prop = mean(prop, na.rm = TRUE), .groups = "drop")
		
		my_plot <-  ggplot(aggregated, aes(x = trial_type, y = prop, colour = trial_type, fill = trial_type)) + 
			geom_hline(yintercept = 0.5, colour = "grey", linetype = "dotted") + 
			geom_violin(colour = "white", alpha = 0.25) +
			geom_boxplot(fill = "white", colour = "black", width = 0.075, size = 0.75, outlier.colour = NA) +
			geom_jitter(size = 2, shape = 1, stroke = 1, width = 0.2, alpha = 0.5) +
			geom_text(data = means,
					  aes(x = trial_type, y = prop, label = percent(prop, accuracy = 0.1)),
					  position = position_nudge(y = 0.35),
					  fontface = "bold", size = 5) +
			labs(x = "Trial type",
				 y = "Mean proportion of target looking",
				 colour = "Trial type",
				 fill = "Trial type") +
			scale_color_d3() +
			scale_fill_d3() +
			scale_y_continuous(labels = percent, limits = c(0.15, 0.90)) + 
			theme_custom() + 
			theme(legend.position = "none", axis.title.x = element_blank())
		
		ggsave(here("img", "gaze_aggregated.png"), my_plot)
	}
	
	return(aggregated)
}
