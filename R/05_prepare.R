# prepare

prepare_data <- function(
		gaze_imputed, 
		participants, # participants dataset, get_participants output
		stimuli, # stimuli dataset, get_stimuli output
		vocabulary, # vocabulary dataset, get_vocabulary output
		attrition, # attrition dataset , get_attrition output
		aoi_coords
){
	suppressMessages({
		
		vocabulary <- vocabulary %>% 
			mutate(
				vocab_size_total_center = scale(vocab_size_total)[,1],
				vocab_size_l1_center = scale(vocab_size_l1)[,1],
				vocab_size_conceptual_center = scale(vocab_size_conceptual)[,1]
			) 
		
		clean <- list(
			select(participants, -filename),
			stimuli,
			gaze_imputed
		) %>% 
			reduce(full_join) %>%  
			select(
				participant, age_group, trial, phase, time, x, y, 
				target_location, filename, valid_sample
			) %>% 
			# evaluate if gaze coordinates are inside any AOI, and which
			mutate(
				aoi_prime = gaze_in_prime(x, y, aoi_coords = aoi_coords),
				aoi_target = gaze_in_target(x, y, target_location, aoi_coords),
				aoi_distractor = gaze_in_distractor(x, y, target_location, aoi_coords)
			) %>% 
			drop_na(filename) %>% 
			left_join(attrition) %>% 
			filter(
				phase=="Target-Distractor",
				valid_trial,
				valid_participant
			) %>% 
			drop_na(trial) %>% 
			left_join(select(participants, participant, age_group, lp)) %>% 
			select(
				participant,
				age_group,
				lp,
				trial, 
				time,
				aoi_target,
				aoi_distractor,
				trial_type,
				cognate, 
				non_cognate,
				unrelated,
				valid_trial,
				valid_participant
			)
		
		# to eyetrackingR format
		gaze <- clean %>% 
			group_by(participant, age_group, lp, trial) %>%
			mutate(time_bin = as.integer(cut_interval(time, length = 0.1, labels = FALSE))) %>% 
			ungroup() %>% 
			group_by(participant, age_group, lp, time_bin, trial_type) %>% 
			summarise(
				sum_target = sum(aoi_target, na.rm = TRUE),
				sum_distractor = sum(aoi_distractor, na.rm = TRUE),
				n_trials = length(unique(trial)),
				n = n(),
				.groups = "drop"
			) %>% 
			mutate(
				sum_target = case_when(
					sum_target==0 ~ 1.0,
					sum_target==n ~ n-1.0,
					TRUE ~ as.double(sum_target)
				),
				prop = sum_target/n,
				logit = log(prop / (1- prop))
			) %>%
			left_join(vocabulary) %>%  
			mutate_at(vars(age_group, lp, trial_type), as.factor) %>% 
			mutate_at(vars(sum_target, sum_distractor, n, time_bin, n_trials), as.integer) %>% 
			mutate(
				time_bin_center = scale(time_bin, scale = FALSE)[, 1],
				vocab_size_l1_center = scale(vocab_size_l1)[, 1],
				vocab_size_total_center = scale(vocab_size_total)[, 1],
				vocab_size_conceptual_center = scale(vocab_size_conceptual)[, 1]
			) %>% 
			drop_na(vocab_size_l1_center, vocab_size_total_center, vocab_size_conceptual_center) %>% 
			arrange(participant, trial_type, time_bin) %>% 
			select(
				participant,
				age_group,
				trial_type,
				time_bin,
				time_bin_center,
				sum_target,
				sum_distractor,
				n,
				prop,
				logit,
				lp,
				n_trials,
				vocab_size_total,
				vocab_size_l1,
				vocab_size_conceptual,
				vocab_size_total_center,
				vocab_size_l1_center,
				vocab_size_conceptual_center
			)
		
		# set a prior contrasts and orthogonal polynomials
		contrasts(gaze$lp) <- c(0.5, -0.5)
		colnames(attr(gaze$lp, "contrasts")) <-  c("LP (Mon vs. Bil)")
		contrasts(gaze$trial_type) <- cbind(
			"related_unrelated" = c(0.25, 0.25, -0.5),
			"cognate_noncognate" = c(0.5, -0.5, 0)
		)
		contrasts(gaze$age_group) <- cbind(
			"21_25" = c(-0.5, 0.5, 0),
			"25_30" = c(0, -0.5, 0.5)
		)
	})
	
	write_csv_arrow(gaze, here("data", "gaze", "gaze.csv"))
	saveRDS(gaze, here("data", "gaze", "gaze.rds"))
	
	return(gaze)
	
}



make_plots_gaze_processed <- function(gaze){
	
	participants <- unique(gaze$participant)
	n_participants <- length(participants)
	n_trials <- gaze %>% 
		distinct(participant, age_group, trial_type, n_trials)
	
	for (i in 1:n_participants) {
		
		plot <- gaze %>% 
			filter(participant==participants[i]) %>% 
			ggplot() +
			aes(
				x = time_bin,
				y = prop,
				colour = trial_type,
				shape = trial_type
			) + 
			facet_grid(~age_group) +
			geom_hline(
				yintercept = 0.5,
				colour = "grey",
				size = 1
			) +
			geom_line(
				size = 1
			) +
			geom_point(
				size = 3,
				na.rm = TRUE
			) +
			geom_text(
				data = filter(n_trials, participant==participants[i]),
				aes(
					x = 4,
					y = 1,
					label = paste0("n = ", n_trials)
				),
				hjust = 0,
				vjust = 1,
				size = 5,
				position = position_dodge(width = 10),
				fontface = "bold",
				show.legend = FALSE
			) +
			labs(
				x = "Time since target-distractor onset (ms)",
				y = "Proportion of target fixations",
				colour = "Trial type",
				title = paste0("Participant ", participants[i]),
				caption = "Proportion of samples in target AOI across trials",
				shape = "Trial type"
			) +
			theme_custom() +
			scale_color_d3() +
			scale_x_continuous(
				limits = c(0, 20),
				breaks = seq(0, 20, 5),
				labels = function(x) format(x*100, big.mark = ",", scientific = FALSE) 
			) +
			scale_y_continuous(
				limits = c(0, 1),
				labels = percent
			) +
			theme(
				legend.position = "top",
				legend.title = element_blank(),
				legend.key = element_rect(fill = NA),
				axis.text.y = element_text(size = 9),
				panel.grid.major.x = element_line(colour = "grey", linetype = "dotted")
				
			)
		
		file_name <- paste0(participants[i], ".png")
		
		n_age_groups <- gaze %>% 
			filter(gaze==participants[i]) %>% 
			pull(age_group) %>% 
			unique() %>% 
			length()
		
		ggsave(here("img", "02_processed", file_name), plot = plot, height = 5, width = 5*n_age_groups)
		
		message(paste0("[", i, "/", n_participants, "] ", file_name, " plot generated"))
	}
	
}

aggregate_data <- function(gaze, plot = TRUE){
	
	aggregated <- gaze %>% 
		group_by(
			participant,
			age_group,
			lp,
			trial_type,
			vocab_size_total_center,
			vocab_size_l1_center,
			vocab_size_conceptual_center
		) %>% 
		summarise(
			prop = mean(prop, na.rm = TRUE),
			logit = mean(logit, na.rm = TRUE),
			sum_target = sum(sum_target, na.rm = TRUE),
			n = sum(n),
			n_trials = unique(n_trials),
			.groups = "drop"
		)
	
	if (plot){
		means <- aggregated %>% 
			group_by(trial_type) %>% 
			summarise(
				prop = mean(prop, na.rm = TRUE),
				.groups = "drop"
			)
		
		my_plot <- aggregated %>% 
			ggplot() + 
			aes(
				x = trial_type, 
				y = prop,
				colour = trial_type, 
				fill = trial_type
			) + 
			geom_hline(
				yintercept = 0.5, 
				colour = "grey",
				linetype = "dotted"
			) + 
			geom_violin(
				colour = "white",
				alpha = 0.25
			) +
			geom_boxplot(
				fill = "white",
				colour = "black",
				width = 0.075,
				size = 0.75,
				outlier.colour = NA
			) +
			geom_jitter(
				size = 2,
				shape = 1,
				stroke = 1,
				width = 0.2,
				alpha = 0.5
			) +
			geom_text(
				data = means,
				aes(
					x = trial_type,
					y = prop,
					label = percent(prop, accuracy = 0.1)
				),
				position = position_nudge(y = 0.35),
				fontface = "bold",
				size = 5
			) +
			labs(
				x = "Trial type",
				y = "Mean proportion of target looking",
				colour = "Trial type",
				fill = "Trial type"
			) +
			scale_color_d3() +
			scale_fill_d3() +
			scale_y_continuous(
				labels = percent,
				limits = c(0.15, 0.90)
			) + 
			theme_custom() + 
			theme(
				legend.position = "none",
				axis.title.x = element_blank()
			)
		
		ggsave(here("img", "gaze_aggregated.png"), my_plot)
	}
	
	return(aggregated)
}
