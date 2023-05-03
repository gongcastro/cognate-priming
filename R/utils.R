# project utils ----------------------------------------------------------------

# run the targets pipeline
make <- function() {
	job::job({{ 
		targets::tar_make()
		job::export("none")  # return nothing
	}}, 
	import = NULL,
	title = "Cognate Priming")
}

# destroy targets products
unmake <- function(keep_rds = TRUE) {
	tar_destroy(ask = FALSE)
	rds_paths <- list.files("results/", pattern = ".rds", full.names = TRUE)
	if (!keep_rds){
		if (length(rds_paths) > 0) lapply(rds_paths, file.remove)
	}
	cli::cli_alert_success("Removed project outputs!")
}

# resolve namespace conflicts between packages
resolve_conflicts <- function() {
	suppressMessages({
		conflict_prefer("last_warnings", "rlang")
		conflict_prefer("filter", "dplyr")
		conflicts_prefer(dplyr::filter)
		conflict_prefer("between", "dplyr")
		conflict_prefer("timestamp", "utils")
		conflict_prefer("ar", "brms")
		conflict_prefer("chisq.test", "stats")
		conflict_prefer("discard", "scales")
		conflict_prefer("duration", "lubridate")
		conflict_prefer("fisher.test", "stats")
		conflict_prefer("lag", "dplyr")
	})
}

# visualisation utils ----------------------------------------------------------

# custom ggplot theme
theme_custom <- function(){
	theme(panel.background = element_rect(fill = "white", colour = NA),
		  panel.border = element_blank(),
		  panel.grid = element_blank(),
		  plot.background = element_rect(fill = "white", color = NA),
		  legend.key = element_rect(fill = "white", colour = NA),
		  text = element_text(colour = "black", size = 15),
		  axis.text.x = element_text(size = 9),
		  axis.text.y = element_text(size = 9),
		  axis.line = element_line(colour = "black", size = 0.75),
		  strip.text = element_text(size = 13, face = "bold"),
		  strip.background = element_rect(fill = "white", colour = NA))
}

# eyetracking utils ------------------------------------------------------------

# get name dictionary 
get_name_dictionary <- function(...) {
	
	col_name_changes <- c("participant" = "id",
						  "system_time_stamp" = "time",
						  "l_1" = "l_x",
						  "l_2" = "l_y",
						  "process" = "phase",
						  "r_1" = "r_x",
						  "r_2" = "r_y",
						  "l_user_coord_3" = "l_user_coord_z",
						  "r_user_coord_3" = "r_user_coord_z",
						  "suje_num" = "id",
						  "numtrial_lista" = "trial_num",
						  "trial_num" = "trial_id")
	
	phase_name_changes <- c("GETTER" = "Getter",
							"PRIMEIMAGE" = "Prime",
							"TARGET_DISTRACTOR" = "Target-Distractor",
							"prime" = "Prime",
							"primeimage" = "Prime",
							"target_distractor" = "Target-Distractor")
	
	relevant_variables <- c("id", "trial", "trial_id", "phase", "time", "x", "y", "valid_sample")
	
	name_dict <- lst(col_name_changes, 
					 phase_name_changes, 
					 relevant_variables)
	
	return(name_dict)
}

# stats utils ------------------------------------------------------------------

# adjusted proportion from Gelman, Hill & Vehtari (2020)
prop_adj <- function(y, n) (y+2)/(n+4)

# adjusted proportion SE from Gelman, Hill & Vehtari (2020)
prop_adj_se <- function(y, n) {
	prop <- prop_adj(y, n)
	sqrt(prop*(1-prop)/(n+4))
}

# adjusted proportion CI from Gelman, Hill & Vehtari (2020)
prop_adj_ci <- function(y, n, conf = 0.95) {
	prop <- (y+2)/(n+4)
	se <- sqrt(prop*(1-prop)/(n+4))
	ci <-  prop + qnorm(c((1-.width)/2, (1-(1-.width)/2)))*se
	ci[1] <- ifelse(ci[1]<0, 0, ci[1]) # truncate at 0
	ci[2] <- ifelse(ci[2]>1, 1, ci[2]) # truncate at 1
	return(ci)
}




