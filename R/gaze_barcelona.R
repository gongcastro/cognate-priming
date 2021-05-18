# set up ----
library(tidyverse)
library(janitor)
library(googlesheets4)
library(data.table)

# set params
relevant.variables <- c(
	"participant", "trial_num", "trial", "phase", "time",
	"l_x", "l_y", "l_v", "r_x", "r_y", "r_v",
)

colname.changes <- c(
	"system_time_stamp" = "time", "l_1" = "l_x", "l_2" = "l_y", "process" = "phase",
	"r_1" = "r_x", "r_2" = "r_y", "l_user_coord_3" = "l_user_coord_z",
	"r_user_coord_3" = "r_user_coord_z", "suje_num" = "participant"
)

screen_resolution <- c(x = 1920, y = 1080)

#  import participant data ----
participants <- range_read(
	ss = "1JkhN4iBh3bi6PSReGGk9jSrVgDhZNOUmve6vNS2eEqE", 
	sheet = "barcelona", 
	na = ""
) %>%
	mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>%
	mutate(
		age_group = as.factor(age_group)
	)

# import gaze data ----
gaze <- list.files("Data/Gaze/Barcelona", full.names = TRUE) %>% 
	map(
		# apply the following function to each file listed above
		function(x) {
			fread(x, sep = ",", na.strings = c("", "NaN", "NA", "<NA>")) %>%
				clean_names() %>% # rename all variables to snake case
				rename_all(str_replace_all, colname.changes) %>% 
				mutate(phase = str_replace_all(
					phase,
					c("GETTER" = "Getter",
					  "PRIMEIMAGE" = "Prime",
					  "TARGET_DISTRACTOR" = "Target-Distractor",
					  "prime" = "Prime",
					  "primeimage" = "Prime",
					  "target_distractor" = "Target-Distractor"
					))) %>%
				filter(phase %in% c("Target-Distractor", "Prime")) %>%
				mutate(
					participant = paste0("cognatepriming", participant),
					time = row_number()*(1000/120) # to fix the timestamp in some files
				) %>%
				mutate_all(function(x) ifelse(is.nan(x), NA_real_, x)) %>%
				mutate_at(vars(starts_with("l_"), starts_with("r_")), as.numeric) %>% # gaze coords to numeric
				mutate_at(vars(contains("_v")), as.logical) %>% # validity to logicals
				select(one_of(relevant.variables))
		}) %>% 
	set_names(list.files("Data/Gaze/Barcelona")) %>% 
	bind_rows(.id = "filename") %>%
	as_tibble() %>% 
	group_by(participant, trial, phase, filename) %>%
	mutate(
		time = as.double(time-min(time, na.rm = TRUE))/1000,
		time_bin = cut_interval(time, length = 0.1, labels = FALSE)
	) %>%
	ungroup() %>%
	filter(between(time, 0, 2)) %>%
	mutate(
		l_v = l_v & !is.na(l_x) & !is.na(l_y) & between(l_x, 0, 1) & between(l_y, 0, 1),
		r_v = r_v & !is.na(l_x) & !is.na(r_y) & between(r_x, 0, 1) & between(r_y, 0, 1),
		v = l_v & r_v,
		l_x = ifelse(l_v, l_x, NA),
		l_y = ifelse(l_v, l_y, NA),
		r_x = ifelse(r_v, r_x, NA),
		r_y = ifelse(r_v, r_y, NA),
		x = ifelse(l_v, l_x, r_x)*screen_resolution["x"],
		y = ifelse(l_v, l_y, r_y)*screen_resolution["y"]
	) %>%
	relocate(time_bin, .before = time, .after = ) %>%
	arrange(participant, trial_num, phase, time_bin)

# export data
saveRDS(gaze, "Data/Gaze/processed_barcelona.rds")

