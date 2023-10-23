#' Get audio durations
get_audio_duration <- function(trials) {
	
	# get dir paths and validate paths
	audio.dir <- paste0("stimuli/sounds/sounds_", c("cat", "spa")) 
	audio.dir.valid <- dir.exists(audio.dir)
	
	if (!all(audio.dir.valid))  {
		missing.dir <- audio.dir[!audio.dir.valid]
		error.msg <- "director{?y/ies} {missing.dir} {?does/do} not exist"
		cli_abort(error.msg)
	}
	
	# get audio paths
	audio.paths <- list.files(audio.dir, full.names = TRUE, pattern = ".wav$")
	durations.vctr <- purrr::map_dbl(audio.paths, get_duration)
	durations <- tibble::tibble(audio_path = audio.paths, 
								duration = durations.vctr)
	durations$audio <- basename(audio.paths)
	durations$test_language <- ifelse(grepl("sounds_cat", audio.paths),
									  "Catalan",
									  "Spanish")
	
	# merge with trials dataset
	trials_tmp <- distinct(trials[trials$location=="Barcelona", ], 
						   test_language, version, audio)
	trials_tmp$audio <- stringi::stri_trans_general(str = trials_tmp$audio, 
													id = "Latin-ASCII")
	duration <- inner_join(trials_tmp, durations,
						   relationship = "many-to-many",
						   by = join_by(test_language, audio))
	
	return(duration)
	
}

#' Get duration of a WAV file
get_duration <- function(audio_path) {
	if (!file.exists(audio_path)) cli::cli_abort("{audio_path} does not exist")
	sound <- tuneR::readWave(audio_path) # extract wave
	sound.length <- round(length(sound@left) / sound@samp.rate, 2) # duration
}

#' Get stimuli data (join everything)
get_stimuli_bcn <- function(trials, # trials dataset
						duration){
	
	# join all datasets (not very elegant, but does the trick)
	# prime and target data are joined separately to avoid duplicates
	stimuli_raw <- trials |> 
		filter(location=="Barcelona") |> 
		mutate(audio = stringi::stri_trans_general(str = audio, 
												   id = "Latin-ASCII")) |>
		left_join(duration, by = join_by(test_language, version, audio))
	
	# select and reorder relevant variables
	stimuli <- stimuli_raw |> 
		select(trial, test_language, version, list, trial_type,
			   prime, target, distractor, audio, duration, target_location,
			   prime_cdi, target_cdi, distractor_cdi) |> 
		mutate(across(c(trial, list), as.integer))
	
	test_stimuli(stimuli)
	
	return(stimuli)
}


# utils ------------------------------------------------------------------------

#' Adjusted proportion from Gelman, Hill & Vehtari (2020)
prop_adj <- function(y, n) {
	
	prop <- (y + 2) / (n + 4)
	
	return(prop)
}

#' Adjusted proportion SE from Gelman, Hill & Vehtari (2020)
prop_adj_se <- function(y, n) {
	
	prop <- prop_adj(y, n)
	se <- sqrt(prop * (1 - prop) / (n + 4))
	
	return(se)
	
}

#' Adjusted proportion CI from Gelman, Hill & Vehtari (2020)
prop_adj_ci <- function(y, n, conf = 0.95) {
	
	prop <- (y + 2) / (n + 4)
	se <- sqrt(prop * (1 - prop)/(n + 4))
	ci <-  prop + qnorm(c((1 - .width) / 2, (1 - (1 - .width) / 2))) * se
	ci[1] <- ifelse(ci[1] < 0, 0, ci[1]) # truncate at 0
	ci[2] <- ifelse(ci[2] > 1, 1, ci[2]) # truncate at 1
	
	return(ci)
}

# Oxford -----------------------------------------------------------------------

get_stimuli_oxf <- function(stimuli_oxf_file) {
	
	my_colnames <- c("remove1", "remove2", "audio",
					 "remove3", "prime", "trial_type",
					 "picture1", "role1", "picture2",
					 "role2", "remove4", "remove5")
	
	stimuli <- stimuli_oxf_file |> 
		file.path() |> 
		map_dfr(readxl::read_xlsx,
				skip = 1,
				col_names = my_colnames,
				.id = "list") |> 
		mutate(across(c(audio, prime, picture1, picture2),
					  \(x) substr(x, 3, nchar(x))),
			   target = if_else(role1=="TARGET", picture1, picture2),
			   distractor = if_else(role2=="TARGET", picture2, picture1),
			   target_location = if_else(role1=="TARGET", "l", "r"),
			   test_language = "English",
			   location = "Oxford",
			   version = "british",
			   trial_type = case_when(trial_type=="CP" ~ "Cognate",
			   					   trial_type=="NP" ~ "Non-cognate",
			   					   trial_type=="UN" ~ "Unrelated"),
			   across(c(prime, target, distractor),
			   	   \(x) paste0(x, "_cdi"), 
			   	   .names = "{.col}_cdi"),
			   list = as.integer(list)) |> 
		mutate(trial = 1:n(), .by = list) |> 
		select(trial, test_language, version, list, trial_type,  
			   matches("prime|target|distractor"), audio, target_location)
	
	test_stimuli(stimuli)
	
	return(stimuli)
}



#' Get stimuli information
# get_stimuli_oxf <- function(gaze_processed, stimuli_cdi){
# 	
# 	stimuli <- gaze_processed |> 
# 		distinct(pick(matches("_stm"))) |> 
# 		left_join(stimuli_cdi, by = c("prime_stm" = "stimulus")) |> 
# 		rename_with(\(x) gsub("item_", "prime_", x)) |> 
# 		left_join(stimuli_cdi, by = c("target_stm" = "stimulus")) |> 
# 		rename_with(\(x) gsub("item_", "target_", x)) |> 
# 		left_join(stimuli_cdi, by = c("distractor_stm" = "stimulus")) |> 
# 		rename_with(\(x) gsub("item_", "distractor_", x)) |> 
# 		mutate(trial_id = row_number()) |> 
# 		relocate(trial_id)
# 	
# 	return(stimuli)
# }