#### utils: helper functions ###################################################

# evaluate if x is NOT an element of y -----------------------------------------
"%!in%" <- function(x, y){!(x %in% y)}

# replace NaNs with NAs --------------------------------------------------------
nan_to_na <- function(x) ifelse(is.nan(x), NA_real_, x)

# find first/last non-missing value in vector ----------------------------------
first_non_na <- function(x) ifelse(is.logical(first(x[!is.na(x)])), NA, first(x[!is.na(x)]))
last_non_na <- function(x) ifelse(is.logical(last(x[!is.na(x)])), NA, last(x[!is.na(x)]))

# big mark format
add_big_mark <- function(x) format(x, big.mark = ",", scientific = FALSE)

# custom ggplot theme ----------------------------------------------------------
theme_custom <- function(){
	theme(
		panel.background = element_rect(fill = "transparent"),
		panel.grid = element_blank(),
		#panel.grid = element_line(colour = "grey", linetype = "dotted"),
		panel.border = element_rect(fill = "transparent", colour = "black"),
		text = element_text(colour = "black", size = 15),
		axis.text = element_text(colour = "black")
	)
}

# get sample -------------------------------------------------------------------
sample_get <- function(
	google_email = NULL,
	pilot = FALSE
) {
	gs4_auth(google_email) 	# log into Google
	
	# retrieve participant info
	x <- suppressMessages({
		range_read(ss = "1JkhN4iBh3bi6PSReGGk9jSrVgDhZNOUmve6vNS2eEqE", sheet = "Participants", na = "") %>%
			drop_na(participant_id, date_test) %>%
			{if (!pilot) filter(., !pilot)}
	})
	return(x)
}

# summarise sample
sample_summary <- function(sample = NULL, ..., google_email = NULL) {
	
	# if not provided, generate sample
	if (is.null(sample)) {
		if (is.null(google_email)){
			google_email <- readline(prompt = "Google email: ")
			sample <- sample_get(google_email)
		} else {
			sample <- sample_get(google_email)
		}
	}
	
	# get all possible conditions
	sample_conditions <- sample %>%
		expand(...) %>%
		arrange(...)
	
	# summarise sample size by condition
	sample_distribution <- sample %>%
		count(...) %>%
		full_join(sample_conditions) %>%
		replace_na(list(n = 0))
	return(sample_distribution)
	
}

# import gaze data -------------------------------------------------------------
import_gaze_data <- function(file.names, relevant.variables) {
	x <- fread(file.names, sep = ",", na.strings = c("", "NaN", "NA")) %>% 
		clean_names() %>% 
		as_tibble() %>% 
		rename_all(~rename_cols(.)) %>% 
		mutate(
			participant = paste0("cognatepriming", participant),
			time = row_number()*(1000/120)
		) %>% 
		mutate_all(function(x) ifelse(is.nan(x), NA_real_, x)) %>% 
		mutate_at(vars(matches("l_|r_")), as.numeric) %>% 
		mutate_at(vars(contains("_v")), function(x) as.logical(as.integer(x))) %>% 
		select(starts_with(relevant.variables)) 
	return(x)
}


# replace column names
rename_cols <- function(x){
	str_replace_all(
		x,
		c(
			"system_time_stamp" = "time",
			"l_x" = "l_1",
			"l_y" = "l_2",
			"r_x" = "r_1",
			"r_y" = "r_2",
			"l_user_coord_z" = "l_origin_user_coord_3",
			"r_user_coord_z" = "r_origin_user_coord_3"
		)
	)
}

# evaluate of gaze is in AOI
gaze_in_aoi <- function(x, y, coords = c(710, 1210, 290, 790)) {
	(x >= coords[1] & x <= coords[2]) & (y >= coords[3] & y <= coords[4])
}


# summarise gaze by trial
gaze_summarise_by_trial <- function(
	sample = NULL,
	processed = NULL,
	google_email = NULL
){
	# if not provided, generate sample and processed gaze data
	if (is.null(processed)){
		if (is.null(sample)) {
			if (is.null(google_email)){
				google_email <- readline(prompt = "Google email: ")
				sample <- sample_get(google_email)
			} else {
				sample <- sample_get(google_email) %>%
					select(participant_id, id_db, location, age_group, lp, test_language, list, version)
			}
		}
		processed <- gaze_process(sample)
	}
	
	# summarise data
	gaze_summary_trial <- processed %>%
		group_by(participant, age_group, trial, lp, trial_type) %>%
		summarise(
			target = sum(target, na.rm = TRUE),
			distractor = sum(distractor, na.rm = TRUE),
			n = n(),
			.groups = "drop"
		) %>%
		rowwise() %>%
		mutate(
			p_target = prod(target, 1/sum(target, distractor, na.rm = TRUE), na.rm = TRUE),
			p_distractor = prod(distractor, 1/sum(target, distractor, na.rm = TRUE), na.rm = TRUE)
		) %>%
		ungroup()
	return(gaze_summary_trial)
}


# valid participants
get_valid_participants <- function(x, threshold = 2) {
	valid_participants <- x %>%
		filter(valid_trial, valid_other) %>%
		count(participant, trial_type) %>%
		mutate(valid_condition = n >= threshold) %>%
		group_by(participant) %>%
		summarise(
			valid_participant = all(valid_condition),
			.groups = "drop"
		) %>%
		filter(valid_participant) %>%
		pull(participant)
	return(mutate(x, valid_participant = participant %in% valid_participants))
}


# extract formants
extract_formants <- function(
	file, time_step = 0.001, n = 2, max_freq = 5500, window_length = 0.001, pre = 50,
	include_frames = TRUE, include_time = TRUE, dec = 3, include_intensity = TRUE,
	dec_intensity = 5, include_n_formats = FALSE, dec_freq = 3, include_bandwidths = FALSE
) {
	require(PraatR)
	require(janitor)
	
	file_paths <- list.files(file, pattern = ".wav", full.names = TRUE)
	file_names <- list.files(file, pattern = ".wav", full.names = FALSE)
	
	arguments1 <- list(time_step, n, max_freq, window_length, pre)
	arguments2 <- list(include_frames, include_time, dec, include_intensity, dec_intensity,
					   include_n_formats, dec_freq, include_bandwidths) 
	
	dir <- tempdir()
	dir_formants <- paste(dir, file_names, sep = "/") %>% str_replace(., ".wav", ".Formant")
	dir_formants_table <- paste(dir, file_names, sep = "/")  %>% str_replace(., ".wav", ".txt")
	
	fun1 <- function(x, y) praat("To Formant (burg)...", arguments = arguments1, input = x, output = y, overwrite = TRUE)
	fun2 <- function(x, y) praat("Down to Table...", arguments = arguments2, input = x, output = y, filetype = "tab-separated", overwrite = TRUE)
	
	invisible(map2(.x = as.list(file_paths), .y = as.list(dir_formants), .f = ~fun1(.x, .y)))
	invisible(map2(.x = as.list(dir_formants), .y = as.list(dir_formants_table), .f = ~fun2(.x, .y)))
	
	x <- map(
		dir_formants_table,
		~fread(., header = TRUE, sep = "\t", dec = ".", na.strings = "--undefined--")) %>%
		set_names(str_remove(file_names, ".wav")) %>%
		bind_rows(.id = "file") %>%
		clean_names() %>%
		rename_all(~str_remove(., "_hz")) %>%
		rename_all(~str_remove(., "_s")
		) 
	return(x)
}

# extract pitch
extract_pitch <- function(
	file,
	time_step = 0.001,
	pitch_floor = 50,
	pitch_ceiling = 800
) {
	require(PraatR)
	require(janitor)
	
	file_paths <- list.files({{ file }}, pattern = ".wav", full.names = TRUE)
	file_names <- list.files({{ file }}, pattern = ".wav", full.names = FALSE)
	
	pb <- progress_estimated(length(file_paths)*2)
	arguments <- list({{ time_step }}, {{ pitch_floor }}, {{ pitch_ceiling }})
	
	dir <- tempdir()
	dir1 <- paste(dir, file_names, sep = "/") %>% str_replace(., ".wav", ".Pitch")
	dir2 <- paste(dir, file_names, sep = "/")  %>% str_replace(., ".wav", ".PitchTier")
	
	fun1 <- function(x, y) {
		pb$tick()$print()
		praat("To Pitch...", arguments = arguments, input = x, output = y, overwrite = TRUE)
	}
	
	fun2 <- function(x, y) {
		pb$tick()$print()
		praat("Down to PitchTier", input = x, output = y, overwrite = TRUE, filetype = "headerless spreadsheet")
	}
	
	invisible(map2(.x = as.list(file_paths), .y = as.list(dir1), .f = ~fun1(.x, .y)))
	invisible(map2(.x = as.list(dir1), .y = as.list(dir2), .f = ~fun2(.x, .y)))
	x <- map(
		as.list(dir2),
		~fread(., header = FALSE, sep = "\t", dec = ".", na.strings = "--undefined--")) %>%
		set_names(str_remove(file_names, ".wav")) %>%
		bind_rows(.id = "file") %>%
		clean_names() %>%
		rename(time = v1, f0 = v2)
	return(x)
}

# download clearpond -----------------------------------------------------------
import_clearpond <- function(language = c("english", "dutch", "french", "spanish", "german")){
	require(tidyverse)
	require(janitor)
	urls <- tribble(
		~lang, ~url, ~data, ~header,
		"english", "https://clearpond.northwestern.edu/englishCPdatabase2.zip", "englishCPdatabase2.txt", "clearpondHeaders_EN.txt",
		"dutch", "https://clearpond.northwestern.edu/dutchCPdatabase2.zip", "dutchCPdatabase2.txt", "clearpondHeaders_NL.txt",
		"french", "https://clearpond.northwestern.edu/frenchCPdatabase2.zip", "frenchCPdatabase2.txt", "clearpondHeaders_FR.txt",
		"german", "https://clearpond.northwestern.edu/germanCPdatabase2.zip", "germanCPdatabase2.txt", "clearpondHeaders_DE.txt",
		"spanish", "https://clearpond.northwestern.edu/spanishCPdatabase2.zip", "spanishCPdatabase2.txt", "clearpondHeaders_SP.txt"
	) %>% 
		filter(lang %in% language)
	
	dir <- tempdir()
	files <- replicate(tempfile(), n = length(urls$lang))
	x <- pmap(
		.l = list(url = as.list(urls$url), file = as.list(files), data = as.list(urls$data), header = as.list(urls$header)),
		.f = function(url = .l[[1]], file = .l[[2]], data = .l[[3]], header = .l[[4]]) {
			download.file(url, destfile = file)
			unzip(zipfile = file, exdir = dir)
			headers <- c("word", read.delim(paste0(dir, .Platform$file.sep, header))[,1])
			d <- read.delim(paste0(dir, .Platform$file.sep, data)) %>% 
				`colnames<-`(., headers) %>% 
				as_tibble() %>% 
				mutate_at(vars(ends_with("W")), ~str_split(., pattern = ";"))
			return(d)
		}
	) %>% 
		set_names(language) %>% 
		bind_rows(.id = "language") %>% 
		clean_names()
	return(x)
}


